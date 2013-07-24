{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy
    ( Welshy, welshy, welshyApp
    , middleware

    , Action, halt, pass

    , RoutePattern, route
    , get, post, put, patch, delete, head, options

    , Parsable, param
    , bearerAuth

    , status, header
    , text, text', html, html'
    , file, filePart
    , source
    ) where

import Blaze.ByteString.Builder (fromByteString)
import Control.Applicative
import Control.Exception
import qualified Control.Exception.Lifted as Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer hiding (pass)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Default
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO

import Prelude hiding (head)

import Web.Welshy.Action
import Web.Welshy.Request
import Web.Welshy.Response

-----------------------------------------------------------------------

-- Note [Exception handling]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Ideally, all exceptions would be caught by the server and a 500 response
-- sent to the client. Alas, due to lazyness this is not possible: any
-- exceptions occuring inside a 'ResponseBuilder' would need to be caught on
-- a higher level. ('Warp' provides 'settingsOnException', but at that point
-- the connection has already been dropped and we can't send a response to
-- the client anymore.)
--
-- Effectively, this means that if something like the following happens
-- the client will simply get an empty reply:
--
-- > test $ error "wat"
--
-- (The exception will be logged to stderr though.)

-----------------------------------------------------------------------

-- this is actually just a Monoid, but the do notation is so very convenient
newtype Welshy a = Welshy (Writer [Middleware] a)
    deriving (Functor, Applicative, Monad)

welshy :: Port -> Welshy () -> IO ()
welshy p w = do
    putStr "Aye, Dwi iawn 'n feddw!"
    putStrLn $ " (port " ++ show p ++ ") (ctrl-c to quit)"
    let settings = defaultSettings { settingsPort = p }
    runSettings settings =<< welshyApp w

welshyApp :: Welshy () -> IO Application
welshyApp (Welshy w) = do
    let ms = defaultExceptionHandler : execWriter w
    return $ foldr id (const notFound) ms
    where
        notFound = return $ ResponseBuilder notFound404 [] mempty

-- see Note [Exception handling]
defaultExceptionHandler :: Middleware
defaultExceptionHandler app req = Lifted.catch (app req) $ \e -> do
    liftIO $ hPrint stderr (e :: SomeException)
    return $ ResponseBuilder status500 [] mempty

-----------------------------------------------------------------------

middleware :: Middleware -> Welshy ()
middleware = Welshy . tell . pure

execAction :: Action () -> [Param] -> Middleware
execAction act params nextApp req =
    (lift $ runAction act params req def) >>= \case
        Ok _ res  -> return res
        Halt act' -> execAction act' params nextApp req
        Pass      -> nextApp req

get     = route GET
post    = route POST
put     = route PUT
patch   = route PATCH
delete  = route DELETE
head    = route HEAD  -- TODO: clashes with Prelude.head (who cares?)
options = route OPTIONS

type RoutePattern = Text

route :: StdMethod -> RoutePattern -> Action () -> Welshy ()
route met pat act = middleware $ \nextApp req ->
    case matchRoute met pat req of
        Nothing       -> nextApp req
        Just captures -> execAction act captures nextApp req

matchRoute :: StdMethod -> RoutePattern -> Request -> Maybe [Param]
matchRoute met pat req =
    if Right met == parseMethod (requestMethod req)
        then go (filter (/= T.empty) $ T.split (=='/') pat) (pathInfo req) []
        else Nothing
    where
        go []     []     prs  = Just prs
        go []     _      _    = Nothing
        go _      []     _    = Nothing
        go (p:ps) (r:rs) prs
            | p == r          = go ps rs prs
            | T.null p        = Nothing
            | T.head p == ':' = go ps rs $ (T.tail p, r) : prs
            | otherwise       = Nothing
