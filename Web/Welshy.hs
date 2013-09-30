{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy
    ( welshy, welshyApp

      -- * Middleware & Routing
    , Welshy, middleware, route, RoutePattern
    , get, post, put, patch, delete, head, options

      -- * Actions
    , Action
    , halt, pass
    , catchIO

      -- ** Request
    , request, body
    , capture, captures
    , queryParam, maybeQueryParam, queryParams
    , jsonParam, maybeJsonParam, jsonParams, jsonData
    , bearerAuth

      -- ** Response
    , status, header
    , text, text'
    , html, html'
    , json
    , file, filePart
    , source

      -- * Parameter Parsing
    , Param
    , FromText(..), maybeFromText
    ) where

import Control.Applicative
import Control.Exception
import qualified Control.Exception.Lifted as Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer hiding (pass)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO

import Prelude hiding (head)

import Web.Welshy.Action
import Web.Welshy.FromText
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
-- > text $ error "wat"
--
-- (The exception will be logged to stderr though.)

-- TODO: clarify this in user-visible documentation.

-----------------------------------------------------------------------

-- | We use this monad to compose WAI 'Middleware', using the 'middleware'
-- and 'route' functions.
newtype Welshy a = Welshy (Writer [Middleware] a)
    deriving (Functor, Applicative, Monad)

-- | Run a Welshy app using the Warp server.
welshy :: Port -> Welshy () -> IO ()
welshy p w = do
    putStr "Aye, Dwi iawn 'n feddw!"
    putStrLn $ " (port " ++ show p ++ ") (ctrl-c to quit)"
    run p (welshyApp w)

-- | Turns a Welshy app into a WAI 'Application'.
welshyApp :: Welshy () -> Application
welshyApp (Welshy w) = foldr id notFound (catchError : execWriter w)
    where
        notFound :: Application
        notFound _ = return $ ResponseBuilder notFound404 [] mempty

        -- see Note [Exception Handling]
        catchError :: Middleware
        catchError app req = Lifted.catch (app req) $ \e -> do
            liftIO $ hPrint stderr (e :: SomeException)
            return $ ResponseBuilder status500 [] mempty

-----------------------------------------------------------------------

-- | Insert middleware into the app. Note that unlike in Scotty,
-- each middleware is run at the point of insertion.
middleware :: Middleware -> Welshy ()
middleware = Welshy . tell . pure

get :: RoutePattern -> Action () -> Welshy ()
get = route GET

post :: RoutePattern -> Action () -> Welshy ()
post = route POST

put :: RoutePattern -> Action () -> Welshy ()
put = route PUT

patch :: RoutePattern -> Action () -> Welshy ()
patch = route PATCH

delete :: RoutePattern -> Action () -> Welshy ()
delete = route DELETE

head :: RoutePattern -> Action () -> Welshy ()
head = route HEAD

options :: RoutePattern -> Action () -> Welshy ()
options = route OPTIONS

-- | Sinatra-style route pattern. Named parameters are prepended with
-- a colon (e.g. @\"\/users\/:id\"@) and can be accessed with 'capture'.
type RoutePattern = Text

-- | Define a route for an HTTP method and URL pattern that runs the given
-- action. Routes are matched in the order they are defined. If no route
-- matches, a 404 response is returned.
route :: StdMethod -> RoutePattern -> Action () -> Welshy ()
route met pat act = middleware $ \nextApp req ->
    case matchRoute met pat req of
        Nothing   -> nextApp req
        Just caps -> execAction act caps nextApp req

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
