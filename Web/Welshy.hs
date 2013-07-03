{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy
    ( welshy, welshyApp

    , Welshy
    , middleware, addroute
    , get, post, put, patch, delete, head, options

    , RequestReader
    , err

    , ResponseWriter
    , status
    , text, html
    , file, filePart
    , source
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (get, put)
import Data.Conduit
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.Text.Lazy as TL

import Web.Welshy.Request
import Web.Welshy.Response

newtype Welshy a = Welshy (StateT [Middleware] IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

welshy :: Port -> Welshy () -> IO ()
welshy p w = do
    putStr "Aye, Dwi iawn 'n feddw!"
    putStrLn $ " (port " ++ show p ++ ") (ctrl-c to quit)"
    let settings = defaultSettings { settingsPort = p }
    runSettings settings =<< welshyApp w

welshyApp :: Welshy () -> IO Application
welshyApp (Welshy w) = do
    ms <- execStateT w []
    return $ foldl (flip ($)) (const notFound) ms
    where
        notFound = return $ ResponseBuilder notFound404 [] mempty

-----------------------------------------------------------------------

middleware :: Middleware -> Welshy ()
middleware = Welshy . modify . (:)

type Pattern = String

addroute :: StdMethod -> Pattern -> RequestReader e a
         -> (a -> ResponseWriter ()) -> (e -> ResponseWriter ())
         -> Welshy ()
addroute met pat reader ok err = middleware $ route met pat reader ok err

get     = addroute GET
post    = addroute POST
put     = addroute PUT
patch   = addroute PATCH
delete  = addroute DELETE
--head    = addroute HEAD  -- clashes with Prelude.head
options = addroute OPTIONS

route :: StdMethod -> Pattern -> RequestReader e a
      -> (a -> ResponseWriter ()) -> (e -> ResponseWriter ())
      -> Application -> Request -> ResourceT IO Response
route met pat reader ok err app req =
    case matchRoute met pat req of
        Just params -> lift $ runRoute params `catch` exHandler
        Nothing -> app req
    where
        runRoute _ = do
            writer <- either err ok <$> runRequestReader reader req
            execResponseWriter writer

        exHandler :: SomeException -> IO Response
        exHandler = const $ return $ ResponseBuilder status500 [] mempty

-- TODO: pattern matching
type Param = ()

matchRoute :: StdMethod -> Pattern -> Request -> Maybe [Param]
matchRoute met pat req =
    if Right met == parseMethod (requestMethod req)
        then Just []
        else Nothing
