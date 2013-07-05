{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy
    ( welshy, welshyApp

    , Welshy
    , middleware
    , RoutePattern, route
    , get, post, put, patch, delete, head, options

    , RequestReader
    , param
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
import Control.Monad.Trans.Error
import Control.Monad.Trans.State hiding (get, put)
import Data.Conduit
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Prelude hiding (head)

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

get     = route GET
post    = route POST
put     = route PUT
patch   = route PATCH
delete  = route DELETE
head    = route HEAD  -- TODO: clashes with Prelude.head
options = route OPTIONS

type RoutePattern = T.Text

route :: StdMethod -> RoutePattern -> RequestReader e a
      -> (a -> ResponseWriter ()) -> (e -> ResponseWriter ())
      -> Welshy ()
route met pat reader ok err =
    middleware $ \app req -> case matchRoute met pat req of
        Just captures -> lift $ handle internalServerError $ do
            result <- runRequestReader reader captures req
            let writer = either err ok result
            execResponseWriter writer
        Nothing -> app req
    where
        internalServerError :: SomeException -> IO Response
        internalServerError e = execResponseWriter $ do
            status internalServerError500
            text $ TL.pack $ show e
            --const $ return $ ResponseBuilder status500 [] mempty

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
