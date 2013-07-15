{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy
    ( Welshy, welshy, welshyApp
    , middleware

    , Action, abortWith

    , RoutePattern, route
    , get, post, put, patch, delete, head, options

    , Parsable, param

    , status, header
    , text, text', html, html'
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
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Prelude hiding (head)

import Web.Welshy.Action
import Web.Welshy.Request
import Web.Welshy.Response

-----------------------------------------------------------------------

-- TODO: WriterT ?
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

type RoutePattern = Text

route :: StdMethod -> RoutePattern -> Action () -> Welshy ()
route met pat act = middleware $ \app req ->
    case matchRoute met pat req of
        Nothing -> app req
        Just captures -> lift $ catch (execAction act env def)
                                      (\e -> execAction (err e) env def)
            where
                env = mkEnv captures req
                err = defaultExceptionHandler

defaultExceptionHandler :: SomeException -> Action ()
defaultExceptionHandler e = do
    status internalServerError500
    text' $ T.pack $ show e

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
