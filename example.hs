{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Welshy

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Types

import Control.Monad.Trans.Resource
import Data.Conduit.Lazy
import Data.Conduit.List
import Data.Conduit
import Network.Wai

blah :: Action Text
blah = undefined

main = welshy 3000 $ do

{ ---------------------------------------------------------------------

; get "/json" $ do
    name <- jsonParam "name"
    age <- maybeJsonParam "age" :: Action (Maybe Int)
    case age of
        Nothing -> text $ "Please state your age!"
        Just x | x > 18 -> text $ mconcat ["Hello ", name]
        _ -> text "You are too young."

; get "/query" $ do
    name <- queryParam "name"
    text' $ mconcat ["hello ", T.pack name]

; put "/echo" $ do
    text =<< TL.decodeUtf8 <$> body

; get "/secure" $ do
    token <- bearerAuth
    liftIO $ putStrLn "why hello there"
    if token == (123 :: Int)
        then text "welcome"
        else status forbidden403

; get "/a" $ do
    -- ~a <- blah
    --status undefined
    --file $ error "wat"
    text' $ error "wat"

; get "/a" $ do
    text "a2"

; get "/b" $ do
    return True >>= \case
        False -> status status418
    text "b"

; get "/c" $ do
    text $ case True of
        False -> "b"

; get "/test/:word" $ do
    word <- mzero <|> capture "blah" <|> capture "word" <|> capture "wat"
    --mzero
    unless (word == "hello") $ halt $ do
        status badRequest400
        text' $ mconcat [T.pack word, "? I don't think so..."]

    text' $ T.pack $ reverse word

; get "/test2/:eid" $ do
    eid <- capture "eid" :: Action Int

    what <- (queryParam "what" :: Action Int) -- <|> (halt $ status status202)

    if eid > 2
        then text "greater than 2"
        else text "too small"


} ---------------------------------------------------------------------
