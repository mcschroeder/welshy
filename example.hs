{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Welshy

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Error
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types

data MyError = InvalidWordError String
             | SomeOtherError Int
             | MyError String

instance Error MyError where
    strMsg = MyError

main = welshy 3000 $ do {

; get "/test/:word"
(do
    word <- param "blah" <|> param "word"
    --err $ SomeOtherError 42
    unless (word == "hello") (err $ InvalidWordError (T.unpack word))
    return $ TL.fromStrict $ T.reverse word
)
(text)
(\case
    (InvalidWordError x) -> do
        status badRequest400
        text $ TL.pack $ x ++ "? I don't think so..."
    (SomeOtherError _) -> do
        status status418
        text "some other error"
)

}
