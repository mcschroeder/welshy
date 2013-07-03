{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Welshy

import Control.Monad
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types

data MyError = InvalidWordError String
             | SomeOtherError Int

main = welshy 3000 $ do {

; get "/test"
(do
    let word = "hell"
    err $ SomeOtherError 42
    unless (word == "hello") (err $ InvalidWordError word)
    return "world"
)
(text)
(\case
    (InvalidWordError x) -> do
        status badRequest400
        text $ TL.pack $ x ++ "? I don't think so..."
)

}
