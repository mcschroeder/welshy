{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Text.Lazy as T
import Network.HTTP.Types
import Web.Welshy

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = welshy 3000 $ do
    get "/fibs" $ do
        offset <- queryParam "offset" <|> return 0
        length <- queryParam "length"

        when (offset < 0 || length < 0)
             (halt $ status badRequest400)

        when (offset + length > 1000)
             (halt $ status requestedRangeNotSatisfiable416)

        let result = take length $ drop offset fibs
        text $ T.pack $ show result
