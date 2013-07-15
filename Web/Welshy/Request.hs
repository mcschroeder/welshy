{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy.Request where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types
import Network.Wai
import Text.Read (readEither)

import Web.Welshy.Action
import Web.Welshy.Response

-----------------------------------------------------------------------

mkEnv :: [Param] -> Request -> Env
mkEnv captures req = Env { _request = req
                         , _params  = captures ++ queryText req }

queryText :: Request -> [Param]
queryText = map (second $ fromMaybe "") . queryToQueryText . queryString

request :: Action Request
request = Action $ \r s -> return (Right $ _request r, s)

params :: Action [Param]
params = Action $ \r s -> return (Right $ _params r, s)

-----------------------------------------------------------------------

param :: Parsable a => Text -> Action a
param k = (lookup k <$> params) >>= \case
    Nothing  -> abortWith $ paramNotFound k
    Just raw -> case parseParam raw of
        Left msg -> abortWith $ parseError msg
        Right v  -> return v

paramNotFound :: Text -> Action ()
paramNotFound = undefined

parseError :: String -> Action ()
parseError = undefined

-- | Minimal complete definition: 'parseParam'
class Parsable a where
    parseParam :: Text -> Either String a

    -- | The default definition uses 'parseParam' to parse
    -- comma-delimited lists.
    parseParamList :: Text -> Either String [a]
    parseParamList = mapM parseParam . T.split (== ',')

instance Parsable a => Parsable [a] where
    parseParam = parseParamList

instance Parsable Char where
    parseParam t = case T.unpack t of
                       [c] -> Right c
                       _   -> Left "parseParam Char: no parse"
    parseParamList = Right . T.unpack

instance Parsable Text    where parseParam = Right
instance Parsable TL.Text where parseParam = Right . TL.fromStrict
instance Parsable Int     where parseParam = readEither . T.unpack
instance Parsable Integer where parseParam = readEither . T.unpack
instance Parsable Bool    where parseParam = readEither . T.unpack
instance Parsable Double  where parseParam = readEither . T.unpack
instance Parsable Float   where parseParam = readEither . T.unpack
