{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy.Request where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types
import Network.Wai
import Text.Read (readEither)

import Web.Welshy.Action
import Web.Welshy.Response

-----------------------------------------------------------------------

queryText :: Request -> [Param]
queryText = map (second $ fromMaybe "") . queryToQueryText . queryString

request :: Action Request
request = Action $ \_ r s -> return $ Ok r s

params :: Action [Param]
params = Action $ \p _ s -> return $ Ok p s

-----------------------------------------------------------------------

-- TODO:
-- if a query param isn't found -> maybe?
--  should we separate param & queryParam?
--  explicit matching on existence of query params like RFC 6570 ?
-- then we could also maybe extract documentaton from just the route pattern!
param :: Parsable a => Text -> Action a
param k = (lookup k <$> params) >>= \case
    Nothing  -> fail ("unknown parameter: " ++ T.unpack k)
    Just raw -> case parseParam raw of
        Left msg -> pass
        Right v  -> return v

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

-----------------------------------------------------------------------

bearerAuth :: Parsable a => Action a
bearerAuth = do
    headers <- requestHeaders <$> request
    maybe (halt $ status unauthorized401) return $ do
        credentials <- lookup hAuthorization headers
        let (scheme, raw) = BS.splitAt 7 credentials
        guard (scheme == "Bearer ")
        either (const Nothing) (Just) (parseParam $ T.decodeUtf8 raw)
