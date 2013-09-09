module Web.Welshy.FromText where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Int
import Data.Word
import Text.Read (readEither)

-- | A type that can be converted from a strict 'Text' value.
-- Used for parsing route captures, query parameters, header values, etc.
--
-- Minimal complete definition: 'fromText'.
class FromText a where
    fromText :: Text -> Either String a

    -- | Allows a specialized way of parsing lists of values.
    -- The default definition uses 'fromText' to parse comma-delimited lists.
    fromTextList :: Text -> Either String [a]
    fromTextList = mapM fromText . T.split (== ',')

-- | > maybeFromText = either (const Nothing) Just . fromText
maybeFromText :: FromText a => Text -> Maybe a
maybeFromText = either (const Nothing) Just . fromText

instance FromText a => FromText [a] where
    fromText = fromTextList

instance FromText Char where
    fromText t = case T.unpack t of
                       [c] -> Right c
                       _   -> Left "fromText Char: no parse"
    fromTextList = Right . T.unpack

instance FromText Text    where fromText = Right
instance FromText TL.Text where fromText = Right . TL.fromStrict
instance FromText Int     where fromText = readEither . T.unpack
instance FromText Int8    where fromText = readEither . T.unpack
instance FromText Int16   where fromText = readEither . T.unpack
instance FromText Int32   where fromText = readEither . T.unpack
instance FromText Int64   where fromText = readEither . T.unpack
instance FromText Integer where fromText = readEither . T.unpack
instance FromText Word    where fromText = readEither . T.unpack
instance FromText Word8   where fromText = readEither . T.unpack
instance FromText Word16  where fromText = readEither . T.unpack
instance FromText Word32  where fromText = readEither . T.unpack
instance FromText Word64  where fromText = readEither . T.unpack
instance FromText Bool    where fromText = readEither . T.unpack
instance FromText Double  where fromText = readEither . T.unpack
instance FromText Float   where fromText = readEither . T.unpack
