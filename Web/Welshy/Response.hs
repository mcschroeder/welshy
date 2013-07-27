{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy.Response where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Types
import Network.Wai

import Web.Welshy.Action

-----------------------------------------------------------------------

-- | Set the HTTP status of the response. The default is 'ok200'.
status :: Status -> Action ()
status s = modifyResponse $ \case
    (ResponseBuilder _ h b)    -> ResponseBuilder s h b
    (ResponseFile    _ h f fp) -> ResponseFile    s h f fp
    (ResponseSource  _ h cs)   -> ResponseSource  s h cs

-- | Add or replace one of the response headers.
header :: HeaderName -> BS.ByteString -> Action ()
header k v = modifyResponse $ \case
    (ResponseBuilder s h b)    -> ResponseBuilder s (update h k v) b
    (ResponseFile    s h f fp) -> ResponseFile    s (update h k v) f fp
    (ResponseSource  s h cs)   -> ResponseSource  s (update h k v) cs
    where update h k v = (k,v) : filter ((/= k) . fst) h

-- | Set the response body to the given lazy 'TL.Text'
-- and the content-type to @text/plain@.
text :: TL.Text -> Action ()
text t = do
    header hContentType "text/plain"
    _builder $ fromLazyByteString $ TL.encodeUtf8 t

-- | Like 'text' but with a strict 'Text' value.
text' :: Text -> Action ()
text' t = do
    header hContentType "text/plain"
    _builder $ fromByteString $ T.encodeUtf8 t

-- | Set the response body to the given lazy 'TL.Text'
-- and the content-type to @text/html@.
html :: TL.Text -> Action ()
html t = do
    header hContentType "text/html"
    _builder $ fromLazyByteString $ TL.encodeUtf8 t

-- | Like 'html' but with a strict 'Text' value.
html' :: Text -> Action ()
html' t = do
    header hContentType "text/html"
    _builder $ fromByteString $ T.encodeUtf8 t

-- | Sends the given file as the response.
file :: FilePath -> Action ()
file = flip _file Nothing

-- | 'filePart' @f offset byteCount@ sends @byteCount@ bytes of the file @f@,
-- beginning at @offset@, as the response.
filePart :: FilePath -> Integer -> Integer -> Action ()
filePart f offset byteCount = _file f (Just $ FilePart offset byteCount)

_file :: FilePath -> Maybe FilePart -> Action ()
_file f part = modifyResponse $ \case
    (ResponseBuilder s h _)   -> ResponseFile s h f part
    (ResponseFile    s h _ _) -> ResponseFile s h f part
    (ResponseSource  s h _)   -> ResponseFile s h f part

_builder :: Builder -> Action ()
_builder b = modifyResponse $ \case
    (ResponseBuilder s h _)   -> ResponseBuilder s h b
    (ResponseFile    s h _ _) -> ResponseBuilder s h b
    (ResponseSource  s h _)   -> ResponseBuilder s h b

-- | Set the response body to the given 'Source'.
source :: Source (ResourceT IO) (Flush Builder) -> Action ()
source src = modifyResponse $ \case
    (ResponseBuilder s h _)   -> ResponseSource s h src
    (ResponseFile    s h _ _) -> ResponseSource s h src
    (ResponseSource  s h _)   -> ResponseSource s h src
