{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy.Response where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Conduit
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-----------------------------------------------------------------------

newtype ResponseWriter a = ResponseWriter (StateT Response IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

execResponseWriter :: ResponseWriter () -> IO Response
execResponseWriter (ResponseWriter writer) = execStateT writer def

instance Default Response where
    def = ResponseBuilder ok200 [] mempty

-- | Set the HTTP status of the response. The default is 'ok200'.
status :: Status -> ResponseWriter ()
status s = ResponseWriter $ modify $ \case
    (ResponseBuilder _ h b)    -> ResponseBuilder s h b
    (ResponseFile    _ h f fp) -> ResponseFile    s h f fp
    (ResponseSource  _ h cs)   -> ResponseSource  s h cs

-- | Add or replace one of the response headers.
header :: HeaderName -> BS.ByteString -> ResponseWriter ()
header k v = ResponseWriter $ modify $ \case
    (ResponseBuilder s h b)    -> ResponseBuilder s (update h k v) b
    (ResponseFile    s h f fp) -> ResponseFile    s (update h k v) f fp
    (ResponseSource  s h cs)   -> ResponseSource  s (update h k v) cs
    where update h k v = (k,v) : filter ((/= k) . fst) h

-- | Set the response body to the given lazy 'TL.Text'
-- and the content-type to \"text/plain\".
text :: TL.Text -> ResponseWriter ()
text t = do
    header hContentType "text/plain"
    _builder $ fromLazyByteString $ TL.encodeUtf8 t

-- | Set the response body to the given lazy 'TL.Text'
-- and the content-type to \"text/html\".
html :: TL.Text -> ResponseWriter ()
html t = do
    header hContentType "text/html"
    _builder $ fromLazyByteString $ TL.encodeUtf8 t

-- | Sends the given file as the response.
file :: FilePath -> ResponseWriter ()
file = flip _file Nothing

-- | 'filePart' @f offset byteCount@ sends @byteCount@ bytes of the file @f@,
-- beginning at @offset@, as the response.
filePart :: FilePath -> Integer -> Integer -> ResponseWriter ()
filePart f offset byteCount = _file f (Just $ FilePart offset byteCount)

_file :: FilePath -> Maybe FilePart -> ResponseWriter ()
_file f part = ResponseWriter $ modify $ \case
    (ResponseBuilder s h _)   -> ResponseFile s h f part
    (ResponseFile    s h _ _) -> ResponseFile s h f part
    (ResponseSource  s h _)   -> ResponseFile s h f part

_builder :: Builder -> ResponseWriter ()
_builder b = ResponseWriter $ modify $ \case
    (ResponseBuilder s h _)   -> ResponseBuilder s h b
    (ResponseFile    s h _ _) -> ResponseBuilder s h b
    (ResponseSource  s h _)   -> ResponseBuilder s h b

-- | Set the response body to the given 'Source'.
source :: Source (ResourceT IO) (Flush Builder) -> ResponseWriter ()
source src = ResponseWriter $ modify $ \case
    (ResponseBuilder s h _)   -> ResponseSource s h src
    (ResponseFile    s h _ _) -> ResponseSource s h src
    (ResponseSource  s h _)   -> ResponseSource s h src
