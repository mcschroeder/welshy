{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy.Request where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Data.Default
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types
import Network.Wai
import Text.Read (readEither)

-----------------------------------------------------------------------

type Param = (Text, Text)

data Env = Env { request :: Request,
                 params  :: [Param] }

newtype RequestReader e a = RequestReader (ErrorT e (ReaderT Env IO) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadIO)

runRequestReader :: RequestReader e a -> [Param] -> Request -> IO (Either e a)
runRequestReader (RequestReader reader) captures req =
    runReaderT (runErrorT reader) (Env req params)
    where
        params = captures ++ queryparams
        queryparams = [] -- TODO

-----------------------------------------------------------------------

param :: (Error e , Parseable a) => Text -> RequestReader e a
param k = RequestReader $ do
    (lift $ lookup k <$> asks params) >>= \case
        Nothing  -> throwError $ strMsg "Welshy.param: not found"
        Just raw -> case parseParam raw of
            Left msg -> throwError $ strMsg msg
            Right v  -> return v

-- | Minimal complete definition: 'parseParam'
class Parseable a where
    parseParam :: Text -> Either String a

    -- | The default definition uses 'parseParam' to parse
    -- comma-delimited lists.
    parseParamList :: Text -> Either String [a]
    parseParamList = mapM parseParam . T.split (== ',')

instance Parseable a => Parseable [a] where
    parseParam = parseParamList

instance Parseable Char where
    parseParam t = case T.unpack t of
                       [c] -> Right c
                       _   -> Left "parseParam Char: no parse"
    parseParamList = Right . T.unpack

instance Parseable Text    where parseParam = Right
instance Parseable TL.Text where parseParam = Right . TL.fromStrict
instance Parseable Int     where parseParam = readEither . T.unpack
instance Parseable Integer where parseParam = readEither . T.unpack
instance Parseable Bool    where parseParam = readEither . T.unpack
instance Parseable Double  where parseParam = readEither . T.unpack
instance Parseable Float   where parseParam = readEither . T.unpack

-----------------------------------------------------------------------

-- | Aborts request handling with the given error value.
err :: Error e => e -> RequestReader e a
err = RequestReader . throwError
