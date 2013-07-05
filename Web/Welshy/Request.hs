{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Welshy.Request where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Either
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Network.Wai

import qualified Data.Text as T

-----------------------------------------------------------------------

type Param = (T.Text, T.Text)

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

param :: (Error e , Parseable a) => T.Text -> RequestReader e a
param k = RequestReader $ do
    (lift $ lookup k <$> asks params) >>= \case
        Nothing  -> throwError $ strMsg "Welshy.param: not found"
        Just raw -> case parseParam raw of
            Left msg -> throwError $ strMsg $ T.unpack msg
            Right v  -> return v

class Parseable a where
    parseParam :: T.Text -> Either T.Text a

instance Parseable T.Text where parseParam = Right
instance Parseable Int    where parseParam = readEither

-- stolen as-is from Scotty
readEither :: (Read a) => T.Text -> Either T.Text a
readEither t = case [ x | (x,"") <- reads (T.unpack t) ] of
                [x] -> Right x
                []  -> Left "Welshy.readEither: no parse"
                _   -> Left "Welshy.readEither: ambiguous parse"

-----------------------------------------------------------------------

-- | Aborts request handling with the given error value.
err :: Error e => e -> RequestReader e a
err = RequestReader . throwError
