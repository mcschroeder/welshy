{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Welshy.Request where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Data.Default
import Data.Monoid
import Network.HTTP.Types
import Network.Wai

newtype RequestReader e a = RequestReader (EitherT e (ReaderT Request IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

runRequestReader :: RequestReader e a -> Request -> IO (Either e a)
runRequestReader (RequestReader reader) = runReaderT (runEitherT reader)

-- | Aborts request handling with the given error value.
err :: e -> RequestReader e ()
err = RequestReader . left
