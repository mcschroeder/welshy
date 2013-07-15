{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Web.Welshy.Action where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Network.Wai

-----------------------------------------------------------------------

type Param = (Text, Text)

data Env = Env { _request :: Request, _params :: [Param] }

newtype Action a = Action { runAction :: Env -> Response
                                      -> IO (Either (Action ()) a, Response) }

execAction :: Action () -> Env -> Response -> IO Response
execAction m r s = runAction m r s >>= \case
    (Left  m1, __) -> execAction m1 r s
    (Right __, s1) -> return s1

instance Functor Action where
    fmap f m = Action $ \r s ->
        fmap (\ ~(a, s1) -> (fmap f a, s1)) $ runAction m r s

instance Applicative Action where
    pure = return
    (<*>) = ap

instance Alternative Action where
    empty = mzero
    (<|>) = mplus

instance Monad Action where
    return a = Action $ \_ s -> return (Right a, s)
    m >>= k  = Action $ \r s -> do
        runAction m r s >>= \case
            (Left  a, s1) -> return (Left a, s1)
            (Right a, s1) -> do
                ~(b, s2) <- runAction (k a) r s1
                return (b, s2)

instance MonadPlus Action where
    mzero       = Action $ \_ s -> return (Left $ fail "mzero", s)
    m `mplus` n = Action $ \r s -> do
        runAction m r s >>= \case
            (Right a, s1) -> return (Right a, s1)
            (Left  _, __) -> do
                ~(b, s2) <- runAction n r s
                return (b, s2)

-----------------------------------------------------------------------

abortWith :: Action () -> Action a
abortWith m = Action $ \_ s -> return (Left m, s)
