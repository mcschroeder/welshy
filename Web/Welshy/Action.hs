{-# LANGUAGE LambdaCase #-}

module Web.Welshy.Action where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Network.Wai

-----------------------------------------------------------------------

type Param = (Text, Text)

data Result a = Ok a Response | Fail (Action ()) | Next

newtype Action a = Action { runAction :: [Param] -> Request -> Response
                                      -> IO (Result a) }

instance Functor Action where
    fmap f m = Action $ \p r s -> runAction m p r s >>= \case
        Ok a s1 -> return $ Ok (f a) s1
        Fail m1 -> return $ Fail m1
        Next    -> return $ Next

instance Applicative Action where
    pure = return
    (<*>) = ap

instance Alternative Action where
    empty = mzero
    (<|>) = mplus

instance Monad Action where
    return a = Action $ \_ _ s -> return $ Ok a s
    m >>= k  = Action $ \p r s -> runAction m p r s >>= \case
        Ok a s1 -> runAction (k a) p r s1
        Fail s1 -> return $ Fail s1
        Next    -> return $ Next

    fail msg = failWith $ error msg

failWith :: Action () -> Action a
failWith m = Action $ \_ _ _ -> return $ Fail m

next :: Action a
next = Action $ \_ _ _ -> return Next

instance MonadPlus Action where
    mzero       = fail "mzero"
    m `mplus` n = Action $ \p r s -> runAction m p r s >>= \case
        Ok a s1 -> return $ Ok a s1
        Fail __ -> runAction n p r s
        Next    -> runAction n p r s
