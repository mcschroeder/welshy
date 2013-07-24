{-# LANGUAGE LambdaCase #-}

module Web.Welshy.Action where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Network.Wai

-----------------------------------------------------------------------

type Param = (Text, Text)

data Result a = Ok a Response | Halt (Action ()) | Pass

newtype Action a = Action { runAction :: [Param] -> Request -> Response
                                      -> IO (Result a) }

instance Functor Action where
    fmap f m = Action $ \p r s -> runAction m p r s >>= \case
        Ok a s1 -> return $ Ok (f a) s1
        Halt m1 -> return $ Halt m1
        Pass    -> return $ Pass

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
        Halt s1 -> return $ Halt s1
        Pass    -> return $ Pass

    fail msg = halt $ error msg

halt :: Action () -> Action a
halt m = Action $ \_ _ _ -> return $ Halt m

pass :: Action a
pass = Action $ \_ _ _ -> return Pass

instance MonadPlus Action where
    mzero       = fail "mzero"
    m `mplus` n = Action $ \p r s -> runAction m p r s >>= \case
        Ok a s1 -> return $ Ok a s1
        Halt __ -> runAction n p r s
        Pass    -> runAction n p r s

instance MonadIO Action where
    liftIO m = Action $ \_ _ s -> do
        a <- m
        return $ Ok a s
