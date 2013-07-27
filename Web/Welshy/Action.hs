{-# LANGUAGE LambdaCase #-}

module Web.Welshy.Action where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai

-----------------------------------------------------------------------

type Param = (Text, Text)

data Env = Env { _params  :: [Param]
               , _body    :: BL.ByteString
               , _request :: Request }

data Result a = Ok a Response | Halt (Action ()) | Pass

newtype Action a = Action { runAction :: Env -> Response -> IO (Result a) }

instance Functor Action where
    fmap f m = Action $ \r s -> runAction m r s >>= \case
        Ok a s1 -> return $ Ok (f a) s1
        Halt m1 -> return $ Halt m1
        Pass    -> return $ Pass

instance Applicative Action where
    pure = return
    (<*>) = ap

-- TODO: this instance should be highlighted in the documentation
instance Alternative Action where
    empty = mzero
    (<|>) = mplus

instance Monad Action where
    return a = Action $ \_ s -> return $ Ok a s
    m >>= k  = Action $ \r s -> runAction m r s >>= \case
        Ok a s1 -> runAction (k a) r s1
        Halt s1 -> return $ Halt s1
        Pass    -> return $ Pass

    fail msg = halt $ error msg

-- | Stop running the current action and continue with another one.
-- The other action will live in the same request environment and can access
-- the same route parameters, but it will start with a fresh default response.
--
-- This is incredibly useful for error handling. For example:
--
-- > get "/user/:id" $ do
-- >     id <- param "id"
-- >     getUserFromDatabase id >>= \case
-- >         Nothing   -> halt $ status notFound404
-- >         Just user -> json user
halt :: Action () -> Action a
halt m = Action $ \_ _ -> return $ Halt m

-- | Stop the current action and continue with the next matching route.
pass :: Action a
pass = Action $ \_ _ -> return Pass

instance MonadPlus Action where
    mzero       = fail "mzero"
    m `mplus` n = Action $ \r s -> runAction m r s >>= \case
        Ok a s1 -> return $ Ok a s1
        Halt __ -> runAction n r s
        Pass    -> runAction n r s

instance MonadIO Action where
    liftIO m = Action $ \_ s -> do
        a <- m
        return $ Ok a s
