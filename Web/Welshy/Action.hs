{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Welshy.Action where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Conduit.Lazy
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai

-----------------------------------------------------------------------

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

-----------------------------------------------------------------------

-- | A route, query or form parameter and its value.
type Param = (Text, Text)

data Env = Env { _captures    :: [Param]
               , _queryParams :: [Param]
               , _body        :: BL.ByteString
               , _jsonParams  :: Maybe A.Object
               , _request     :: Request }

mkEnv :: [Param] -> Request -> ResourceT IO Env
mkEnv _captures _request = do
    _body <- BL.fromChunks <$> lazyConsume (requestBody _request)
    let _queryParams = queryText _request ++ formParams _body _request
        _jsonParams = either (const Nothing) Just (A.eitherDecode _body)
    return Env {..}

queryText :: Request -> [Param]
queryText = map (second $ fromMaybe "") . queryToQueryText . queryString

formParams :: BL.ByteString -> Request -> [Param]
formParams body req =
    case lookup hContentType (requestHeaders req) of
        Just "application/x-www-form-urlencoded" ->
            map (second $ fromMaybe "") $ queryToQueryText $
            parseQuery $ BL.toStrict $ body
        _ -> []


execAction :: Action () -> [Param] -> Middleware
execAction act captures nextApp req = run act =<< mkEnv captures req
    where
        run :: Action () -> Env -> ResourceT IO Response
        run act env = (lift $ runAction act env okRes) >>= \case
            Ok _ res  -> return res
            Halt act' -> run act' env
            Pass      -> nextApp req

        okRes :: Response
        okRes = ResponseBuilder ok200 [] mempty

-----------------------------------------------------------------------

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

-- | Get the raw WAI 'Request'.
request :: Action Request
request = Action $ \r s -> return $ Ok (_request r) s

-- | Get all query parameters.
queryParams :: Action [Param]
queryParams = Action $ \r s -> return $ Ok (_queryParams r) s

-- | Get all route captures.
captures :: Action [Param]
captures = Action $ \r s -> return $ Ok (_captures r) s

-- | Get the request body.
body :: Action BL.ByteString
body = Action $ \r s -> return $ Ok (_body r) s

-- | Modify the raw WAI 'Response'.
modifyResponse :: (Response -> Response) -> Action ()
modifyResponse f = Action $ \_ s -> return $ Ok () (f s)
