{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE RecordWildCards #-}
module ControllerSimple where

import Control.Monad (msum, void)
import Control.Monad.Trans.Maybe
import Data.Attoparsec.ByteString (Parser, parseOnly, string, takeByteString)
import Data.ByteString.Char8 (ByteString)
import Control.Applicative (empty)
import Domain.UserService (getUsers, getUser)
import Domain.User ( renderUser, renderUsers )
import Data.Pool (Pool)
import Database.SQLite.Simple (Connection)
import Control.Monad.Managed ( Managed )
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

type RawHandler m = ByteString -> MaybeT m ByteString

runRawHandlers :: Monad m => [RawHandler m] -> RawHandler m
runRawHandlers xs input = msum $ ($ input) <$> xs

data Handler m = forall i o.
  Handler
  { parser :: Parser i
  , handler :: i -> m o
  , renderer :: o -> ByteString
  }

runHandler :: Monad m => Handler m -> RawHandler m 
runHandler Handler {..} input = case 
    parseOnly parser input of 
        Left _ -> empty 
        Right x -> MaybeT $ Just . renderer <$> handler x 

type Controller m = [Handler m]

runController :: Monad m => [Handler m] -> RawHandler m 
runController = runRawHandlers . fmap runHandler 

getUsersH :: Pool Connection -> Handler Managed
getUsersH pool = Handler 
    do void $ string "\r\n" 
    do const $ getUsers pool
    do renderUsers 
    
getUserH :: Pool Connection -> Handler Managed
getUserH pool = Handler 
    do T.strip . decodeUtf8 <$> takeByteString
    do getUser pool 
    do renderUser 

queryControllers :: Pool Connection -> [Handler Managed]
queryControllers pool = ($ pool) <$> [getUsersH, getUserH]