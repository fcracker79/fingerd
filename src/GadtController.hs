{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module GadtController where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
    ( Parser, char, decimal, space, string, takeByteString, sepBy )
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf16BE, encodeUtf8)
import Domain.User (User (..), UserData (..), UserName, renderUser, renderUsers)
import Domain.Parser ( parseUser, parseUserName, parseUserData )
import Control.Monad.Managed
import Repository.Database
import Domain.UserService

-- | index the API in types
data APIPoints = GetUserAPI | GetUsersAPI | SaveUserAPI | DeleteUserAPI | UpdateUserAPI

-- | index the 2 services in types
data ServiceKind = ServiceQueryType | ServiceEditType

-- | all requests with their arguments indexed by API point and service types
data RequestG v a where
  GetUsersReq :: RequestG ServiceQueryType GetUsersAPI
  GetUserReq :: Text -> RequestG ServiceQueryType GetUserAPI
  SaveUserReq :: UserData -> RequestG ServiceEditType SaveUserAPI
  DeleteUserReq :: UserName -> RequestG ServiceEditType DeleteUserAPI
  UpdateUserReq :: UserData -> RequestG ServiceEditType UpdateUserAPI

-- | all responses with their arguments indexed by API point and service types
data ResponseG v a where
  GetUsersResp :: [UserName] -> ResponseG ServiceQueryType GetUsersAPI
  GetUserResp :: Maybe User -> ResponseG ServiceQueryType GetUserAPI
  SaveUserResp :: Bool -> ResponseG ServiceEditType SaveUserAPI
  DeleteUserResp :: Bool -> ResponseG ServiceEditType DeleteUserAPI
  UpdateUserResp :: Bool -> ResponseG ServiceEditType UpdateUserAPI

-- | a generic handler for any request that has to produce the right type of response
newtype Respond v m = Respond (forall a. RequestG v a -> m (ResponseG v a))

hoistRespond :: (forall a . m a -> n a) -> Respond v m -> Respond v n
hoistRespond n (Respond f) = Respond $ n <$> f

-- | render all responses
render :: ResponseG v a -> ByteString
render (GetUserResp n) = renderUser n
render (GetUsersResp n) = renderUsers n
render (SaveUserResp b) = if b then "Ok" else "User already present"
render (DeleteUserResp b) = if b then "Ok" else "Could not delete user"
render (UpdateUserResp b) = if b then "Ok" else "Could not update user"

-- | any request for a 'v' service
data Request v = forall a. Request (RequestG v a)

-- | any response for a 'v' service
data Response v = forall a. Response (ResponseG v a)

-- | apply a respond handler to a request for the 'v' service
applyRespond :: Functor m => Respond v m -> Request v -> m (Response v)
applyRespond (Respond f) (Request x) = Response <$> f x


-- | parser for the ServiceQueryType
parseQuery :: Parser (Request ServiceQueryType)
parseQuery =
  Request GetUsersReq <$ string "\r\n"
    <|> Request . GetUserReq . T.strip . decodeUtf8 <$> takeByteString

-- | parser for the ServiceEditType
parseEdit :: Parser (Request ServiceEditType)
parseEdit =
  Request . SaveUserReq <$> do string "+" >> space >> parseUserData
    <|> Request . DeleteUserReq <$> do string "-" >> space >> parseUserName
    <|> Request . UpdateUserReq <$> do string "~" >> space >> parseUserData


responderQuery :: MonadManaged m => Respond ServiceQueryType (WithPool m)
responderQuery = Respond \case
  GetUsersReq -> GetUsersResp <$> getUsers 
  GetUserReq user -> GetUserResp <$> getUser user 

responderEdit :: MonadManaged m => Respond ServiceEditType (WithPool m)
responderEdit = Respond \case
  SaveUserReq user -> do 
    saveUser user
    pure $ SaveUserResp True 
  UpdateUserReq user -> do 
    UpdateUserResp <$> updateUser user   
  DeleteUserReq userName -> do 
    DeleteUserResp <$> deleteUser userName 
