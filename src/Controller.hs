{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Controller where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf16BE, encodeUtf8)
import Domain.User (User (..), UserName, renderUser, renderUsers)

data RequestK = GetUserK | GetUsersK | SaveUserK | DeleteUserK | UpdateUserK

data Service = ServiceQueryK | ServiceEditK

data RequestG v a where
  GetUsersReq :: RequestG ServiceQueryK GetUsersK
  GetUserReq :: Text -> RequestG ServiceQueryK GetUserK
  SaveUserReq :: User -> RequestG ServiceEditK SaveUserK
  DeleteUserReq :: UserName -> RequestG ServiceEditK DeleteUserK
  UpdateUserReq :: User -> RequestG ServiceEditK UpdateUserK

data ResponseG v a where
  GetUsersResp :: [UserName] -> ResponseG ServiceQueryK GetUsersK
  GetUserResp :: Maybe User -> ResponseG ServiceQueryK GetUserK
  SaveUserResp :: ResponseG ServiceEditK SaveUserK
  DeleteUserResp :: ResponseG ServiceEditK DeleteUserK
  UpdateUserResp :: ResponseG ServiceEditK UpdateUserK

newtype Respond v m = Respond (forall a. RequestG v a -> m (ResponseG v a))

render :: ResponseG v a -> ByteString
render (GetUserResp n) = renderUser n
render (GetUsersResp n) = renderUsers n
render SaveUserResp = "OK"
render DeleteUserResp = "not implemented"
render UpdateUserResp = "not implemented"

data Request v = forall a. Request (RequestG v a)

data Response v = forall a. Response (ResponseG v a)

applyRespond :: Functor m => Respond v m -> Request v -> m (Response v)
applyRespond (Respond f) (Request x) = Response <$> f x

type Parser v = ByteString -> Maybe (Request v)

parseQuery :: Parser ServiceQueryK
parseQuery = \case
  "\r\n" -> Just $ Request GetUsersReq
  name -> Just $ Request $ GetUserReq (T.strip $ decodeUtf8 name)

parseEdit :: Parser ServiceEditK
parseEdit = \case
  "+\r\n" -> Just $ Request $ SaveUserReq $ error "notImplemented"
  "-\r\n" -> Just $ Request $ DeleteUserReq $ error "notImplemented"
  "~\r\n" -> Just $ Request $ UpdateUserReq $ error "notImplemented"
