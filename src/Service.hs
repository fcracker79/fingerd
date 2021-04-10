{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Service where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Domain.User (User (..), UserName, renderUser, renderUsers)

data GetK = GetUserK | GetUsersK

data RequestG a where
  UsersReq :: RequestG GetUsersK
  UserReq :: Text -> RequestG GetUserK

data ResponseG a where
  UsersResp :: [UserName] -> ResponseG GetUsersK
  UserResp :: Maybe User -> ResponseG GetUserK

newtype RespondG m = RespondG (forall a. RequestG a -> m (ResponseG a))

serialize :: ResponseG a -> ByteString
serialize (UserResp n) = renderUser n
serialize (UsersResp n) = renderUsers n
