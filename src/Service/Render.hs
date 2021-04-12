module Service.Render where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Model.User (User (User), UserData (UserData), UserName)

renderUser :: Maybe User -> ByteString
renderUser Nothing = "No such user"
renderUser (Just (User _ (UserData username shell homeDir realName phoneNumber))) =
  BS.concat
    [ "Login: "
    , e username
    , "\t\t\t\t"
    , "Name: "
    , e realName
    , "\n"
    , "Directory: "
    , e homeDir
    , "\t\t\t"
    , "Shell: "
    , e shell
    , "\n"
    , "Phone: "
    , e phoneNumber
    , "\n"
    ]
  where
    e = encodeUtf8

renderUsers :: [UserName] -> ByteString
renderUsers userNames = encodeUtf8 $ T.concat $ intersperse "\n" userNames
