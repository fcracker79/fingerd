module Service where 


import Data.ByteString (ByteString)
import Data.Text (Text)
import Domain.User ( User(..), UserName )
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import Data.List (intersperse)


data Request = ReqUser Text | ReqUsers

type Respond m = Request -> m ByteString

renderUser :: Maybe User -> ByteString
renderUser Nothing = "No such user"
renderUser (Just (User _ username shell homeDir realName _)) = BS.concat
    [
        "Login: ", e username, "\t\t\t\t",
        "Name: ", e realName, "\n",
        "Directory: ", e homeDir, "\t\t\t",
        "Shell: ", e shell, "\n"
        ]
    where e = encodeUtf8


renderUsers :: [UserName] -> ByteString
renderUsers userNames = encodeUtf8 $ T.concat $ intersperse "\n" userNames
