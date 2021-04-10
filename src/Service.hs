module Service where 


import Data.ByteString (ByteString)
import Data.Text (Text)
import Domain.User ( User, UserName )
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

data Request = ReqUser Text | ReqUsers

type Respond m = Request -> m ByteString

renderUser :: Maybe User -> ByteString
renderUser = dumb

renderUsers :: [UserName] -> ByteString
renderUsers = dumb

dumb :: Show a => a -> ByteString
dumb = encodeUtf8 . T.pack . show