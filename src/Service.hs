module Service where 


import Data.ByteString (ByteString)
import Data.Text (Text)

data Request = ReqUser Text | ReqUsers

type Respond = Request -> IO ByteString
