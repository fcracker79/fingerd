{-# LANGUAGE RecordWildCards #-}

module Service.Parse where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, sepBy, space, takeByteString)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import User (User (User), UserData (..), UserName)

parseUser :: Parser User
parseUser = do
  id <- decimal
  space
  char ','
  User id <$> parseUserData

parseUserName :: Parser UserName
parseUserName = T.strip . decodeUtf8 <$> takeByteString

-- TODO: validate
parseUserData :: Parser UserData
parseUserData = do
  [username, shell, homeDirectory, realName, phone] <-
    fmap (T.strip . decodeUtf8)
      <$> A.takeWhile (/= ',') `sepBy` char ','
  pure $ UserData {..}
