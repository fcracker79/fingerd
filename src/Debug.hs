module Debug where

import qualified Repository.Database as Database

main :: IO ()
main = do
  connectionPool <- Database.pool
  return ()
