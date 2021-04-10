module Main where

import qualified Debug as D
import TCPServer (server)
import Domain.UserService (responder)
import Repository.Database (pool)

main :: IO ()
main = pool >>= server "7979" . responder 
