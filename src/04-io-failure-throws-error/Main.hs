{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Directory (doesFileExist)

type API = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server :: Server API
server = do
  exists <- liftIO $ doesFileExist "myfile.txt"
  if exists
  then
    FileContent <$> liftIO (readFile "myfile.txt")
  else
    throwError err404 {
        errBody = "myfile.txt just isn't there, please leave this server alone."
      }

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  let portNum = 8081
  putStrLn $ "Now running on " ++ (show portNum)
  run portNum app
