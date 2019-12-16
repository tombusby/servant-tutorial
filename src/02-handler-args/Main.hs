{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson (ToJSON, FromJSON)
import Data.List (intercalate)
import Network.Wai.Handler.Warp (run)
import GHC.Generics (Generic)
import Servant

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from'    = "great@company.com"
    to'      = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body'    = "Hi " ++ clientName c ++ ",\n\n"
            ++ "Since you've recently turned " ++ show (clientAge c)
            ++ ", have you checked out our latest "
            ++ intercalate ", " (clientInterestedIn c)
            ++ " products? Give us a visit!"

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

server :: Server API
server = position
     :<|> hello
     :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)
    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n  -> "Hello, " ++ n
    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = return (emailForClient clientinfo)

api :: Proxy API
api = Proxy

app1 :: Application
app1 = serve api server

main :: IO ()
main = do
  let portNum = 8081
  putStrLn $ "Now running on " ++ (show portNum)
  run portNum app1
