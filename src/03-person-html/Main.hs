{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Lucid (ToHtml, Html, toHtml, toHtmlRaw, table_, tr_, th_, td_, renderBS)
import Servant

data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic

instance ToJSON Person

-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)
  -- do not worry too much about this
  toHtmlRaw = toHtml

  -- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"
    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons
  toHtmlRaw = toHtml

data HTMLLucid

instance Accept HTMLLucid where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTMLLucid (Html a) where
  mimeRender _ = renderBS

type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

api :: Proxy PersonAPI
api = Proxy

server :: Server PersonAPI
server = return people

app :: Application
app = serve api server

main :: IO ()
main = do
  let portNum = 8081
  putStrLn $ "Now running on " ++ (show portNum)
  run portNum app
