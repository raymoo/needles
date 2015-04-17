module Needles.Bot.Login (LoginInfo(..), Challenge(..), getAssertion) where

import           Control.Applicative ((<$>))
import           Data.List           (intercalate)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Network.HTTP
import           Network.Stream
import           Text.JSON           hiding (Result)

loginServer = "http://play.pokemonshowdown.com/action.php"
requestType = "application/x-www-form-urlencoded"

data LoginInfo =
  LoginInfo { liUsername :: String
            , liPassword :: String
            }

data Challenge =
  Challenge { challKey :: String
            , challStr :: String
            }

-- Creates the body of the login request
makeRequestString :: LoginInfo -> Challenge -> String
makeRequestString li chall =
  intercalate "&" . map (\(x,y) -> x ++ "=" ++ y) $ template
  where template =
          [ ("act", "login")
          , ("name", liUsername li)
          , ("pass", liPassword li)
          , ("challengekeyid", challKey chall)
          , ("challenge", challStr chall)
          ]

-- Tries to find the assertion from the response
aFromResponse :: String -> Maybe String
aFromResponse = getA . decode
  where getA (Ok (JSObject o)) =
          (>>= valueToString) . lookup "assertion" . fromJSObject $ o
        getA _ = Nothing

-- Stringify a JSON value
valueToString :: JSValue -> Maybe String
valueToString js = case result of
                    Ok s    -> Just s
                    Error _ -> Nothing
  where result = readJSON js

-- Make the request
getAssertion :: LoginInfo -> Challenge -> IO (Maybe String)
getAssertion li chall = do
  response <- simpleHTTP $ postRequestWithBody loginServer requestType requestString
  aFromResponse . drop 1 <$> getResponseBody response
    where requestString = makeRequestString li chall
