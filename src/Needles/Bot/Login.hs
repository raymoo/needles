{-
Copyright (C) 2015 Leon Medvinsky

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
-}

{-|
Module      : Needles.Bot.Login
Description : Provides getAssertion for logging into PS
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}


module Needles.Bot.Login (LoginInfo(..), Challenge(..), getAssertion) where

import           Control.Applicative ((<$>))
import           Data.List           (intercalate)
import           Network.HTTP
import           Text.JSON           hiding (Result)

loginServer :: String
loginServer = "http://play.pokemonshowdown.com/action.php"

requestType :: String
requestType = "application/x-www-form-urlencoded"


-- | User login information
data LoginInfo =
  LoginInfo { liUsername :: String
            , liPassword :: String
            }


-- | Challenge data
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

-- | Makes a login request, and possibly returns an assertion.
getAssertion :: LoginInfo -> Challenge -> IO (Maybe String)
getAssertion li chall = do
  response <- simpleHTTP $ postRequestWithBody loginServer requestType requestString
  aFromResponse . drop 1 <$> getResponseBody response
    where requestString = makeRequestString li chall
