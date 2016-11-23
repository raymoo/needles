{-
Copyright (C) 2015 Leon Medvinsky

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
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
Module      : Needles.Bot.Message.Handle
Description : Handles messages
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE OverloadedStrings #-}
module Needles.Bot.Message.Handle (handleBS) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                  (ByteString)
import           Data.Map.Strict                  (findWithDefault, insert)
import           Data.Text                        (Text, pack)
import qualified Data.Text.IO                     as T
import           Needles.Bot.Login
import           Needles.Bot.Message.In.Parse
import           Needles.Bot.Message.Info
import           Needles.Bot.Trigger
import           Needles.Bot.Types



-- Runs all the triggers in a bot
passTriggers :: Message -> StateT Session IO ()
passTriggers m = do
  case makeMInfo m of
   Nothing -> liftIO $ putStrLn "Unhandled Message"
   Just mi -> do
     trigs <- sTriggers <$> get
     newTriggers <- mapM (doTrigger mi) trigs
     bstate <- fmap sBotState get
     let newBstate = bstate { bTriggers = newTriggers }
     get >>= ($!) put .  (\sstate -> sstate { sBotState = newBstate })

doTrigger :: MessageInfo -> Trigger -> StateT Session IO Trigger
doTrigger mi trig@(Trigger test _)
  | test mi = tryTrigger mi trig
  | otherwise = return trig

printError :: Exception e => MessageInfo -> e -> IO ()
printError mi e = do
  putStrLn "Error in Trigger"
  putStrLn $ "MessageInfo: " ++ displayMInfo mi
  putStrLn $ "Error:"
  print e

tryTrigger :: MessageInfo -> Trigger -> StateT Session IO Trigger
tryTrigger mi trig@(Trigger _ act) = do
  sstate <- get
  (res, sstate') <- liftIO $
                    catch (runStateT (act mi) sstate)
                    (\e -> do printError mi (e :: SomeException)
                              return (trig, sstate))
  put sstate'
  return res

-- | Main handler
handleBS :: ByteString -> StateT Session IO ()
handleBS = mapM_ handleMessage . parseMessage

handleMessage :: Message -> StateT Session IO ()
handleMessage (Unknown m) = liftIO $ putStrLn "Unknown Message: " >> T.putStrLn m
handleMessage (ChallStr key str) = do
  liftIO $ putStrLn "Received Challenge - Logging in"
  sstate <- get
  let loginfo = LoginInfo (sName sstate) (sPass sstate)
      challenge = Challenge (show key) str
  result <- liftIO $ getAssertion loginfo challenge
  case result of
   Nothing -> liftIO $ putStrLn "Failed to log in."
   Just assertion ->
     let com = pack $ "|/trn " ++ (sName sstate) ++ ",0," ++ assertion
     in do
       liftIO $ sMessChan sstate com
       liftIO $ putStrLn "Logged In"
       liftIO $ putStrLn "Joining Rooms..."
       get >>= mapM_ joinRoom . cRooms . sConfig
       liftIO $ putStrLn "Done."
handleMessage m@(Chat r t _ _) = do
  joinTime <- getTimestamp r
  if t > joinTime
    then passTriggers m
    else return ()
handleMessage (Timestamp r t) = putTimestamp r t
handleMessage m = passTriggers m

getTimestamp :: Text -> StateT Session IO Integer
getTimestamp r =
  findWithDefault 0 r . sTimestamps <$> get

putTimestamp :: Text -> Integer -> StateT Session IO ()
putTimestamp r t = do
  sstate <- get
  put $ sstate { sBotState = (sBotState sstate){
                   bTimestamps = insert r t (sTimestamps sstate)
               }}

joinRoom :: String -> StateT Session IO ()
joinRoom r = do
  chan <- sMessChan <$> get
  liftIO $ chan com
  where com = pack $ "|/join " ++ r
