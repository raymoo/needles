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

module Needles.Bot (runBot) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Exception
import Data.ByteString (ByteString)
import Data.Map.Strict (empty)
import Needles.Bot.Message.Handle
import Needles.Bot.Configuration
import Needles.Bot.Message
import Needles.Bot.State
import qualified Network.WebSockets as WS
import Data.IORef

-- | Runs a bot with the given 'Configuration'
runBot :: Configuration -> IO ()
runBot config = do
  WS.runClient (cServer config) (cPort config) (cPath config) (bot config)

bot :: Configuration -> WS.ClientApp ()
bot c conn = do
  putStrLn "Connected"
  (chan, _) <- mkPSQueue conn (const $ return () :: SomeException -> IO ())
  let newBot = BotState { bName = cUsername c
                        , bPass = cPassword c
                        , bConn = conn
                        , bTriggers = cTriggers c
                        , bConfig = c
                        , bMessChan = chan
                        , bTimestamps = empty
                        }
  backup <- newIORef newBot
  catch (loop backup newBot) (reinitialize backup)

loop :: IORef BotState -> BotState -> IO ()
loop backup thebot = flip evalStateT thebot . forever $ do
  mess <- getData
  handleBS mess
  get >>= liftIO . writeIORef backup

getData :: StateT BotState IO ByteString
getData = get >>= liftIO . WS.receiveData . bConn

reinitialize :: IORef BotState -> SomeException -> IO ()
reinitialize backup _ = do
  reclaim <- readIORef backup
  let config = bConfig reclaim
  catch (WS.runClient (cServer config) (cPort config) (cPath config) (fixBot reclaim)) (reinitialize backup)
  where fixBot oldBot conn = do
          (queue, _) <- mkPSQueue conn (const $ return () :: SomeException -> IO ())
          let newBot = oldBot { bConn = conn
                              , bMessChan = queue
                              }
          writeIORef backup newBot
          loop backup newBot
