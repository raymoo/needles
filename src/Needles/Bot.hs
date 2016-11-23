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
  backup <- newIORef $ configuredBot config
  connectBot config backup

connectBot :: Configuration -> IORef BotState -> IO ()
connectBot config backup =
  WS.runClient (cServer config) (cPort config) (cPath config) (bot backup)
  `catch` handleProblem config backup

configuredBot :: Configuration -> BotState
configuredBot config = BotState { bName = cUsername config
                                , bPass = cPassword config
                                , bTriggers = cTriggers config
                                , bConfig = config
                                , bTimestamps = empty
                                }

handleProblem :: Configuration -> IORef BotState -> SomeException -> IO ()
handleProblem config backup e = do
  putStrLn . displayException $ e
  connectBot config backup

bot :: IORef BotState -> WS.ClientApp ()
bot backup conn = do
  putStrLn "Connected"
  bstate <- readIORef backup
  (chan, _) <- mkPSQueue conn (const $ return () :: SomeException -> IO ())
  let newSession = Session { sBotState = bstate
                           , sConn = conn
                           , sMessChan = chan
                           }
  loop backup newSession

loop :: IORef BotState -> Session -> IO ()
loop backup session = flip evalStateT session . forever $ do
  mess <- getData
  handleBS mess
  get >>= liftIO . writeIORef backup . sBotState

getData :: StateT Session IO ByteString
getData = get >>= liftIO . WS.receiveData . sConn
