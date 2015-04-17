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
Module      : Needles.Bot.Message
Description : Generic message queue module
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}


module Needles.Bot.Message (
                             mkMessageQueue
                           ) where

import           Control.Concurrent      (forkIO, threadDelay, ThreadId)
import           Control.Concurrent.Chan (Chan, newChan, readChan)
import           Control.Exception
import           Control.Monad
import           Data.Text               (Text)

-- First argument is what to do with received messages. Second argument is
-- what to do if there is an error. Third argument is how many microseconds to
-- wait between processing messages. Returns a Chan to send to.
mkMessageQueue :: Exception e => (a -> IO ()) -> (e -> IO ()) -> Int -> IO (Chan a, ThreadId)
mkMessageQueue action handler delay = do
  chan <- newChan
  tId <- forkIO $ catch (messageLoop chan) handler
  return (chan, tId)
  where messageLoop channel =
          forever $ readChan channel >>= action >> threadDelay delay
