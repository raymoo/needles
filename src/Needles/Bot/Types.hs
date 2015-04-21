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
Module      : Needles.Bot.Types
Description : Some important datatypes
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Needles.Bot.Types (
                           BotState(..)
                         , Trigger(..)
                         , TriggerAct(..)
                         , Configuration(..)
                         , MessageType(..)
                         , MessageInfo(..)
                         ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.State.Strict
import           Data.Map                         (Map)
import           Data.Text                        (Text)

import qualified Network.WebSockets               as WS

-- | The state of a bot
data BotState =
  BotState { bName       :: String
           , bPass       :: String
           , bConn       :: WS.Connection
           , bTriggers   :: [Trigger] -- ^ The triggers in use
           , bConfig     :: Configuration
           , bMessChan   :: Text -> IO ()
           , bTimestamps :: Map Text Integer -- ^ The enter time of each room
           }

-- | The type of a message
data MessageType = MTChat
                 | MTPm
                 | MTRaw
                 | MTBase
                 | MTUnknown
                 deriving (Show, Eq)

-- | The info of a message
data MessageInfo = MessageInfo { mType   :: MessageType
                               , what    :: Text
                               , who     :: Text
                               , rank    :: Char
                               , mRoom   :: Text
                               , respond :: forall a b. Text -> TriggerAct a b ()
                               }

-- | A trigger. They respond to certain messages by doing things.
data Trigger =
  Trigger { trigTest :: MessageInfo -> Bool
          , trigAct  :: MessageInfo -> StateT BotState IO (Trigger)
          }

-- | An effect that a 'Trigger' might produce.
-- `var` is the type of the runtime variable the trigger uses to store data.
-- `perma` is the type of the persistent data for this trigger.
-- `r` is the type of the result of this effect.
data TriggerAct var perma r where
  Send           :: Text -> TriggerAct a b ()
  Log            :: Text -> TriggerAct a b ()
  GetVar         :: TriggerAct a b a
  StoreVar       :: a -> TriggerAct a b ()
  DuraGet        :: TriggerAct a b b
  DuraStore      :: b -> TriggerAct a b ()
  DoIO           :: IO c -> TriggerAct a b c
  Bind           :: TriggerAct a b c -> (c -> TriggerAct a b d) -> TriggerAct a b d
  Pure           :: c -> TriggerAct a b c

instance Functor (TriggerAct a b) where
  fmap = liftM

instance Applicative (TriggerAct a b) where
  pure = Pure
  (<*>) = ap

instance Monad (TriggerAct a b) where
  return = Pure
  (>>=) = Bind

instance MonadIO (TriggerAct a b) where
  liftIO = DoIO

-- | The configuration for a ps bot.
data Configuration =
  Configuration { cUsername :: String
                , cPassword :: String
                , cServer   :: String    -- ^ The server address
                , cPort     :: Int       -- ^ The server port
                , cPath     :: String    -- ^ The server's websocket path
                , cRooms    :: [String]  -- ^ Which rooms to autojoin
                , cTriggers :: [Trigger] -- ^ Which triggers to use
                }
