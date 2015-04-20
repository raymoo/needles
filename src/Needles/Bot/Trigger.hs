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
Module      : Needles.Bot.Triggers
Description : Library for specifying triggers
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Needles.Bot.Trigger (
                             MessageInfo(..)
                           , MessageType(..)
                             -- * Triggers
                           , Trigger
                           , TriggerAct
                             -- ** Construction
                           , mkTrigger
                           , mkTrigger_
                             -- * Actions
                             -- ** Basic
                           , send
                           , printLn
                           , getVar
                           , storeVar
                           , duraGet
                           , duraStore
                             -- ** Convenience
                           , sendChat
                           , sendPm
                           , command
                           , clusterTrigger
                             -- * Utilities
                           , normalizeName
                           ) where

import           Control.Applicative
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Data.Text                        (Text, append)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO (putStrLn)
import           Needles.Bot.Types

-- | 'mkTrigger' takes a predicate on message types, and a function to create
-- a 'TriggerAct'. The predicate is used to see if a message should be
-- processed by this 'Trigger', and the second function determines what actually
-- happens when the 'Trigger' is triggered. The third argument is the initial
-- state of the trigger.
mkTrigger :: (MessageInfo -> Bool) -> (MessageInfo -> TriggerAct a b c)
             -> a -> Trigger
mkTrigger p action s = Trigger p actFun
  where actFun mi = do
          let baked = bakeAction (action mi) s
          (_, s') <- baked
          return (mkTrigger p action s')

-- | Version for 'TriggerAct's with no state
mkTrigger_ :: (MessageInfo -> Bool) -> (MessageInfo -> TriggerAct () b c)
              -> Trigger
mkTrigger_ p action = mkTrigger p action ()

bakeAction :: TriggerAct a b c -> a -> StateT BotState IO (c, a)
bakeAction (Send text) a = do
  sender <- bMessChan <$> get
  liftIO $ mapM_ sender (T.lines text)
  return ((), a)
bakeAction (PrintLn text) a = liftIO $ TIO.putStrLn text >> return ((), a)
bakeAction GetVar a = return (a, a)
bakeAction (StoreVar a') _ = return ((), a')
bakeAction DuraGet _ = error "Durable storage not implemented yet"
bakeAction (DuraStore _) _ = error "Durable storage not implemented yet"
bakeAction (DoIO io) a = flip (,) a <$> liftIO io
bakeAction (Bind ma k) a = do
  (res, a') <- firstAct
  let secondAct = k res
  bakeAction secondAct a'
  where firstAct = bakeAction ma a
bakeAction (Pure c) a = return (c, a)


-- | Sends the given message to the server.
send :: Text -> TriggerAct a b ()
send = Send


-- | Prints the given message to the console.
printLn :: Text -> TriggerAct a b ()
printLn = PrintLn


-- | Gets the value of the trigger's runtime store.
getVar :: TriggerAct a b a
getVar = GetVar


-- | Stores a value into the trigger's runtime store.
storeVar :: a -> TriggerAct a b ()
storeVar = StoreVar


-- | Gets the persistent data for this trigger.
duraGet :: TriggerAct a b b
duraGet = DuraGet


-- | Stores persistent data for this trigger.
duraStore :: b -> TriggerAct a b ()
duraStore = DuraStore

-- | Takes a room and a message, then sends a message to that room
sendChat :: Text -> Text -> TriggerAct a b ()
sendChat r m = mapM_ send roomMessages
  where roomMessages = map (append roomPrefix) (T.lines m)
        roomPrefix   = T.snoc r '|'

-- | Takes a user and a message, sending a pm to that user
sendPm :: Text -> Text -> TriggerAct a b ()
sendPm u m = mapM_ send userMessages
  where userMessages = map (append userPrefix) (T.lines m)
        userPrefix   = T.snoc (append "|/pm " u) ','

-- | Executes a command in the given room (empty string means default room) and
-- given command string.
command :: Text -> Text -> TriggerAct a b ()
command r c = send (append roomPrefix c)
  where roomPrefix = T.snoc r '|'

-- | Combines multiple predicate and TriggerActions into one big blob that
-- shares runtime state. Second argument is initial state.
clusterTrigger :: forall a b. [(MessageInfo -> Bool, MessageInfo -> TriggerAct a b ())] -> a -> Trigger
clusterTrigger triggers initState = mkTrigger clusterPred clusterAction initState
  where clusterPred mi = any ($mi) . map fst $ triggers
        clusterAction :: MessageInfo -> TriggerAct a b ()
        clusterAction mi = mapM_ (checkAndDo mi) triggers
        checkAndDo mi (p, act) = if p mi then act mi else return ()


-- | Makes a name lowercase and takes out non-alphanumeric characters. Useful
-- when you want a consistent nick to refer to the same account.
normalizeName :: Text -> Text
normalizeName = T.toLower . T.filter isAlphaNum
