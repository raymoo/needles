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
                           , Room(..)
                           , User(..)
                             -- * Compatibility
                           , mType
                           , what
                           , who
                           , rank
                           , mRoom
                           , respond
                           , MessageType(..)
                             -- * Triggers
                           , Trigger
                           , TriggerAct
                             -- ** Construction
                           , ProtoTrigger(..)
                           , mkTrigger
                           , mkTrigger_
                             -- * Actions
                             -- ** Basic
                           , send
                           , getVar
                           , storeVar
                           , writeLog
                             -- ** Convenience
                           , sendChat
                           , sendPm
                           , command
                           , printLn
                           , clusterTrigger
                             -- * Tests
                           , contentIs
                           , startsWith
                             -- ** Combinators
                           , (<&&>)
                           , (<||>)
                             -- * Utilities
                           , normalizeName
                           ) where

import           Control.Applicative
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Data.Functor
import           Data.Text                        (Text, append)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO (putStrLn)
import           Needles.Bot.Types


mType :: MessageInfo -> MessageType
mType (MIChat _ _ _) = MTChat
mType (MIPm _ _) = MTPm
mType (MIJoin _ _) = MTJoin
mType (MILeave _ _) = MTLeave
mType (MIRaw _ _) = MTRaw
mType (MIBase _) = MTBase
mType MIUnknown = MTUnknown

what :: MessageInfo -> Text
what (MIChat _ _ w) = w
what (MIPm _ w) = w
what (MIRaw _ w) = w
what (MIBase w) = w
what _ = ""

who :: MessageInfo -> Text
who (MIChat _ w _) = userName w
who (MIPm w _) = userName w
who (MIJoin _ u) = userName u
who (MILeave _ u) = userName u
who _ = ""

rank :: MessageInfo -> Char
rank (MIChat _ w _) = userRank w
rank (MIPm w _) = userRank w
rank (MIJoin _ u) = userRank u
rank (MILeave _ u) = userRank u
rank _ = ' '

mRoom :: MessageInfo -> Text
mRoom (MIChat r _ _) = roomName r
mRoom (MIJoin r _) = roomName r
mRoom (MILeave r _) = roomName r
mRoom (MIRaw r _) = roomName r
mRoom _ = ""

respond :: MessageInfo -> Text -> TriggerAct a b ()
respond (MIChat r _ _) = sendChat (roomName r)
respond (MIPm w _) = sendPm (userName w)
respond _ = const (return ())


-- | A 'ProtoTrigger' is a trigger that is not ready to be used. The type
-- parameters are the same as the first two of 'TriggerAct'.
data ProtoTrigger a b =
  forall c. ProtoTrigger { ptPred :: MessageInfo -> Bool
                           -- ^ Predicate that determines which triggers
                           -- to process
                         , ptAct  :: MessageInfo -> TriggerAct a b c
                           -- ^ The actual action to take when activated
                         }

-- | 'mkTrigger' takes a 'ProtoTrigger' and creates a 'Trigger'. The 'String'
-- argument is a name for the trigger, used in case of permanent data storage.
mkTrigger :: String -> ProtoTrigger a b -> a -> Trigger
mkTrigger name pt@(ProtoTrigger p action) s = Trigger p actFun
  where actFun mi = do
          let baked = bakeAction name (action mi) s
          (_, s') <- baked
          return (mkTrigger name pt s')

-- | Version for 'TriggerAct's with no state
mkTrigger_ :: String -> ProtoTrigger () b -> Trigger
mkTrigger_ name pt = mkTrigger name pt ()

bakeAction :: String -> TriggerAct a b c -> a -> StateT Session IO (c, a)
bakeAction _ (Send text) a = do
  sender <- sMessChan <$> get
  liftIO $ mapM_ sender (T.lines text)
  return ((), a)
bakeAction name (Log str) a = do
  logger <- cLogger . sConfig <$> get
  let message = T.pack (name ++ ": ") `T.append` str
  liftIO $ logger message
  return ((), a)
bakeAction _ GetVar a = return (a, a)
bakeAction _ (StoreVar a') _ = return ((), a')
bakeAction _ DuraGet _ = error "Durable storage not implemented yet"
bakeAction _ (DuraStore _) _ = error "Durable storage not implemented yet"
bakeAction _ (DoIO io) a = flip (,) a <$> liftIO io
bakeAction name (Bind ma k) a = do
  (res, a') <- firstAct
  let secondAct = k res
  bakeAction name secondAct a'
  where firstAct = bakeAction name ma a
bakeAction _ (Pure c) a = return (c, a)


-- | Sends the given message to the server.
send :: Text -> TriggerAct a b ()
send = Send


-- | Log the message
writeLog :: Text -> TriggerAct a b ()
writeLog = Log


-- | Gets the value of the trigger's runtime store.
getVar :: TriggerAct a b a
getVar = GetVar


-- | Stores a value into the trigger's runtime store.
storeVar :: a -> TriggerAct a b ()
storeVar = StoreVar


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


-- | Prints the given message to the console.
printLn :: Text -> TriggerAct a b ()
printLn = DoIO . TIO.putStrLn


-- | Combines multiple 'ProtoTrigger's into one big blob that
-- shares runtime state. Third argument is initial state, First is the
-- trigger name.
clusterTrigger :: forall a b. String -> [ProtoTrigger a b] -> a -> Trigger
clusterTrigger name triggers initState =
  mkTrigger name (ProtoTrigger clusterPred clusterAction) initState
  where clusterPred mi = any ($mi) . map ptPred $ triggers
        clusterAction :: MessageInfo -> TriggerAct a b ()
        clusterAction mi = mapM_ (checkAndDo mi) triggers
        checkAndDo mi (ProtoTrigger p act) = if p mi then void $ act mi else return ()


-- | Makes a name lowercase and takes out non-alphanumeric characters. Useful
-- when you want a consistent nick to refer to the same account.
normalizeName :: Text -> Text
normalizeName = T.toLower . T.filter isAlphaNum


infixr 1 <&&>
infixr 1 <||>

-- | Makes a new test, where both its arguments must be true
(<&&>) :: (MessageInfo -> Bool) -> (MessageInfo -> Bool) -> (MessageInfo -> Bool)
(<&&>) = liftA2 (&&)

-- | Makes a new test, where either of its arguments can be true
(<||>) :: (MessageInfo -> Bool) -> (MessageInfo -> Bool) -> (MessageInfo -> Bool)
(<||>) = liftA2 (||)


-- | The message is exactly this 'Text'
contentIs :: Text -> (MessageInfo -> Bool)
contentIs content = (== content) . what


-- | The message starts with this 'Text'
startsWith :: Text -> (MessageInfo -> Bool)
startsWith prefix = T.isPrefixOf prefix . what
