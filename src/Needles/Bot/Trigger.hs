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

{-# LANGUAGE OverloadedStrings #-}

module Needles.Bot.Trigger (
                             Trigger
                           , TriggerAct
                           , send
                           , printLn
                           , getVar
                           , storeVar
                           , duraGet
                           , duraStore
                           ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text)
import           Needles.Bot.Types

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
