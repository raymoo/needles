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

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Needles.Bot.Trigger where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)

data Trigger

data TriggerAct a b c where
  Send           :: Text -> TriggerAct a b ()
  PrintLn        :: Text -> TriggerAct a b ()
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

send :: Text -> TriggerAct a b ()
send = Send

printLn :: Text -> TriggerAct a b ()
printLn = PrintLn

getVar :: TriggerAct a b a
getVar = GetVar

storeVar :: a -> TriggerAct a b ()
storeVar = StoreVar

duraGet :: TriggerAct a b b
duraGet = DuraGet

duraStore :: b -> TriggerAct a b ()
duraStore = DuraStore
