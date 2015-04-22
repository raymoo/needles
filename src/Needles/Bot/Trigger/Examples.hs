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
Module      : Needles.Bot.Trigger.Examples
Description : Example Triggers
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE OverloadedStrings #-}
module Needles.Bot.Trigger.Examples (
                                      replyTrig
                                    , replyPmTrig
                                    , simpleStateTrig
                                    , stateTrig
                                    ) where

import           Data.Text           (Text, append)
import qualified Data.Text           as T
import           Needles.Bot.Trigger


-- | Trigger that replies with an example message
replyTrig :: Trigger
replyTrig = mkTrigger "reply" (ProtoTrigger replyTest replyAct) ()

replyTest :: MessageInfo -> Bool
replyTest mi = mType mi == MTChat && (".replychat" == what mi)

replyAct :: MessageInfo -> TriggerAct a b ()
replyAct mi = respond mi message
  where message = "Hi " `append` who mi `append` ", this is an example message."

-- | Replies in pm
replyPmTrig :: Trigger
replyPmTrig = mkTrigger "replyPm" (ProtoTrigger replyPmTest replyPmAct) ()

replyPmTest :: MessageInfo -> Bool
replyPmTest mi = (mType mi == MTChat || mType mi == MTPm) && (".replypm" == what mi)

replyPmAct :: MessageInfo -> TriggerAct a b ()
replyPmAct mi = sendPm (who mi) "This is an example pm."

-- | Simple state
simpleStateTrig :: Trigger
simpleStateTrig =
  mkTrigger "simpleState" (ProtoTrigger simpleStateTest simpleStateAct) "Not Set"

simpleStateTest :: MessageInfo -> Bool
simpleStateTest mi = (mType mi == MTChat || mType mi == MTPm) &&
                     (".sSimple " `T.isPrefixOf` what mi)

simpleStateAct :: MessageInfo -> TriggerAct Text b ()
simpleStateAct mi = do
  prev <- getVar
  respond mi ("Previous data: " `append` prev)
  storeVar (T.drop 9 $ what mi)

-- | Shared state
statePutTest :: MessageInfo -> Bool
statePutTest mi = (mType mi == MTChat || mType mi == MTPm) &&
                  (".sPut " `T.isPrefixOf` what mi)

statePutAct :: MessageInfo -> TriggerAct Text b ()
statePutAct mi = do
  storeVar (T.drop 6 $ what mi)
  testdata <- getVar
  respond mi ("Data stored: " `append` testdata)

statePutProto :: ProtoTrigger Text b
statePutProto = ProtoTrigger statePutTest statePutAct


stateGetTest :: MessageInfo -> Bool
stateGetTest mi = (mType mi == MTChat || mType mi == MTPm) &&
                  (".sGet" `T.isPrefixOf` what mi)

stateGetAct :: MessageInfo -> TriggerAct Text b ()
stateGetAct mi = do
  datas <- getVar
  respond mi ("Data: " `append` datas)

stateGetProto :: ProtoTrigger Text b
stateGetProto = ProtoTrigger stateGetTest stateGetAct


stateTrig :: Trigger
stateTrig =
  clusterTrigger "state" [stateGetProto, statePutProto] ""
