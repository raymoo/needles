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
                                    ) where

import           Data.Text           (append)
import qualified Data.Text           as T
import           Needles.Bot.Trigger


-- | Trigger that replies with an example message
replyTrig :: Trigger
replyTrig = mkTrigger replyTest replyAct ()

replyTest :: MessageInfo -> Bool
replyTest mi = mType mi == MTChat && ("!replychat" == what mi)

replyAct :: MessageInfo -> TriggerAct a b ()
replyAct mi = respond mi message
  where message = "Hi " `append` who mi `append` ", this is an example message."

-- | Replies in pm
replyPmTrig :: Trigger
replyPmTrig = mkTrigger replyPmTest replyPmAct ()

replyPmTest :: MessageInfo -> Bool
replyPmTest mi = (mType mi == MTChat || mType mi == MTPm) && ("!replypm" == what mi)

replyPmAct :: MessageInfo -> TriggerAct a b ()
replyPmAct mi = sendPm (who mi) "This is an example pm."
