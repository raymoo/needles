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
Module      : Needles.Bot.Message.Info
Description : Creation of the "MessageInfo" data that is passed to triggers
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

module Needles.Bot.Message.Info (makeMInfo) where

import Needles.Bot.Trigger
import Needles.Bot.Types
import Needles.Bot.Message.In.Parse
import Data.Text (Text, uncons)


defaultMInfo :: MessageInfo
defaultMInfo =
  MessageInfo { mType   = MTUnknown
              , what    = ""
              , who     = ""
              , rank    = ' '
              , mRoom   = ""
              , respond = const (return ())
              }

decoupleName :: Text -> (Char, Text)
decoupleName name = maybe (' ', "") id (uncons name)

makeMInfo :: Message -> Maybe MessageInfo
makeMInfo (Chat r t user mess) =
  let (userrank, username) = decoupleName user
  in Just
     defaultMInfo { mType   = MTChat
                  , what    = mess
                  , who     = username
                  , rank    = userrank
                  , mRoom   = r
                  , respond = sendChat r
                  }
makeMInfo (Pm u w) =
  let (userrank, username) = decoupleName u
  in Just
     defaultMInfo { mType   = MTPm
                  , what    = w
                  , who     = username
                  , rank    = userrank
                  , respond = sendPm u
                  }
makeMInfo (Raw r w) = Just
  defaultMInfo { mType = MTRaw
               , what  = w
               , mRoom = r
               }
makeMInfo _ = Nothing
