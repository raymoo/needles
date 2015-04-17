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
Description : Generic message queue module. Also PS-specific queue.
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Needles.Bot.Configuration (
                                   Configuration(..)
                                 , mainServer
                                 , mainPort
                                 , mainPath
                                 ) where

-- | The configuration for a ps bot.
data Configuration =
  Configuration { cUsername :: String
                , cPassword :: String
                , cServer   :: String    -- ^ The server address
                , cPort     :: Int       -- ^ The server port
                , cPath     :: String    -- ^ The server's websocket path
                , cRooms     :: [String] -- ^ Which rooms to autojoin
                } deriving (Show)

-- | Address of the main Pokemon Showdown server
mainServer :: String
mainServer = "sim.smogon.com"

-- | Port used by the main Pokemon Showdown server
mainPort :: Int
mainPort = 8000

-- | Path used by the main Pokemon Showdown server
mainPath :: String
mainPath = "/showdown/websocket"
