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
Module      : Needles.Bot.Message.In.Parse
Description : Handles parsing of PS messages
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : ghc
-}

{-# LANGUAGE OverloadedStrings #-}
module Needles.Bot.Message.In.Parse (
                                      Message(..)
                                    , parseMessage
                                    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (decodeUtf8With)
import           Data.Text.Encoding.Error   (lenientDecode)
import           Data.Word                  (Word8)

import           Safe                       (readMay)

type Room = Text
type User = Text
type What = Text
type Time = Integer

-- | The types of messages you get from Pokemon Showdown
data Message = Unknown Text
             | ChallStr Integer String
             | Chat Room Time User What
             | Pm User What
             | Timestamp Room Integer
             | Raw Text Text
             | Base Text
             deriving (Show)

parseMessage :: ByteString -> [Message]
parseMessage input = case AP.parseOnly messages input of
                Left _   -> [Unknown (decodeSmooth input)]
                Right ms -> ms

decodeSmooth :: ByteString -> Text
decodeSmooth = decodeUtf8With lenientDecode

-- Character Codes

pipeCode :: Word8
pipeCode = 124

newlineCode :: Word8
newlineCode = 10

rightAngleCode :: Word8
rightAngleCode = 62


-- Basic Parsers

die :: Parser Word8
die = AP.satisfy (const False)

inMessage :: Parser Text
inMessage = decodeSmooth <$> AP.takeWhile notThese
  where notThese w = w /= newlineCode && w /= pipeCode

mLex :: Parser Text
mLex = AP.word8 pipeCode *> inMessage

dLex :: Parser Integer
dLex = readMay . unpack <$> mLex >>= check
  where check Nothing  = fromIntegral <$> die
        check (Just x) = pure x

contLex :: Parser Text
contLex = AP.word8 pipeCode *> (decodeSmooth <$> AP.takeWhile (/= newlineCode))

room :: Parser Room
room = AP.word8 rightAngleCode *> (decodeSmooth <$> AP.takeWhile (/= newlineCode))

getArgs :: Parser [Text]
getArgs = many mLex

newline :: Parser Word8
newline = AP.word8 newlineCode

-- Message type specific parsers

pm :: Parser Message
pm = AP.string "|pm" *> (Pm <$> mLex <*> (mLex *> contLex))

chatTime :: Text -> Parser Message
chatTime r = AP.string "|c:" *> (Chat r <$> dLex <*> mLex <*> contLex)

chat :: Text -> Parser Message
chat r = AP.string "|c" *> (Chat r 9999999999999999 <$> mLex <*> contLex)

raw :: Text -> Parser Message
raw r = AP.string "|raw" *> (Raw r <$> contLex)

challStr :: Parser Message
challStr = AP.string "|challstr" *> (ChallStr <$> dLex <*> fmap unpack mLex)

time :: Text -> Parser Message
time r = Timestamp r <$> dLex

baseStr :: Parser Message
baseStr = Base . decodeSmooth <$> AP.takeWhile (/= newlineCode)


-- Main message parser

messages :: Parser [Message]
messages =  roomMessages <|> lobbyMessages
  where lobbyMessages = liftA2 (:) (message "") (many (newline *> message ""))
        roomMessages = do
          r <- room
          many (newline *> message r)


message :: Text -> Parser Message
message r = AP.choice parsers <|> baseStr
  where nonRoomParsers = [ pm
                         , challStr
                         ]
        roomParsers = map ($ r) [ chatTime
                                , chat
                                , raw
                                , time
                                ]
        parsers =  nonRoomParsers ++ roomParsers
