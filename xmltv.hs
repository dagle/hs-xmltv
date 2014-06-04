module Text.XmlTv (
    Channel(..)
    , Program(..)
    , xmlToChannel
    , xmlToProgram
    , parseChannels
    , parsePrograms
    , filterChans
    , updateChannel
    , previous
    , current
    , later
) where

import Control.Monad
import Data.Maybe
import Text.XML.Light
import Data.Time
import Data.Time.Clock
import System.Locale

data Channel = Channel {
    cid :: String
    , lang :: String
    , name :: String
    , base :: String
    , programs :: [Program]
} deriving (Show, Eq)

data Program = Program {
    start :: UTCTime
    , stop :: UTCTime
    , title :: String
    , description :: String
} deriving (Show, Eq)

toDate :: String -> Maybe UTCTime
toDate str = parseTime defaultTimeLocale "%Y%m%d%H%M%S %z" str

previous, current, later :: Program -> UTCTime -> Bool
previous (Program start stop _ _) now = diffUTCTime start now < 0 && diffUTCTime stop now < 0
current (Program start stop _ _) now = diffUTCTime start now < 0 && diffUTCTime stop now > 0
later (Program start stop _ _) now =  diffUTCTime start now > 0 && diffUTCTime stop now > 0

xmlToChannel :: Element -> Maybe Channel
xmlToChannel e = do
        id <- findAttr (QName "id" Nothing Nothing) e
        d <- findChild (QName "display-name" Nothing Nothing) e
        lang <- findAttr (QName "lang" Nothing Nothing) d
        title <- listToMaybe . map cdData . onlyText . elContent $ d
        b <- findChild (QName "base-url" Nothing Nothing) e
        base <- listToMaybe . map cdData . onlyText . elContent $ b
        return $ Channel id lang title  base []

-- A lot of optional fields that we should parse
xmlToProgram :: Element -> Maybe Program
xmlToProgram e = do
    start <- findAttr (QName "start" Nothing Nothing) e >>= toDate
    stop <- findAttr (QName "stop" Nothing Nothing) e >>= toDate
    t <- findChild (QName "title" Nothing Nothing) e
    --d <- findChild (QName "desc" Nothing Nothing) e
    title <- listToMaybe . map cdData . onlyText . elContent $ t
    --desc <- listToMaybe . map cdData . onlyText . elContent $ d
    return (Program start stop title "")

parseChannels :: String -> [Maybe Channel]
parseChannels str = do
    case parseXMLDoc str of
        Just p -> 
            let f = findElements (QName "channel" Nothing Nothing) p
            in map xmlToChannel f
        Nothing -> []

parsePrograms :: String -> [Maybe Program]
parsePrograms str = do
    case parseXMLDoc str of
        Just p -> 
            let f = findElements (QName "programme" Nothing Nothing) p
            in map xmlToProgram f
        Nothing -> []

-- Starts of by filtering empty channels and then applies another filter.
filterChans :: (Channel -> Bool) -> [Maybe Channel] -> [Channel]
filterChans f chans = 
    let pure = catMaybes chans
    in filter f pure

-- takes a channel, a prefix and a fetch method;
-- then etches all programs for that channel using prefix
-- (often date).
--updateChannel :: Channel -> String -> (String -> IO String) -> IO Channel
updateChannel prefix fetch c = do
    let url = base c ++ cid c ++ prefix 
    tv <- liftM (catMaybes . parsePrograms) . fetch $ url
    return c { programs = tv}
