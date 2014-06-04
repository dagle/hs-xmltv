module Main (
    downloadURL
    , main
) where

import Codec.Compression.GZip
import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import Data.List
import Data.Time.LocalTime
import System.Console.Terminfo.Base
import Text.PrettyPrint.Free hiding (width)
import System.Console.Terminfo.PrettyPrint
import Text.XmlTv
import Data.Time.Clock
import Data.Maybe
import Data.Time
import System.Locale
import Control.Monad
import System.Environment

type Coloring = TermDoc -> TermDoc

data XmlTvPP = XmlTvPP {
    tz :: TimeZone
    , colors :: (Coloring, Coloring, Coloring) -- past, current, future
    , currentTime :: UTCTime
    , width :: Int
    , minWidth :: Int
    , yesterLeft :: Bool -- Show programs from yesterday that haven't haven't aired yet
}

-- minum title, if we can't fit with 15, break the row.
minTitle = 20

downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url

xml url = do
   rsp <- downloadURL url
   case rsp of
    Left x -> return Nothing
    Right s -> return . Just . gunzip $ s

showHour :: UTCTime -> TimeZone -> String
showHour time tz =
    let local = utcToLocalTime tz time
        in formatTime defaultTimeLocale "%R" local 

-- This is really horrible, REALLY
conv s = LB.fromChunks [C8.pack s]
reconv = C8.unpack . B.concat . LB.toChunks

--highligt :: (UTCTime, UTCTime) -> UTCTime -> (Coloring, Coloring, Coloring) -> Coloring
highligt prg currentTime (b, c, a)
    | previous prg currentTime = b
    | current prg currentTime = c
    | later prg currentTime = a

--entry :: XmlTvPP -> Int -> Maybe Program -> Doc 
entry _ i Nothing = fill i space
entry cfg i (Just p@(Program start stop title desc)) = highligt p (currentTime cfg) (colors cfg) $ 
                        text "[" <> (text $ showHour start (tz cfg)) 
                        <> text "] " <> fill (i-9) (text $ take (i-9) title) <> space

-- this is to make printing much easier, since all list are equally long
fillNoth :: [[a]] -> [[Maybe a]]
fillNoth cs =
    let longest = foldr (max . length) 0 cs
    in map (addNoth longest) cs
    where addNoth long l = map Just l ++ replicate (long - length l) Nothing

divide num [] = []
divide num xs = helper realnum xs
    where 
        len = length xs
        lines = div (len + num) num
        realnum = ceiling $ (fromIntegral len) / (fromIntegral lines)
        helper _ [] = []
        helper num xs = cur : helper num after
            where
                cur = take num xs
                after = drop num xs

-- min is how small a title can be to make sense, 
-- max is how much width you want to use to print
showPrograms _ [] = empty
showPrograms cfg cs = 
    map (showGroup cfg) css
    where 
        css = divide num cs
        num = div (width cfg) (minWidth cfg)

--showing :: [Channel] -> String
showGroup cfg cs = (hcat $ map (fill maxtitle . text . name) cs) <> hardline <> chans
    where maxtitle = div (width cfg) $ length cs
          zprogs = transpose . fillNoth $ map programs cs
          chans = hcat $ map (\x -> (hcat $ map (\y -> entry cfg maxtitle y) x) <> hardline) zprogs

gunzip str = do
    reconv . decompress . conv $ str

-- this should work on all unix terminal
getCols :: IO Int
getCols = do
    t <- setupTerm "vt100"
    let Just x = getCapability t (tiGetNum "cols")
    return x

xml' uri = do
    r <- xml uri
    return $ fromJust r
--getTvToday :: String -> [String] -> IO [Channel]
getTvToday uri channels = do
   day <- liftM (show . utctDay) $ getCurrentTime
   str <- xml' uri
   let c = parseChannels str
   let w = filterChans (\a -> elem (name a) channels) c
   t <- mapM (updateChannel ("_" ++ day ++ ".xml.gz") xml') w
   return t

showTvToday uri channels = do
    chans <- getTvToday uri channels
    width <- getCols
    tz <- getCurrentTimeZone
    current <- getCurrentTime
    display $ showPrograms (XmlTvPP tz (red, magenta, white) current width minTitle False) chans

url = "http://tv.swedb.se/xmltv/channels.xml.gz"  

main = do
    args <- getArgs
    showTvToday url args
