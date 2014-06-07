{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Configurator as CFG
import Data.Configurator.Types

type Coloring = TermDoc -> TermDoc

data XmlTvPP = XmlTvPP {
    url :: String
    , channels :: [String]
    , tz :: TimeZone
    , currentTime :: UTCTime
    , width :: Int
    , minWidth :: Int
    , colors :: (Coloring, Coloring, Coloring, Coloring) -- channel, past, current, future
    , yesterLeft :: Bool -- Show programs from yesterday that haven't haven't aired yet
    , doSort :: Bool
    , showAired :: Bool
}

instance Show XmlTvPP where
    show (XmlTvPP u c t cu w mw _ y d s) = "XmlTvPP" ++ u ++ show c ++ show cu ++
            show w ++ show mw ++ show y ++ show d ++ show s
        

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
          uri = fromJust1 $ parseURI url

fromJust1 (Just x) = x

xml url = do
   rsp <- downloadURL url
   case rsp of
    Left x -> return ""
    Right s -> return . gunzip $ s

showHour :: UTCTime -> TimeZone -> String
showHour time tz =
    let local = utcToLocalTime tz time
        in formatTime defaultTimeLocale "%R" local 

showDay tz utc = 
    let local = utcToLocalTime tz utc
        in formatTime defaultTimeLocale "%F" local

-- This is really horrible, REALLY
conv s = LB.fromChunks [C8.pack s]
reconv = C8.unpack . B.concat . LB.toChunks

highligt :: Program -> UTCTime -> (t, t1, t1, t1) -> t1
highligt prg currentTime (_,b, c, a)
    | previous prg currentTime = b
    | current prg currentTime = c
    | later prg currentTime = a

entry :: XmlTvPP -> Int -> Maybe Program -> Doc Effect
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

divide :: Int -> [a] -> [[a]]
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

showPrograms :: XmlTvPP -> [Channel] -> Doc Effect
showPrograms _ [] = empty
showPrograms cfg cs = 
    hcat $ map (showGroup cfg) css
    where 
        css = divide num cs
        num = div (width cfg) (minWidth cfg)

showGroup :: XmlTvPP -> [Channel] -> Doc Effect
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

wasYesterday :: UTCTime -> Program -> Bool
wasYesterday day (Program _ stop _ _) = utctDay day <= utctDay stop

filterToday :: UTCTime -> [Channel] -> [Channel]
filterToday time channels =
    map (\c -> c { programs = filter (\p -> not $ previous p time) (programs c)}) channels
    --map (\c -> c { programs = filter (wasYesterday day) (programs c)}) channels

yester :: UTCTime -> UTCTime
yester t = 
    let d = utctDay t
        c = utctDayTime t
        in UTCTime (addDays (-1) d) c

getTvDay :: XmlTvPP -> IO [Channel]
getTvDay cfg = do
   str <- xml (url cfg)
   let day = currentTime cfg
   let y = yester day -- day - 1
   let chans = parseChannels str
   let fc = filterChans (\a -> elem (name a) (channels cfg)) chans
   --mapM (updateChannel (toPrefix (showDay (tz cfg) day)) xml) fc
   if yesterLeft cfg 
        then 
            do k <- mapM (updateChannel (toPrefix (showDay (tz cfg) y)) xml) fc
               let k' = filterToday (currentTime cfg) k
               mapM (updateChannel (toPrefix (showDay (tz cfg) day)) xml) k'
        else mapM (updateChannel (toPrefix (showDay (tz cfg) day)) xml) fc
    where
        toPrefix date = "_" ++ date ++ ".xml.gz"

showTvDay :: XmlTvPP -> IO ()
showTvDay cfg = do
    chans <-  getTvDay cfg
    let schans = if doSort cfg then sortChans (channels cfg) chans else chans
    display $ showPrograms cfg chans

getHomoList :: Config -> Name -> IO (Maybe [String])
getHomoList cfg str = do
    r <- CFG.lookup cfg str
    case r of
        (Just (List v)) -> return $ helper v
        _ -> return $ Nothing
    where
        helper values =
            let vs = map convert values :: [Maybe String]
                homo = foldr (\x y -> y && isJust x) True vs
                in if homo
                        then Just $ map fromJust vs
                        else Nothing 

getConfig :: [CFG.Worth FilePath] -> IO XmlTvPP
getConfig paths = do
    cfg <- CFG.load paths
    url <- CFG.require cfg "url"
    chans <- liftM (fromMaybe []) $ getHomoList cfg "channels" -- CFG.lookup cfg "channels" :: IO (Maybe [String])
    minWidth <- liftM (fromMaybe 20) $ CFG.lookup cfg "minW-width"
    --colors <- liftM (fromMaybe (white, red, magenta, white)) $ CFG.lookup cfg "colors"
    colors <- return $ (white, red, magenta, white)
    yester <- liftM (fromMaybe True) $ CFG.lookup cfg "show-trailing"
    sort <- liftM (fromMaybe True) $ CFG.lookup cfg "sort-channels"
    showAired <- liftM (fromMaybe True) $ CFG.lookup cfg "show-aired"
    width <- getCols
    tz <- getCurrentTimeZone
    current <- getCurrentTime
    return $ XmlTvPP url chans tz current width minWidth colors yester sort showAired

main = do
    args <- getArgs -- parse this for real
    cfg <- getConfig ["$(HOME)/.config/tv/tv.cfg"]
    showTvDay cfg
