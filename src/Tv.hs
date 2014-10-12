{-# LANGUAGE OverloadedStrings #-}

import Data.Time.LocalTime
import Data.Time.Clock
import Data.Maybe
import Control.Monad
import System.Environment
import qualified Data.Configurator as CFG
import Data.Configurator.Types
import Format
import System.Console.Terminfo.PrettyPrint
import Data.Time.Calendar
import Data.Time.Clock
import System.Console.GetOpt
import Text.Read

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

addDaysTime :: Integer -> UTCTime -> UTCTime
addDaysTime i (UTCTime day t) =
    let dday = addDays i day
    in UTCTime dday t

-- can't we do this better, also missing colors
str2color :: String -> TermDoc -> TermDoc
str2color str
    | str == "white" = white
    | str == "red" = red
    | str == "magenta" = magenta

getColors :: Config -> Name -> IO (Coloring, Coloring, Coloring, Coloring)
getColors cfg name = do
    s <- CFG.lookup cfg name
    case s of
        Nothing -> return (white, red, magenta, white)
        (Just (a,b,c,d)) -> return (str2color a, str2color b, str2color c, str2color d)

getConfig :: [Worth FilePath] -> IO XmlTvPP
getConfig paths = do
    cfg <- CFG.load paths
    url <- liftM (fromMaybe "") $ CFG.lookup cfg "url"
    chans <- liftM (fromMaybe []) $ getHomoList cfg "channels" 
    minWidth <- liftM (fromMaybe 20) $ CFG.lookup cfg "min-width"
    colors <- getColors cfg "colors"
    yester <- liftM (fromMaybe False) $ CFG.lookup cfg "show-trailing"
    sort <- liftM (fromMaybe True) $ CFG.lookup cfg "sort-channels"
    showAired <- liftM (fromMaybe True) $ CFG.lookup cfg "show-aired"
    width <- getCols
    tz <- getCurrentTimeZone
    current <- getCurrentTime
    return $ XmlTvPP url chans tz current width minWidth colors yester sort showAired True

readpos :: String -> Int
readpos str =
    let i = read str
    in if i >= 0 then i else error "negative number"

parseChannels :: String -> [String]
parseChannels str =
    case readMaybe str of
        (Just c) -> c
        Nothing -> [str]

options :: [OptDescr (XmlTvPP -> XmlTvPP)]
options =
    [ Option ['t']  ["time"]
        (ReqArg (\d cfg -> cfg { currentTime = addDaysTime (read d) (currentTime cfg)
            , today = (read d) == 0}) "relative day")
            "relative day, -1,2,+1"
    , Option ['c']  ["channel"]
        (ReqArg (\c cfg -> cfg { channels = parseChannels c}) "channels")
        "channels you want to display"
    , Option ['w']  ["width"]
        (ReqArg (\w cfg -> cfg { minWidth = readpos w }) "width")
        "Min width of channel"
    , Option ['u']  ["url"]
        (ReqArg (\u cfg -> cfg { url = u }) "xmltv url")
        "url to the xmltv"
    ]

parseArgs :: XmlTvPP -> [String] -> IO (XmlTvPP, [String])
parseArgs cfg args =
    case getOpt Permute options args of
        (o,n,[]) -> return (foldl (flip id) cfg o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: tv [OPTION...]"

main :: IO ()
main = do
    args <- getArgs
    cfg <- getConfig ["$(HOME)/.config/tv/tv.cfg"]
    (parsed, _) <- parseArgs cfg args
    showTvDay parsed
