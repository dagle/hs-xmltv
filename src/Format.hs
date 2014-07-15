module Format (
    XmlTvPP(..)
    , getCols
    , showTvDay
) where

import System.Console.Terminfo.Base
import Text.PrettyPrint.Free hiding (width)
import System.Console.Terminfo.PrettyPrint
import Data.Time.Clock
import Data.Time
import System.Locale
import Text.XmlTv
import Data.List
import Get

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

showHour :: UTCTime -> TimeZone -> String
showHour time tz =
    let local = utcToLocalTime tz time
        in formatTime defaultTimeLocale "%R" local 

showDay :: TimeZone -> UTCTime -> String
showDay tz utc = 
    let local = utcToLocalTime tz utc
        in formatTime defaultTimeLocale "%F" local

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
   str <- getAny (url cfg)
   let day = currentTime cfg
   let y = yester day -- day - 1
   let chans = parseChannels str
   let fc = filterChans (\a -> elem (name a) (channels cfg)) chans
   --mapM (updateChannel (toPrefix (showDay (tz cfg) day)) getAny) fc
   if yesterLeft cfg 
        then 
            do k <- mapM (updateChannel (toPrefix (showDay (tz cfg) y)) getAny) fc
               let k' = filterToday (currentTime cfg) k
               mapM (updateChannel (toPrefix (showDay (tz cfg) day)) getAny) k'
        else mapM (updateChannel (toPrefix (showDay (tz cfg) day)) getAny) fc
    where
        toPrefix date = "_" ++ date ++ ".getAny.gz"

showTvDay :: XmlTvPP -> IO ()
showTvDay cfg = do
    chans <-  getTvDay cfg
    let schans = if doSort cfg then sortChans (channels cfg) chans else chans
    display $ showPrograms cfg chans

