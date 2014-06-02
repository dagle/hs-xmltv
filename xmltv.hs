import Codec.Compression.GZip
import Network.HTTP
import Data.Maybe
import Network.URI
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import Control.Monad
import Data.Maybe
import Text.XML.Light
import qualified Data.ByteString as B
import Data.Time
import Data.List
import Data.Time.Clock
import System.Locale
import Data.Time.LocalTime
import System.Console.Terminfo.Base
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

data Channel = Channel {
    cid :: String
    , lang :: String
    , name :: String
    , url :: String
    , programs :: [Program]
} deriving (Show, Eq)

data Program = Program {
    start :: UTCTime
    , stop :: UTCTime
    , title :: String
    , description :: String
} deriving (Show, Eq)

-- minum title, if we can't fit with 15, break the row.
minTitle = 20

conv s = LB.fromChunks [C8.pack s]
reconv = C8.unpack . B.concat . LB.toChunks

toDate :: String -> Maybe UTCTime
toDate str = parseTime defaultTimeLocale "%Y%m%d%H%M%S %z" str

showHour t tz = do 
    let local = utcToLocalTime tz t
        in formatTime defaultTimeLocale "%R" local 

-- worldcup in choosing bad colors
highligt sta sto current
    | diffUTCTime sta current < 0 && diffUTCTime sto current < 0 = red
    | diffUTCTime sta current < 0 && diffUTCTime sto current > 0 = magenta
    | otherwise = white

entry Nothing _ i _ = fill i space
entry (Just (Program sta sto t d)) tz i current = highligt sta sto current $ 
                        text "[" <> (text $ showHour sta tz) 
                        <> text "] " <> fill (i-9) (text $ take (i-9) t) <> space

fillNoth cs =
    let longest = foldr (max . length) 0 cs
    in map (addNoth longest) cs
    where addNoth long l = map Just l ++ replicate (long - length l) Nothing

-- min is how small a title can be to make sense, 
-- max is how much width you want to use to print
showPrograms tz min max _ [] = empty
showPrograms tz min max current cs = 
    let num = div max min
        cur = take num cs
        after = drop num cs
        in showGroup tz max current cur <> showPrograms tz min max current after

--showing :: [Channel] -> String
--showGroup = undefined
showGroup tz max current cs = (hcat $ map (fill maxtitle . text . name) cs) <> hardline <> chans
    where maxtitle = div max $ length cs
          zprogs = transpose . fillNoth $ map programs cs
          chans = hcat $ map (\x -> (hcat $ map (\y -> entry y tz maxtitle current) x) <> hardline) zprogs

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

gunzip str = do
    reconv . decompress . conv $ str

-- this should work on all unix terminal
getCols = do
    t <- setupTerm "vt100"
    let Just x = getCapability t (tiGetNum "cols")
    return x

-- safer version for use inside of a Maybe Monad.
headm [] = Nothing
headm (x:xs) = Just x

xmltoChannel :: String -> Element -> Maybe Channel
xmltoChannel date e = do
        id <- findAttr (QName "id" Nothing Nothing) e
        d <- findChild (QName "display-name" Nothing Nothing) e
        lang <- findAttr (QName "lang" Nothing Nothing) d
        title <- headm . map cdData . onlyText . elContent $ d
        b <- findChild (QName "base-url" Nothing Nothing) e
        base <- headm . map cdData . onlyText . elContent $ b
        return $ Channel id lang title (base ++ id ++ "_" ++ date ++ ".xml.gz") []

-- This is broken, looks nice atleast.
xmltoProgram :: Element -> Maybe Program
xmltoProgram e = do
    start <- findAttr (QName "start" Nothing Nothing) e >>= toDate
    stop <- findAttr (QName "stop" Nothing Nothing) e >>= toDate
    t <- findChild (QName "title" Nothing Nothing) e
    d <- findChild (QName "desc" Nothing Nothing) e
    title <- headm . map cdData . onlyText . elContent $ t
    desc <- headm . map cdData . onlyText . elContent $ d
    return (Program start start title desc)

takeJust :: [Maybe b] -> [b]
takeJust a = map fromJust $ filter (\x ->
                                case x of
                                  Nothing -> False
                                  _ -> True) a

myChannels :: [String] -> [Maybe Channel] -> [Channel]
myChannels list a = 
    let j = takeJust a
    in concatMap (\y -> filter (\x -> (name x) == y) j) list

parseChannels :: String -> String -> IO [Maybe Channel]
parseChannels url day = do
    s <- liftM fromJust $ xml url
    case parseXMLDoc s of
        Just p -> 
            let f = findElements (QName "channel" Nothing Nothing) p
            in return $ map (xmltoChannel day) f
        Nothing -> return []

getPrograms :: String -> IO [Maybe Program]
getPrograms url = do
    s <- liftM fromJust $ xml url
    case parseXMLDoc s of
        Just p -> 
            let f = findElements (QName "programme" Nothing Nothing) p
            in return $ map xmltoProgram f
        Nothing -> return []

updateChannel :: Channel -> IO Channel
updateChannel c = do
    tv <- liftM takeJust $ getPrograms (url c)
    return c { programs = tv}

getTvToday :: String -> [String] -> IO [Channel]
getTvToday uri channels = do
   day <- liftM (show . utctDay) $ getCurrentTime
   c <- parseChannels uri day
   let w = myChannels channels c
   t <- mapM updateChannel w
   return t

showTvToday uri channels = do
    chans <- getTvToday uri channels
    width <- getCols
    tz <- getCurrentTimeZone
    current <- getCurrentTime
    display $ showPrograms tz minTitle width current chans

main = do
    showTvToday "http://tv.swedb.se/xmltv/channels.xml.gz" ["SVT 1", "TV3"]
