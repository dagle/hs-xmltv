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

data Channel = Channel {
    id :: String
    , lang :: String
    , name :: String
    , url :: String
    , programs :: [Program]
} deriving (Show, Eq)

data Program = Program {
    start :: String
    , stop :: String
    , title :: String
    , description :: String
} deriving (Show, Eq)

conv s = LB.fromChunks [C8.pack s]
reconv = C8.unpack . B.concat . LB.toChunks

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

xmltoAir :: Element -> Maybe Program
xmltoAir e = do
    start <- findAttr (QName "start" Nothing Nothing) e
    stop <- findAttr (QName "stop" Nothing Nothing) e
    t <- findChild (QName "title" Nothing Nothing) e
    d <- findChild (QName "desc" Nothing Nothing) e
    title <- headm . map cdData . onlyText . elContent $ t
    desc <- headm . map cdData . onlyText . elContent $ d
    return (Program start stop title desc)

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
            in return $ map xmltoAir f
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

-- needs an output format or something
