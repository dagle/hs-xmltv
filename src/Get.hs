module Get (
    getAny
) where

import Data.ByteString.Lazy.Internal
import Network.HTTP.Client
import Network.URI
import System.IO
import Control.Exception
import System.Environment.XDG.BaseDir
import System.Posix.Files
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy as BL

getAny :: Bool -> String -> IO [Char]
getAny today url = do
    s <- downloadURI today url
    return $ unpackChars s

downloadURL :: String -> IO ByteString
downloadURL url = do
    man <- newManager defaultManagerSettings
    req <- parseUrl url
    rsp <- httpLbs req man
    return $ responseBody rsp

openCache :: String -> IOMode -> IO Handle
openCache url mode = do 
    f <- translate url
    stat <- getFileStatus f
    bool <- today . modificationTime $ stat
    if bool then openFile f mode else error "no file"

-- this is wrong: should be locale times etc
today :: Real s => s -> IO Bool
today time = do
    c <- getCurrentTime
    let t = utctDay c
    let t2 = utctDay . posixSecondsToUTCTime . realToFrac $ time
    return (t == t2)

writeCache :: String -> ByteString -> IO ()
writeCache url str = translate url >>= flip BL.writeFile str

-- ewwwwww
fixname :: [Char] -> [Char]
fixname s =  
    if s == s' then s else s' ++ ".xml"
    where 
        s' = takeWhile ((/=) '_') . rdrop 3 $ s
        rdrop n = reverse . drop n . reverse 

translate :: String -> IO [Char]
translate url = do 
    let f = maybe url uriPath $ parseURI url
    let f' = fixname f
    getUserCacheDir "" >>= \x -> return $ x ++ f'

cache :: String -> IO ByteString
cache url = do
    r <- mytry $ openCache url ReadMode
    case r of
        Left _ -> do
            rsp <- downloadURL url
            mytry $ writeCache url rsp
            return rsp
        Right f ->
            BL.hGetContents f >>= return

mytry :: IO a -> IO (Either SomeException a)
mytry = try

downloadURI :: Bool -> String -> IO ByteString
downloadURI today url
    | today = cache url 
    | otherwise = downloadURL url
