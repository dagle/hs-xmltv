module Get (
    get
    , getAny
) where

import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import Network.HTTP
import Network.URI
import System.FilePath.Posix
import Data.List
import Data.Maybe

-- This is really horrible, REALLY
conv s = LB.fromChunks [C8.pack s]
reconv = C8.unpack . B.concat . LB.toChunks

-- we support http and local files
--get :: String -> IO (Maybe String)
get url = do
        match url
    where
        match str | Just _ <- stripPrefix "http" str = downloadURI str
        match str = (\y -> readFile y >>= return . Just) str

-- a hack for now
getAny url = do
    s <- get url
    case s of
        Nothing -> return ""
        (Just u) -> return $ uzip url u

uzip url s =
    case takeExtension url of
        ".gz" -> gunzip s
        _ -> s

-- doesn't support https... eeeew
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

downloadURI url = do
   rsp <- downloadURL url
   case rsp of
    Left _ -> return Nothing
    Right s -> return $ Just s

gunzip str = do
    reconv . decompress . conv $ str

