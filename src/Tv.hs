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
    minWidth <- liftM (fromMaybe 20) $ CFG.lookup cfg "min-width"
    --colors <- liftM (fromMaybe (white, red, magenta, white)) $ CFG.lookup cfg "colors"
    colors <- return $ (white, red, magenta, white)
    yester <- liftM (fromMaybe True) $ CFG.lookup cfg "show-trailing"
    sort <- liftM (fromMaybe True) $ CFG.lookup cfg "sort-channels"
    showAired <- liftM (fromMaybe True) $ CFG.lookup cfg "show-aired"
    width <- getCols
    tz <- getCurrentTimeZone
    current <- getCurrentTime
    return $ XmlTvPP url chans tz current width minWidth colors yester sort showAired

main :: IO ()
main = do
    args <- getArgs -- parse this for real
    cfg <- getConfig ["$(HOME)/.config/tv/tv.cfg"]
    showTvDay cfg
