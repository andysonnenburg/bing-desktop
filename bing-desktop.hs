{-# LANGUAGE CPP, DeriveDataTypeable #-}
#ifdef OS_WINDOWS
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Control.Exception (bracket)
import Control.Monad

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.List (find, nub)
import Data.Maybe (fromMaybe)

#ifdef OS_WINDOWS
import Foreign.C (CInt (..))
#else
import Foreign.C (CInt)
#endif

#ifndef OS_WINDOWS
import Graphics.X11.Xlib (Display)
import Graphics.X11.Xlib.Display
#endif

import Network.HTTP
import Network.URI

import System.Console.CmdArgs
#ifdef OS_WINDOWS
import System.Environment (getProgName)
#else
import System.Environment (getEnv, getProgName)
#endif
import System.FilePath (takeFileName)
import System.IO (hPrint, stderr)
#ifndef OS_WINDOWS
import System.IO.Error (catchIOError, isDoesNotExistError, isUserError)
#endif

import Text.XML.HXT.Core

data BingDesktop = BingDesktop {} deriving (Data, Typeable)

bingDesktop :: BingDesktop
bingDesktop = BingDesktop {} &= verbosity

main :: IO ()
main = do
  progName <- getProgName
  BingDesktop {} <- cmdArgs $ bingDesktop &= program progName
  bracket openStream' close $ \ stream ->
    forM mkts (\ mkt ->
      let request = httpGet "/HPImageArchive.aspx" ("?idx=0&n=8&" ++ mkt) in
      whenLoud (hPrint stderr request) >>
      sendHTTP stream request >>=
      getResponseBody >>= \ xmlString ->
      runX (readString [] (ByteString.unpack xmlString) >>>
            get "images" >>>
            get "image" >>>
            get "urlBase" >>>
            getChildren >>>
            getText)) >>= \ urlBases -> do
      (width, height) <- getResolution
      forM_ (nub . concat $ urlBases) $ \ urlBase ->
        let imagePath = urlBase ++ "_" ++
                        show width ++ "x" ++ show height ++ ".jpg"
            imageRequest = httpGet imagePath "" in
        whenLoud (hPrint stderr imageRequest) >>
        sendHTTP stream imageRequest >>=
        getResponseBody >>=
        ByteString.writeFile (takeFileName imagePath)

get :: ArrowXml a => String -> a XmlTree XmlTree
get n = getChildren >>> isElem >>> hasName n

openStream' :: HStream a => IO (HandleStream a)
openStream' = openStream hostname 80

httpGet :: String -> String -> Request ByteString
httpGet uriPath uriQuery =
  mkRequest GET URI { uriScheme = "http:"
                    , uriAuthority = Just URIAuth { uriUserInfo = ""
                                                  , uriRegName = hostname
                                                  , uriPort = ""
                                                  }
                    , uriPath
                    , uriQuery
                    , uriFragment = ""
                    }

getResolution :: IO (CInt, CInt)
#ifdef OS_WINDOWS
getResolution = liftM2 normalizeResolution getScreenWidth getScreenHeight

foreign import ccall unsafe "GetScreenWidth" getScreenWidth :: IO CInt

foreign import ccall unsafe "GetScreenHeight" getScreenHeight :: IO CInt
#else
getResolution =
  bracket openDefaultDisplay closeDisplay
  (\ display ->
    let screenNumber = defaultScreen display in
    return $
    normalizeResolution
    (displayWidth display screenNumber)
    (displayHeight display screenNumber))
  `catchIOError` \ e ->
  if isUserError e then return highestResolution else ioError e

openDefaultDisplay :: IO Display
openDefaultDisplay =
  openDisplay =<<
  getEnv "DISPLAY"
  `catchIOError` \ e ->
  if isDoesNotExistError e then return ":0" else ioError e
#endif

normalizeResolution :: CInt -> CInt -> (CInt, CInt)
normalizeResolution width height =
  fromMaybe highestResolution $ find (resolution <=) resolutions
  where
    resolution = (width, height)

resolutions :: [(CInt, CInt)]
resolutions = [ (1024, 768)
              , (1280, 720)
              , (1366, 768)
              , highestResolution
              ]

highestResolution :: (CInt, CInt)
highestResolution = (1920, 1200)

hostname :: String
hostname = "www.bing.com"

mkts :: [String]
mkts = [ "en-US"
       , "zh-CN"
       , "ja-JP"
       , "en-AU"
       , "en-UK"
       , "de-DE"
       , "en-NZ"
       , "en-CA"
       ]
