{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudprint where

import           Network.Cloudprint.Internal

import           Control.Monad.IO.Class
import           Data.ByteString
import qualified Data.ByteString.Char8                 as B8
import           Data.Monoid
import           Network.Google.OAuth2
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit
import           Network.HTTP.Types                    (hAuthorization)

type PrinterId = ByteString
type PrintContentType = ByteString
type PrintJobName = ByteString

formData :: MonadIO m => PrinterId        -- ^ Cloudprint ID for the printer
                      -> PrintContentType -- ^ MIME type for the print job
                      -> PrintJobName     -- ^ Name of the print job
                      -> ByteString       -- ^ File contents to print
                      -> Request          -- ^ Request to add the form data to
                      -> m Request        -- ^ Resulting request
formData printerId contentType name conts = formDataBody [
                    -- partBS "xsrf"                                        --  hidden text
                    partBS "printerid" printerId                                   -- hidden text
                  , partBS "jobid" ""                                       -- hidden text
                  , partBS "ticket" "{\"version\": \"1.0\",\"print\": {}}"  --  text
                  , partBS "title" name
                  , partBS "contentType" contentType
                  , partBS "content" conts                --  file
                   ]


addPrintJob :: OAuth2Client
            -> FilePath
            -> PrinterId        -- ^ Cloudprint ID for the printer
            -> PrintContentType -- ^ MIME type for the print job
            -> PrintJobName     -- ^ Name of the print job
            -> ByteString       -- ^ File contents to print
            -> IO ()
addPrintJob client tokenFp pid ctype name conts = do
  let addFd = formData pid ctype name conts
  token <- useAccessToken client tokenFp
  case token of
    (Just t) -> do
      req <- (addFd . (authorize t)) =<< parseUrl "https://www.google.com/cloudprint/submit"
      resp <- withManager $ httpLbs req
      return ()
    Nothing -> do
      return ()
  where authorize token request = request
                                    { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " <> token)] }

