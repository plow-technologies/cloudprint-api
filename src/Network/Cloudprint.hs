{-# LANGUAGE OverloadedStrings #-}
module Network.Cloudprint (
  addPrintJob
)where

import           Control.Monad.IO.Class
import           Data.ByteString
import qualified Data.ByteString.Char8                 as B8
import qualified Data.Either.Validation                as V
import           Data.Monoid

import           Network.Google.OAuth2
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit
import           Network.HTTP.Types                    (hAuthorization)
import           Network.HTTP.Types.Status

type PrinterId = ByteString
type PrintContentType = ByteString
type PrintJobName = ByteString
type FileName = ByteString

formData :: MonadIO m => PrinterId        -- ^ Cloudprint ID for the printer
                      -> PrintContentType -- ^ MIME type for the print job
                      -> PrintJobName     -- ^ Name of the print job
                      -> FilePath
                      -> ByteString       -- ^ File contents to print
                      -> Request          -- ^ Request to add the form data to
                      -> m Request        -- ^ Resulting request
formData printerId contentType name fn conts = formDataBody [
                    -- partBS "xsrf"                                        --  hidden text
                    partBS "printerid" printerId                                   -- hidden text
                  , partBS "jobid" ""                                       -- hidden text
                  , partBS "ticket" "{\"version\": \"1.0\",\"print\": {}}"  --  text
                  , partBS "title" name
                  , partBS "contentType" contentType
                  , partFileRequestBody "content" fn $ RequestBodyBS conts                --  file
                   ]

-- | Add a print job to a given oauth session on google print
addPrintJob :: OAuth2Client
            -> FilePath         -- ^ Filepath for the Oauth token 
            -> PrinterId        -- ^ Cloudprint ID for the printer
            -> PrintContentType -- ^ MIME type for the print job
            -> PrintJobName     -- ^ Name of the print job
            -> FilePath         -- ^ Fake filename for the uploaded file
            -> ByteString       -- ^ File contents to print
            -> IO (V.Validation String ())
addPrintJob client tokenFp pid ctype name fn conts = do
  let addFd = formData pid ctype name fn conts 
  token <- useAccessToken client tokenFp
  case token of
    (Just t) -> do
      req <- (addFd . (authorize t)) =<< parseUrl "https://www.google.com/cloudprint/submit"
      resp <- withManager $ httpLbs req
      case responseStatus resp of
        (Status 200 _) -> return $ V.Success ()
        _ -> return $ V.Failure (show resp)
    Nothing -> return $ V.Failure "Failure to load token"
    
  where authorize token request = request
                                    { requestHeaders = [(hAuthorization, B8.pack $ "Bearer " <> token)] }



