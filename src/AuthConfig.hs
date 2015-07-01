{-# LANGUAGE OverloadedStrings #-}
module AuthConfig
  ( readAuthConfig
  )
where

import Data.Default
import Data.Ini
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Web.Authenticate.OAuth (Credential(..), OAuth(..))
import Web.Twitter.Conduit.Types (TWInfo(..))
import Web.Twitter.Conduit.Monad
import System.Exit (exitFailure)

readAuthConfig :: IO TWInfo
readAuthConfig = do
    -- Read configuration file
    config <- readIniFile "toot.cfg"
    ini <- case config of
             Left e -> putStrLn e >> exitFailure
             Right i -> return i

    -- Build configuration
    let cfg = unIni ini
        consumerCfg = cfg H.! "consumer"
        tokenCfg = cfg H.! "token"
        textToBS = BS.pack . T.unpack
        oauthData = twitterOAuth { oauthConsumerKey = textToBS $ consumerCfg H.! "oauth_consumer_key"
                                 , oauthConsumerSecret = textToBS $ consumerCfg H.! "oauth_consumer_secret"
                                 }
        creds = Credential [ ("oauth_token", textToBS $ tokenCfg H.! "oauth_token")
                           , ("oauth_token_secret", textToBS $ tokenCfg H.! "oauth_token_secret")
                           ]
        twInfo = def { twToken = def { twOAuth = oauthData
                                     , twCredential = creds
                                     }
                     , twProxy = Nothing
                     }

    return twInfo
