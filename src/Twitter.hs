{-# LANGUAGE ScopedTypeVariables #-}
module Twitter where

import Web.Twitter.Conduit
import Web.Twitter.Types
import Control.Applicative
import Control.Exception
import Data.Conduit
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as H
import qualified Data.Conduit.List as CL
import Control.Lens ((&), (?~))

getHomeTimeline :: TWInfo -> Maybe StatusId -> IO (Either String [Status])
getHomeTimeline twInfo since =
    let req = case since of
                Nothing -> homeTimeline & includeEntities ?~ True
                Just s -> homeTimeline & includeEntities ?~ True & sinceId ?~ s

        timeline = withManager $ \mgr ->
            sourceWithMaxId twInfo mgr req $= CL.isolate 50 $$ CL.consume

    in (Right <$> timeline) `catch` handleException

handleException :: HttpException -> IO (Either String [Status])
handleException (StatusCodeException sc _ _) = do
    case H.statusCode sc of
        429 -> return $ Left "API request exceeded rate limit"
        s -> return $ Left $ "Unknown error, status = " ++ show s
handleException e = return $ Left $ "Unknown error: " ++ show e
