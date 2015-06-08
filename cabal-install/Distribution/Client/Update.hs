-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Update
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Update
    ( update
    ) where

import Distribution.Client.Types
         ( Repo(..), RemoteRepo(..), repoRemote' )
import Distribution.Client.HttpUtils
         ( DownloadResult(..), HttpTransport(..) )
import Distribution.Client.FetchUtils
         ( downloadIndex )
import Distribution.Client.IndexUtils
         ( updateRepoIndexCache )
import Distribution.Client.JobControl
         ( newParallelJobControl, spawnJob, collectJob )

import Distribution.Simple.Utils
         ( writeFileAtomic, warn, notice )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.ByteString.Lazy       as BS
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath (dropExtension)
import Data.Maybe (catMaybes)

import qualified Hackage.Security.Client as Sec

-- | 'update' downloads the package list from all known servers
update :: HttpTransport -> Verbosity -> Bool -> [Repo] -> IO ()
update _ verbosity _ [] =
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
update transport verbosity ignoreExpiry repos = do
  jobCtrl <- newParallelJobControl
  let remoteRepos = catMaybes (map repoRemote' repos)
  case remoteRepos of
    [] -> return ()
    [remoteRepo] ->
        notice verbosity $ "Downloading the latest package list from "
                        ++ remoteRepoName remoteRepo
    _ -> notice verbosity . unlines
            $ "Downloading the latest package lists from: "
            : map (("- " ++) . remoteRepoName) remoteRepos
  mapM_ (spawnJob jobCtrl . updateRepo transport verbosity ignoreExpiry) repos
  mapM_ (\_ -> collectJob jobCtrl) repos

updateRepo :: HttpTransport -> Verbosity -> Bool -> Repo -> IO ()
updateRepo transport verbosity ignoreExpiry repo = case repo of
  RepoLocal _localDir -> return ()
  RepoRemote remoteRepo localDir -> do
    downloadResult <- downloadIndex transport verbosity remoteRepo localDir
    case downloadResult of
      FileAlreadyInCache -> return ()
      FileDownloaded indexPath -> do
        writeFileAtomic (dropExtension indexPath) . maybeDecompress
                                                =<< BS.readFile indexPath
        updateRepoIndexCache verbosity repo
  RepoSecure _ _ rep -> do
    let ce = if ignoreExpiry then Sec.DontCheckExpiry else Sec.CheckExpiry
    updated <- Sec.checkForUpdates rep ce
    -- Update cabal's internal index as well so that it's not out of sync
    -- (If all access to the cache goes through hackage-secury this can go)
    case updated of
      Sec.NoUpdates  -> return ()
      Sec.HasUpdates -> updateRepoIndexCache verbosity repo
