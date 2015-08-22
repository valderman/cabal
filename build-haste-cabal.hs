import Control.Applicative
import Control.Monad
import Control.Shell
import System.Directory (copyPermissions)

main = void . shell $ do
    -- Build library and executable
    inDirectory "Cabal" $ run_ "cabal" ["install"] ""
    libs <- inDirectory "cabal-install" $ do
      run_ "cabal" ["configure"] ""
      run_ "cabal" ["build"] ""

      -- Find libraries to bundle
      inDirectory "dist/build/haste-cabal" $ do
        run_ "strip" ["-s", "haste-cabal"] ""
        libs <- map words . lines <$> run "ldd" ["haste-cabal"] ""
        return $ map (!! 2) $ filter shouldBeIncluded libs

    -- Prepare haste-cabal bundle
    direxists <- isDirectory "haste-cabal"
    when direxists $ rmdir "haste-cabal"
    mkdir True "haste-cabal"
    inDirectory "haste-cabal" $ do
      mapM_ (\f -> cp f (takeFileName f)) libs
      cp "../cabal-install/dist/build/haste-cabal/haste-cabal" "haste-cabal.bin"
      () <- file "haste-cabal" launcher
      liftIO $ copyPermissions "haste-cabal.bin" "haste-cabal"
  where
    specials = ["libgmp", "libffi"]
    shouldBeIncluded (l:_) = any (and . zipWith (==) l) specials

launcher :: String
launcher = unlines [
    "#!/bin/bash",
    "DIR=\"$(dirname $0)\"",
    "export LD_LIBRARY_PATH=$DIR",
    "exec $DIR/haste-cabal.bin $@"
  ]
