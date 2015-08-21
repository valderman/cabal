{-# LANGUAGE CPP #-}
module Distribution.Simple.Haste (
        configure, getInstalledPackages, getPackageDBContents,
        buildLib, buildExe,
        replLib, replExe,
        startInterpreter,
        installLib, installExe,
        hcPkgInfo,
        registerPackage,
        componentGhcOptions,
        getLibDir,
        isDynamic,
        getGlobalPackageDB
  ) where

import Distribution.Simple.GHC.ImplInfo ( getImplInfo, hasteVersionImplInfo )
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), Executable(..)
         , Library(..), libModules, exeModules
         , hcOptions, hcProfOptions, hcSharedOptions
         , allExtensions )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
                                ( InstalledPackageInfo(..) )
import Distribution.Package ( LibraryName(..), pkgName, unPackageName )
import Distribution.Simple.PackageIndex ( InstalledPackageIndex )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..) )
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.InstallDirs hiding ( absoluteInstallDirs )
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Simple.Program
         ( Program(..), ConfiguredProgram(..), ProgramConfiguration
         , ProgramSearchPath
         , rawSystemProgramConf
         , rawSystemProgramStdout, rawSystemProgramStdoutConf
         , requireProgramVersion, requireProgram
         , userMaybeSpecifyPath, programPath
         , lookupProgram, addKnownPrograms
         , hasteProgram, hastePkgProgram, c2hsProgram, hsc2hsProgram
         , haddockProgram, stripProgram )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
         ( toFlag, fromFlag, configCoverage, configDistPref )
import qualified Distribution.Simple.Setup as Cabal
        ( Flag(..) )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), CompilerId(..), Compiler(..)
         , PackageDB(..), PackageDBStack, AbiTag(..) )
import Distribution.Version
         ( Version(..), anyVersion, orLaterVersion )
import Distribution.System
         ( Platform(..) )
import Distribution.Verbosity
import Distribution.Utils.NubList
         ( overNubListR, toNubListR )
import Distribution.Text ( display )
import Language.Haskell.Extension ( Extension(..)
                                  , KnownExtension(..))

import Control.Monad            ( unless, when )
import Data.Char                ( isSpace )
import qualified Data.Map as M  ( fromList  )
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid              ( Monoid(..) )
#endif
import System.Directory         ( doesFileExist, renameFile )
import System.FilePath          ( (</>), (<.>), takeExtension,
                                  takeDirectory, replaceExtension,
                                  splitExtension )

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramConfiguration
          -> IO (Compiler, Maybe Platform, ProgramConfiguration)
configure verbosity hcPath hcPkgPath conf0 = do
  (hasteProg, hasteVersion, conf1) <-
    requireProgramVersion verbosity hasteProgram
      (orLaterVersion (Version [0,1] []))
      (userMaybeSpecifyPath "hastec" hcPath conf0)
  Just hasteGhcVersion <- findHasteGhcVersion verbosity (programPath hasteProg)
  let implInfo = hasteVersionImplInfo hasteVersion

  -- This is slightly tricky, we have to configure Haste first, then we use the
  -- location of Haste to help find haste-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (hastePkgProg, hastePkgVersion, conf2) <-
    requireProgramVersion verbosity hastePkgProgram {
      programFindLocation = guessHastePkgFromHastePath hasteProg
    }
    anyVersion (userMaybeSpecifyPath "haste-pkg" hcPkgPath conf1)

  Just hastePkgHasteVersion <- findHastePkgHasteVersion
                                  verbosity (programPath hastePkgProg)

  when (hasteVersion /= hastePkgHasteVersion) $ die $
       "Version mismatch between hastec and haste-pkg: "
    ++ programPath hasteProg ++ " is version " ++ display hasteVersion ++ " "
    ++ programPath hastePkgProg ++ " is version " ++ display hastePkgHasteVersion

  when (hasteGhcVersion /= hastePkgVersion) $ die $
       "Version mismatch between hastec and haste-pkg: "
    ++ programPath hasteProg
    ++ " was built with GHC version " ++ display hasteGhcVersion ++ " "
    ++ programPath hastePkgProg
    ++ " was built with GHC version " ++ display hastePkgVersion

  -- be sure to use our versions of hsc2hs, c2hs, haddock and ghc
  let hsc2hsProgram' =
        hsc2hsProgram { programFindLocation =
                          guessHsc2hsFromHastePath hasteProg }
      c2hsProgram' =
        c2hsProgram { programFindLocation =
                          guessC2hsFromHastePath hasteProg }

      haddockProgram' =
        haddockProgram { programFindLocation =
                          guessHaddockFromHastePath hasteProg }
      conf3 = addKnownPrograms [ hsc2hsProgram', c2hsProgram', haddockProgram' ] conf2

  languages  <- Internal.getLanguages  verbosity implInfo hasteProg
  extensions <- Internal.getExtensions verbosity implInfo hasteProg

  ghcInfo <- Internal.getGhcInfo verbosity implInfo hasteProg
  let ghcInfoMap = M.fromList ghcInfo

  let comp = Compiler {
        compilerId         = CompilerId Haste hasteVersion,
        compilerAbiTag     = AbiTag $
          "ghc-" ++ intercalate "." (map show . versionBranch $ hasteGhcVersion),
        compilerCompat     = [CompilerId GHC hasteGhcVersion],
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = ghcInfoMap
      }
      compPlatform = Internal.targetPlatform ghcInfo
  return (comp, compPlatform, conf3)

guessHastePkgFromHastePath :: ConfiguredProgram -> Verbosity
                           -> ProgramSearchPath -> IO (Maybe FilePath)
guessHastePkgFromHastePath = guessToolFromHastePath hastePkgProgram

guessHsc2hsFromHastePath :: ConfiguredProgram -> Verbosity
                         -> ProgramSearchPath -> IO (Maybe FilePath)
guessHsc2hsFromHastePath = guessToolFromHastePath hsc2hsProgram

guessC2hsFromHastePath :: ConfiguredProgram -> Verbosity
                       -> ProgramSearchPath -> IO (Maybe FilePath)
guessC2hsFromHastePath = guessToolFromHastePath c2hsProgram

guessHaddockFromHastePath :: ConfiguredProgram -> Verbosity
                          -> ProgramSearchPath -> IO (Maybe FilePath)
guessHaddockFromHastePath = guessToolFromHastePath haddockProgram

guessToolFromHastePath :: Program -> ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe FilePath)
guessToolFromHastePath tool hasteProg verbosity searchpath
  = do let toolname          = programName tool
           path              = programPath hasteProg
           dir               = takeDirectory path
           versionSuffix     = takeVersionSuffix (dropExeExtension path)
           guessNormal       = dir </> toolname <.> exeExtension
           guessHasteVersioned = dir </> (toolname ++ "-haste" ++ versionSuffix)
                                 <.> exeExtension
           guessHaste        = dir </> (toolname ++ "-haste")
                               <.> exeExtension
           guessVersioned    = dir </> (toolname ++ versionSuffix) <.> exeExtension
           guesses | null versionSuffix = [guessHaste, guessNormal]
                   | otherwise          = [guessHasteVersioned,
                                           guessHaste,
                                           guessVersioned,
                                           guessNormal]
       info verbosity $ "looking for tool " ++ toolname
         ++ " near compiler in " ++ dir
       exists <- mapM doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
                   -- If we can't find it near ghc, fall back to the usual
                   -- method.
         []     -> programFindLocation tool verbosity searchpath
         (fp:_) -> do info verbosity $ "found " ++ toolname ++ " in " ++ fp
                      return (Just fp)

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = reverse . takeWhile (`elem ` "0123456789.-") .
                            reverse

        dropExeExtension :: FilePath -> FilePath
        dropExeExtension filepath =
          case splitExtension filepath of
            (filepath', extension) | extension == exeExtension -> filepath'
                                   | otherwise                 -> filepath


-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getPackageDBContents verbosity packagedb conf = do
  pkgss <- getInstalledPackages' verbosity [packagedb] conf
  toPackageIndex verbosity pkgss conf

-- | Given a package DB stack, return all installed packages.
getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity packagedbs conf = do
  checkPackageDbEnvVar
  checkPackageDbStack packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs conf
  index <- toPackageIndex verbosity pkgss conf
  return $! index

toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramConfiguration
               -> IO InstalledPackageIndex
toPackageIndex verbosity pkgss conf = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- getLibDir' verbosity hasteProg
  let indices = [ PackageIndex.fromList (map (Internal.substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! (mconcat indices)

  where
    Just hasteProg = lookupProgram hasteProgram conf

checkPackageDbEnvVar :: IO ()
checkPackageDbEnvVar =
    Internal.checkPackageDbEnvVar "HASTE" "HASTE_PACKAGE_PATH"

checkPackageDbStack :: PackageDBStack -> IO ()
checkPackageDbStack (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStack rest
  | GlobalPackageDB `notElem` rest =
  die $ "With current ghc versions the global package db is always used "
     ++ "and must be listed first. This ghc limitation may be lifted in "
     ++ "future, see http://hackage.haskell.org/trac/ghc/ticket/5977"
checkPackageDbStack _ =
  die $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"

getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramConfiguration
                      -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs conf =
  sequence
    [ do pkgs <- HcPkg.dump (hcPkgInfo conf) verbosity packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdoutConf verbosity hasteProgram
     (withPrograms lbi) ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity hasteProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity hasteProg ["--print-libdir"]

-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity hasteProg =
    (reverse . dropWhile isSpace . reverse) `fmap`
     rawSystemProgramStdout verbosity hasteProg ["--print-global-package-db"]

toJSLibName :: String -> String
toJSLibName lib
  | takeExtension lib `elem` [".dll",".dylib",".so"]
                              = replaceExtension lib "jslib"
  | takeExtension lib == ".a" = replaceExtension lib "jslib"
  | otherwise                 = lib <.> "jslib"
  where

buildLib, replLib :: Verbosity -> Cabal.Flag (Maybe Int) -> PackageDescription
                  -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
                  -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs pkg_descr lbi lib clbi = do
  let libName@(LibraryName cname) = componentLibraryName clbi
      libNameWithoutVersion =
        LibraryName . unPackageName . pkgName $ package pkg_descr
      libTargetDir = buildDir lbi
      whenVanillaLib forceVanilla =
        when (not forRepl && (forceVanilla || withVanillaLib lbi))
      whenProfLib = when (not forRepl && withProfLib lbi)
      whenSharedLib forceShared =
        when (not forRepl &&  (forceShared || withSharedLib lbi))
      ifReplLib = when forRepl
      comp = compiler lbi
      implInfo = getImplInfo comp
      hole_insts = map (\(k,(p,n)) -> (k,(InstalledPackageInfo.packageKey p,n)))
                       (instantiatedWith lbi)

  (hasteProg, _) <- requireProgram verbosity hasteProgram (withPrograms lbi)
  let runHasteProg        = runGHC verbosity hasteProg comp
      libBi               = libBuildInfo lib
      isHasteDynamic      = isDynamic comp
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions libBi
      forceVanillaLib = doingTH && not isHasteDynamic
      forceSharedLib  = doingTH &&     isHasteDynamic
      -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = fromFlag $ configCoverage $ configFlags lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way cname
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cObjs       = map (`replaceExtension` objExtension) (cSources libBi)
      jsSrcs      = jsSources libBi
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      jsLibOpts   = mempty {
                      ghcOptExtra = toNubListR $
                                      ["--with-js=" ++ intercalate "," jsSrcs,
                                       "--link-jslib"]
                    }
      vanillaOptsNoJsLib = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptSigOf        = hole_insts,
                      ghcOptInputModules = toNubListR $ libModules lib,
                      ghcOptHPCDir       = hpcdir Hpc.Vanilla
                    }
      vanillaOpts = vanillaOptsNoJsLib `mappend` jsLibOpts

      profOpts    = adjustExts "p_hi" "p_o" vanillaOpts `mappend` mempty {
                        ghcOptProfilingMode = toFlag True,
                        ghcOptExtra         = toNubListR $
                                              hasteProfOptions libBi,
                        ghcOptHPCDir        = hpcdir Hpc.Prof
                      }
      sharedOpts  = adjustExts "dyn_hi" "dyn_o" vanillaOpts `mappend` mempty {
                        ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                        ghcOptFPic        = toFlag True,
                        ghcOptExtra       = toNubListR $
                                            hasteSharedOptions libBi,
                        ghcOptHPCDir      = hpcdir Hpc.Dyn
                      }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = toNubListR $ PD.ldOptions libBi,
                      ghcOptLinkLibs       = toNubListR $ extraLibs libBi,
                      ghcOptLinkLibPath    = toNubListR $ extraLibDirs libBi,
                      ghcOptLinkFrameworks = toNubListR $ PD.frameworks libBi,
                      ghcOptInputFiles     =
                        toNubListR $ [libTargetDir </> x | x <- cObjs] ++ jsSrcs
                   }
      replOpts    = vanillaOptsNoJsLib {
                      ghcOptExtra        = overNubListR
                                           Internal.filterGhciFlags
                                           (ghcOptExtra vanillaOpts),
                      ghcOptNumJobs      = mempty
                    }
                    `mappend` linkerOpts
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                    }

  unless (forRepl || (null (libModules lib) && null jsSrcs && null cObjs)) $
    do let vanilla = whenVanillaLib forceVanillaLib (runHasteProg vanillaOpts)
           shared  = whenSharedLib  forceSharedLib  (runHasteProg sharedOpts)
       if isHasteDynamic
         then do shared;  vanilla
         else do vanilla; shared
       whenProfLib (runHasteProg profOpts)

  -- build any C sources
  unless (null (cSources libBi)) $ do
     info verbosity "Building C Sources..."
     sequence_
       [ do let vanillaCcOpts =
                  (Internal.componentCcGhcOptions verbosity implInfo
                     lbi libBi clbi libTargetDir filename)
                profCcOpts    = vanillaCcOpts `mappend` mempty {
                                  ghcOptProfilingMode = toFlag True,
                                  ghcOptObjSuffix     = toFlag "p_o"
                                }
                sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                  ghcOptFPic        = toFlag True,
                                  ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                  ghcOptObjSuffix   = toFlag "dyn_o"
                                }
                odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
            createDirectoryIfMissingVerbose verbosity True odir
            runHasteProg vanillaCcOpts
            whenSharedLib forceSharedLib (runHasteProg sharedCcOpts)
            whenProfLib (runHasteProg profCcOpts)
       | filename <- cSources libBi]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  unless (null (libModules lib)) $
     ifReplLib (runHasteProg replOpts)

  -- link:
  when (not forRepl) $ do
    info verbosity "Linking..."
    hObjs     <- Internal.getHaskellObjects implInfo lib lbi
                      libTargetDir objExtension True
    unless (null hObjs && null cObjs) $ do
      let vanillaLibFilePath = libTargetDir </> mkLibName libName
          unversionedLibFilePath =
            toJSLibName $ libTargetDir </> mkLibName libNameWithoutVersion
          versionedLibFilePath = toJSLibName vanillaLibFilePath

      versionedLibExists <- doesFileExist versionedLibFilePath
      unversionedLibExists <- doesFileExist unversionedLibFilePath
      when (unversionedLibExists && not versionedLibExists) $ do
        renameFile unversionedLibFilePath versionedLibFilePath

-- | Start a REPL without loading any source files.
startInterpreter :: Verbosity -> ProgramConfiguration -> Compiler
                 -> PackageDBStack -> IO ()
startInterpreter verbosity conf comp packageDBs = do
  let replOpts = mempty {
        ghcOptMode       = toFlag GhcModeInteractive,
        ghcOptPackageDBs = packageDBs
        }
  checkPackageDbStack packageDBs
  (hasteProg, _) <- requireProgram verbosity hasteProgram conf
  runGHC verbosity hasteProg comp replOpts

buildExe, replExe :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe = buildOrReplExe False
replExe  = buildOrReplExe True

buildOrReplExe :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildOrReplExe forRepl verbosity numJobs _pkg_descr lbi
  exe@Executable { exeName = exeName', modulePath = modPath } clbi = do

  (hasteProg, _) <- requireProgram verbosity hasteProgram (withPrograms lbi)
  let comp         = compiler lbi
      implInfo     = getImplInfo comp
      runHasteProg = runGHC verbosity hasteProg comp
      exeBi        = buildInfo exe

  -- exeNameReal, the name that GHC really uses (with .exe on Windows)
  let exeNameReal = exeName' <.>
                    (if takeExtension exeName' /= ('.':exeExtension)
                       then exeExtension
                       else "")

  let targetDir = (buildDir lbi) </> exeName'
  let exeDir    = targetDir </> (exeName' ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True exeDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = fromFlag $ configCoverage $ configFlags lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way exeName'
        | otherwise = mempty

  -- build executables

  srcMainFile         <- findFile (exeDir : hsSourceDirs exeBi) modPath
  let isHasteDynamic      = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      buildRunner = case clbi of
                       ExeComponentLocalBuildInfo {} -> False
                       _                             -> True
      isHaskellMain = elem (takeExtension srcMainFile) [".hs", ".lhs"]
      jsSrcs        = jsSources exeBi
      cSrcs         = cSources exeBi ++ [srcMainFile | not isHaskellMain]
      cObjs         = map (`replaceExtension` objExtension) cSrcs
      baseOpts   = (componentGhcOptions verbosity lbi exeBi clbi exeDir)
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptInputFiles   = toNubListR $
                        [ srcMainFile | isHaskellMain],
                      ghcOptInputModules = toNubListR $
                        [ m | not isHaskellMain, m <- exeModules exe],
                      ghcOptExtra =
                        if buildRunner then toNubListR ["-build-runner"]
                                       else mempty
                    }
      staticOpts = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticOnly,
                      ghcOptHPCDir         = hpcdir Hpc.Vanilla
                   }
      profOpts   = adjustExts "p_hi" "p_o" baseOpts `mappend` mempty {
                      ghcOptProfilingMode  = toFlag True,
                      ghcOptExtra          = toNubListR $ hasteProfOptions exeBi,
                      ghcOptHPCDir         = hpcdir Hpc.Prof
                    }
      dynOpts    = adjustExts "dyn_hi" "dyn_o" baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcDynamicOnly,
                      ghcOptExtra          = toNubListR $
                                             hasteSharedOptions exeBi,
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      dynTooOpts = adjustExts "dyn_hi" "dyn_o" staticOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticAndDynamic,
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions    = toNubListR $ PD.ldOptions exeBi,
                      ghcOptLinkLibs       = toNubListR $ extraLibs exeBi,
                      ghcOptLinkLibPath    = toNubListR $ extraLibDirs exeBi,
                      ghcOptLinkFrameworks = toNubListR $ PD.frameworks exeBi,
                      ghcOptInputFiles     = toNubListR $
                                             [exeDir </> x | x <- cObjs] ++ jsSrcs
                   }
      replOpts   = baseOpts {
                      ghcOptExtra          = overNubListR
                                             Internal.filterGhciFlags
                                             (ghcOptExtra baseOpts)
                   }
                   -- For a normal compile we do separate invocations of ghc for
                   -- compiling as for linking. But for repl we have to do just
                   -- the one invocation, so that one has to include all the
                   -- linker stuff too, like -l flags and any .o files from C
                   -- files etc.
                   `mappend` linkerOpts
                   `mappend` mempty {
                      ghcOptMode           = toFlag GhcModeInteractive,
                      ghcOptOptimisation   = toFlag GhcNoOptimisation
                   }
      commonOpts  | withProfExe lbi = profOpts
                  | withDynExe  lbi = dynOpts
                  | otherwise       = staticOpts
      compileOpts | useDynToo = dynTooOpts
                  | otherwise = commonOpts
      withStaticExe = (not $ withProfExe lbi) && (not $ withDynExe lbi)

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = EnableExtension TemplateHaskell `elem` allExtensions exeBi
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo = dynamicTooSupported && isHasteDynamic
                  && doingTH && withStaticExe && null (hasteSharedOptions exeBi)
      compileTHOpts | isHasteDynamic = dynOpts
                    | otherwise      = staticOpts
      compileForTH
        | forRepl      = False
        | useDynToo    = False
        | isHasteDynamic = doingTH && (withProfExe lbi || withStaticExe)
        | otherwise      = doingTH && (withProfExe lbi || withDynExe lbi)

      linkOpts = commonOpts `mappend`
                 linkerOpts `mappend` mempty {
                      ghcOptLinkNoHsMain   = toFlag (not isHaskellMain)
                 }

  -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runHasteProg compileTHOpts { ghcOptNoLink  = toFlag True
                               , ghcOptNumJobs = numJobs }

  unless forRepl $
    runHasteProg compileOpts { ghcOptNoLink  = toFlag True
                             , ghcOptNumJobs = numJobs }

  -- build any C sources
  unless (null cSrcs) $ do
   info verbosity "Building C Sources..."
   sequence_
     [ do let opts = (Internal.componentCcGhcOptions verbosity implInfo lbi exeBi
                         clbi exeDir filename) `mappend` mempty {
                       ghcOptDynLinkMode   = toFlag (if withDynExe lbi
                                                       then GhcDynamicOnly
                                                       else GhcStaticOnly),
                       ghcOptProfilingMode = toFlag (withProfExe lbi)
                     }
              odir = fromFlag (ghcOptObjDir opts)
          createDirectoryIfMissingVerbose verbosity True odir
          runHasteProg opts
     | filename <- cSrcs ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  when forRepl $ runHasteProg replOpts

  -- link:
  unless forRepl $ do
    info verbosity "Linking..."
    runHasteProg linkOpts { ghcOptOutputFile = toFlag (targetDir </> exeNameReal) }

-- TODO: fixa -fhaste-cabal och --libdir=blah by default!

-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic libraries
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir _pkg lib clbi = do
  whenVanilla $ copyModuleFiles "hi"
  whenProf    $ copyModuleFiles "p_hi"
  whenShared  $ copyModuleFiles "dyn_hi"

  whenVanilla $ installOrdinary builtDir targetDir $ toJSLibName vanillaLibName
  whenProf    $ installOrdinary builtDir targetDir $ toJSLibName profileLibName
  whenShared  $ installShared   builtDir dynlibTargetDir $ toJSLibName sharedLibName
  where
    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True dstDir
      if isShared
        then do when (stripLibs lbi) $ Strip.stripLib verbosity
                                       (hostPlatform lbi) (withPrograms lbi) src
                installExecutableFile verbosity src dst
        else installOrdinaryFile   verbosity src dst

    installOrdinary = install False
    installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (libModules lib)
      >>= installOrdinaryFiles verbosity targetDir

    cid = compilerId (compiler lbi)
    libName = componentLibraryName clbi
    vanillaLibName = mkLibName              libName
    profileLibName = mkProfLibName          libName
    sharedLibName  = (mkSharedLibName cid)  libName

    hasLib    = not $ null (libModules lib)
                   && null (cSources (libBuildInfo lib))
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf    = when (hasLib && withProfLib    lbi)
    whenShared  = when (hasLib && withSharedLib  lbi)

installExe :: Verbosity
              -> LocalBuildInfo
              -> InstallDirs FilePath -- ^Where to copy the files to
              -> FilePath  -- ^Build location
              -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
              -> PackageDescription
              -> Executable
              -> IO ()
installExe verbosity lbi installDirs buildPref
           (progprefix, progsuffix) _pkg exe = do
  let binDir = bindir installDirs
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeFileName = exeName exe
      fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix
      installBinary dest = do
        rawSystemProgramConf verbosity hasteProgram (withPrograms lbi) $
          [ "--install-executable"
          , buildPref </> exeName exe </> exeFileName
          , "-o", dest
          ] ++
          case (stripExes lbi, lookupProgram stripProgram $ withPrograms lbi) of
           (True, Just strip) -> ["-strip-program", programPath strip]
           _                  -> []
  installBinary (binDir </> fixedExeBaseName)

adjustExts :: String -> String -> GhcOptions -> GhcOptions
adjustExts hiSuf objSuf opts =
  opts `mappend` mempty {
    ghcOptHiSuffix  = toFlag hiSuf,
    ghcOptObjSuffix = toFlag objSuf
  }

registerPackage :: Verbosity
                -> InstalledPackageInfo
                -> PackageDescription
                -> LocalBuildInfo
                -> Bool
                -> PackageDBStack
                -> IO ()
registerPackage verbosity installedPkgInfo _pkg lbi _inplace packageDbs =
  HcPkg.reregister (hcPkgInfo $ withPrograms lbi) verbosity packageDbs
    (Right installedPkgInfo)

componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let opts = Internal.componentGhcOptions verbosity lbi bi clbi odir
  in  opts { ghcOptExtra = ghcOptExtra opts `mappend` toNubListR
                             (hcOptions Haste bi)
           }

hasteProfOptions :: BuildInfo -> [String]
hasteProfOptions bi =
  hcProfOptions GHC bi `mappend` hcProfOptions Haste bi

hasteSharedOptions :: BuildInfo -> [String]
hasteSharedOptions bi =
  hcSharedOptions GHC bi `mappend` hcSharedOptions Haste bi

isDynamic :: Compiler -> Bool
isDynamic = Internal.ghcLookupProperty "GHC Dynamic"

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

findHasteGhcVersion :: Verbosity -> FilePath -> IO (Maybe Version)
findHasteGhcVersion verbosity pgm =
  findProgramVersion "--numeric-ghc-version" id verbosity pgm

findHastePkgHasteVersion :: Verbosity -> FilePath -> IO (Maybe Version)
findHastePkgHasteVersion verbosity pgm =
  findProgramVersion "--numeric-version" id verbosity pgm

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramConfiguration -> HcPkg.HcPkgInfo
hcPkgInfo conf = HcPkg.HcPkgInfo { HcPkg.hcPkgProgram    = hastePkgProg
                                 , HcPkg.noPkgDbStack    = False
                                 , HcPkg.noVerboseFlag   = False
                                 , HcPkg.flagPackageConf = False
                                 , HcPkg.useSingleFileDb = v < [7,9]
                                 }
  where
    v                 = versionBranch ver
    Just hastePkgProg = lookupProgram hastePkgProgram conf
    Just ver          = programVersion hastePkgProg
