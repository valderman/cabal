-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Builtin
-- Copyright   :  Isaac Jones 2006, Duncan Coutts 2007-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- The module defines all the known built-in 'Program's.
--
-- Where possible we try to find their version numbers.
--
module Distribution.Simple.Program.Builtin (

    -- * The collection of unconfigured and configured programs
    builtinPrograms,

    -- * Programs that Cabal knows about
    ghcProgram,
    ghcPkgProgram,
    ghcjsProgram,
    ghcjsPkgProgram,
    hasteProgram,
    hastePkgProgram,
    lhcProgram,
    lhcPkgProgram,
    hmakeProgram,
    jhcProgram,
    haskellSuiteProgram,
    haskellSuitePkgProgram,
    uhcProgram,
    gccProgram,
    arProgram,
    stripProgram,
    happyProgram,
    alexProgram,
    hsc2hsProgram,
    c2hsProgram,
    cpphsProgram,
    hscolourProgram,
    haddockProgram,
    greencardProgram,
    ldProgram,
    tarProgram,
    cppProgram,
    pkgConfigProgram,
    hpcProgram,
  ) where

import Distribution.Simple.Program.Find
         ( ProgramSearchPath, ProgramSearchPathEntry (..)
         , findProgramOnSearchPath )
import Distribution.Simple.Program.Internal
         ( stripExtractVersion )
import Distribution.Simple.Program.Run
         ( getProgramInvocationOutput, programInvocation )
import Distribution.Simple.Program.Types
         ( Program(..), ConfiguredProgram(..), simpleProgram )
import Distribution.Simple.Utils
         ( findProgramVersion )
import Distribution.Compat.Exception
         ( catchIO )
import Distribution.Verbosity
         ( Verbosity, lessVerbose )
import Distribution.Version
         ( Version(..), withinRange, earlierVersion, laterVersion
         , intersectVersionRanges )
import Data.Char
         ( isDigit )

import Data.List
         ( isInfixOf )
import qualified Data.Map as Map

import System.Environment
         ( getExecutablePath )
import System.FilePath
         ( takeDirectory )

-- ------------------------------------------------------------
-- * Known programs
-- ------------------------------------------------------------

-- | The default list of programs.
-- These programs are typically used internally to Cabal.
builtinPrograms :: [Program]
builtinPrograms =
    [
    -- compilers and related progs
      ghcProgram
    , ghcPkgProgram
    , ghcjsProgram
    , ghcjsPkgProgram
    , hasteProgram
    , hastePkgProgram
    , haskellSuiteProgram
    , haskellSuitePkgProgram
    , hmakeProgram
    , jhcProgram
    , lhcProgram
    , lhcPkgProgram
    , uhcProgram
    , hpcProgram
    -- preprocessors
    , hscolourProgram
    , haddockProgram
    , happyProgram
    , alexProgram
    , hsc2hsProgram
    , c2hsProgram
    , cpphsProgram
    , greencardProgram
    -- platform toolchain
    , gccProgram
    , arProgram
    , stripProgram
    , ldProgram
    , tarProgram
    -- configuration tools
    , pkgConfigProgram
    ]

ghcProgram :: Program
ghcProgram = (simpleProgram "ghc") {
    programFindVersion = findProgramVersion "--numeric-version" id,

    -- Workaround for https://ghc.haskell.org/trac/ghc/ticket/8825
    -- (spurious warning on non-english locales)
    programPostConf    = \_verbosity ghcProg ->
    do let ghcProg' = ghcProg {
             programOverrideEnv = ("LANGUAGE", Just "en")
                                  : programOverrideEnv ghcProg
             }
           -- Only the 7.8 branch seems to be affected. Fixed in 7.8.4.
           affectedVersionRange = intersectVersionRanges
                                  (laterVersion   $ Version [7,8,0] [])
                                  (earlierVersion $ Version [7,8,4] [])
       return $ maybe ghcProg
         (\v -> if withinRange v affectedVersionRange
                then ghcProg' else ghcProg)
         (programVersion ghcProg)
  }

ghcPkgProgram :: Program
ghcPkgProgram = (simpleProgram "ghc-pkg") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "ghc-pkg --version" gives a string like
      -- "GHC package manager version 6.4.1"
      case words str of
        (_:_:_:_:ver:_) -> ver
        _               -> ""
  }

ghcjsProgram :: Program
ghcjsProgram = (simpleProgram "ghcjs") {
    programFindVersion = findProgramVersion "--numeric-ghcjs-version" id
  }

-- note: version is the version number of the GHC version that ghcjs-pkg was built with
ghcjsPkgProgram :: Program
ghcjsPkgProgram = (simpleProgram "ghcjs-pkg") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "ghcjs-pkg --version" gives a string like
      -- "GHCJS package manager version 6.4.1"
      case words str of
        (_:_:_:_:ver:_) -> ver
        _               -> ""
  }

findHasteProgram :: Verbosity -> ProgramSearchPath -> FilePath
                 -> IO (Maybe FilePath)
findHasteProgram v p name = do
  me <- getExecutablePath
  let mypath = ProgramSearchPathDir (takeDirectory me)
  findProgramOnSearchPath v (mypath : p) name

hasteProgram :: Program
hasteProgram = (simpleProgram "hastec") {
    programFindVersion  = findProgramVersion "--version" id,
    programFindLocation = \v p -> findHasteProgram v p "hastec"
  }

-- note: version is the version number of the GHC version that haste-pkg was built with
hastePkgProgram :: Program
hastePkgProgram = (simpleProgram "haste-pkg") {
    programFindVersion  = findProgramVersion "--numeric-ghc-version" id,
    programFindLocation = \v p -> findHasteProgram v p "haste-pkg"
  }

lhcProgram :: Program
lhcProgram = (simpleProgram "lhc") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

lhcPkgProgram :: Program
lhcPkgProgram = (simpleProgram "lhc-pkg") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "lhc-pkg --version" gives a string like
      -- "LHC package manager version 0.7"
      case words str of
        (_:_:_:_:ver:_) -> ver
        _               -> ""
  }

hmakeProgram :: Program
hmakeProgram = (simpleProgram "hmake") {
    programFindVersion = findProgramVersion "--version" $ \str ->
    -- Invoking "hmake --version" gives a string line
    -- "/usr/local/bin/hmake: 3.13 (2006-11-01)"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

jhcProgram :: Program
jhcProgram = (simpleProgram "jhc") {
    programFindVersion = findProgramVersion "--version" $ \str ->
    -- invoking "jhc --version" gives a string like
    -- "jhc 0.3.20080208 (wubgipkamcep-2)
    -- compiled by ghc-6.8 on a x86_64 running linux"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

uhcProgram :: Program
uhcProgram = (simpleProgram "uhc") {
    programFindVersion = findProgramVersion "--version-dotted" id
  }

hpcProgram :: Program
hpcProgram = (simpleProgram "hpc")
    {
        programFindVersion = findProgramVersion "version" $ \str ->
            case words str of
                (_ : _ : _ : ver : _) -> ver
                _ -> ""
    }

-- This represents a haskell-suite compiler. Of course, the compiler
-- itself probably is not called "haskell-suite", so this is not a real
-- program. (But we don't know statically the name of the actual compiler,
-- so this is the best we can do.)
--
-- Having this Program value serves two purposes:
--
-- 1. We can accept options for the compiler in the form of
--
--   --haskell-suite-option(s)=...
--
-- 2. We can find a program later using this static id (with
-- requireProgram).
--
-- The path to the real compiler is found and recorded in the ProgramDb
-- during the configure phase.
haskellSuiteProgram :: Program
haskellSuiteProgram = (simpleProgram "haskell-suite") {
    -- pretend that the program exists, otherwise it won't be in the
    -- "configured" state
    programFindLocation =
      \_verbosity _searchPath -> return $ Just "haskell-suite-dummy-location"
  }

-- This represent a haskell-suite package manager. See the comments for
-- haskellSuiteProgram.
haskellSuitePkgProgram :: Program
haskellSuitePkgProgram = (simpleProgram "haskell-suite-pkg") {
    programFindLocation =
      \_verbosity _searchPath -> return $ Just "haskell-suite-pkg-dummy-location"
  }


happyProgram :: Program
happyProgram = (simpleProgram "happy") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "happy --version" gives a string like
      -- "Happy Version 1.16 Copyright (c) ...."
      case words str of
        (_:_:ver:_) -> ver
        _           -> ""
  }

alexProgram :: Program
alexProgram = (simpleProgram "alex") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "alex --version" gives a string like
      -- "Alex version 2.1.0, (c) 2003 Chris Dornan and Simon Marlow"
      case words str of
        (_:_:ver:_) -> takeWhile (\x -> isDigit x || x == '.') ver
        _           -> ""
  }

gccProgram :: Program
gccProgram = (simpleProgram "gcc") {
    programFindVersion = findProgramVersion "-dumpversion" id
  }

arProgram :: Program
arProgram = simpleProgram "ar"

stripProgram :: Program
stripProgram = (simpleProgram "strip") {
    programFindVersion = \verbosity ->
      findProgramVersion "--version" stripExtractVersion (lessVerbose verbosity)
  }

hsc2hsProgram :: Program
hsc2hsProgram = (simpleProgram "hsc2hs") {
    programFindVersion =
      findProgramVersion "--version" $ \str ->
        -- Invoking "hsc2hs --version" gives a string like "hsc2hs version 0.66"
        case words str of
          (_:_:ver:_) -> ver
          _           -> ""
  }

c2hsProgram :: Program
c2hsProgram = (simpleProgram "c2hs") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

cpphsProgram :: Program
cpphsProgram = (simpleProgram "cpphs") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "cpphs --version" gives a string like "cpphs 1.3"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

hscolourProgram :: Program
hscolourProgram = (simpleProgram "hscolour") {
    programFindLocation = \v p -> findProgramOnSearchPath v p "HsColour",
    programFindVersion  = findProgramVersion "-version" $ \str ->
      -- Invoking "HsColour -version" gives a string like "HsColour 1.7"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

haddockProgram :: Program
haddockProgram = (simpleProgram "haddock") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "haddock --version" gives a string like
      -- "Haddock version 0.8, (c) Simon Marlow 2006"
      case words str of
        (_:_:ver:_) -> takeWhile (`elem` ('.':['0'..'9'])) ver
        _           -> ""
  }

greencardProgram :: Program
greencardProgram = simpleProgram "greencard"

ldProgram :: Program
ldProgram = simpleProgram "ld"

tarProgram :: Program
tarProgram = (simpleProgram "tar") {
  -- See #1901. Some versions of 'tar' (OpenBSD, NetBSD, ...) don't support the
  -- '--format' option.
  programPostConf = \verbosity tarProg -> do
     tarHelpOutput <- getProgramInvocationOutput
                      verbosity (programInvocation tarProg ["--help"])
                      -- Some versions of tar don't support '--help'.
                      `catchIO` (\_ -> return "")
     let k = "Supports --format"
         v = if ("--format" `isInfixOf` tarHelpOutput) then "YES" else "NO"
         m = Map.insert k v (programProperties tarProg)
     return $ tarProg { programProperties = m }
  }

cppProgram :: Program
cppProgram = simpleProgram "cpp"

pkgConfigProgram :: Program
pkgConfigProgram = (simpleProgram "pkg-config") {
    programFindVersion = findProgramVersion "--version" id
  }
