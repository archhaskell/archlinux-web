{-# LANGUAGE FlexibleInstances #-}
-- | Construct reports about a set of packages in AUR
--
module Distribution.ArchLinux.Report where

import Distribution.ArchLinux.AUR
import Distribution.ArchLinux.PkgBuild

import Distribution.Text

import System.FilePath
import Data.Maybe

import Text.XHtml.Transitional
import Control.OldException
import Control.Monad
import Data.List
import Data.Ord
import Data.Char

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Strict
import Control.Concurrent.MVar.Strict
import qualified Control.OldException as C
import System.IO
import System.Process
import System.Time
import GHC.Conc (numCapabilities)
import Text.Printf
import Control.Parallel.Strategies

import Distribution.Version
import qualified Data.Map as M

instance NFData (IO a) where rnf x = ()

instance NFData Version where rnf x = x `seq` ()
instance NFData AURInfo where rnf x = x `seq` ()
instance NFData AnnotatedPkgBuild where rnf x = x `seq` ()

-- Parallel work queue
parM tests f = do
    let n = numCapabilities
    chan <- newChan
    ps   <- getChanContents chan -- results
    work <- newMVar tests        -- where to take jobs from
    forM_ [1..n] $ forkIO . thread work chan    -- how many threads to fork

    -- wait on i threads to close
    -- logging them as they go
    let wait xs i acc
            | i >= n     = return acc -- done
            | otherwise = case xs of
                    Nothing     : xs -> wait xs (i+1) acc
                    Just (s,a)  : xs -> do a ; wait xs i     (s : acc)
    wait ps 0 []

  where
    -- thread :: MVar [Test] -> Chan (Maybe String) -> Int -> IO ()
    thread work chan me = loop
      where
        loop = do
            job <- modifyMVar work $ \jobs -> return $ case jobs of
                        []     -> ([], Nothing)
                        (j:js) -> (js, Just j)
            case job of
                Nothing   -> writeChan chan Nothing -- done
                Just name -> do
                    v <- f name
                    writeChan chan . Just $ (v, printf "%d: %-25s\n" me name)
                    loop


-- | Take as input a list of package names, return a pandoc object
-- reporting on interesting facts about the packages.
--
report :: [String] -> IO String
report xs = do
    -- load current index.
    putStr "Loading package index ... " >> hFlush stdout
    index <- loadPackageIndex
    putStrLn "Done."

    -- collect sets of results
    res_ <- parM (nub xs) $ \p -> do
        handle (\s -> return (p, Nothing, (Left (show s), Left []))) $ do
            k <- package p

            -- if there is a hackage path, lookup version.
            --  cabal info xmonad -v0
            vers <- case k of

                 (Right aur, _) | not (null (packageURL aur))  -> do
                     let name = takeFileName (packageURL aur) -- haskell package name
                     return $! M.lookup name index
                 _ -> return Nothing

            vers `seq` k `seq` return $! (p, vers, k)

    time <- getClockTime

    let results = sortBy (\(n,x,_) (m,y,_) -> n `compare` m) res_

    return. showHtml $
        (header $
            (thetitle (toHtml "Arch Haskell Package Report")) +++
            ((thelink noHtml) ! [ rel "stylesheet"
                                , href "http://galois.com/~dons/arch-haskell.css"
                                , thetype "text/css" ])) +++

        (body $
            center ((h2 (toHtml "Arch Haskell Package Status")))
            +++
            (table $
                (tr (td (toHtml $ "Results from " ++ show time)))
                +++
                (tr (td (toHtml $ "Found  " ++ show (length results) ++ " packages" )))
            )
            +++
            (scores . table $
                tr (concatHtml
                      [ td . categoryTag . toHtml $ "Package"
                      , td . categoryTag . toHtml $ "Hackage"
                      , td . categoryTag . toHtml $ "Version"
                      , td . categoryTag . toHtml $ "Latest"
                      , td . categoryTag . toHtml $ "cabal2arch"
                      , td . categoryTag . toHtml $ "Votes"
                      , td . categoryTag . toHtml $ "Description"
                      ]) +++

                concatHtml

                    [

                     tr $ concatHtml $
                      case aur_ of
                       Left  err ->
                          [ td $ toHtml p
                          , td $ bad (toHtml "No AUR entry found!")
                          ]

                       Right aur -> case pkg_ of

                        -- Didn't find a PKGBUILD
                         Left  err ->
                          [ td . toHtml $
                              hotlink
                                (packageURLinAUR aur)
                                (toHtml p)

                          , td .
                              (if null (packageURL aur) then bad else id) . toHtml $
                              hotlink
                                (packageURL aur)
                                (toHtml (takeFileName (packageURL aur)))

                          , td  $ case packageVersion aur of
                                     Left s  -> bad $ toHtml s
                                     Right (v,_) -> toHtml $ display v

                          , td  $

                              case vers of
                                   Nothing | packageLocation aur == 3 -> toHtml ""
                                   Nothing -> bad (toHtml "-")
                                   Just v  -> case packageVersion aur of
                                     Left s  -> toHtml (display v)
                                     Right (v',_) | v == v' -> toHtml (display v)
                                                 | otherwise -> bad (toHtml (display v))

                          , td $ if packageLocation aur /= 3
                                    then bad (toHtml "PKGBUILD not found")
                                    else toHtml ""

                          , td  $ if packageVotes aur > 10
                                     then good $ toHtml $ show $ packageVotes aur
                                     else        toHtml $ show $ packageVotes aur

                          , td $ toHtml $ packageDesc aur
                          ]

                        -- Found everything
                         Right pkg ->
                          [ td . toHtml $
                              hotlink
                                (packageURLinAUR aur)
                                (toHtml p)

                          , td .
                              (if null (packageURL aur) then bad else id) . toHtml $
                              hotlink
                                (packageURL aur)
                                (toHtml (takeFileName (packageURL aur)))

                          , td  $
                                case packageVersion aur of
                                     Left s  -> bad $ toHtml s
                                     Right (v,_) -> toHtml $ display v

                          , td  $
                              case vers of
                                   Nothing | packageLocation aur == 3 -> toHtml ""
                                   Nothing -> bad (toHtml "-")
                                   Just v  -> case packageVersion aur of
                                     Left s  -> toHtml (display v)
                                     Right (v',_) | v == v' -> toHtml (display v)
                                                 | otherwise -> bad (toHtml (display v))

                          , td  $
                              if oldCabal2Arch pkg
                                 then bad . toHtml $
                                            case pkgBuiltWith pkg of
                                                     Nothing -> "Nothing"
                                                     Just v  -> display v

                                 else toHtml $
                                            case pkgBuiltWith pkg of
                                                     Nothing -> "Nothing"
                                                     Just v  -> display v

                          , td  $ if packageVotes aur > 10
                                     then good $ toHtml $ show $ packageVotes aur
                                     else        toHtml $ show $ packageVotes aur

                          , td  $ toHtml $ packageDesc aur

                          ]

                        | (p, vers, (aur_,pkg_)) <- results
                        ]
                )
            )


    {-
    (Right (AURInfo {packageID = 17480, packageName = "haskell-json", packageVersion = Right (Version {versionBranch = [0,4,3], versionTags = []},"1"), packageCategory = 10, packageDesc = "Support for serialising Haskell to and from JSON", packageLocation = 2, packageURL = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/json", packagePath = "/packages/haskell-json/haskell-json.tar.gz", packageLicense = "custom:BSD3", packageVotes = 16, packageOutOfDate = False}),Right (AnnotatedPkgBuild {pkgBuiltWith = Just (Version {versionBranch = [0,5,3], versionTags = []}), pkgHeader = "# Contributor: Arch Haskell Team <arch-haskell@haskell.org>", pkgBody = PkgBuild {arch_pkgname = "haskell-json", arch_pkgver = Version {versionBranch = [0,4,3], versionTags = []}, arch_pkgrel = 1, arch_pkgdesc = "\"Support for serialising Haskell to and from JSON\"", arch_arch = ArchList [Arch_X86,Arch_X86_64], arch_url = "\"http://hackage.haskell.org/cgi-bin/hackage-scripts/package/json\"", arch_license = ArchList [BSD3], arch_makedepends = ArchList [], arch_depends = ArchList [], arch_source = ArchList [], arch_md5sum = ArchList [], arch_build = [], arch_install = Nothing, arch_options = ArchList []}}))
    -}

categoryTag  x = thediv x ! [identifier "Category"    ]
bad     x = thediv x ! [identifier "Bad"  ]
good    x = thediv x ! [identifier "Best" ]
scores  x = thediv x ! [identifier "Scores" ]


------------------------------------------------------------------------
--
-- Strict process reading
--
myReadProcess :: FilePath                              -- ^ command to run
            -> [String]                              -- ^ any arguments
            -> String                                -- ^ standard input
            -> IO (Either (ExitCode,String,String) String)  -- ^ either the stdout, or an exitcode and any output

myReadProcess cmd args input = C.handle (return . handler) $ do
    (inh,outh,errh,pid) <- runInteractiveProcess cmd args Nothing Nothing

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ (C.evaluate (length output) >> putMVar outMVar ())

    errput  <- hGetContents errh
    errMVar <- newEmptyMVar
    forkIO $ (C.evaluate (length errput) >> putMVar errMVar ())

    when (not (null input)) $ hPutStr inh input
    takeMVar outMVar
    takeMVar errMVar
    ex     <- C.catch (waitForProcess pid) (\_e -> return ExitSuccess)
    hClose outh
    hClose inh          -- done with stdin
    hClose errh         -- ignore stderr

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left (ex, errput, output)

  where
    handler (C.ExitException e) = Left (e,"","")
    handler e                   = Left (ExitFailure 1, show e, "")

------------------------------------------------------------------------
--
-- Load a table of packages and their latest versions
--
loadPackageIndex :: IO (M.Map String Version)
loadPackageIndex = do
    v <- myReadProcess "cabal" ["list", "--simple-output"] []
    case v of
         Left err   -> error (show err)
         Right idx  -> do

            let table :: M.Map String Version
                table = M.fromList  -- last value is used. cabal prints in version order.
                     [  (name, vers)
                     |  pkg <- lines idx
                     ,  let (name, _:vers_) = break isSpace pkg
                     ,  let vers = fromJust (simpleParse vers_)
                     ]

            return $! table

            -- Data.Map String Version
            --



