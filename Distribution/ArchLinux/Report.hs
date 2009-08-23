{-# LANGUAGE FlexibleInstances #-}
-- | Construct reports about a set of packages in AUR
--
module Distribution.ArchLinux.Report (
    report , loadPackageIndex
    ) where

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

import Text.CSV
import Network.HTTP

instance NFData (IO a) where rnf x = ()

instance NFData Version where rnf x = x `seq` ()
instance NFData AURInfo where rnf x = x `seq` ()
instance NFData AnnotatedPkgBuild where rnf x = x `seq` ()

-- Parallel work queue, similar to forM
--
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


-- | Take as input a list of package names, return an html page
-- with interesting facts about the state of the packages.
--
report :: [String] -> IO String
report xs = do
    -- load current index.
    putStr "Loading package index ... " >> hFlush stdout
    index     <- loadPackageIndex
    downloads <- loadHackageDownloads
    builds    <- loadBuildStatus
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
            (script ! [src  "http://galois.com/~dons/sorttable.js"]) (toHtml "") +++
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
            (scores . sortable . table $
                tr (concatHtml
                      [ th . categoryTag . toHtml $ "Package"
                      , th . categoryTag . toHtml $ "Hackage"
                      , th . categoryTag . toHtml $ "Version"
                      , th . categoryTag . toHtml $ "Latest"
                      , th . categoryTag . toHtml $ "cabal2arch"
                      , th . categoryTag . toHtml $ "Votes"
                      , th . categoryTag . toHtml $ "Downloads"
                      , th . categoryTag . toHtml $ "Description"
                      ]) +++

                concatHtml

                    [

                     tr $ concatHtml $
                      case aur_ of
                       Left  err ->
                          [ td $ (maybe id (\n -> if n then good else bad) (M.lookup p builds)) ( toHtml p )
                          , td $ bad (toHtml "No AUR entry found!")
                          , td $ toHtml ""
                          , td $ toHtml ""
                          , td $ toHtml ""
                          , td $ toHtml ""
                          , td $ toHtml ""
                          , td $ toHtml ""
                          ]

                       Right aur -> case pkg_ of

                        -- Didn't find a PKGBUILD
                         Left  err ->
                          [ td . toHtml $
                              hotlink
                                (packageURLinAUR aur)
                                ((maybe id (\n -> if n then good else bad)
                                    (M.lookup p builds)) (toHtml p))

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
                                    then bad (toHtml "Not Found")
                                    else toHtml ""

                          , td  $ if packageVotes aur > 10
                                     then good $ toHtml $ show $ packageVotes aur
                                     else        toHtml $ show $ packageVotes aur

                          , td  $ case M.lookup (let n = takeFileName (packageURL aur) in if null n then packageName aur else n) downloads of
                                    Nothing -> toHtml ""
                                    Just n | n >= 1000 -> good (toHtml (show n))
                                           | otherwise -> (toHtml (show n))

                          , td $ toHtml $ packageDesc aur
                          ]

                        -- Found everything
                        -- TODO parallelise this and remove redundancies
                         Right pkg ->
                          [ td . toHtml $
                              hotlink
                                (packageURLinAUR aur)
                                ((maybe id (\n -> if n then good else bad)
                                    (M.lookup p builds)) (toHtml p))

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

                                -- bug in computing real Haskell name.
                          , td  $ case M.lookup (let n = takeFileName (packageURL aur) in if null n then packageName aur else n) downloads of
                                    Nothing -> toHtml ""
                                    Just n | n >= 1000 -> good (toHtml (show n))
                                           | otherwise -> (toHtml (show n))

                          , td  $ toHtml $ packageDesc aur

                          ]

                        | (p, vers, (aur_,pkg_)) <- results
                        ]
                )
            )

categoryTag  x = thediv x ! [identifier "Category"    ]
bad     x = thediv x ! [identifier "Bad"  ]
good    x = thediv x ! [identifier "Best" ]
scores  x = thediv x ! [identifier "Scores" ]

sortable x = x ! [theclass "sortable"]


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
-- | Load a table of packag names associated with their latest versions
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

------------------------------------------------------------------------

url :: String
url = "http://www.galois.com/~dons/hackage/august-2009/hackage-downloads-august-2009.csv"

loadHackageDownloads :: IO (M.Map String Integer)
loadHackageDownloads = do
    rsp <- simpleHTTP (getRequest url)
    case rsp of
         Left err -> error "Unable to get Hackage data"
         Right _  -> do
            idx <- getResponseBody rsp -- TODO 404
            case parseCSV "hackage.csv" idx of
                 Left err  -> error (show err)
                 Right cvs -> do
                    let
                        table = M.fromList  -- last value is used. cabal prints in version order.
                                    [ (head row
                                      ,read (last row))
                                    | row <- init (tail cvs) ]
                    return $! table

filepath :: FilePath
filepath = "/home/dons/.build-all.log"

loadBuildStatus :: IO (M.Map String Bool)
loadBuildStatus = do
    s <- readFile filepath
    let table = M.fromList
                    [ (n, read k)
                    | l <- lines s
                    , let [n, k] = words l
                    ]

    return $! table
