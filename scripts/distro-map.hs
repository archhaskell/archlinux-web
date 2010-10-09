import Distribution.ArchLinux.Report
import Distribution.ArchLinux.AUR
import Distribution.ArchLinux.PkgBuild

import System.FilePath
import Control.Monad

import Data.List
import Distribution.Text
import Distribution.Version

import System.Directory
import Text.Printf
import Control.DeepSeq

import GHC.Conc (numCapabilities)
import Control.Concurrent
import qualified Control.OldException as C
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Parallel.Strategies


{-
Generate the report of mappings of hackage to distro urls for Hackage
-}

k = 32

-- Parallel work queue, similar to forM
--
parM tests f = do
    let n = numCapabilities
    chan <- newChan
    ps   <- getChanContents chan -- results
    work <- newMVar tests        -- where to take jobs from
    forM_ [1..n*k] $ forkIO . thread work chan    -- how many threads to fork

    -- wait on i threads to close
    -- logging them as they go
    let wait xs i acc
            | i >= (n*k)     = return acc -- done
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

writer ch = do
    v <- readChan ch
    case v of
         Nothing -> return ()
         Just s  -> do appendFile "cabalArchMap.txt" s
                       writer ch

me = "arch-haskell"

main = do
    C.handle (\e -> return ()) (removeFile "cabalArchMap.txt")

    -- todo: replace this with call to AUR json interface.
    -- s <- lines `fmap` readFile "arch-haskell-packages.txt"
    packages <- maintainer me
    let s = sort $ map packageName packages

    -- just write out the ones we already know.
    forM community $ \p@(name, vers, url) -> do
                appendFile "cabalArchMap.txt" $ show p ++ "\n"

    -- one output writer
    out <- newChan
    forkIO $ writer out

    -- start with the Arch Package names, deriving the Cabal names, the
    -- version, and the URL to find them.
    --
    parM s $ \p -> do
        k <- info p
        case k of
             Left err -> do putStrLn $ "Couldn't find package: " ++ show p
                            return ()
             Right aur -> do

                let name = takeFileName (packageURL aur)
                    vers = case packageVersion aur of
                                Left _  -> ""
                                Right (v,_) -> display v
                    url  = packageURLinAUR aur

                -- (Agda,"2.2.4+dfsg","http://packages.debian.org/source/sid/agda")

                let s = show (name, vers, Just url) ++ "\n"
                rnf s `seq` writeChan out . Just $ s

    writeChan out Nothing

    putStrLn "scp cabalArchMap.txt www.galois.com:www/"

-- hand search:
--
-- http://www.archlinux.org/packages/?sort=&arch=x86_64&repo=Extra&q=haskell&last_update=&limit=all
--
community =
    [ ("xmonad", "0.9.1", Just "http://www.archlinux.org/packages/community/i686/xmonad/")

    , ("packedstring", "0.1.0.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-packedstring/")
    , ("deepseq",      "1.1.0.0", Just "http://www.archlinux.org/packages/extra/i686/haskell-deepseq/")
    , ("haskell-src",  "1.0.1.3", Just "http://www.archlinux.org/packages/extra/i686/haskell-haskell-src/")
    , ("HUnit",        "1.2.2.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-hunit/")
    , ("parallel",     "2.2.0.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-parallel/")
    , ("QuickCheck",   "2.1.0.3", Just "http://www.archlinux.org/packages/extra/i686/haskell-quickcheck/")
    , ("stm",          "2.1.1.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-stm/")
    , ("xhtml",     "3000.2.0.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-xhtml/")
    , ("extensible-exceptions", "0.1.1.0", Just "http://www.archlinux.org/packages/extra/i686/haskell-extensible-exceptions/")
    , ("haskeline", "0.6.2.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-haskeline/")
    , ("terminfo", "0.3.0.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-terminfo/")
    , ("mtl", "1.1.0.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-mtl/")
    , ("network", "2.2.1.7", Just "http://www.archlinux.org/packages/extra/i686/haskell-network/")
    , ("dataenc", "0.13.0.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-dataenc/")
    , ("hashed-storage",    "0.4.13",  Just  "http://www.archlinux.org/packages/extra/i686/haskell-hashed-storage/")

    , ("html",     "1.0.1.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-html/")
    , ("mmap", "0.4.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-mmap/")
    , ("parsec", "3.0.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-parsec/")
    , ("regex-base", "0.93.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-regex-base/")
    , ("regex-compat", "0.92", Just "http://www.archlinux.org/packages/extra/i686/haskell-regex-compat/")
    , ("regex-posix", "0.94.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-regex-posix/")
    , ("utf8-string","0.3.6", Just "http://www.archlinux.org/packages/extra/i686/haskell-utf8-string/")
    , ("zlib", "0.5.2.0", Just "http://www.archlinux.org/packages/community/i686/haskell-zlib/")

    , ("happy" ,"1.18.4", Just "http://www.archlinux.org/packages/extra/i686/happy/")
    , ("alex", "2.3.1", Just "http://www.archlinux.org/packages/community/i686/alex/")
    , ("X11-xft", "0.3", Just "http://www.archlinux.org/packages/community/i686/haskell-x11-xft/")
    , ("X11", "1.5.0.0", Just "http://www.archlinux.org/packages/community/i686/haskell-x11/")
    , ("HTTP", "4000.0.9", Just "http://www.archlinux.org/packages/community/i686/haskell-http/")

    , ("gtk2hs", "0.10.1", Just "http://www.archlinux.org/packages/community/i686/gtk2hs/")

    , ("darcs", "2.3.1", Just "http://www.archlinux.org/packages/extra/i686/darcs/")
    , ("cabal-install", "0.8.0", Just "http://www.archlinux.org/packages/community/i686/cabal-install/")
    ]
