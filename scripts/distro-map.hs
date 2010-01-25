import Distribution.ArchLinux.Report
import Distribution.ArchLinux.AUR
import Distribution.ArchLinux.PkgBuild

import System.FilePath
import Control.Monad

import Distribution.Text
import Distribution.Version

import System.Directory
import Text.Printf

import GHC.Conc (numCapabilities)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Parallel.Strategies

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

main = do
    removeFile "cabalArchMap.txt"
    s <- lines `fmap` readFile "arch-haskell-packages.txt"

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

    putStrLn "scp cabalArchMap.txt galois.com:www/"

-- hand search
community =
    [ ("xmonad", "0.9", Just "http://www.archlinux.org/packages/community/i686/xmonad/")
    , ("X11", "1.4.6.1", Just "http://www.archlinux.org/packages/community/i686/haskell-x11/")
    , ("X11-xft", "0.3", Just "http://www.archlinux.org/packages/community/i686/haskell-x11-xft/")
    , ("utf8-string","0.3.5", Just "http://www.archlinux.org/packages/extra/i686/haskell-utf8-string/")
    , ("haskeline", "0.6.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-haskeline/")
    , ("extensible-exceptions", "0.1.1.0", Just "http://www.archlinux.org/packages/extra/i686/haskell-extensible-exceptions/")
    , ("hashed-storage",    "0.3.6",  Just  "http://www.archlinux.org/packages/extra/i686/haskell-hashed-storage/")
    , ("mmap", "0.4.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-mmap/")
    , ("zlib", "0.5.2", Just "http://www.archlinux.org/packages/community/i686/haskell-zlib/")
    , ("gtk2hs", "0.10.1", Just "http://www.archlinux.org/packages/community/i686/gtk2hs/")
    , ("alex", "2.3.1", Just "http://www.archlinux.org/packages/community/i686/alex/")
    , ("HTTP", "4000.0.7", Just "http://www.archlinux.org/packages/community/i686/haskell-http/")
    , ("pandoc", "0.46", Just "http://www.archlinux.org/packages/community/i686/pandoc/")
    , ("terminfo", "0.3.0.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-terminfo/")
    , ("happy" ,"1.18.4", Just "http://www.archlinux.org/packages/extra/i686/happy/")
    , ("darcs", "2.3.0", Just "http://www.archlinux.org/packages/extra/i686/darcs/")
    , ("cabal-install", "0.6.2", Just "http://www.archlinux.org/packages/community/i686/cabal-install/")
    ]
