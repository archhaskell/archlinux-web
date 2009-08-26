import Distribution.ArchLinux.Report
import Distribution.ArchLinux.AUR
import Distribution.ArchLinux.PkgBuild

import System.FilePath
import Control.Monad

import Distribution.Text
import Distribution.Version

import System.Directory

main = do
    removeFile "cabalArchMap.txt"
    s <- lines `fmap` readFile "arch-haskell-packages.txt"

    forM community $ \p@(name, vers, url) -> do
                appendFile "cabalArchMap.txt" $ show p ++ "\n"
    --
    -- start with the Arch Package names, deriving the Cabal names, the
    -- version, and the URL to find them.
    --
    forM s $ \p -> do
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

                appendFile "cabalArchMap.txt" $ show (name, vers, Just url) ++ "\n"

    putStrLn "scp cabalArchMap.txt galois.com:www/"

-- hand search
community =
    [ ("xmonad", "0.8.1", Just "http://www.archlinux.org/packages/community/i686/xmonad/")
    , ("X11", "1.4.5", Just "http://www.archlinux.org/packages/community/i686/haskell-x11/")
    , ("X11-xft", "0.3", Just "http://www.archlinux.org/packages/community/i686/haskell-x11-xft/")
    , ("utf8-string","0.3.5", Just "http://www.archlinux.org/packages/extra/i686/haskell-utf8-string/")
    , ("haskeline", "0.6.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-haskeline/")
    , ("extensible-exceptions", "0.1.1.0", Just "http://www.archlinux.org/packages/extra/i686/haskell-extensible-exceptions/")
    , ("hashed-storage",    "0.3.6",  Just  "http://www.archlinux.org/packages/extra/i686/haskell-hashed-storage/")
    , ("mmap", "0.4.1", Just "http://www.archlinux.org/packages/extra/i686/haskell-mmap/")
    , ("zlib", "0.5.2", Just "http://www.archlinux.org/packages/community/i686/haskell-zlib/")
    , ("gtk2hs", "0.10.1", Just "http://www.archlinux.org/packages/community/i686/gtk2hs/")
    , ("alex", "2.3.1", Just "http://www.archlinux.org/packages/community/i686/alex/")
    , ("http", "4000.0.7", Just "http://www.archlinux.org/packages/community/i686/haskell-http/")
    , ("pandoc", "0.46", Just "http://www.archlinux.org/packages/community/i686/pandoc/")
    , ("terminfo", "0.3.0.2", Just "http://www.archlinux.org/packages/extra/i686/haskell-terminfo/")
    , ("happy" ,"1.18.4", Just "http://www.archlinux.org/packages/extra/i686/happy/")
    , ("darcs", "2.3.0", Just "http://www.archlinux.org/packages/extra/i686/darcs/")
    , ("cabal-install", "0.6.2", Just "http://www.archlinux.org/packages/community/i686/cabal-install/")
    ]
