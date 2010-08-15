-- auto update all packages maintained by arch-haskell that were 
-- built with cabal2arch older as asserted by oldCabal2Arch.
--
-- generate the pkgbuild, upload it. 
--


import Distribution.ArchLinux.AUR
import Distribution.ArchLinux.PkgBuild
import Distribution.ArchLinux.Report

import Data.Map as M hiding (update)
import System.FilePath
import System.Cmd
import Distribution.Text
import Control.Monad

import Text.Printf

me = "arch-haskell"

main = do
    packages <- maintainer me

    idx      <- loadPackageIndex

    print (length packages)

    -- for all my packages
    forM_ packages $ \p -> do

        -- grab the parsed pkgbuild.
        (_,k) <- package (packageName p) -- need a short circuit
        case k of
             Left err  -> print err
             Right pkg ->
                if oldCabal2Arch pkg
                   then do

                       -- convert arch name to hackage name
                       let name = takeFileName (packageURL p)

                       printf "Updating %s -> %s\n" (packageName p) name

                       update idx name

                   else return () -- putStrLn $ "OK: " ++  (packageName p)

update v p = do
    case M.lookup p v of -- arch haskell name, not cabal name
         Nothing   -> do
             putStrLn $ "No package found for " ++ show p
         Just version -> do
             let url = "http://hackage.haskell.org/packages/archive"
                            </> p </> display version </> p <.> "cabal"

             system $ "update-packages.sh " ++ url
             return ()

