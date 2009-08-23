
import Distribution.ArchLinux.Report
import Data.Map as M
import System.Environment
import System.FilePath
import Control.Monad
import System.Cmd

import Distribution.Text

main = do
    packages <- getArgs
    v <- loadPackageIndex
    forM_ packages $ \p -> do
        case M.lookup p v of
             Nothing   -> do
                 putStrLn $ "No package found for " ++ show p
             Just version -> do
                 let url = "http://hackage.haskell.org/packages/archive"
                                </> p </> display version </> p <.> "cabal"

                 system $ "update-packages.sh " ++ url
                 return ()
