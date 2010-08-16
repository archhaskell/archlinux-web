import Distribution.ArchLinux.Report
import Distribution.ArchLinux.AUR
import Data.List

-- $ time ./arch-report +RTS -N4
-- compiled with -threaded.

--
-- Assumes the .build-all.log is in the current directory
-- To generate this file,
--
-- > mypackages | sort > arch-haskell-packages.txt
--

me = "arch-haskell"

main = do
    -- s <- lines `fmap` readFile "arch-haskell-packages.txt"
    packages <- maintainer me
    let s = sort $ map packageName packages

    writeFile "/tmp/arch-haskell-status.html" =<< report s
    putStrLn "scp /tmp/arch-haskell-status.html code.haskell.org:/srv/code/arch/"

