import Distribution.ArchLinux.Report

-- $ time ./arch-report +RTS -N4
-- compiled with -threaded.

--
-- Assumes the file "arch-haskell-packages" is in the current directory.
-- Assumes the .build-all.log is in the current directory
-- To generate this file,
--
-- > mypackages | sort > arch-haskell-packages.txt
--

main = do
    s <- lines `fmap` readFile "arch-haskell-packages.txt"
    writeFile "/tmp/arch-haskell-status.html" =<< report s
    putStrLn "scp /tmp/arch-haskell-status.html code.haskell.org:/srv/code/arch/"

