import Distribution.ArchLinux.Report

-- $ time ./arch-report +RTS -N4
-- compiled with -threaded.

main = do
    s <- lines `fmap` readFile "arch-haskell-packages.txt"
    writeFile "/tmp/arch-haskell-status.html" =<< report s
    putStrLn "Now: scp /tmp/arch-haskell-status.html galois.com:www/"

