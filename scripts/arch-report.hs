import Distribution.ArchLinux.Report

main = do
    s <- lines `fmap` readFile "arch-haskell-packages.txt"
    writeFile "/tmp/x.html" =<< report s

