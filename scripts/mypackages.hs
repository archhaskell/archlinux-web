import Distribution.ArchLinux.AUR
import Control.Monad

me = "arch-haskell"

main = do
    packages <- maintainer me
    forM_ packages $ putStrLn . packageName
