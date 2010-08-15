import Distribution.ArchLinux.AUR
import Control.Monad

--
-- packages maintained by arch-haskell
--

me = "arch-haskell"

main = do
    packages <- maintainer me
    forM_ packages $ putStrLn . packageName
