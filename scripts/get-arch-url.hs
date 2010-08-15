import Distribution.ArchLinux.AUR
import System.Environment
import System.Exit

--
-- given the name of a package in AUR, find the URL in AUR for that package.
--

main = do
    [arch_name] <- getArgs
    q <- info arch_name
    case q of
        Left err -> exitWith (ExitFailure 1)
        Right v  -> do
            putStrLn (packageURLinAUR v)
            exitSuccess
