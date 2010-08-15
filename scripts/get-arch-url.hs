import Distribution.ArchLinux.AUR
import System.Environment
import System.Exit

main = do
    [arch_name] <- getArgs
    q <- info arch_name
    case q of
        Left err -> exitWith (ExitFailure 1)
        Right v  -> do
            putStrLn (packageURLinAUR v)
            exitSuccess
