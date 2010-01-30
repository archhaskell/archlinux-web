-- |
-- Module      : Distribution.ArchLinux.AUR
-- Copyright   : (c) 2009 Don Stewart
-- License     : BSD3
-- Maintainer  : Don Stewart
--
-- Support for querying the AUR database.
--
module Distribution.ArchLinux.AUR (
        AURInfo(..),
        info,
        search,
        maintainer,
        package

    ) where

{-
The methods currently allowed are:

    * search
    * info

Each method requires the following HTTP GET syntax:
   type=methodname&arg=data

Where methodname is the name of an allowed method, and data is the argument to the call.

If you need jsonp type callback specification, you can provide an additional variable callback.
Example URL:
   http://aur-url/rpc.php?type=search&arg=foobar&callback=jsonp1192244621103
-}

import Network.HTTP
import Distribution.Version
import Distribution.Text
import Text.JSON
import Text.JSON.String
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import qualified Data.Map as M
import Control.Monad
import System.FilePath
import Data.List
import Data.Char

import Distribution.ArchLinux.PkgBuild

------------------------------------------------------------------------

-- | Query AUR for information on a package 
--
-- > $ info "xmonad"
-- >
-- > Right (AURInfo { packageID       = 10593
-- >              , packageName     = "xmonad"
-- >              , packageVersion  = Right (Version {versionBranch = [0,8,1] , versionTags = []},"1.2")i
-- >              , packageCategory = 17
-- >              , packageDesc     = "A lightweight X11 tiled window manager written in Haskell"
-- >              , packageLocation = 3
-- >              , packageURL      = "http://xmonad.org/"
-- >              , packagePath     = "/packages/xmonad/xmonad.tar.gz"
-- >              , packageLicense = "custom:BSD3"
-- >              , packageVotes = 260
-- >              , packageOutOfDate = False })
--
info :: String -> IO (Either String AURInfo)
info m = eval (InfoRequest (Name m))

-- | Search AUR for packages matching pattern. Returns a list of info results.
search :: String -> IO [AURInfo]
search m = do
    v <- eval (SearchRequest (Name m))
    return $ case v of
        Left  e -> []
        Right a -> flatten a

-- | Search AUR for packages owned by maintainer. Returns a list of info results.
maintainer :: String -> IO [AURInfo]
maintainer m = do
    v <- eval (MaintainerRequest (Name m))
    return $ case v of
        Left  e -> []
        Right a -> maintainerPackages a

------------------------------------------------------------------------
--
-- TODO: programmatically list all packages by arch-haskell
-- generate table of versions.
-- colour out of date things.
--

-- | Return the parsed PKGBUILD
-- pkgbuild :: String -> IO (Either String [String]) -- (Either String PkgBuild)

package :: String -> IO (Either String AURInfo, Either String AnnotatedPkgBuild)
package m = do
    v <- info m
    case v of
        Left s  -> return $ (Left s, Left "No PKGBUILD found")
        Right p -> do
            let name   = packageName p
                aurUrl = "http://aur.archlinux.org/packages" </> name </> name </> "PKGBUILD"

            rsp <- simpleHTTP (getRequest aurUrl)
            case rsp of
                 Left err -> return $ (Right p, Left (show err))
                 Right _  -> do
                    pkg <- getResponseBody rsp -- TODO 404
                    case decodePackage pkg of
                         Left e ->  return (Right p, Left e)
                         Right k -> return (Right p, Right k)

------------------------------------------------------------------------
--
-- TODO:
-- generate report on packages. groupBy
--

-- | List packages not built with up to date version of cabal2arch
{-
lint_cabal2arch = do
    aurs <- search "haskell"
    forM_ aurs $ \aur -> do
        let n = packageName aur
        k <- pkgbuild n
        case k of
             Left _ -> putStrLn $ "No pkgbuild for" ++ packageName aur
             Right p -> do

                case cabal2arch_version p of
                     v | v == simpleParse "0.6"
                             -> putStrLn $ "GOOD " ++ packageName aur 

                     v       -> do putStrLn $ "Missing cabal2arch version for: " ++ packageName aur ++ " " ++ show v
                                   putStrLn $ "http://aur.archlinux.org/packages.php?ID=" ++ show ( packageID aur)
                                   putStrLn ""
-}

------------------------------------------------------------------------
--
-- TODO: from the packagename, construct the url to the AUR page.
--

-- | URL for AUR RPC server
url :: Doc
url = text "http://aur.archlinux.org/rpc.php"

-- | Query the server
eval :: JSON a => AURRequest -> IO (Either String a)
eval m = do
    rsp  <- simpleHTTP (getRequest call)
    json <- getResponseBody rsp
    return . resultToEither . decode $ json
  where
    call  = render $ url <?> pPrint m

------------------------------------------------------------------------
-- RPC requests

-- | Type for AUR RPC requests. They can be info queries, or search queries.
--
data AURRequest
    = SearchRequest     Name
    | MaintainerRequest Name
    | InfoRequest       Name
    deriving Show

instance Pretty AURRequest where
    pPrint (SearchRequest n) = text "type" <=> text "search" <&> text "arg" <=> pPrint n
    pPrint (InfoRequest   n) = text "type" <=> text "info"   <&> text "arg" <=> pPrint n
    pPrint (MaintainerRequest n) = text "type" <=> text "msearch" <&> text "arg" <=> pPrint n

-- | Wrap up a package name a bit safely.
newtype Name = Name String
    deriving Show

instance Pretty Name where
    pPrint (Name n) = text n

-- | Useful combinators
infixl 6 <=>, <&>, <?>

(<=>), (<&>), (<?>)  :: Doc -> Doc -> Doc
p <=> q = p <> char '=' <> q
p <&> q = p <> char '&' <> q
p <?> q = p <> char '?' <> q

------------------------------------------------------------------------
-- RPC response values

-- We can in turn use this info to query PKGBUILDs on the server

-- | A type for maintainer search.
data AURMaintainer = AURMaintainer { maintainerPackages :: [AURInfo] } deriving Show

instance JSON AURMaintainer where
    showJSON = undefined

    readJSON (JSObject o) = do
            -- sanity check:
            case M.lookup "type" json of
                Just (JSString t) | fromJSString t == "msearch" -> do
                    as <- forM results $ \(JSObject o) -> do
                                let obj = M.fromList (fromJSObject o) :: M.Map String JSValue
                                parseInfo obj

                    return (AURMaintainer as)

                s -> fail $ "No type field in JSON response!" ++ show s

        where
            json     = M.fromList (fromJSObject o) :: M.Map String JSValue
            results  = case M.lookup "results" json of
                            Nothing           -> error $ "No results for info object"
                            Just (JSArray a)  -> a -- a list.

------------------------------------------------------------------------

-- | Type for AUR RPC responses.
data AURInfo
    = AURInfo {
         packageID        :: Integer                        -- ^ unique ID of the package on AUR
        ,packageURLinAUR  :: String                         -- ^ url of AUR package
        ,packageName      :: String                         -- ^ string name of package
        ,packageVersion   :: Either String (Version,String) -- ^ either the AUR version (version,rev)  or a string
        ,packageCategory  :: Integer                        -- ^ numeric category of the package (e.g. 17 == System)
        ,packageDesc      :: String                         -- ^ package synopsis
        ,packageLocation  :: Integer                        -- ^ which repository is it stored in (community, AUR etc)
        ,packageURL       :: String                         -- ^ url (sanity check: should be hackage url mostly)
        ,packagePath      :: FilePath                       -- ^ url path to package source.
        ,packageLicense   :: String                         -- ^ type of license
        ,packageVotes     :: Integer                        -- ^ votes on package
        ,packageOutOfDate :: Bool                           -- ^ is the package flagged as out of date
      }

    deriving Show

instance JSON AURInfo where
    showJSON = undefined

    readJSON (JSObject o) = do
            -- sanity check:
            case M.lookup "type" json of
                Just (JSString t) | fromJSString t == "info" -> parseInfo results
                s -> fail $ "No type field in JSON response!" ++ show s
        where
            json     = M.fromList (fromJSObject o) :: M.Map String JSValue

            results  = case M.lookup "results" json of
                            Nothing            -> error $ "No results for info object"
                            Just (JSObject o)  -> M.fromList (fromJSObject o) :: M.Map String JSValue

-- Need a different JSON instance
data AURSearch = AURSearch { flatten :: [AURInfo] }

-- A list of results
instance JSON AURSearch where
    showJSON = undefined

    readJSON (JSObject o) = do
            -- sanity check:
            case M.lookup "type" json of
                Just (JSString t) | fromJSString t == "search" -> do
                    as <- forM results $ \(JSObject o) -> do
                                let obj = M.fromList (fromJSObject o) :: M.Map String JSValue
                                parseInfo obj

                    return (AURSearch as)

                s -> fail $ "No type field in JSON response!" ++ show s

        where
            json     = M.fromList (fromJSObject o) :: M.Map String JSValue
            results  = case M.lookup "results" json of
                            Nothing           -> error $ "No results for info object"
                            Just (JSArray a)  -> a -- a list.


-- | Parse a AURInfo.
parseInfo :: M.Map String JSValue -> Result AURInfo
parseInfo info_obj = do
    JSString id_   <- label "ID"
    JSString name_ <- label "Name"
    JSString vers_ <- label "Version"
    JSString cat_  <- label "CategoryID"
    JSString desc_ <- label "Description"
    JSString loc_  <- label "LocationID"
    JSString url_  <- label "URL"
    JSString path_ <- label "URLPath"
    JSString lic_  <- label "License"
    JSString vote_ <- label "NumVotes"
    JSString date_ <- label "OutOfDate"

    let vers__  = fromJSString vers_
        (x,xs)  = break (== '-') vers__
        version | '-' `elem` vers__ = case simpleParse x of
                                        Nothing -> Left vers__
                                        Just v  -> Right (v, tail xs)
                | otherwise         = Left vers__

        id_ident = read (fromJSString id_)

    return $ AURInfo {
                packageID          = id_ident
               ,packageURLinAUR    = "http://aur.archlinux.org/packages.php?ID=" ++ show id_ident
               ,packageName        = fromJSString name_
               ,packageVersion     = version
               ,packageCategory    = read (fromJSString cat_)
               ,packageDesc        = fromJSString desc_
               ,packageLocation    = read (fromJSString loc_)
               ,packageURL         = fromJSString url_  -- TODO : should be hackage url
               ,packagePath        = fromJSString path_ -- TODO : should be hackage url
               ,packageLicense     = fromJSString lic_  -- TODO : should be hackage url
               ,packageVotes       = read (fromJSString vote_)
               ,packageOutOfDate   = case fromJSString date_ of
                                        "0" -> False
                                        _   -> True
             }
  where

    label k = case M.lookup k info_obj of
                    Nothing -> fail $ "No field " ++ show k
                    Just o  -> return o



{-
JSObject (JSONObject {fromJSObject = [("type",JSString (JSONString {fromJSString = "info"})),("results",JSObject (JSONObject {fromJSObject = [("ID",JSString (JSONString {fromJSString = "10593"})),("Name",JSString (JSONString {fromJSString = "xmonad"})),("Version",JSString (JSONString {fromJSString = "0.8.1-1.2"})),("CategoryID",JSString (JSONString {fromJSString = "17"})),("Description",JSString (JSONString {fromJSString = "A lightweight X11 tiled window manager written in Haskell"})),("LocationID",JSString (JSONString {fromJSString = "3"})),("URL",JSString (JSONString {fromJSString = "http://xmonad.org/"})),("URLPath",JSString (JSONString {fromJSString = "/packages/xmonad/xmonad.tar.gz"})),("License",JSString (JSONString {fromJSString = "custom:BSD3"})),("NumVotes",JSString (JSONString {fromJSString = "259"})),("OutOfDate",JSString (JSONString {fromJSString = "0"}))]}))]})
Right ()
-}

------------------------------------------------------------------------
