
-- | Construct reports about a set of packages in AUR
--
module Distribution.ArchLinux.Report where

import Distribution.ArchLinux.AUR
import Distribution.ArchLinux.PkgBuild

import Distribution.Text

import System.FilePath
import Data.Maybe

import Text.XHtml.Transitional
import Control.OldException
import Control.Monad
import Data.List
import Data.Ord

-- | Take as input a list of package names, return a pandoc object
-- reporting on interesting facts about the packages.
--
report :: [String] -> IO String
report xs = do
    -- collect sets of results
    res_ <- forM xs $ \p -> do
        handle (\s -> return (p, (Left (show s), Left []))) $ do
            k <- package p
            return (p, k)

    let results = sortBy (comparing fst) res_

    return. showHtml $
        (header $
            (thetitle (toHtml "Arch Haskell Package Report")) +++
            ((thelink noHtml) ! [ rel "stylesheet"
                                , href "http://galois.com/~dons/arch-haskell.css"
                                , thetype "text/css" ])) +++

        (body $

            (scores . table $
                tr (concatHtml
                      [ td . categoryTag . toHtml $ "Package"
                      , td . categoryTag . toHtml $ "Hackage"
                      , td . categoryTag . toHtml $ "Version"
                      , td . categoryTag . toHtml $ "cabal2arch"
                      , td . categoryTag . toHtml $ "Votes"
                      , td . categoryTag . toHtml $ "Description"
                      ]) +++

                concatHtml

                    [

                     tr $ concatHtml $
                      case aur_ of
                       Left  err ->
                          [ td $ toHtml p
                          , td $ bad (toHtml "No AUR entry found!")
                          ]

                       Right aur -> case pkg_ of

                        -- Didn't find a PKGBUILD
                         Left  err ->
                          [ td . toHtml $
                              hotlink
                                (packageURLinAUR aur)
                                (toHtml p)
                          , td . toHtml $
                              hotlink
                                (packageURL aur)
                                (toHtml (takeFileName (packageURL aur)))
                          , td  $
                                case packageVersion aur of
                                     Left s  -> bad $ toHtml s
                                     Right (v,_) -> toHtml $ display v
                          , td $ bad (toHtml "PKGBUILD not found")
                          , td $ toHtml $ show $ packageVotes aur
                          , td $ toHtml $ packageDesc aur
                          ]

                        -- Found everything
                         Right pkg ->
                          [ td . toHtml $
                              hotlink
                                (packageURLinAUR aur)
                                (toHtml p)

                          , td . toHtml $
                              hotlink
                                (packageURL aur)
                                (toHtml (takeFileName (packageURL aur)))

                          , td  $
                                case packageVersion aur of
                                     Left s  -> bad $ toHtml s
                                     Right (v,_) -> toHtml $ display v

                          , td  $
                              if oldCabal2Arch pkg
                                 then bad (toHtml $  display $ fromJust $ pkgBuiltWith pkg)
                                 else toHtml $ display $ fromJust $ pkgBuiltWith pkg

                          , td  $ toHtml $ show $ packageVotes aur
                          , td  $ toHtml $ packageDesc aur

                          ]

                        | (p, (aur_,pkg_)) <- results
                        ]
                )
            )


    {-
    (Right (AURInfo {packageID = 17480, packageName = "haskell-json", packageVersion = Right (Version {versionBranch = [0,4,3], versionTags = []},"1"), packageCategory = 10, packageDesc = "Support for serialising Haskell to and from JSON", packageLocation = 2, packageURL = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/json", packagePath = "/packages/haskell-json/haskell-json.tar.gz", packageLicense = "custom:BSD3", packageVotes = 16, packageOutOfDate = False}),Right (AnnotatedPkgBuild {pkgBuiltWith = Just (Version {versionBranch = [0,5,3], versionTags = []}), pkgHeader = "# Contributor: Arch Haskell Team <arch-haskell@haskell.org>", pkgBody = PkgBuild {arch_pkgname = "haskell-json", arch_pkgver = Version {versionBranch = [0,4,3], versionTags = []}, arch_pkgrel = 1, arch_pkgdesc = "\"Support for serialising Haskell to and from JSON\"", arch_arch = ArchList [Arch_X86,Arch_X86_64], arch_url = "\"http://hackage.haskell.org/cgi-bin/hackage-scripts/package/json\"", arch_license = ArchList [BSD3], arch_makedepends = ArchList [], arch_depends = ArchList [], arch_source = ArchList [], arch_md5sum = ArchList [], arch_build = [], arch_install = Nothing, arch_options = ArchList []}}))
    -}

categoryTag  x = thediv x ! [identifier "Category"    ]
bad     x = thediv x ! [identifier "Bad"  ]
good    x = thediv x ! [identifier "Best" ]
scores  x = thediv x ! [identifier "Scores" ]

