{-# LANGUAGE OverloadedStrings #-}

module Site.MultiLang where

import System.FilePath.Posix ((</>))
import Hakyll
import Site.Types

matchMultiLang
  :: Rules () -> Rules () -> FilePath -> Rules ()
matchMultiLang ruRules enRules path =
  do match ruPages $ ruRules
     match enPages $ enRules
  where ruPages = fromGlob $ "ru" </> path
        enPages = fromGlob $ "en" </> path
