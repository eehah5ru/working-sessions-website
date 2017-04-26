{-# LANGUAGE OverloadedStrings #-}

module Site.MultiLang where

import System.FilePath.Posix ((</>))
import Hakyll
import Hakyll.Core.Util.String (replaceAll)
import Site.Types

matchMultiLang
  :: Rules () -> Rules () -> FilePath -> Rules ()
matchMultiLang ruRules enRules path =
  do match ruPages $ ruRules
     match enPages $ enRules
  where ruPages = fromGlob $ "ru" </> path
        enPages = fromGlob $ "en" </> path

ruUrlField :: Context String
ruUrlField = multiLangUrlField "ru" "en"

enUrlField :: Context String
enUrlField = multiLangUrlField "en" "ru"

multiLangUrlField :: String -> String -> Context String
multiLangUrlField lang fromLang = field fieldName (\x -> getUrl fromLang x >>= return . translateUrl fromLang lang >>= return . (++) "/")
  where fieldName = lang ++ "_url"
        getUrl :: String -> Item a -> Compiler String
        getUrl langPrefix i = return (itemIdentifier i)
          >>= getRoute
          >>= return . maybe ("/" ++ langPrefix ++ "/404.html") id

        translateUrl :: String -> String -> String -> String
        translateUrl fromLang toLang = replaceAll (fromLang ++ "/") (const (toLang ++ "/"))
