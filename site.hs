--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, (<>))
import Control.Monad ((>=>))
import Hakyll
import Hakyll.Web.Sass (sassCompilerWith)
import Hakyll.Core.Configuration (Configuration, previewPort)
import Data.Default (def)
import qualified Text.Sass.Options as SO

--------------------------------------------------------------------------------
config :: Configuration
config = def { previewPort = 8001
             , previewHost = "0.0.0.0" }


main :: IO ()
main =
  hakyllWith config $
  do
     --
     -- images and static content
     --
     imagesRules
     fontsRules
     -- dataRules

     --
     -- CSS and SASS
     --
     cssAndSassRules

     --
     -- JS
     --
     jsRules

     --
     -- static pages
     --
     -- staticPagesRules

     --
     -- projects and posts
     --
     -- postsRules
     -- projectsRules

     --
     -- collections
     --
     -- aggregatePagesRules

     --
     -- index page
     --
     indexPageRules
     indexEnPageRules

     --
     -- templates
     --

     templatesRules

--------------------------------------------------------------------------------

-- dataRules =
--   match "data/**" $
--         do route idRoute
--            compile copyFileCompiler

imagesRules =
  match "images/**" $
  do route idRoute
     compile copyFileCompiler

fontsRules =
  match "fonts/*" $
        do route idRoute
           compile copyFileCompiler



sassOptions :: SO.SassOptions
sassOptions = def { SO.sassIncludePaths = Just [ "css/"
                                               , "bower_components/foundation-sites/scss/"] }

cssAndSassRules =
  do match "css/app.scss" $
       do scssDeps <- makePatternDependency "css/_*.scss"
          rulesExtraDependencies [scssDeps] $ do
            route $ setExtension "css"
            compile $ sassCompilerWith sassOptions >>= return . fmap compressCss

     match "css/**/*.css" $
       do route idRoute
          compile compressCssCompiler


jsRules =
  do match "js/*.js" $
           do route idRoute
              compile copyFileCompiler
     match "js/vendor/*.js" $
           do route idRoute
              compile copyFileCompiler


-- staticPagesRules =
--   match (fromList ["about.rst","contact.markdown"]) $
--   do route $ setExtension "html"
--      compile $
--        pandocCompiler >>=
--        loadAndApplyTemplate "templates/default.html" defaultContext >>=
--        relativizeUrls

-- postsRules =
--   match "posts/*" $
--   do route $ setExtension "html"
--      compile $
--        pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
--        loadAndApplyTemplate "templates/default.html" postCtx >>=
--        relativizeUrls

-- aggregatePagesRules =
--   create ["archive.html"] $
--   do route idRoute
--      compile $
--        do posts <- recentFirst =<< loadAll "posts/*"
--           let archiveCtx =
--                 listField "posts" postCtx (return posts) `mappend`
--                 constField "title" "Archives" `mappend`
--                 defaultContext
--           makeItem "" >>=
--             (loadAndApplyTemplate "templates/archive.html" archiveCtx) >>=
--             (loadAndApplyTemplate "templates/default.html" archiveCtx) >>=
--             relativizeUrls



-- indexPageRules =
--   match "index.html" $
--   do route idRoute
--      compile $
--        do posts <- recentFirst =<< loadAll "posts/*"
--           let indexCtx =
--                 listField "posts" postCtx (return posts) `mappend`
--                 constField "title" "Home" `mappend`
--                 defaultContext
--           getResourceBody >>= applyAsTemplate indexCtx >>=
--             loadAndApplyTemplate "templates/default.html" indexCtx >>=
--             relativizeUrls

indexPageRules = doIndexPageRules "index.html" "templates/index.html"
indexEnPageRules = doIndexPageRules "en/index.html" "templates/index-en.html"

doIndexPageRules indexPage indexTemplate =
  match (fromList [indexPage]) $
  do route $ setExtension "html"
     compile $
       pandocCompiler >>=
       loadAndApplyTemplate indexTemplate indexCtx >>=
       loadAndApplyTemplate "templates/default.html" indexCtx >>=
       relativizeUrls


-- projectsRules =
--   do match "projects/*.md" $
--        do route $ setExtension "html"
--           compile $ pandocCompiler
--             >>= loadAndApplyTemplate "templates/project.html" projectCtx
--             >>= loadAndApplyTemplate "templates/default.html" projectCtx
--             >>= relativizeUrls

--      match "projects/*.slim" $
--            do route $ setExtension "html"
--               compile $ slimCompiler
--                 >>= loadAndApplyTemplate "templates/project.html" projectCtx
--                 >>= loadAndApplyTemplate "templates/default.html" projectCtx
--                 >>= relativizeUrls

templatesRules =
  match "templates/*" $ compile templateCompiler


-- compilers

coffee :: Compiler (Item String)
coffee = getResourceString >>= withItemBody processCoffee
  where
    processCoffee = unixFilter "coffee" ["-c", "-s"] >=>
                    unixFilter "yuicompressor" ["--type", "js"]

slimCompiler :: Compiler (Item String)
slimCompiler = getResourceString >>= withItemBody processSlim
  where processSlim = unixFilter "slimrb" ["-s", "-p"]

--
--
-- contexts
--
--
-- postCtx :: Context String
-- postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

-- archiveCtx = defaultContext

-- projectCtx = defaultContext

-- indexCtx = projectsListCtx `mappend` defaultContext

indexCtx = defaultContext

-- projectsInfo = [ ("Paranoiapp", "https://paranoiapp.net")
--                ,("OBJ", "http://ooooobj.org")
--                ,("Cat-scout", "/projects/cat-scout.html")
--                ,("Psychodata", "/projects/psychodata.html")]


-- projectsListCtx = listField "projects"
--                             ((field "project-name" (return . fst . itemBody))
--                              <> (field "project-link" (return . snd . itemBody)))
--                             (sequence . map makeItem $ projectsInfo)
