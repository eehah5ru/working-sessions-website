{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad ((>=>))
-- import           Data.ByteString.Lazy as BSL
import           Data.Default (def)
-- import           Data.Monoid (mappend, (<>))
import           Hakyll
-- import           Hakyll.Core.Configuration (Configuration, previewPort)
-- import           Hakyll.Core.Metadata
import           Hakyll.Web.Sass (sassCompilerWith)
import Hakyll.Core.Compiler (getUnderlying)
import Hakyll.Web.Template.Internal (readTemplate)
import qualified Text.Sass.Options as SO

import Site.Types
import Site.SlimPage
import Site.MultiLang
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
     staticPagesRules

     archiveIndexPageRules

     archivePagesRules

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
     -- indexPageRules
     -- indexEnPageRules

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

--
-- templates
--
ruIndexTemplate = "templates/default.slim"
enIndexTemplate = "templates/default.html"

defaultTemplate = "templates/default.html"

-- ruIndexPageRules = doIndexPageRules "index.html" ruIndexTemplate
-- enIndexPageRules = doIndexPageRules "en/index.html" enIndexTemplate

staticPagesRules =
  do
    -- 2017
    indexPage "2017/index.slim"
    indexPage "2017/invitation.slim"

    -- 2016
    indexPage "2016/index.slim"
    -- indexPage "2016/archive.slim"
  where
    indexPage = matchMultiLang ruRules enRules
    ruRules = slimPageRules $ (\x -> return x
                                >>= applyAsTemplate siteCtx
                                >>= loadAndApplyTemplate "templates/default.slim" siteCtx)
    enRules = ruRules

    -- readSlimTemplate :: Identifier -> Compiler (Template)
    -- readSlimTemplate p =
    --   loadBody p >>= compileSlimWithEmptyLocals >>= return . readTemplate

    -- applyRuTemplate x = do
    --   tpl <- readSlimTemplate "templates/default.slim"
    --   applyTemplate tpl defaultContext x

archivePagesRules =
  do
    rules "2016/archive/*.slim"
  where rules = matchMultiLang ruRules enRules
        ruRules = slimPageRules $ \x ->
                    return x
                    >>= applyAsTemplate siteCtx
                    >>= loadAndApplyTemplate "templates/archive-project.slim" siteCtx
                    >>= loadAndApplyTemplate ruIndexTemplate siteCtx
        enRules = ruRules


archiveIndexPageRules =
  matchMultiLang ruRules enRules "2016/archive.slim"
  where ruRules = slimPageRules $ \x ->
                    do pTpl <- loadBody "templates/archive-item.slim"
                       plTpl <- loadBody "templates/archive-projects-list-item.slim"
                       projects <- loadAll "ru/2016/archive/*.slim"
                       projects2 <- return . take 100 . cycle $ projects
                       s <- applyTemplateList pTpl siteCtx projects
                       s2 <- applyTemplateList plTpl siteCtx projects2
                       let archiveCtx = constField "projects" s `mappend`
                                        constField "projects_list" s2 `mappend`
                                        siteCtx
                       applyAsTemplate archiveCtx x
                         >>= loadAndApplyTemplate "templates/default.slim" archiveCtx
        enRules = ruRules

-- doIndexPageRules indexPage indexTemplate =
--   match (fromList [indexPage]) $
--   do route $ setExtension "html"
--      compile $
--        pandocCompiler >>=
--        loadAndApplyTemplate indexTemplate indexCtx >>=
--        loadAndApplyTemplate "templates/default.html" indexCtx >>=
--        relativizeUrls

-- doStaticPageRules (page, template) =
--   match (fromList [page]) $
--         do m <- getMetadata page
--            route $ setExtension "html"
--            compile $ (slimCompiler m)
--              >>= loadAndApplyTemplate template defaultContext
--              >>= loadAndApplyTemplate defaultTemplate defaultContext
--              >>= relativizeUrls

-- staticPagesRules =
--   match (fromList ["about.rst","contact.markdown"]) $
--   do route $ setExtension "html"
--      compile $
--        pandocCompiler >>=
--        loadAndApplyTemplate "templates/default.html" defaultContext >>=
--        relativizeUrls

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
  do
    match "templates/*.html" $ compile templateCompiler
    match ("templates/*.slim" .&&. (complement "templates/_*.slim")) $ do
      slimDeps <- makePatternDependency "templates/_*.slim"
      rulesExtraDependencies [slimDeps] $ compile $
        getResourceString >>= withItemBody compileSlimWithEmptyLocals >>= withItemBody (return . readTemplate)


-- compilers

coffee :: Compiler (Item String)
coffee = getResourceString >>= withItemBody processCoffee
  where
    processCoffee = unixFilter "coffee" ["-c", "-s"] >=>
                    unixFilter "yuicompressor" ["--type", "js"]




--
--
-- contexts
--

siteCtx = ruUrlField `mappend`
          enUrlField `mappend`
          defaultContext
--
-- postCtx :: Context String
-- postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

-- archiveCtx = defaultContext

-- projectCtx = defaultContext

-- indexCtx = projectsListCtx `mappend` defaultContext

-- indexCtx = defaultContext

-- projectsInfo = [ ("Paranoiapp", "https://paranoiapp.net")
--                ,("OBJ", "http://ooooobj.org")
--                ,("Cat-scout", "/projects/cat-scout.html")
--                ,("Psychodata", "/projects/psychodata.html")]


-- projectsListCtx = listField "projects"
--                             ((field "project-name" (return . fst . itemBody))
--                              <> (field "project-link" (return . snd . itemBody)))
--                             (sequence . map makeItem $ projectsInfo)
