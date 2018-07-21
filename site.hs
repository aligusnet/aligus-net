--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Hakyll.Contrib.LaTeX (initFormulaCompilerDataURI)
import Image.LaTeX.Render (defaultEnv)
import Image.LaTeX.Render.Pandoc (PandocFormulaOptions, defaultPandocFormulaOptions)
import Text.Pandoc.Definition (Pandoc)


--------------------------------------------------------------------------------
main :: IO ()
main = initFormulaCompilerDataURI 1000 defaultEnv >>= main'


--------------------------------------------------------------------------------
main' :: (PandocFormulaOptions -> Pandoc -> Compiler Pandoc) -> IO ()
main' renderFormulae = hakyllWith config $ do
    let compiler = pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions
                 $ renderFormulae defaultPandocFormulaOptions

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tag/*.html")

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Tag: " ++ tag
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title                           `mappend`
                      constField "blogid" "selected"                     `mappend`
                      listField "posts" (postCtx tags) (return posts)    `mappend`
                      field "tags" (\_ -> renderTagList tags)            `mappend`
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


    match "sass/default.sass" $ do
        route $ constRoute "css/default.css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ postPage compiler "templates/post.html" tags

    create ["archive.html"] $ 
        postListPage "Archive" "templates/archive.html" "posts/*" tags

    match "tea-meetup/*" $ postPage compiler "templates/tea-meetup.html" tags

    create ["tea-meetup-history.html"] $ 
        postListPage "Tea Meetup" "templates/tea-meetup-history.html" "tea-meetup/*" tags 

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            teameetups <- recentFirst =<< loadAll "tea-meetup/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    listField "tea-meetup" (postCtx tags) (return teameetups) `mappend`
                    field "tags" (\_ -> renderTagList tags)         `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "projects/*" $ do
        route $ setExtension "tmp"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/project.html" (projectCtx tags)
            >>= relativizeUrls

    match "projects.html" $ do
        route idRoute
        compile $ do
            projects <- loadAll "projects/*"
            let ctx =
                    listField "projects" defaultContext (return projects) `mappend`
                    tagsField "tags" tags                                 `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


    match "astro.html" $ contentPage

    match "stats.html" $ contentPage

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%e %B %Y" `mappend`
    tagsField "tags" tags       `mappend`
    defaultContext


--------------------------------------------------------------------------------
projectCtx :: Tags -> Context String
projectCtx tags =
    tagsField "tags" tags        `mappend`
    defaultContext


--------------------------------------------------------------------------------
listContextWith :: Context String -> String -> Context a
listContextWith ctx s = listField s ctx $ do
    identifier <- getUnderlying
    metadata <- getMetadata identifier
    let metas = maybe [] (map trim . splitAll ",") $ lookupString s metadata
    return $ map (\x -> Item (fromFilePath x) x) metas


--------------------------------------------------------------------------------
listContext :: String -> Context a
listContext = listContextWith defaultContext


--------------------------------------------------------------------------------
contentPage :: Rules()
contentPage = do
    route idRoute
    compile $ do
        let ctx =
                listContext "scripts"           `mappend`
                defaultContext
        getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

--------------------------------------------------------------------------------
postPage :: Compiler (Item String) -> Identifier -> Tags -> Rules()
postPage compiler template tags = do
    route $ setExtension "html"
    compile $ compiler
        >>= loadAndApplyTemplate template                 (postCtx tags)
        >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
        >>= relativizeUrls


--------------------------------------------------------------------------------
postListPage :: String -> Identifier -> Pattern -> Tags -> Rules()
postListPage name template path tags = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll path
        let archiveCtx =
                listField "posts" (postCtx tags) (return posts) `mappend`
                constField "title" name                         `mappend`
                field "tags" (\_ -> renderTagList tags)         `mappend`
                defaultContext
        makeItem ""
            >>= loadAndApplyTemplate template                 archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "aws s3 sync _site s3://aws-website-aligus-net-09yna --region us-east-1"
    }
