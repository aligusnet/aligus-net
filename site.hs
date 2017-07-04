--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

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

    match "astro-ui/public/assets/js/elm.js" $ do
        route $ constRoute "js/astro.js"
        compile copyFileCompiler

    match "astro-ui/public/assets/js/port.js" $ do
        route $ constRoute "js/astro-port.js"
        compile copyFileCompiler

    match (fromList ["about.rst"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "astro.html" $ contentPage

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
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
