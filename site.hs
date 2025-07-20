--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.SideNote (usingSideNotes)

--------------------------------------------------------------------------------
-- Function to convert Obsidian-style image links to standard markdown
convertObsidianImages :: String -> String
convertObsidianImages = convertImages
  where
    convertImages [] = []
    convertImages ('!' : '[' : '[' : rest) =
      let (filename, remaining) = span (/= ']') rest
       in case remaining of
            (']' : ']' : after) -> "![](attachments/" ++ filename ++ ")" ++ convertImages after
            _ -> "![[" ++ convertImages rest
    convertImages (c : cs) = c : convertImages cs

-- Custom compiler for Obsidian markdown files with sidenotes
obsidianCompiler :: Compiler (Item String)
obsidianCompiler = do
  body <- getResourceBody
  let processedBody = fmap convertObsidianImages body
  pandocItem <- readPandocWith defaultHakyllReaderOptions processedBody
  let processedPandoc = fmap usingSideNotes pandocItem
  return $ writePandocWith defaultHakyllWriterOptions processedPandoc

main :: IO ()
main = hakyll $ do
  match "posts/attachments/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/**" $ do
    route idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "posts/*.md" $ do
    route $ setExtension "html"
    compile $
      obsidianCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/post-default.html" postCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let indexCtx =
            listField "posts" (postCtx `mappend` bodyField "body") (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index-default.html" indexCtx
        >>= relativizeUrls

  match "about.html" $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate defaultContext
        >>= loadAndApplyTemplate "templates/about-default.html" defaultContext
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d"
    `mappend` metadataField
    `mappend` defaultContext
