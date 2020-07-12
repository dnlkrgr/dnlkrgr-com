--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                    ( mappend )
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main =
    hakyllWith
            (defaultConfiguration
                { deployCommand =
                    unwords
                        [ "rsync -cave 'ssh' _site daniel@dnlkrgr.com:hakyll && "
                        , "ssh -t dnlkrgr.com '"
                        , "sudo rm -rf /var/www/dnlkrgr.com/html &&"
                        , "sudo cp -r ~/hakyll/_site /var/www/dnlkrgr.com/html"
                        , "'"
                        ]
                }
            )
        $ do
              match "images/*" $ do
                  route idRoute
                  compile copyFileCompiler

              match "css/*" $ do
                  route idRoute
                  compile compressCssCompiler

              match (fromList ["about.rst", "contact.markdown"]) $ do
                  route $ setExtension "html"
                  compile
                      $   pandocCompiler
                      >>= loadAndApplyTemplate "templates/default.html"
                                               defaultContext
                      >>= relativizeUrls

              tags <- buildTags "posts/*" (fromCapture "tags/*.html")

              tagsRules tags $ \tag pattern -> do
                  let title = "Posts tagged \"" ++ tag ++ "\""
                  route idRoute
                  compile $ do
                      posts <- recentFirst =<< loadAll pattern
                      let ctx = constField "title" title
                                `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                                `mappend` defaultContext
              
                      makeItem ""
                          >>= loadAndApplyTemplate "templates/tag.html" ctx
                          >>= loadAndApplyTemplate "templates/default.html" ctx
                          >>= relativizeUrls

              match "posts/*" $ do
                  route $ setExtension "html"
                  compile
                      $   pandocCompiler
                      >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
                      >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
                      >>= relativizeUrls

              create ["archive.html"] $ do
                  route idRoute
                  compile $ do
                      posts <- recentFirst =<< loadAll "posts/*"
                      let archiveCtx =
                              listField "posts" postCtx (return posts)
                                  `mappend` constField "title" "Posts"
                                  `mappend` defaultContext

                      makeItem ""
                          >>= loadAndApplyTemplate "templates/archive.html"
                                                   archiveCtx
                          >>= loadAndApplyTemplate "templates/default.html"
                                                   archiveCtx
                          >>= relativizeUrls

              create ["sitemap.xml"] $ do
                  route idRoute
                  compile $ do
                      posts <- recentFirst =<< loadAll "posts/*"
                      singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])
                      let pages      = posts <> singlePages
                          sitemapCtx = constField "root" root <> listField "pages" postCtx (return pages)
                      makeItem "" 
                          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


              match "index.html" $ do
                  route idRoute
                  compile $ do
                      posts <- recentFirst =<< loadAll "posts/*"
                      let indexCtx =
                              listField "posts" postCtx (return posts)
                                  `mappend` constField "title" "Home"
                                  `mappend` defaultContext

                      getResourceBody
                          >>= applyAsTemplate indexCtx
                          >>= loadAndApplyTemplate "templates/default.html"
                                                   indexCtx
                          >>= relativizeUrls

              match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = 
    constField "root" root 
    <> dateField "date" "%B %e, %Y" 
    <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

root :: String
root = "https://dnlkrgr.com"
