{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home 
  ( getHomeR
  , postHomeR
  , getEntryR
  , postEntryR
  , putEntryR
  , deleteEntryR
  )
where

import Import
import Handler.Helper
import Yesod.Auth

-- entryAForm :: AForm App App Entry
-- entryAForm = Entry
--   <$> areq textField "Creator" Nothing
--   <*> areq textField "Title" Nothing
--   <*> areq urlField "Url" Nothing
--   <*> areq intField "Yeah" 0
--   <*> areq intField "Neah" 0

data PersonRequest = PersonRequest {
                      title :: Text
                    , url   :: Text
                     }
entryForm :: Form PersonRequest
entryForm = renderDivs  $ PersonRequest
  <$> areq textField "Title" Nothing
  <*> areq urlField  "Url"   Nothing

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
  currentUserId <- maybeAuthId
  (widget,enctype) <- generateFormPost entryForm
  entries <- runDB $ selectList [] [Desc EntryTitle]
  defaultLayout $ do
    aDomId <- lift newIdent
    setTitle "Inmanis"
    $(widgetFile "homepage")
  where
    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False


postHomeR :: Handler RepHtml
postHomeR = do
  currentUserId <- maybeAuthId
  case currentUserId of
    Nothing -> errorPage "You're not logged"
    _ -> do -- errorPage "You're logged"
      ((res,entryWidget),enctype) <- runFormPost entryForm
      case res of
        FormSuccess personRequest -> do
          let newEntry = Entry currentUserId (title personRequest) (url personRequest) 0 0
          entryId <- runDB $ insert newEntry
          setMessage $ toHtml (title personRequest)
          redirect $ EntryR entryId
        _ -> errorPage "Please correct your entry form"

getEntryR :: EntryId -> Handler RepHtmlJson
getEntryR entryId = do
  currentUserId <- maybeAuthId
  maybeEntry <- runDB $ get entryId
  -- maybe "" entryTitle maybeEntry
  -- if maybeEntry is Nothing returns ""
  -- else returns (entryTitle maybeEntry)
  let title = maybe "" entryTitle maybeEntry
  errorPageJson title

postEntryR :: EntryId -> Handler RepHtmlJson
postEntryR = undefined
putEntryR :: EntryId -> Handler RepHtmlJson
putEntryR = undefined
deleteEntryR :: EntryId -> Handler RepHtmlJson
deleteEntryR = undefined
