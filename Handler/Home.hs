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
import Data.Maybe (isNothing)

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

-- the name getHomeR is for
-- handle the request GET on the resource HomeR
getHomeR :: Handler RepHtml
getHomeR = do
  -- We get the current user id (return Nothing if not logged in)
  currentUserId <- maybeAuthId

  -- Generate a couple
  -- widget => some HTML containing the form labels and input fields
  -- enctype => the encoding
  (widget,enctype) <- generateFormPost entryForm

  -- We get the list of entries sorted by entry title
  entries <- runDB $ selectList [] [Desc EntryTitle]

  -- We return some HTML (not full)
  defaultLayout $ do
    aDomId <- lift newIdent
    setTitle "Inmanis"
    $(widgetFile "homepage")

-- When we receive a post request on HomeR resource (/ path)
-- We create a new resource
postHomeR :: Handler RepHtml
postHomeR = do
  currentUserId <- maybeAuthId
  case currentUserId of
    Nothing -> errorPage "You're not logged"
    _ -> do -- errorPage "You're logged"
      ((res,_),_) <- runFormPost entryForm
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
  --    if maybeEntry is Nothing returns ""
  --    else returns (entryTitle maybeEntry)
  let titleEntry = maybe "" entryTitle maybeEntry
  errorPageJson titleEntry

postEntryR :: EntryId -> Handler RepHtmlJson
postEntryR = undefined
putEntryR :: EntryId -> Handler RepHtmlJson
putEntryR = undefined
deleteEntryR :: EntryId -> Handler RepHtmlJson
deleteEntryR = undefined
