{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home
  ( getHomeR
  , postHomeR
  )
where

import Import
import Handler.Helper
import Yesod.Auth
import Data.Maybe
import Data.Text (pack)

data EntryRequest = EntryRequest {
                      title :: Text
                    , url   :: Text
                     }
entryForm :: Form EntryRequest
entryForm = renderDivs  $ EntryRequest
  <$> areq textField "Title" Nothing
  <*> areq urlField  "Url"   Nothing

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

cssClassVoteForEntry entryId votes =
  case (filter voteForId votes) of
    [] -> "" :: Text
    (Entity voteId vote):vs -> case voteValue vote of
                                  1    -> " yeah_voted"
                                  (-1) -> " neah_voted"
                                  _    -> ""
  where
    voteForId (Entity voteId vote) = voteEntry vote == entryId


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
  (entries,votes) <- runDB $ do
    entries <- selectList [] [Desc EntryTitle, LimitTo 25]
    allVotesOfCurrentUser   <- case currentUserId of
                  Nothing   -> return []
                  Just user -> selectList [VoteCreator ==. user][]
    return (entries,map fst $ joinTables voteEntry allVotesOfCurrentUser entries)

  -- We return some HTML (not full)
  defaultLayout $ do
    aDomId <- lift newIdent
    setTitle "Inmanis"
    $(widgetFile "homepage")

-- When we receive a post request on HomeR resource (/ path)
-- We create a new resource
postHomeR :: Handler RepHtml
postHomeR = do
  userId <- maybeAuthId
  case userId of
    Nothing -> errorPage "You're not logged"
    Just currentUserId -> do -- errorPage "You're logged"
      ((res,_),_) <- runFormPost entryForm
      case res of
        FormSuccess personRequest -> do
          let newEntry = Entry currentUserId (title personRequest) (url personRequest) 0 0
          entryId <- runDB $ insert newEntry
          setMessage $ toHtml (title personRequest)
          redirect $ EntryR entryId
        _ -> errorPage "Please correct your entry form"


