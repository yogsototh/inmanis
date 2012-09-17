{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home
  ( getHomeR
  , postEntriesR
  )
where

import Import
import Handler.Helper
import Yesod.Auth
import Data.Maybe

-- |The `EntryRequest` correspond to the data needed
-- to create a new entry.
data EntryRequest = EntryRequest {
                      title :: Text
                    , url   :: Maybe Text
                    , text  :: Maybe Textarea
                    }

-- |the form associated to the EntryRequest data type
entryForm :: Form EntryRequest
entryForm = renderDivs  $ EntryRequest
  <$> areq textField "Title" Nothing
  <*> aopt urlField  "Url"   Nothing
  <*> aopt textareaField  "Text"   Nothing

-- |This function write a CSS class if the object were voted
cssClassVoteForEntry :: (Eq a) => a
                        -> [(a, [Entity (VoteGeneric backend)])]
                        -> Text
cssClassVoteForEntry entryId votes =
  maybe "" strOfVote (lookup entryId votes)
    where
      strOfVote [] = "" :: Text
      strOfVote ((Entity _ vote):_) =
        case voteValue vote of
             1    -> " yeah_voted"
             (-1) -> " neah_voted"
             _    -> ""

-- |Return the creator of an Entry
creatorOfEntry  :: (Eq a) => a               -- ^ The object id
    -> [(a, [Entity (UserGeneric backend)])] -- ^ the lookup table (id, user)
    -> Text -- ^ the creator name
creatorOfEntry entryId creators =
  maybe "" strOfVote (lookup entryId creators)
    where
      strOfVote [] = "" :: Text
      strOfVote ((Entity _ creator):_) = userIdent creator

-- |return True if the creator is the creator of the Entry
currentCreator :: EntryGeneric backend                 -- ^ The entry
                  -> Key backend (UserGeneric backend) -- ^ the user id
                  -> Bool
currentCreator entry userId = entryCreator entry == userId

-- |the name `getHomeR` is for
-- |handle the request `GET` on the resource HomeR
getHomeR :: Handler RepHtml
getHomeR = do
  -- We get the current user id (return Nothing if not logged in)
  currentUserId <- maybeAuthId
  currentUser <- maybe (return Nothing) (\u -> runDB $ get u) currentUserId
  -- We get the current time
  currentTime <- liftIO getCurrentTime
  -- If we login, we get back here
  setUltDestCurrent

  -- Generate a couple
  -- widget => some HTML containing the form labels and input fields
  -- enctype => the encoding
  (widget,enctype) <- generateFormPost entryForm

  -- We get the list of entries sorted by entry title
  -- the votes for the current user for all entries
  -- the creators of all entries
  (entries,votes,creators) <- runDB $ do
    entries <- selectList [] [Desc EntryCreated, LimitTo 25]
    votes <- case currentUserId of
                 Just user -> do
                   let getVoteForUserEntry (Entity entryId _) = do
                            votes <- selectList [VoteCreator ==. user,
                                                 VoteEntry ==. entryId]
                                                [LimitTo 1]
                            return (entryId,votes)
                   mapM getVoteForUserEntry entries
                 Nothing -> return []
    creators <- do
        let getCreatorOfEntry (Entity entryId entry) = do
              creators <- selectList [UserId ==. entryCreator entry] [LimitTo 1]
              return (entryId,creators)
        mapM getCreatorOfEntry entries
    return (entries, votes, creators)

  -- We return some HTML (not full)
  defaultLayout $ do
    aDomId <- lift newIdent
    setTitle "Inmanis"
    $(widgetFile "homepage")


-- |When we receive a post request on HomeR resource (/ path)
-- |We create a new resource
postEntriesR :: Handler RepHtmlJson
postEntriesR =
  testLogged $ \currentUserId -> do
      ((res,_),_) <- runFormPost entryForm
      case res of
        FormSuccess personRequest -> do
              time <- liftIO getCurrentTime
              let newEntry = Entry currentUserId
                                  (title personRequest)
                                  (url personRequest)
                                  (text personRequest)
                                  1 0 1 time
                  badEntry = isNothing (url personRequest)
                             && isNothing (text personRequest)
              case badEntry of
                True -> errorPageJson "You must enter some text or some URL"
                False -> do
                  entryId <- runDB $ insert newEntry
                  voteId <- runDB $ insert $ Vote currentUserId entryId 1
                  setMessage $ toHtml (title personRequest)
                  redirect $ EntryR entryId
        _ -> errorPageJson "Please correct your entry form"


