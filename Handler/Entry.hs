module Handler.Entry (
    getEntryR
  , postEntryR
  , putEntryR
  , deleteEntryR
  )
where

import Import
import Handler.Helper
import Yesod.Auth
import Data.Maybe
import Data.Text (pack)

cssClassVote :: [Entity Vote] -> Text
cssClassVote [] = ""
cssClassVote ((Entity voteId vote):vs) = case voteValue vote of
                                            1    -> " yeah_voted"
                                            (-1) -> " neah_voted"
                                            _    -> ""

getEntryR :: EntryId -> Handler RepHtmlJson
getEntryR entryId = do
  currentUserId <- maybeAuthId
  maybeEntry <- runDB $ get entryId
    -- maybe "" entryTitle maybeEntry
    --    if maybeEntry is Nothing returns ""
    --    else returns (entryTitle maybeEntry)
    -- errorPageJson titleEntry
  let
      titleEntry = maybe "This entry does not exist" entryTitle maybeEntry
  case maybeEntry of
    Just entry -> do
        case currentUserId of
          Nothing -> do
            votes <- return []
            defaultLayoutJson
                        (htmlWidget titleEntry entry currentUserId votes)
                        (jsonWidget titleEntry entry currentUserId votes)
          Just userId -> do
            votes <- runDB $ selectList
                                [VoteCreator ==. userId,
                                 VoteEntry ==. entryId]
                                [LimitTo 1]
            defaultLayoutJson
                        (htmlWidget titleEntry entry currentUserId votes)
                        (jsonWidget titleEntry entry currentUserId votes)
    Nothing    -> errorPageJson titleEntry
  where
      jsonWidget titleStr entry currentUserId votes = object ["msg" .= titleStr]
      htmlWidget titleStr entry currentUserId votes = do
        setTitle $ toHtml titleStr
        $(widgetFile "entry")


testLogged ::  (UserId -> Handler RepHtmlJson) -> Handler RepHtmlJson
testLogged v = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Nothing -> errorPageJson "You're not logged"
    Just currentUserId -> (v currentUserId)

postEntryR :: EntryId -> Handler RepHtmlJson
postEntryR entry = do
  testLogged $ \userId -> do
    req <- runRequestBody
    let  yeah = lookup "yeah" (fst req)
         neah = lookup "neah" (fst req)
    case yeah of
      Nothing ->
        case neah of
          Nothing -> errorPageJson $ "Neither Yeah nor Neah!"
          _       -> downvote userId entry
      _ -> upvote userId entry

downvote :: UserId -> EntryId -> Handler RepHtmlJson
downvote = setVoteValue (-1)

upvote :: UserId -> EntryId -> Handler RepHtmlJson
upvote = setVoteValue 1

setVoteValue :: Int -> UserId -> EntryId -> Handler RepHtmlJson
setVoteValue value user entry = do
  votes  <- runDB $ selectList [VoteEntry ==. entry,VoteCreator ==. user]
                                [LimitTo 1]
  msg <- insertOrUpdateVote votes
  errorPageJson $ pack msg
  where

    -- Depending of votes insert a new vote
    -- update the existing one.
    -- Also must synchronize the total number of yeah/neah for the entry
    --
    -- Insert case
    insertOrUpdateVote [] = do
      _ <- runDB $ do
        _ <- insert $ Vote user entry value
        case value of
           1    -> update entry [EntryYeah +=. 1]
           (-1) -> update entry [EntryYeah +=. 1]
           _ -> return ()
      return "inserted"

    -- Update case
    insertOrUpdateVote (vote:_) = do
          runDB $ updateVote oldvalue value (entityKey vote)
          return "updated"
          where
            oldvalue = (voteValue $ entityVal vote)

    updateVote  0 1 key = do
          update key [VoteValue =. 1]
          update entry [EntryYeah +=. 1]
    updateVote 0 (-1) key = do
          update key [VoteValue =. (-1)]
          update entry [EntryNeah +=. 1]

    updateVote  1 1 key = do
          update key [VoteValue =. 0]
          update entry [EntryYeah -=. 1]
    updateVote  1 (-1) key = do
          update key [VoteValue =. (-1)]
          update entry [EntryYeah -=. 1]
          update entry [EntryNeah +=. 1]

    updateVote  (-1) 1 key = do
          update key [VoteValue =. 1]
          update entry [EntryNeah -=. 1]
          update entry [EntryYeah +=. 1]
    updateVote (-1) (-1) key = do
          update key [VoteValue =. 0]
          update entry [EntryNeah -=. 1]
    updateVote _ _ _ = return ()




putEntryR :: EntryId -> Handler RepHtmlJson
putEntryR = undefined
deleteEntryR :: EntryId -> Handler RepHtmlJson
deleteEntryR = undefined
