module Handler.Entry (
    getEntryR
  , postEntryR
  , putEntryR
  , deleteEntryR
  , postPushCommentR
  )
where

import Import
import Handler.Helper
import Yesod.Auth
import Data.Maybe
import Data.Text (pack)
import Data.Tree

data CommentRequest = CommentRequest { text   :: Text }
commentForm :: Form CommentRequest
commentForm = renderDivs  $ CommentRequest <$> areq textField "Text" Nothing

postPushCommentR :: EntryId -> Handler RepHtml
postPushCommentR entryId = do
  maybeUserId <- maybeAuthId
  case maybeUserId of
    Nothing -> errorPage "You're not logged"
    Just userId -> do
      ((res,_),_) <- runFormPost commentForm
      case res of
        FormSuccess commentRequest -> do
          time <- liftIO getCurrentTime
          let newComment = Comment entryId userId Nothing time (text commentRequest)
          commentId <- runDB $ insert newComment
          redirect $ EntryR entryId
        _ -> errorPage "Please correct your entry form"


cssClassVote :: [Entity Vote] -> Text
cssClassVote [] = ""
cssClassVote ((Entity _ vote):_) = case voteValue vote of
                                            1    -> " yeah_voted"
                                            (-1) -> " neah_voted"
                                            _    -> ""

-- getCommentSons :: [a]              -> b              -> (a,[b])
getCommentSons :: [Entity Comment] -> 
                  Entity Comment -> (Entity Comment, [Entity Comment])
getCommentSons comments father@(Entity commentId _) =
  (father, 
   filter (\(Entity _ c) -> commentReplyTo c == Just commentId)  comments)
    

getEntryR :: EntryId -> Handler RepHtmlJson
getEntryR entryId = do
  currentUserId <- maybeAuthId
  maybeEntry <- runDB $ get entryId
  currentTime <- liftIO getCurrentTime
  (widget,enctype) <- generateFormPost commentForm
  (entry,comments,maybeCreator) <- runDB $ do
      entry <- get404 entryId
      -- # TODO sort by score
      comments <- selectList [CommentEntry ==. entryId][LimitTo 100]
      maybeCreator <- get (entryCreator entry)
      return (entry, comments, maybeCreator)
  votes <- runDB $ maybe
                    (return [])
                    (\userId -> selectList [VoteEntry ==. entryId,
                                            VoteCreator ==. userId] [LimitTo 1])
                    currentUserId
  let creator = maybe "Unknown" userIdent maybeCreator
      isEntryOwned = if isNothing currentUserId
                        then False
                        else entryCreator entry == fromJust currentUserId
      rootComments = filter (\(Entity _ c) -> commentReplyTo c == Nothing) comments
      commentTree = unfoldForest (getCommentSons comments) rootComments
  defaultLayoutJson (do
          setTitle $ toHtml $ entryTitle entry
          $(widgetFile "entry"))
          (object ["msg" .= entryTitle entry])


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
           (-1) -> update entry [EntryNeah +=. 1]
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

deleteEntryR :: EntryId -> Handler RepHtmlJson
deleteEntryR entryId = do
  currentUserId <- maybeAuthId
  case currentUserId of
    Nothing -> errorPageJson "You're not logged"
    Just user -> do
      entries <- runDB $ selectList [EntryId ==. entryId,EntryCreator ==. user] [LimitTo 1]
      case entries of
                  [] -> errorPageJson "Either entry doesn't exists or not yours"
                  _  -> do
                          _ <- runDB $ do
                            -- delete the entry
                            delete entryId
                            -- delete all corresponding votes
                            votes <- selectList [VoteEntry ==. entryId][]
                            mapM_ (\(Entity voteId _) -> delete voteId) votes
                            -- delete all corresponding comments
                            comments <- selectList [CommentEntry ==. entryId][]
                            mapM_ (\(Entity commentId _) -> delete commentId) comments
                          errorPageJson "deleted"

putEntryR :: EntryId -> Handler RepHtmlJson
putEntryR = undefined

