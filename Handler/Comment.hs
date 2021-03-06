module Handler.Comment (
  getCommentR
, postCommentR
, deleteCommentR
, putCommentR
, getCommentVoteR
, postCommentVoteR
) where

import Import
import Handler.Helper
import Data.Text (pack)

getCommentR :: CommentId -> Handler RepHtml
getCommentR _ = error "Not yet implemented: getCommentR"

isCommentCreator commentId f =
  testLogged $ \userId -> do
    maybeComment <- runDB $ get commentId
    case maybeComment of
      Nothing -> errorPageJson "The comment doesn't exists"
      Just comment ->
            case commentCreator comment of
              Nothing -> errorPageJson "You are not the creator of this comment"
              Just creatorOfComment -> do
                case creatorOfComment == userId of
                    False -> errorPageJson "You are not the creator of this comment"
                    True -> f userId comment

deleteCommentR :: CommentId -> Handler RepHtmlJson
deleteCommentR commentId = isCommentCreator commentId $ \userId comment -> do
  _ <- runDB $ update commentId [CommentCreator =. Nothing,
                                 CommentContent =. deletedTextarea]
  redirect $ EntryR (commentEntry comment)
  where
    deletedTextarea = Textarea { unTextarea = "deleted" }


-- |The comment data needed for creating a comment
data CommentRequest = CommentRequest { textComment   :: Textarea }

-- |The comment form data structure
commentForm :: Form CommentRequest
commentForm = renderDivs  $ CommentRequest <$> areq textareaField "Text" Nothing

postCommentR :: CommentId -> Handler RepHtmlJson
postCommentR commentId = do
  testLogged $ \userId -> do
    maybeComment <- runDB $ get commentId
    case maybeComment of
      Nothing -> errorPageJson "The comment doesn't exists"
      Just comment ->
            case commentCreator comment of
              Nothing -> errorPageJson "You are not the creator of this comment"
              Just creatorOfComment -> do
                case creatorOfComment == userId of
                  False -> errorPageJson "You are not the creator of this comment"
                  True -> do
                    ((res,_),_) <- runFormPost commentForm
                    case res of
                      FormSuccess commentRequest -> do
                        _ <- runDB $ update commentId [CommentContent =. textComment commentRequest]
                        redirect $ EntryR (commentEntry comment)
                      _ -> errorPageJson "Please correct your comment form"

putCommentR :: CommentId -> Handler RepHtml
putCommentR _ = error "Not yet implemented: putCommentR"

getCommentVoteR :: CommentId -> Handler RepHtmlJson
getCommentVoteR _ = error "Not yet implemented: getCommentVoteR"

postCommentVoteR :: CommentId -> Handler RepHtmlJson
postCommentVoteR commentId = do
  testLogged $ \userId -> do
    req <- runRequestBody
    let  yeah = lookup "yeah" (fst req)
         neah = lookup "neah" (fst req)
    case yeah of
      Nothing ->
        case neah of
          Nothing -> errorPageJson $ "Neither Yeah nor Neah!"
          _       -> downvote userId commentId
      _ -> upvote userId commentId

downvote :: UserId -> CommentId -> Handler RepHtmlJson
downvote = setVoteValue (-1)

upvote :: UserId -> CommentId -> Handler RepHtmlJson
upvote = setVoteValue 1

setVoteValue :: Int -> UserId -> CommentId -> Handler RepHtmlJson
setVoteValue value user commentId = do
  votes  <- runDB $ selectList [VoteCommentComment ==. commentId,
                                VoteCommentCreator ==. user]
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
        _ <- insert $ VoteComment user commentId value
        case value of
           1    -> update commentId [CommentYeah +=. 1]
           (-1) -> update commentId [CommentNeah +=. 1]
           _ -> return ()
        update commentId [CommentNbView +=. 1]
      return "inserted"

    -- Update case
    insertOrUpdateVote (vote:_) = do
          runDB $ updateVote oldvalue value (entityKey vote)
          return "updated"
          where
            oldvalue = (voteCommentValue $ entityVal vote)

    updateVote  0 1 key = do
          update key [VoteCommentValue =. 1]
          update commentId [CommentYeah +=. 1]
    updateVote 0 (-1) key = do
          update key [VoteCommentValue =. (-1)]
          update commentId [CommentNeah +=. 1]

    updateVote  1 1 key = do
          update key [VoteCommentValue =. 0]
          update commentId [CommentYeah -=. 1]
    updateVote  1 (-1) key = do
          update key [VoteCommentValue =. (-1)]
          update commentId [CommentYeah -=. 1]
          update commentId [CommentNeah +=. 1]

    updateVote  (-1) 1 key = do
          update key [VoteCommentValue =. 1]
          update commentId [CommentNeah -=. 1]
          update commentId [CommentYeah +=. 1]
    updateVote (-1) (-1) key = do
          update key [VoteCommentValue =. 0]
          update commentId [CommentNeah -=. 1]
    updateVote _ _ _ = return ()
