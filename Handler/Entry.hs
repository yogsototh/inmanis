module Handler.Entry (
    getEntryR
  , postEntryR
  , putEntryR
  , deleteEntryR
  , postCommentsR
  , postReplyCommentR
  )
where

import Import
import Handler.Helper
import Data.Maybe
import Data.Text (pack)
import Data.Tree
import Yesod.Markdown
import Yesod.Auth
import Database.Persist.GenericSql.Raw

-- |The comment data needed for creating a comment
data CommentRequest = CommentRequest { text   :: Textarea }

-- |The comment form data structure
commentForm :: Form CommentRequest
commentForm = renderDivs  $ CommentRequest <$> areq textareaField "Text" Nothing

-- |add a comment
postCommentsR :: EntryId -> Handler RepHtmlJson
postCommentsR entryId =
  testLogged $ \userId -> do
      ((res,_),_) <- runFormPost commentForm
      case res of
        FormSuccess commentRequest -> do
          time <- liftIO getCurrentTime
          let newComment = Comment entryId userId Nothing time (text commentRequest) 0 0
          _ <- runDB $ insert newComment
          redirect $ EntryR entryId
        _ -> errorPageJson "Please correct your entry form"

cssClassVote :: [Entity Vote] -> Text
cssClassVote [] = ""
cssClassVote ((Entity _ vote):_) =
  case voteValue vote of
    1    -> " yeah_voted"
    (-1) -> " neah_voted"
    _    -> ""

-- getCommentSons :: [a]              -> b              -> (a,[b])
getCommentSons :: [Entity Comment] ->
                  Entity Comment -> (Entity Comment, [Entity Comment])
getCommentSons comments father@(Entity commentId _) =
  (father,
   filter (\(Entity _ c) -> commentReplyTo c == Just commentId)  comments)

-- showCommentForest :: [Tree (Entity Comment)] -> Hamlet
showCommentForest [] _ _ _ _ = [whamlet|$newline never
|]
showCommentForest trees creators wdg enc voteComments=
  [whamlet|$newline always
    <ul>
      $forall tree <- trees
         ^{showCommentTree tree creators wdg enc voteComments}|]

cssClassVoteForVote :: (Eq a) => a
                        -> [(a, [Entity (VoteCommentGeneric backend)])]
                        -> Text
cssClassVoteForVote commentId voteComments =
  maybe "" strOfVote (lookup commentId voteComments)
    where
      strOfVote [] = "" :: Text
      strOfVote ((Entity _ vote):_) =
        case voteCommentValue vote of
             1    -> " yeah_voted"
             (-1) -> " neah_voted"
             _    -> ""

-- showCommentTree :: Tree (Entity Comment) -> Metadatas -> Hamlet
showCommentTree tree creators widget enctype voteComments=
  [whamlet|$newline always
    <li url=@{ReplyCommentR entryId commentId}>
      <div .meta>
        <div class="vote#{cssClassVoteForVote commentId voteComments}" url=@{CommentVoteR commentId}>
          <div .yeah>#{commentYeah comment}
          <div .neah>#{commentNeah comment}
        <span .creator>by #{creatorOfEntity commentId creators}
      <div .content>
        #{commentContent comment}
      <div .actions>
        <span .edit>edit
        \ - #
        <span .delete>delete
        \ - #
        <span .reply flipshow="##{showId commentId}">reply
      <div .replyForm .hide ##{showId commentId}>
        <form method=post action=@{ReplyCommentR entryId commentId} enctype=#{enctype}>
          ^{widget}
          <input type=submit value="Post">
      ^{showCommentForest (subForest tree) creators widget enctype voteComments}|]
   where
     comment = commentFromEntity (rootLabel tree)
     commentFromEntity (Entity _ c) = c
     commentId = commentIdFromEntity (rootLabel tree)
     commentIdFromEntity (Entity i _) = i
     entryId   = commentEntry comment

creatorOfEntity :: CommentId -> [(CommentId,[Entity User])] -> Text
creatorOfEntity entityId creators =
  maybe "Anonymous Coward" entUserIdent (lookup entityId creators)
    where
      entUserIdent [] = "No name"
      entUserIdent ((Entity _ creator):_) = userIdent creator

getEntryR :: EntryId -> Handler RepHtmlJson
getEntryR entryId = do
  currentUserId <- maybeAuthId
  maybeEntry <- runDB $ get entryId
  currentTime <- liftIO getCurrentTime
  (widget,enctype) <- generateFormPost commentForm
  (entry,comments,maybeCreator,creators) <- runDB $ do
      entry <- get404 entryId
      -- # TODO sort by score
      comments <- selectList [CommentEntry ==. entryId][LimitTo 100]
      maybeCreator <- get (entryCreator entry)
      creators <- do
            let getUserCreatorIdent (Entity commentId comment) = do
                  creators <- selectList [UserId ==. commentCreator comment][LimitTo 1]
                  return (commentId, creators)
            mapM getUserCreatorIdent comments
      return (entry, comments, maybeCreator,creators)
  votes <- runDB $ maybe
                    (return [])
                    (\userId -> selectList [VoteEntry ==. entryId,
                                            VoteCreator ==. userId] [LimitTo 1])
                    currentUserId
  voteComments <- runDB $
    case currentUserId of
      Nothing -> return []
      Just user -> do
              let getVoteForUserVote (Entity commentId _) = do
                      voteComments <- selectList
                                        [VoteCommentCreator ==. user,
                                         VoteCommentComment ==. commentId]
                                        [LimitTo 1]
                      return (commentId,voteComments)
              mapM getVoteForUserVote comments

  let creator = maybe "Unknown" userIdent maybeCreator
      isEntryOwned = if isNothing currentUserId
                        then False
                        else entryCreator entry == fromJust currentUserId
      rootComments = filter (\(Entity _ c) -> commentReplyTo c == Nothing) comments
      commentForest = unfoldForest (getCommentSons comments) rootComments
  defaultLayoutJson (do
          setTitle $ toHtml $ entryTitle entry
          $(widgetFile "entry")
          )
          (object ["msg" .= entryTitle entry])


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

postReplyCommentR :: EntryId -> CommentId -> Handler RepHtmlJson
postReplyCommentR entryId commentId =
  testLogged $ \userId -> do
    ((res,_),_) <- runFormPost commentForm
    case res of
      FormSuccess commentRequest -> do
        time <- liftIO getCurrentTime
        let newComment = Comment entryId userId (Just commentId) time (text commentRequest) 0 0
        _ <- runDB $ insert newComment
        redirect $ EntryR entryId
      _ -> errorPageJson "Please correct your entry form"
