module Handler.Comment where

import Import

getCommentR :: CommentId -> Handler RepHtml
getCommentR _ = error "Not yet implemented: getCommentR"

postCommentR :: CommentId -> Handler RepHtml
postCommentR _ = error "Not yet implemented: postCommentR"

putCommentR :: CommentId -> Handler RepHtml
putCommentR _ = error "Not yet implemented: putCommentR"

deleteCommentR :: CommentId -> Handler RepHtml
deleteCommentR _ = error "Not yet implemented: deleteCommentR"

postReplyCommentR :: CommentId -> Handler RepHtml
postReplyCommentR _ = error "Not yet implemented: postReplyCommentR"
