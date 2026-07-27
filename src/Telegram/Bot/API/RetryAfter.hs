{-# LANGUAGE OverloadedStrings #-}

{- | Recognising Telegram's flood-control answer on a failed request.

 Telegram answers a rate-limited request with HTTP @429@ and says how long to
 wait. Nothing here acts on that; this module only turns a 'ClientError' into
 that number, so the bot loops in "Telegram.Bot.Simple.BotApp.Internal" can
 decide what to do with it.

 Without it, a @429@ is just another 'ClientError': it escapes whichever loop
 raised it, kills the supervised thread, and @immortal-worker@ restarts that
 thread after its own fixed one-second delay — so we return well before
 Telegram asked us to and are refused again, which is how a five-second
 rate limit turned into twenty-three restarts in prod on 2026-07-26.
-}
module Telegram.Bot.API.RetryAfter (
    retryAfterFromClientError,
    retryAfterCapSeconds,
    retryAfterFallbackSeconds,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, join)
import Data.Aeson (Value, decode, withObject, (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Header (Header, hRetryAfter)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (ClientError (..), responseBody, responseHeaders, responseStatusCode)
import Telegram.Bot.API.Types (Seconds (..))

{- | How long Telegram wants us to wait before repeating a request, or
 'Nothing' if this error is not flood control — in which case the caller must
 rethrow it unchanged.

 Deliberately narrow: only HTTP @429@ is recognised. A decode failure, a 5xx,
 a dropped connection all keep the behaviour they have always had, which is to
 escape the loop and let the supervisor restart it. This is a rate-limit
 handler, not a general retry policy, and it should not grow into one.

 The number comes from Telegram's own answer, preferring the documented JSON
 field @parameters.retry_after@ over the @Retry-After@ response header. That
 order is not arbitrary: the header is added by Telegram's nginx, is not part
 of the Bot API, and we have observed it exactly once (prod 2026-07-26, where
 the two agreed at 5). It is corroboration, not the source of truth. A @429@
 carrying neither falls back to 'retryAfterFallbackSeconds'.

 The result is always clamped to @[0, 'retryAfterCapSeconds']@.
-}
retryAfterFromClientError :: ClientError -> Maybe Seconds
retryAfterFromClientError (FailureResponse _ response)
    | statusCode (responseStatusCode response) == 429 =
        Just . clampToCap . fromMaybe retryAfterFallbackSeconds $
            retryAfterFromBody (responseBody response)
                <|> retryAfterFromHeader (responseHeaders response)
retryAfterFromClientError _ = Nothing

{- | Upper bound on a single wait.

 Capping cannot lose a rate limit. Every attempt re-reads a fresh
 @retry_after@ from Telegram's next answer, so a cap that is too low costs one
 more refused request and nothing else. What it buys is that a bot is never
 asleep and unreachable for an unbounded stretch on a number we did not
 choose. 60 matches the cap the consuming service already uses for the same
 purpose against a different rate-limited portal, so there is one number to
 reason about rather than two.
-}
retryAfterCapSeconds :: Seconds
retryAfterCapSeconds = 60

{- | Wait used when a @429@ carries neither @retry_after@ nor @Retry-After@.

 5 seconds, because that is the only figure Telegram has ever actually asked
 us for. This is a guess about a response we have never seen, so it is
 deliberately the smallest of the three sources rather than the most cautious:
 if Telegram cares how long we wait it says so, and then this value is unused.
-}
retryAfterFallbackSeconds :: Seconds
retryAfterFallbackSeconds = 5

-- | @{"ok":false,"error_code":429,"parameters":{"retry_after":5}}@.
--
-- This cannot go through 'Telegram.Bot.API.MakingRequests.Response'. That
-- type's @responseResult@ field is not 'Maybe' and an error body carries no
-- @result@ key, so its generic parser fails for every choice of payload type
-- — hence a hand-written parser for the single field we need.
retryAfterFromBody :: LBS.ByteString -> Maybe Seconds
retryAfterFromBody body = join (parseMaybe parameters =<< decode body)
  where
    parameters :: Value -> Parser (Maybe Seconds)
    parameters = withObject "Telegram error response" $ \o ->
        o .:? "parameters" >>= maybe (pure Nothing) (.:? "retry_after")

-- | @Retry-After: 5@. Only the delta-seconds form is accepted; the HTTP-date
-- form is rejected rather than misparsed, and falls through to
-- 'retryAfterFallbackSeconds' like any other unusable header.
--
-- Takes any 'Foldable' rather than the @Seq@ it is actually passed, purely so
-- this module needs no dependency on @containers@ to name that one type.
retryAfterFromHeader :: Foldable f => f Header -> Maybe Seconds
retryAfterFromHeader headers = do
    (_, value) <- find ((== hRetryAfter) . fst) (toList headers)
    (seconds, rest) <- BS8.readInt (BS8.dropWhile (== ' ') value)
    guard (BS8.null rest)
    pure (Seconds (fromIntegral seconds))

-- | Clamped low as well as high: 'Seconds' wraps a signed 'Data.Int.Int64',
-- and the delay this feeds is built from a natural, so a negative
-- @retry_after@ from a malformed answer would underflow rather than be
-- ignored.
clampToCap :: Seconds -> Seconds
clampToCap (Seconds n) = Seconds (max 0 (min cap n))
  where
    Seconds cap = retryAfterCapSeconds
