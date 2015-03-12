> {-# LANGUAGE OverloadedStrings #-}

The goal of this project is to turn posts from Facebook into Cards Against Humanity questions.
We're going to achieve this by reading the wall posts from the Facebook graph API.


The HTTP library allows us to fetch the RSS feed from the internet.

> import Network.HTTP.Conduit (Request(..), parseUrl, withManager, setQueryString, httpLbs, responseBody)
> import Control.Monad.IO.Class (liftIO)

The Aeson library allows us to parse the graph API.

> import Data.Aeson (eitherDecode)

The Aeson library needs to know how to parse the graph API results.
The Facebook JSON modules has these types defined.

> import Facebook.Types (Result(..), Post(..), Person(..))
> import Facebook.JSON

You can get an access token for Facebook's graph API by going to https://developers.facebook.com/tools/explorer and clicking the "Get Access Token" button.
Make sure to enable access to your posts.

> feedURL = "https://graph.facebook.com/v2.2/me/feed"
> accessToken = "CAACEdEose0cBAJ1x1VJ8BZA6kCIeLrh9sHrDZAzVCuJyFJ4EMaN3eampeHnDVp9XcN69ktFR8lbHi34xfzFWc9w0Sn8nZAneQv0Bl9P7ci30kyHLWZBTA29BbnVivxTLZAKwBigxGyis8ulRExqeqVLAhfe3glFWhZBjpgNHJG8QwDUt9SvmXeuJ4rGHTsb2qNGo6NwhEMCoPF6T0Vo1Ie0oU2jrO94tMZD"

We can authenticate graph API requests by added the access token to the query string

> auth = setQueryString [("access_token", Just accessToken)]


Now we can request, parse and print the feed.

> main = do
>   case parseUrl feedURL of
>       Nothing      -> putStrLn "Feed URL is invalid"
>       Just request -> withManager $ \manager -> do
>           let authRequest = auth request
>           response <- httpLbs authRequest manager
>           liftIO $ case eitherDecode (responseBody response) of
>               Left  error  -> putStrLn error
>               Right result -> do
>                   let latestPost = head (wallPosts (resultdata result))
>                   print latestPost


To find only the wall posts we simply check the status type

> wallPosts = filter (\post -> status_type post == "wall_post")
