The goal of this project is to turn posts from Facebook into Cards Against Humanity questions.
We're going to achieve this by reading the wall posts from an RSS feed of Facebook notifications.


The HTTP library allows us to fetch the RSS feed from the internet.

> import Network.HTTP.Conduit (simpleHttp)
> import qualified Data.ByteString.Lazy.Char8 as L

The feed library allows us to interpret  RSS feeds.

> import Text.Feed.Import (parseFeedString)

You can create an RSS feed for your Facebook notifications from your Notifications page:
1. Go to facebook.com/notifications.
2. At the top of the page, click "RSS" next to "Get notifications via:"
3. Copy the link and paste it below.

> rssFeed = ""


Now we can request, parse and print the feed.

> main = do
>   response <- simpleHttp rssFeed
>   case parseFeedString (L.unpack response) of
>       Just feed -> print feed
>       Nothing   -> putStrLn "Unable to parse feed"
