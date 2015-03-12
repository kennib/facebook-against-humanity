> {-# LANGUAGE OverloadedStrings #-}

The goal of this project is to turn posts from Facebook into Cards Against Humanity questions.
We're going to achieve this by reading the wall posts from an RSS feed of Facebook notifications.


The HTTP library allows us to fetch the RSS feed from the internet.

> import Network.HTTP.Conduit (Request(..), parseUrl, withManager, httpLbs, responseBody)
> import Network.HTTP.Types.Header (hUserAgent)
> import Network.HTTP.Headers (mkHeader)
> import qualified Data.ByteString.Lazy.Char8 as L
> import Control.Monad.IO.Class (liftIO)

The feed library allows us to interpret RSS feeds.

> import Text.Feed.Import (parseFeedString)
> import Text.Feed.Query
> import Text.Feed.Types (Item)

You can create an RSS feed for your Facebook notifications from your Notifications page:
1. Go to facebook.com/notifications.
2. At the top of the page, click "RSS" next to "Get notifications via:"
3. Copy the link and paste it below.

> rssFeed = ""


Unfortunately the Facebook RSS page only works for browser user agents.
So we have to user a user agent string like the following one.

> userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/40.0.2214.111 Safari/537.36"
> userAgentHeader = (hUserAgent, userAgent)


Now we can request, parse and print the first wall post in the feed.

> main = do
>   case parseUrl rssFeed of
>       Nothing      -> putStrLn "Feed URL is invalid"
>       Just request -> withManager $ \manager -> do
>           let browserRequest = request { requestHeaders = [userAgentHeader] }
>           response <- httpLbs browserRequest manager
>           liftIO $ case parseFeedString (L.unpack (responseBody response)) of
>               Nothing   -> putStrLn "Unable to parse feed"
>               Just feed -> do
>                   print feed
