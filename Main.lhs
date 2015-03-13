> {-# LANGUAGE OverloadedStrings #-}

The goal of this project is to turn posts from Facebook into Cards Against Humanity questions.
We're going to achieve this by reading the wall posts from the Facebook graph API.
Then once we have the wall posts, displaying them as web pages.


The Scotty library will allow us to serve our cards as web pages.

> import Web.Scotty (scotty, get, html, text, raw)
> import Data.String (fromString)

The Lucid library will help us create HTML for displaying our cards.

> import qualified Lucid as HTML

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

> fetchLatestPost :: IO (Either String Post)
> fetchLatestPost = do
>   case parseUrl feedURL of
>       Nothing      -> return $ Left "Feed URL is invalid"
>       Just request -> withManager $ \manager -> do
>           let authRequest = auth request
>           response <- httpLbs authRequest manager
>           return $ case eitherDecode (responseBody response) of
>               Left  error  -> Left error
>               Right result -> Right $ head (wallPosts (resultdata result))


To find only the wall posts we simply check the status type

> wallPosts = filter (\post -> status_type post == "wall_post")


We don't just want to display the latest post.
We want to display it in the style of a Cards Against Humanity card.

> card :: Post -> HTML.Html ()
> card post = HTML.html_ $ do
>   HTML.head_ $ do
>       HTML.link_ [HTML.type_ "text/css", HTML.rel_ "stylesheet", HTML.href_ "facebook-against-humanity.css"]
>   HTML.body_ $ do
>       HTML.section_ [HTML.class_ "question card"] $ do
>          HTML.h1_ $ HTML.toHtml (message post)
>          HTML.p_ [HTML.class_ "author"] $ HTML.toHtml (name . from $ post)
>       HTML.section_ [HTML.class_ "answer card"] $ do
>          HTML.h1_ $ HTML.toHtml ("Bees?" :: String)


Now that we can get the latest post nad make it look like a card let's serve it as a web page.

> cardHandler = do
>   latestPost <- liftIO $ fetchLatestPost
>   case latestPost of
>       Left  error -> html' $ error
>       Right post  -> raw $ HTML.renderBS $ card post

The html' function is useful for turning Strings (as opposed to Text) into HTML.

> html' = html . fromString


We can now create a server which serves our web page!

> main = scotty 8008 $ do
>   get "/" cardHandler
>   get "/facebook-against-humanity.css" $ do
>       css <- liftIO $ readFile "facebook-against-humanity.css"
>       text $ fromString $ css
