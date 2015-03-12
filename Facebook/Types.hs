module Facebook.Types where

import Data.Text

type URL = Text

data Result = Result
    { resultdata   :: [Post]
    , resultpaging :: Paging
    } deriving (Show, Eq)

data Paging = Paging
    { previous :: URL
    , next     :: URL
    } deriving (Show, Eq)

data Post = Post
    { status_type :: Text
    , message     :: Text
    , from        :: Person
    } deriving (Show, Eq)

data Person = Person
    { id   :: Text
    , name :: Text
    } deriving (Show, Eq)
