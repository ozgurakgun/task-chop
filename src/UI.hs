{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module UI ( UI(..), NextCmds(..), ui ) where

-- base
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )

-- text
import Data.Text

-- cmdargs
import System.Console.CmdArgs


ui :: UI
ui = modes
    [ Add
        { uiAddText = def       &= typ "TEXT"
                                &= argPos 0
        , uiAddTextMore = []    &= typ "TEXT"
                                &= args
        }                       &= name "add"
                                &= explicit
                                &= help "This is how you add new tasks."
    , Next                      &= name "next"
                                &= explicit
                                &= help "Wanna do something? Here is something you can do!"
    , List                      &= name "list"
                                &= explicit
                                &= help "List all the tasks."
                                &= auto
    ]                           &= program "chop"
                                &= summary "CHOP: a task management tool which let's you avoid\
                                           \ work by chopping and deferring tasks.\
                                           \ It's great!"

data UI
    = Add { uiAddText :: String, uiAddTextMore :: [String] }
    | Next
    | List
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data NextCmds
    = Done
    | Defer
    | Split [Text]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
