{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module UI ( UI(..), NextCmd(..), ui ) where

-- base
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )

-- text
import Data.Text ( Text )

-- cmdargs
import System.Console.CmdArgs ( (&=), modes, help, typ, def, argPos, args, name, explicit, auto, program, summary )


ui :: UI
ui = modes
    [ Add
        { uiAddText = def       &= typ "TEXT"
                                &= argPos 0
        , uiAddTextMore = []    &= typ "MORE TEXT"
                                &= args
        }                       &= name "add"
                                &= explicit
                                &= help "This is how you add new tasks."
    , Next                      &= name "next"
                                &= explicit
                                &= help "Wanna do something? Here is something you can do!"
    , List
        { uiListAll = False     &= name "all"
                                &= explicit
                                &= help "List all tasks, i.e. do not filter out those that are done."
        }                       &= name "list"
                                &= explicit
                                &= help "List all the tasks."
                                &= auto
    , Context
        { uiContextTo = []      &= typ "CONTEXT"
                                &= args
        }                       &= name "context"
                                &= explicit
                                &= help "When called without arguments, list all contexts.\n\
                                        \When called with an argument, switch to that context."
    ]                           &= program "chop"
                                &= summary "CHOP: a task management tool which let's you avoid\
                                           \ work by chopping and deferring tasks.\
                                           \ It's great!"

data UI
    = Add { uiAddText :: String, uiAddTextMore :: [String] }
    | Next
    | List { uiListAll :: Bool }
    | Context { uiContextTo :: [String] }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data NextCmd
    = MarkAsDone
    | Defer
    | Split [Text]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
