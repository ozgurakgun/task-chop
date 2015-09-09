{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Tasks(..)
    , Task(..)
    , TaskAction(..)
    , TaskStatus(..)
    , sameTask
    ) where

import Pretty

-- base
import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )

-- time
import Data.Time ( UTCTime )

-- aeson
import Data.Aeson as X ( ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON )
import qualified Data.Aeson.Types as JSON

-- text
import Data.Text ( Text )


data Tasks = Tasks [Task]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Task = Task { descr        :: Text
                 , actionTimes  :: [(UTCTime, TaskAction)]      -- backwards, the earliest is the latest
                 , children     :: [Task]
                 , status       :: TaskStatus
                 }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Check if two tasks are the same.
--   Works by comparing descriptions and creation times.
sameTask :: Task -> Task -> Bool
sameTask t1 t2 = (descr t1, last (actionTimes t1)) == (descr t2, last (actionTimes t2))

data TaskAction = Created | Deferred | Splitted | MarkedAsDone
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

data TaskStatus = Done | NotDoneYet
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToJSON    Tasks      where toJSON    = genericToJSON    jsonOptions
instance FromJSON  Tasks      where parseJSON = genericParseJSON jsonOptions
instance ToJSON    Task       where toJSON    = genericToJSON    jsonOptions
instance FromJSON  Task       where parseJSON = genericParseJSON jsonOptions
instance ToJSON    TaskAction where toJSON    = genericToJSON    jsonOptions
instance FromJSON  TaskAction where parseJSON = genericParseJSON jsonOptions
instance ToJSON    TaskStatus where toJSON    = genericToJSON    jsonOptions
instance FromJSON  TaskStatus where parseJSON = genericParseJSON jsonOptions

jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions
    { JSON.allNullaryToStringTag = True
    , JSON.omitNothingFields = True
    , JSON.sumEncoding = JSON.ObjectWithSingleField
    }


-- Pretty instances

instance Pretty Tasks where
    pretty (Tasks ts) = vcat (map pretty ts)

instance Pretty Task where
    pretty Task{..} = vcat $  [ pretty descr
                              , padded 12 "Status"      <> ":" <+> pretty status
                              ]
                           ++ [ padded 12 (show action) <> ":" <+> pretty time
                              | (time, action) <- actionTimes
                              ]
                           ++ map (nest 4 . pretty) children
                           ++ [ "" ]

instance Pretty TaskAction where pretty = pretty . show

instance Pretty TaskStatus where pretty = pretty . show
