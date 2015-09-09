{-# LANGUAGE FlexibleContexts #-}

module CalcNext where

import Types
import UI

-- base
import Data.Ord ( comparing )
import Data.List ( sortBy )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( Any(..) )

-- mtl
import Control.Monad.Writer ( Writer, runWriter, tell )

-- time
import Data.Time ( UTCTime )


-- | What to do next?
calcNext :: Tasks -> Maybe Task
calcNext (Tasks tasks) =
    let
        flatten t | status t == Done = []
        flatten t = concatMap flatten (children t)
                 ++ [ t | allChildrenDone t ]           -- only add this if all it's children are done
        flattened =  concatMap flatten tasks
                  |> sortBy (comparing lastAction)

    in
        listToMaybe flattened

allChildrenDone :: Task -> Bool
allChildrenDone t = all ((== Done) . status) (children t)

lastAction :: Task -> UTCTime
lastAction t = fst (head (actionTimes t))

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

applyNextCmd :: UTCTime -> NextCmd -> Task -> Tasks -> Maybe Tasks
applyNextCmd now cmd t (Tasks tasks) =
    case runWriter (mapM apply tasks) of
        (res, Any True ) -> Just (Tasks res)
        (_  , Any False) -> Nothing

    where
        apply :: Task -> Writer Any Task
        apply t2 | sameTask t t2 = do
            tell (Any True)
            case cmd of
                MarkAsDone ->
                    return t2 { actionTimes = (now, MarkedAsDone) : actionTimes t2
                              , status = Done
                              }
                Defer      ->
                    return t2 { actionTimes = (now, Deferred    ) : actionTimes t2 }
                Split chs  -> do
                    let newChildren = [ Task ch [(now, Created)] [] NotDoneYet | ch <- chs ]
                    return t2 { actionTimes = (now, Splitted    ) : actionTimes t2
                              , children    = newChildren ++        children t2
                              }
        apply t2 = do
            children' <- mapM apply (children t2)
            return t2 { children = children' }
