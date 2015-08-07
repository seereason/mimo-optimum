-- | This module is the authoritative source of all information
-- used to generate the Optimum web site.
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             RecordWildCards, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Types where

import Data.Data (Data, Typeable)
import Data.Default (Default(def))
import Data.Set
import Data.Text (Text)
import Happstack.Authenticate.Core (UserId(..))
import Happstack.Foundation ({-(<$>), Data, Typeable,-} PathInfo)
import Language.Haskell.TH (nameBase)
import Language.Haskell.TH.Path.Graph (SinkType)
import Language.Haskell.TH.TypeGraph.Shape (fName)
import Language.Haskell.TH.TypeGraph.Stack (StackElement(StackElement))
import MIMO.Hint (Hint(HideColumn, Div, Area))

data Trainer =
    Trainer
    { trainerId :: UserId
    , trainerName :: Text
    , trainerActive :: Bool
    , trainerClients :: Set UserId
    } deriving (Eq, Ord, Show, Data, Typeable)

data Client =
    Client
    { clientId :: UserId
    , clientName :: Text
    , clientActive :: Bool
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype ExerciseId = ExerciseId {unExerciseId :: Integer} deriving (Enum, Eq, Ord, Show, Data, Typeable, PathInfo)
instance SinkType ExerciseId

data Exercise =
    Exercise
    { exerciseId :: ExerciseId
    , exerciseAuthor :: UserId
    , exerciseTitle :: Text
    , exerciseText :: Text
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype ProgramId = ProgramId {unProgramId :: Integer} deriving (Enum, Eq, Ord, Show, Data, Typeable, PathInfo)
instance SinkType ProgramId

data Program =
    Program
    { programId :: ProgramId
    , programTitle :: Text
    , programNotes :: [(Text, Text)]
    , programCircuits :: [Circuit]
    , programAuthor :: UserId
    , programClients :: Set UserId
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype CircuitId = CircuitId {unCircuitId :: Integer} deriving (Enum, Eq, Ord, Show, Data, Typeable, PathInfo)
instance SinkType CircuitId

data Circuit =
    Circuit
    { circuitId :: CircuitId
    , circuitExercise :: Maybe ExerciseId
    , circuitName :: Maybe Char
    , circuitOrder :: Maybe Integer
    , circuitRest :: Text
    , circuitIntensity :: Text
    , circuitReps :: Text
    , circuitTempo :: Text
    , circuitSets :: Text
    , circuitTotal :: Text
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype ProgramViewId = ProgramViewId {unProgramViewId :: Integer} deriving (Enum, Eq, Ord, Show, Data, Typeable, PathInfo)
instance SinkType ProgramViewId

instance Default Char where
    def = 'A'

data ProgramView =
    ProgramView
    { programViewId :: ProgramViewId
    , programViewProgram :: ProgramId
    , programViewNoteList :: [ViewNote]
    } deriving (Eq, Ord, Show, Data, Typeable)

data ViewNote =
    ViewNote
    { noteName :: Maybe Char
    , noteOrder :: Maybe Integer
    , noteText :: ExerciseId
    } deriving (Eq, Ord, Show, Data, Typeable)

{-
-- | The type of primary key for each table  (Is this implied by the type in idField?)
keyType :: Name -> KeyType
keyType typeName | typeName == ''Trainer = Owner
keyType typeName | typeName == ''Client = Owner
keyType typeName | typeName == ''Exercise = Private
keyType typeName | typeName == ''Program = Private
keyType typeName | typeName == ''Circuit = Private
keyType typeName | typeName == ''ProgramView = Private
keyType _ = NoKey
-}

hints :: [StackElement] -> [Hint]
hints =
    concatMap hints'
    where
      hints' (StackElement fld _ _) =
          case fName fld of
            -- Don't show the exercise text in the multi-view, put it in a
            -- div in the single-view and use a textarea to input it.
            Right n | n == 'exerciseText -> [HideColumn, Div, Area]
            _ -> []

instance SinkType UserId
instance SinkType Integer
instance SinkType Text

{-
$(inferRowType User ''Trainer)
$(inferRowType User ''Client)
$(inferRowType Custom ''Exercise)
$(inferRowType Custom ''Program)
$(inferRowType Custom ''Medium)


$(inferIdInstance ''HasUserId [t|TrainerId|] [t|TrainerRow|] 'trainerId)
$(inferIdInstance ''HasUserId [t|ClientId|] [t|ClientRow|] 'clientId)
$(inferIdInstance ''HasExerciseId [t|ExerciseId|] [t|ExerciseRow|] 'exerciseId)
$(inferIdInstance ''HasProgramId [t|ProgramId|] [t|ProgramRow|] 'programId)
$(inferIdInstance ''HasMediumId [t|MediumId|] [t|MediumRow|] 'mediumId)


$(deriveSafeCopy 0 'base ''TrainerRow)
$(deriveSafeCopy 0 'base ''ClientRow)
$(deriveSafeCopy 0 'base ''ExerciseRow)
$(deriveSafeCopy 0 'base ''ProgramRow)

$(deriveSafeCopy 1 'base ''MediumRow)

$(inferIxSet "Trainers" ''TrainerRow 'noCalcs [''UserId])
$(inferIxSet "Clients" ''ClientRow 'noCalcs [''UserId])
$(inferIxSet "Exercises" ''ExerciseRow 'noCalcs [''ExerciseId, ''UserId])
$(inferIxSet "Programs" ''ProgramRow 'noCalcs [''ProgramId, ''UserId])

$(inferIxSet "Mediums" ''MediumRow 'noCalcs [''MediumId])

lensList :: Q [LensSpec]
lensList = Control.Monad.sequence $
  [([t|Program|] >>= \ a ->
     [t|ProgramView|] >>= \ b -> 
     return $ LensSpec
                { aType = a
                , bType = b
                , lensExpr =
                    [| lens ({-
                       \ p ->
                         let look e = Data.Map.lookup e (lookupExercise (context cc)) in 
                         return $ ProgramView
                           { viewProgram = p
                           , viewNotes = mapMaybe (\ c -> circuitExercise c >>= look >>= \ e ->
                                                          Just (ViewNote (circuitName c) (circuitOrder c) (exerciseText e)))
                                                  (programCircuits p)
                           }
                       -}
                       (\ p ->
                         ProgramView
                           { viewProgram = p
                           , viewNotes = mapMaybe (\ c -> case circuitExercise c of
                                                            Just e -> Just (ViewNote (circuitName c) (circuitOrder c) e)
                                                            Nothing -> Nothing) (programCircuits p) })
                            )
                            ((\ v _ -> viewProgram v) :: ProgramView -> Program -> Program
                       -- This is type correct, but it won't update exercise descriptions
                            ) :: Lens (Program) (ProgramView) |]
                })]
-}
