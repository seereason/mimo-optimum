-- | This module is the authoritative source of all information
-- used to generate the Optimum web site.
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             RecordWildCards, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Stage1Def where

import Data.Data (Data, Typeable)
import Data.Default (Default(def))
import Data.Set
import Data.Text (Text)
import Data.UserId (UserId(..))
import Distribution.License (License(..))
import Happstack.Foundation ({-(<$>), Data, Typeable,-} PathInfo)
import Language.Haskell.TH.Path.Graph (SinkType)
import Language.Haskell.TH.TypeGraph.Shape (fName)
import Language.Haskell.TH.TypeGraph.Stack (StackElement(StackElement), TypeStack(_typeStack))
import MIMO.App (AppInfo(..))
import MIMO.Base (version)
import MIMO.Hint (Hint(HideColumn, Div, Area))
import MIMO.Id (IdField(idField), KeyType(Private, Owner), makeIdType', makeIdInstances)
import MIMO.Spec (Spec(..))
import qualified Ports (optimum)

$(makeIdType' Owner "Trainer")
$(makeIdType' Owner "Client")
$(makeIdType' Private "Exercise")
$(makeIdType' Private "Program")
$(makeIdType' Private "Circuit")
$(makeIdType' Private "ProgramView")

data Trainer =
    Trainer
    { trainerId :: TrainerId
    , trainerName :: Text
    , trainerActive :: Bool
    , trainerClients :: Set UserId
    } deriving (Eq, Ord, Show, Data, Typeable)

data Client =
    Client
    { clientId :: ClientId
    , clientName :: Text
    , clientActive :: Bool
    } deriving (Eq, Ord, Show, Data, Typeable)

data Exercise =
    Exercise
    { exerciseId :: ExerciseId
    , exerciseAuthor :: TrainerId
    , exerciseTitle :: Text
    , exerciseText :: Text
    } deriving (Eq, Ord, Show, Data, Typeable)

data Program =
    Program
    { programId :: ProgramId
    , programTitle :: Text
    , programNotes :: [(Text, Text)]
    , programCircuits :: [Circuit]
    , programAuthor :: TrainerId
    , programClients :: Set UserId
    } deriving (Eq, Ord, Show, Data, Typeable)

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

$(makeIdInstances ''Client 'clientId ''ClientId)
$(makeIdInstances ''Trainer 'trainerId ''TrainerId)
$(makeIdInstances ''Exercise 'exerciseId ''ExerciseId)
$(makeIdInstances ''Program 'programId ''ProgramId)
$(makeIdInstances ''Circuit 'circuitId ''CircuitId)
$(makeIdInstances ''ProgramView 'programViewId ''ProgramViewId)

instance SinkType UserId
instance SinkType Integer
instance SinkType Text

theAppInfo :: AppInfo
theAppInfo =
  AppInfo  { _spec = theSpec
           , _indexTypes =
               let f n = []
               in f
           , _hints = theHints
           }

theSpec :: Spec
theSpec =
    Spec { siteName = "Optimum"
         , siteVersion = version "1.2.3"
         , siteHomepage = "homepage"
         , siteAuthor = "author"
         , siteLicense = PublicDomain
         , siteSynopsis = "synopsis"
         , siteDescription = "description"
         , siteOwner = UserId 1
         , siteDomain = "optimumhealth.com"
         , siteTestHost = Nothing
         , sitePorts = Ports.optimum
         , siteAdmin = "logic@seereason.com"
         , siteParent = "/srv"
         , siteBackupsDir = "/srv/backups"
         , siteBackupsUser = "upload"
         , siteRowTypes = [''Trainer, ''Client, ''Exercise, ''Program, ''Circuit, ''ProgramView, ''ViewNote]
         }

theHints :: TypeStack -> [Hint]
theHints =
    concatMap hints' . _typeStack
    where
      hints' (StackElement fld _ _) =
          case fName fld of
            -- Don't show the exercise text in the multi-view, put it in a
            -- div in the single-view and use a textarea to input it.
            Right n | n == 'exerciseText -> [HideColumn, Div, Area]
            _ -> []
