{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Stage2Def where

import Stage1Def
import Stage1Gen (App)
import MIMO.Updatable (Updatable(appForm))

instance Updatable App ProgramId where
    appForm _ = error "Updatable App ProgramId"

instance Updatable App ExerciseId where
    appForm _ = error "Updatable App ExerciseId"

instance Updatable App CircuitId where
    appForm _ = error "Updatable App CircuitId"

instance Updatable App ProgramViewId where
    appForm _ = error "Updatable App ProgramViewId"
