{-# LANGUAGE TemplateHaskell #-}
-- | This is here so that MIMO.Main can say import App and get the app
-- specific spec value implied by Hs-Source-Dirs.
module App
    ( module Types
    , theAppInfo
    ) where

import Types
import Spec (spec)
import Happstack.Authenticate.Core (UserId)
import MIMO.App (AppInfo(..))

theAppInfo :: AppInfo
theAppInfo =
  AppInfo  { _spec = spec
           , _idField =
               let f n | n == ''Trainer = Just (''UserId, 'trainerId)
                   f n | n == ''Client = Just (''UserId, 'clientId)
                   f n | n == ''Exercise = Just (''ExerciseId, 'exerciseId)
                   f n | n == ''Program = Just (''ProgramId, 'programId)
                   f n | n == ''Circuit = Just (''CircuitId, 'circuitId)
                   f n | n == ''ProgramView = Just (''ProgramViewId, 'programViewId)
                   f n | n == ''ViewNote = Nothing
                   f _ = Nothing
               in f
           , _indexTypes =
               let f n = []
               in f
           , _hints = hints
           , _typeNames = [''Trainer, ''Client, ''Exercise, ''Program, ''Circuit, ''ProgramView, ''ViewNote]
           }
