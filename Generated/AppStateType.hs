data AppState
    = AppState {trainers :: (IxSet Trainer),
                clients :: (IxSet Client),
                exercises :: (IxSet Exercise),
                nextExerciseId :: ExerciseId,
                programs :: (IxSet Program),
                nextProgramId :: ProgramId,
                circuits :: (IxSet Circuit),
                nextCircuitId :: CircuitId,
                programViews :: (IxSet ProgramView),
                nextProgramViewId :: ProgramViewId}
    deriving (Data, Typeable)
