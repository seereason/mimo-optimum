initialAppState :: AppState
initialAppState = AppState{trainers = empty,
                           clients = empty,
                           exercises = empty,
                           nextExerciseId = ExerciseId (1 :: Integer),
                           programs = empty,
                           nextProgramId = ProgramId (1 :: Integer),
                           circuits = empty,
                           nextCircuitId = CircuitId (1 :: Integer),
                           programViews = empty,
                           nextProgramViewId = ProgramViewId (1 :: Integer)}
