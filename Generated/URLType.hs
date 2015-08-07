data AppURL
    = ViewTrainer UserId
    | SomeTrainers
    | CreateTrainer
    | UpdateTrainer UserId
    | ViewClient UserId
    | SomeClients
    | CreateClient
    | UpdateClient UserId
    | ViewExercise ExerciseId
    | SomeExercises
    | CreateExercise
    | UpdateExercise ExerciseId
    | ViewProgram ProgramId
    | SomePrograms
    | CreateProgram
    | UpdateProgram ProgramId
    | ViewCircuit CircuitId
    | SomeCircuits
    | CreateCircuit
    | UpdateCircuit CircuitId
    | ViewProgramView ProgramViewId
    | SomeProgramViews
    | CreateProgramView
    | UpdateProgramView ProgramViewId
    deriving (Eq, Ord, Show, Data, Typeable)
instance Default AppURL
    where def = SomeTrainers
