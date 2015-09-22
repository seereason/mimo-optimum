data AppURL
    = ViewProgramView ProgramViewId
    | SomeProgramViews
    | CreateProgramView
    | UpdateProgramView ProgramViewId
    | ViewCircuit CircuitId
    | SomeCircuits
    | CreateCircuit
    | UpdateCircuit CircuitId
    | ViewProgram ProgramId
    | SomePrograms
    | CreateProgram
    | UpdateProgram ProgramId
    | ViewExercise ExerciseId
    | SomeExercises
    | CreateExercise
    | UpdateExercise ExerciseId
    | ViewTrainer TrainerId
    | SomeTrainers
    | CreateTrainer
    | UpdateTrainer TrainerId
    | ViewClient ClientId
    | SomeClients
    | CreateClient
    | UpdateClient ClientId
    deriving (Eq, Ord, Show, Data, Typeable)
instance Default AppURL
    where def = SomeProgramViews
