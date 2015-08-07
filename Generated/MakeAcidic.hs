instance IsAcidic AppState
    where acidEvents = [QueryEvent (\(GetTrainerByIdEvent arg) -> getTrainerByIdEvent arg),
                        QueryEvent (\(SomeTrainersEvent arg
                                                        arg) -> someTrainersEvent arg arg),
                        UpdateEvent (\(CreateTrainerEvent arg) -> createTrainerEvent arg),
                        UpdateEvent (\(UpdateTrainerEvent arg) -> updateTrainerEvent arg),
                        UpdateEvent (\(DeleteTrainerEvent arg) -> deleteTrainerEvent arg),
                        QueryEvent (\(GetClientByIdEvent arg) -> getClientByIdEvent arg),
                        QueryEvent (\(SomeClientsEvent arg
                                                       arg) -> someClientsEvent arg arg),
                        UpdateEvent (\(CreateClientEvent arg) -> createClientEvent arg),
                        UpdateEvent (\(UpdateClientEvent arg) -> updateClientEvent arg),
                        UpdateEvent (\(DeleteClientEvent arg) -> deleteClientEvent arg),
                        QueryEvent (\(GetExerciseByIdEvent arg) -> getExerciseByIdEvent arg),
                        QueryEvent (\(SomeExercisesEvent arg
                                                         arg) -> someExercisesEvent arg arg),
                        UpdateEvent (\(CreateExerciseEvent arg) -> createExerciseEvent arg),
                        UpdateEvent (\(UpdateExerciseEvent arg) -> updateExerciseEvent arg),
                        UpdateEvent (\(DeleteExerciseEvent arg) -> deleteExerciseEvent arg),
                        QueryEvent (\(GetProgramByIdEvent arg) -> getProgramByIdEvent arg),
                        QueryEvent (\(SomeProgramsEvent arg
                                                        arg) -> someProgramsEvent arg arg),
                        UpdateEvent (\(CreateProgramEvent arg) -> createProgramEvent arg),
                        UpdateEvent (\(UpdateProgramEvent arg) -> updateProgramEvent arg),
                        UpdateEvent (\(DeleteProgramEvent arg) -> deleteProgramEvent arg),
                        QueryEvent (\(GetCircuitByIdEvent arg) -> getCircuitByIdEvent arg),
                        QueryEvent (\(SomeCircuitsEvent arg
                                                        arg) -> someCircuitsEvent arg arg),
                        UpdateEvent (\(CreateCircuitEvent arg) -> createCircuitEvent arg),
                        UpdateEvent (\(UpdateCircuitEvent arg) -> updateCircuitEvent arg),
                        UpdateEvent (\(DeleteCircuitEvent arg) -> deleteCircuitEvent arg),
                        QueryEvent (\(GetProgramViewByIdEvent arg) -> getProgramViewByIdEvent arg),
                        QueryEvent (\(SomeProgramViewsEvent arg
                                                            arg) -> someProgramViewsEvent arg arg),
                        UpdateEvent (\(CreateProgramViewEvent arg) -> createProgramViewEvent arg),
                        UpdateEvent (\(UpdateProgramViewEvent arg) -> updateProgramViewEvent arg),
                        UpdateEvent (\(DeleteProgramViewEvent arg) -> deleteProgramViewEvent arg)]
newtype GetTrainerByIdEvent
  = GetTrainerByIdEvent UserId
    deriving (Typeable)
instance SafeCopy GetTrainerByIdEvent
    where putCopy (GetTrainerByIdEvent arg) = contain (do {safePut arg;
                                                           return ()})
          getCopy = contain (return GetTrainerByIdEvent <*> safeGet)
instance Method GetTrainerByIdEvent
    where type MethodResult GetTrainerByIdEvent = Maybe Trainer
          type MethodState GetTrainerByIdEvent = AppState
instance QueryEvent GetTrainerByIdEvent
data SomeTrainersEvent
    = SomeTrainersEvent Int Int
    deriving (Typeable)
instance SafeCopy SomeTrainersEvent
    where putCopy (SomeTrainersEvent arg
                                     arg) = contain (do {safePut arg; safePut arg; return ()})
          getCopy = contain ((return SomeTrainersEvent <*> safeGet) <*> safeGet)
instance Method SomeTrainersEvent
    where type MethodResult SomeTrainersEvent = [Trainer]
          type MethodState SomeTrainersEvent = AppState
instance QueryEvent SomeTrainersEvent
newtype CreateTrainerEvent
  = CreateTrainerEvent Trainer
    deriving (Typeable)
instance SafeCopy CreateTrainerEvent
    where putCopy (CreateTrainerEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return CreateTrainerEvent <*> safeGet)
instance Method CreateTrainerEvent
    where type MethodResult CreateTrainerEvent = UserId
          type MethodState CreateTrainerEvent = AppState
instance UpdateEvent CreateTrainerEvent
newtype UpdateTrainerEvent
  = UpdateTrainerEvent Trainer
    deriving (Typeable)
instance SafeCopy UpdateTrainerEvent
    where putCopy (UpdateTrainerEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return UpdateTrainerEvent <*> safeGet)
instance Method UpdateTrainerEvent
    where type MethodResult UpdateTrainerEvent = UserId
          type MethodState UpdateTrainerEvent = AppState
instance UpdateEvent UpdateTrainerEvent
newtype DeleteTrainerEvent
  = DeleteTrainerEvent Trainer
    deriving (Typeable)
instance SafeCopy DeleteTrainerEvent
    where putCopy (DeleteTrainerEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return DeleteTrainerEvent <*> safeGet)
instance Method DeleteTrainerEvent
    where type MethodResult DeleteTrainerEvent = ()
          type MethodState DeleteTrainerEvent = AppState
instance UpdateEvent DeleteTrainerEvent
newtype GetClientByIdEvent
  = GetClientByIdEvent UserId
    deriving (Typeable)
instance SafeCopy GetClientByIdEvent
    where putCopy (GetClientByIdEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return GetClientByIdEvent <*> safeGet)
instance Method GetClientByIdEvent
    where type MethodResult GetClientByIdEvent = Maybe Client
          type MethodState GetClientByIdEvent = AppState
instance QueryEvent GetClientByIdEvent
data SomeClientsEvent
    = SomeClientsEvent Int Int
    deriving (Typeable)
instance SafeCopy SomeClientsEvent
    where putCopy (SomeClientsEvent arg
                                    arg) = contain (do {safePut arg; safePut arg; return ()})
          getCopy = contain ((return SomeClientsEvent <*> safeGet) <*> safeGet)
instance Method SomeClientsEvent
    where type MethodResult SomeClientsEvent = [Client]
          type MethodState SomeClientsEvent = AppState
instance QueryEvent SomeClientsEvent
newtype CreateClientEvent
  = CreateClientEvent Client
    deriving (Typeable)
instance SafeCopy CreateClientEvent
    where putCopy (CreateClientEvent arg) = contain (do {safePut arg;
                                                         return ()})
          getCopy = contain (return CreateClientEvent <*> safeGet)
instance Method CreateClientEvent
    where type MethodResult CreateClientEvent = UserId
          type MethodState CreateClientEvent = AppState
instance UpdateEvent CreateClientEvent
newtype UpdateClientEvent
  = UpdateClientEvent Client
    deriving (Typeable)
instance SafeCopy UpdateClientEvent
    where putCopy (UpdateClientEvent arg) = contain (do {safePut arg;
                                                         return ()})
          getCopy = contain (return UpdateClientEvent <*> safeGet)
instance Method UpdateClientEvent
    where type MethodResult UpdateClientEvent = UserId
          type MethodState UpdateClientEvent = AppState
instance UpdateEvent UpdateClientEvent
newtype DeleteClientEvent
  = DeleteClientEvent Client
    deriving (Typeable)
instance SafeCopy DeleteClientEvent
    where putCopy (DeleteClientEvent arg) = contain (do {safePut arg;
                                                         return ()})
          getCopy = contain (return DeleteClientEvent <*> safeGet)
instance Method DeleteClientEvent
    where type MethodResult DeleteClientEvent = ()
          type MethodState DeleteClientEvent = AppState
instance UpdateEvent DeleteClientEvent
newtype GetExerciseByIdEvent
  = GetExerciseByIdEvent ExerciseId
    deriving (Typeable)
instance SafeCopy GetExerciseByIdEvent
    where putCopy (GetExerciseByIdEvent arg) = contain (do {safePut arg;
                                                            return ()})
          getCopy = contain (return GetExerciseByIdEvent <*> safeGet)
instance Method GetExerciseByIdEvent
    where type MethodResult GetExerciseByIdEvent = Maybe Exercise
          type MethodState GetExerciseByIdEvent = AppState
instance QueryEvent GetExerciseByIdEvent
data SomeExercisesEvent
    = SomeExercisesEvent Int Int
    deriving (Typeable)
instance SafeCopy SomeExercisesEvent
    where putCopy (SomeExercisesEvent arg
                                      arg) = contain (do {safePut arg; safePut arg; return ()})
          getCopy = contain ((return SomeExercisesEvent <*> safeGet) <*> safeGet)
instance Method SomeExercisesEvent
    where type MethodResult SomeExercisesEvent = [Exercise]
          type MethodState SomeExercisesEvent = AppState
instance QueryEvent SomeExercisesEvent
newtype CreateExerciseEvent
  = CreateExerciseEvent Exercise
    deriving (Typeable)
instance SafeCopy CreateExerciseEvent
    where putCopy (CreateExerciseEvent arg) = contain (do {safePut arg;
                                                           return ()})
          getCopy = contain (return CreateExerciseEvent <*> safeGet)
instance Method CreateExerciseEvent
    where type MethodResult CreateExerciseEvent = ExerciseId
          type MethodState CreateExerciseEvent = AppState
instance UpdateEvent CreateExerciseEvent
newtype UpdateExerciseEvent
  = UpdateExerciseEvent Exercise
    deriving (Typeable)
instance SafeCopy UpdateExerciseEvent
    where putCopy (UpdateExerciseEvent arg) = contain (do {safePut arg;
                                                           return ()})
          getCopy = contain (return UpdateExerciseEvent <*> safeGet)
instance Method UpdateExerciseEvent
    where type MethodResult UpdateExerciseEvent = ExerciseId
          type MethodState UpdateExerciseEvent = AppState
instance UpdateEvent UpdateExerciseEvent
newtype DeleteExerciseEvent
  = DeleteExerciseEvent Exercise
    deriving (Typeable)
instance SafeCopy DeleteExerciseEvent
    where putCopy (DeleteExerciseEvent arg) = contain (do {safePut arg;
                                                           return ()})
          getCopy = contain (return DeleteExerciseEvent <*> safeGet)
instance Method DeleteExerciseEvent
    where type MethodResult DeleteExerciseEvent = ()
          type MethodState DeleteExerciseEvent = AppState
instance UpdateEvent DeleteExerciseEvent
newtype GetProgramByIdEvent
  = GetProgramByIdEvent ProgramId
    deriving (Typeable)
instance SafeCopy GetProgramByIdEvent
    where putCopy (GetProgramByIdEvent arg) = contain (do {safePut arg;
                                                           return ()})
          getCopy = contain (return GetProgramByIdEvent <*> safeGet)
instance Method GetProgramByIdEvent
    where type MethodResult GetProgramByIdEvent = Maybe Program
          type MethodState GetProgramByIdEvent = AppState
instance QueryEvent GetProgramByIdEvent
data SomeProgramsEvent
    = SomeProgramsEvent Int Int
    deriving (Typeable)
instance SafeCopy SomeProgramsEvent
    where putCopy (SomeProgramsEvent arg
                                     arg) = contain (do {safePut arg; safePut arg; return ()})
          getCopy = contain ((return SomeProgramsEvent <*> safeGet) <*> safeGet)
instance Method SomeProgramsEvent
    where type MethodResult SomeProgramsEvent = [Program]
          type MethodState SomeProgramsEvent = AppState
instance QueryEvent SomeProgramsEvent
newtype CreateProgramEvent
  = CreateProgramEvent Program
    deriving (Typeable)
instance SafeCopy CreateProgramEvent
    where putCopy (CreateProgramEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return CreateProgramEvent <*> safeGet)
instance Method CreateProgramEvent
    where type MethodResult CreateProgramEvent = ProgramId
          type MethodState CreateProgramEvent = AppState
instance UpdateEvent CreateProgramEvent
newtype UpdateProgramEvent
  = UpdateProgramEvent Program
    deriving (Typeable)
instance SafeCopy UpdateProgramEvent
    where putCopy (UpdateProgramEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return UpdateProgramEvent <*> safeGet)
instance Method UpdateProgramEvent
    where type MethodResult UpdateProgramEvent = ProgramId
          type MethodState UpdateProgramEvent = AppState
instance UpdateEvent UpdateProgramEvent
newtype DeleteProgramEvent
  = DeleteProgramEvent Program
    deriving (Typeable)
instance SafeCopy DeleteProgramEvent
    where putCopy (DeleteProgramEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return DeleteProgramEvent <*> safeGet)
instance Method DeleteProgramEvent
    where type MethodResult DeleteProgramEvent = ()
          type MethodState DeleteProgramEvent = AppState
instance UpdateEvent DeleteProgramEvent
newtype GetCircuitByIdEvent
  = GetCircuitByIdEvent CircuitId
    deriving (Typeable)
instance SafeCopy GetCircuitByIdEvent
    where putCopy (GetCircuitByIdEvent arg) = contain (do {safePut arg;
                                                           return ()})
          getCopy = contain (return GetCircuitByIdEvent <*> safeGet)
instance Method GetCircuitByIdEvent
    where type MethodResult GetCircuitByIdEvent = Maybe Circuit
          type MethodState GetCircuitByIdEvent = AppState
instance QueryEvent GetCircuitByIdEvent
data SomeCircuitsEvent
    = SomeCircuitsEvent Int Int
    deriving (Typeable)
instance SafeCopy SomeCircuitsEvent
    where putCopy (SomeCircuitsEvent arg
                                     arg) = contain (do {safePut arg; safePut arg; return ()})
          getCopy = contain ((return SomeCircuitsEvent <*> safeGet) <*> safeGet)
instance Method SomeCircuitsEvent
    where type MethodResult SomeCircuitsEvent = [Circuit]
          type MethodState SomeCircuitsEvent = AppState
instance QueryEvent SomeCircuitsEvent
newtype CreateCircuitEvent
  = CreateCircuitEvent Circuit
    deriving (Typeable)
instance SafeCopy CreateCircuitEvent
    where putCopy (CreateCircuitEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return CreateCircuitEvent <*> safeGet)
instance Method CreateCircuitEvent
    where type MethodResult CreateCircuitEvent = CircuitId
          type MethodState CreateCircuitEvent = AppState
instance UpdateEvent CreateCircuitEvent
newtype UpdateCircuitEvent
  = UpdateCircuitEvent Circuit
    deriving (Typeable)
instance SafeCopy UpdateCircuitEvent
    where putCopy (UpdateCircuitEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return UpdateCircuitEvent <*> safeGet)
instance Method UpdateCircuitEvent
    where type MethodResult UpdateCircuitEvent = CircuitId
          type MethodState UpdateCircuitEvent = AppState
instance UpdateEvent UpdateCircuitEvent
newtype DeleteCircuitEvent
  = DeleteCircuitEvent Circuit
    deriving (Typeable)
instance SafeCopy DeleteCircuitEvent
    where putCopy (DeleteCircuitEvent arg) = contain (do {safePut arg;
                                                          return ()})
          getCopy = contain (return DeleteCircuitEvent <*> safeGet)
instance Method DeleteCircuitEvent
    where type MethodResult DeleteCircuitEvent = ()
          type MethodState DeleteCircuitEvent = AppState
instance UpdateEvent DeleteCircuitEvent
newtype GetProgramViewByIdEvent
  = GetProgramViewByIdEvent ProgramViewId
    deriving (Typeable)
instance SafeCopy GetProgramViewByIdEvent
    where putCopy (GetProgramViewByIdEvent arg) = contain (do {safePut arg;
                                                               return ()})
          getCopy = contain (return GetProgramViewByIdEvent <*> safeGet)
instance Method GetProgramViewByIdEvent
    where type MethodResult GetProgramViewByIdEvent = Maybe ProgramView
          type MethodState GetProgramViewByIdEvent = AppState
instance QueryEvent GetProgramViewByIdEvent
data SomeProgramViewsEvent
    = SomeProgramViewsEvent Int Int
    deriving (Typeable)
instance SafeCopy SomeProgramViewsEvent
    where putCopy (SomeProgramViewsEvent arg
                                         arg) = contain (do {safePut arg; safePut arg; return ()})
          getCopy = contain ((return SomeProgramViewsEvent <*> safeGet) <*> safeGet)
instance Method SomeProgramViewsEvent
    where type MethodResult SomeProgramViewsEvent = [ProgramView]
          type MethodState SomeProgramViewsEvent = AppState
instance QueryEvent SomeProgramViewsEvent
newtype CreateProgramViewEvent
  = CreateProgramViewEvent ProgramView
    deriving (Typeable)
instance SafeCopy CreateProgramViewEvent
    where putCopy (CreateProgramViewEvent arg) = contain (do {safePut arg;
                                                              return ()})
          getCopy = contain (return CreateProgramViewEvent <*> safeGet)
instance Method CreateProgramViewEvent
    where type MethodResult CreateProgramViewEvent = ProgramViewId
          type MethodState CreateProgramViewEvent = AppState
instance UpdateEvent CreateProgramViewEvent
newtype UpdateProgramViewEvent
  = UpdateProgramViewEvent ProgramView
    deriving (Typeable)
instance SafeCopy UpdateProgramViewEvent
    where putCopy (UpdateProgramViewEvent arg) = contain (do {safePut arg;
                                                              return ()})
          getCopy = contain (return UpdateProgramViewEvent <*> safeGet)
instance Method UpdateProgramViewEvent
    where type MethodResult UpdateProgramViewEvent = ProgramViewId
          type MethodState UpdateProgramViewEvent = AppState
instance UpdateEvent UpdateProgramViewEvent
newtype DeleteProgramViewEvent
  = DeleteProgramViewEvent ProgramView
    deriving (Typeable)
instance SafeCopy DeleteProgramViewEvent
    where putCopy (DeleteProgramViewEvent arg) = contain (do {safePut arg;
                                                              return ()})
          getCopy = contain (return DeleteProgramViewEvent <*> safeGet)
instance Method DeleteProgramViewEvent
    where type MethodResult DeleteProgramViewEvent = ()
          type MethodState DeleteProgramViewEvent = AppState
instance UpdateEvent DeleteProgramViewEvent
data Acid
    = Acid {acidAuthenticate :: (AcidState AuthenticateState),
            acidApp :: (AcidState AppState)}
withAcid :: forall a . AcidState AuthenticateState ->
                       (Acid -> IO a) -> IO a
withAcid authenticateState f = bracket (openLocalStateFrom "state/AppState" initialAppState) createCheckpointAndClose $ (\app -> f (Acid authenticateState app))
instance (Functor m, Monad m) => HasAcidState (FoundationT' url
                                                            Acid
                                                            reqSt
                                                            m)
                                              AuthenticateState
    where getAcidState = acidAuthenticate <$> getAcidSt
instance (Functor m, Monad m) => HasAcidState (FoundationT' url
                                                            Acid
                                                            requestState
                                                            m)
                                              AppState
    where getAcidState = acidApp <$> getAcidSt
