getTrainerByIdEvent :: UserId -> Query AppState (Maybe Trainer)
getTrainerByIdEvent i = (getOne . (getEQ i . trainers)) <$> ask
createTrainerEvent :: Trainer -> Update AppState UserId
createTrainerEvent p = do {cvs <- get;
                           let pid' = trainerId p;
                           let p' = p{trainerId = pid'};
                           put cvs{trainers = insert p' (trainers cvs)};
                           return pid'}
updateTrainerEvent :: Trainer -> Update AppState UserId
updateTrainerEvent p = do {cvs <- get;
                           put cvs{trainers = updateIx (trainerId p) p (trainers cvs)};
                           return (trainerId p)}
deleteTrainerEvent :: Trainer -> Update AppState ()
deleteTrainerEvent p = do {cvs <- get;
                           let _pid = trainerId p;
                           put cvs{trainers = delete p (trainers cvs)}}
someTrainersEvent :: Int -> Int -> Query AppState ([Trainer])
someTrainersEvent limit offset = do {cvs <- ask;
                                     return $ (take limit $ (drop offset $ toDescList (Proxy :: Proxy UserId) (trainers cvs)))}
getClientByIdEvent :: UserId -> Query AppState (Maybe Client)
getClientByIdEvent i = (getOne . (getEQ i . clients)) <$> ask
createClientEvent :: Client -> Update AppState UserId
createClientEvent p = do {cvs <- get;
                          let pid' = clientId p;
                          let p' = p{clientId = pid'};
                          put cvs{clients = insert p' (clients cvs)};
                          return pid'}
updateClientEvent :: Client -> Update AppState UserId
updateClientEvent p = do {cvs <- get;
                          put cvs{clients = updateIx (clientId p) p (clients cvs)};
                          return (clientId p)}
deleteClientEvent :: Client -> Update AppState ()
deleteClientEvent p = do {cvs <- get;
                          let _pid = clientId p;
                          put cvs{clients = delete p (clients cvs)}}
someClientsEvent :: Int -> Int -> Query AppState ([Client])
someClientsEvent limit offset = do {cvs <- ask;
                                    return $ (take limit $ (drop offset $ toDescList (Proxy :: Proxy UserId) (clients cvs)))}
getExerciseByIdEvent :: ExerciseId ->
                        Query AppState (Maybe Exercise)
getExerciseByIdEvent i = (getOne . (getEQ i . exercises)) <$> ask
createExerciseEvent :: Exercise -> Update AppState ExerciseId
createExerciseEvent p = do {cvs <- get;
                            let pid' = nextExerciseId cvs;
                            let p' = p{exerciseId = pid'};
                            put cvs{exercises = insert p' (exercises cvs),
                                    nextExerciseId = succ pid'};
                            return pid'}
updateExerciseEvent :: Exercise -> Update AppState ExerciseId
updateExerciseEvent p = do {cvs <- get;
                            put cvs{exercises = updateIx (exerciseId p) p (exercises cvs)};
                            return (exerciseId p)}
deleteExerciseEvent :: Exercise -> Update AppState ()
deleteExerciseEvent p = do {cvs <- get;
                            let _pid = exerciseId p;
                            put cvs{exercises = delete p (exercises cvs)}}
someExercisesEvent :: Int -> Int -> Query AppState ([Exercise])
someExercisesEvent limit offset = do {cvs <- ask;
                                      return $ (take limit $ (drop offset $ toDescList (Proxy :: Proxy ExerciseId) (exercises cvs)))}
getProgramByIdEvent :: ProgramId -> Query AppState (Maybe Program)
getProgramByIdEvent i = (getOne . (getEQ i . programs)) <$> ask
createProgramEvent :: Program -> Update AppState ProgramId
createProgramEvent p = do {cvs <- get;
                           let pid' = nextProgramId cvs;
                           let p' = p{programId = pid'};
                           put cvs{programs = insert p' (programs cvs),
                                   nextProgramId = succ pid'};
                           return pid'}
updateProgramEvent :: Program -> Update AppState ProgramId
updateProgramEvent p = do {cvs <- get;
                           put cvs{programs = updateIx (programId p) p (programs cvs)};
                           return (programId p)}
deleteProgramEvent :: Program -> Update AppState ()
deleteProgramEvent p = do {cvs <- get;
                           let _pid = programId p;
                           put cvs{programs = delete p (programs cvs)}}
someProgramsEvent :: Int -> Int -> Query AppState ([Program])
someProgramsEvent limit offset = do {cvs <- ask;
                                     return $ (take limit $ (drop offset $ toDescList (Proxy :: Proxy ProgramId) (programs cvs)))}
getCircuitByIdEvent :: CircuitId -> Query AppState (Maybe Circuit)
getCircuitByIdEvent i = (getOne . (getEQ i . circuits)) <$> ask
createCircuitEvent :: Circuit -> Update AppState CircuitId
createCircuitEvent p = do {cvs <- get;
                           let pid' = nextCircuitId cvs;
                           let p' = p{circuitId = pid'};
                           put cvs{circuits = insert p' (circuits cvs),
                                   nextCircuitId = succ pid'};
                           return pid'}
updateCircuitEvent :: Circuit -> Update AppState CircuitId
updateCircuitEvent p = do {cvs <- get;
                           put cvs{circuits = updateIx (circuitId p) p (circuits cvs)};
                           return (circuitId p)}
deleteCircuitEvent :: Circuit -> Update AppState ()
deleteCircuitEvent p = do {cvs <- get;
                           let _pid = circuitId p;
                           put cvs{circuits = delete p (circuits cvs)}}
someCircuitsEvent :: Int -> Int -> Query AppState ([Circuit])
someCircuitsEvent limit offset = do {cvs <- ask;
                                     return $ (take limit $ (drop offset $ toDescList (Proxy :: Proxy CircuitId) (circuits cvs)))}
getProgramViewByIdEvent :: ProgramViewId ->
                           Query AppState (Maybe ProgramView)
getProgramViewByIdEvent i = (getOne . (getEQ i . programViews)) <$> ask
createProgramViewEvent :: ProgramView ->
                          Update AppState ProgramViewId
createProgramViewEvent p = do {cvs <- get;
                               let pid' = nextProgramViewId cvs;
                               let p' = p{programViewId = pid'};
                               put cvs{programViews = insert p' (programViews cvs),
                                       nextProgramViewId = succ pid'};
                               return pid'}
updateProgramViewEvent :: ProgramView ->
                          Update AppState ProgramViewId
updateProgramViewEvent p = do {cvs <- get;
                               put cvs{programViews = updateIx (programViewId p) p (programViews cvs)};
                               return (programViewId p)}
deleteProgramViewEvent :: ProgramView -> Update AppState ()
deleteProgramViewEvent p = do {cvs <- get;
                               let _pid = programViewId p;
                               put cvs{programViews = delete p (programViews cvs)}}
someProgramViewsEvent :: Int ->
                         Int -> Query AppState ([ProgramView])
someProgramViewsEvent limit offset = do {cvs <- ask;
                                         return $ (take limit $ (drop offset $ toDescList (Proxy :: Proxy ProgramViewId) (programViews cvs)))}
