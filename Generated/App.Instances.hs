instance SafeCopy Trainer
    where putCopy (Trainer arg
                           arg
                           arg
                           arg) = contain (do {safePut_TrainerId <- getSafePut;
                                               safePut_Text <- getSafePut;
                                               safePut_Bool <- getSafePut;
                                               safePut_SetUserId <- getSafePut;
                                               safePut_TrainerId arg;
                                               safePut_Text arg;
                                               safePut_Bool arg;
                                               safePut_SetUserId arg;
                                               return ()})
          getCopy = contain (label "Stage1Def.Trainer:" (do {safeGet_TrainerId <- getSafeGet;
                                                             safeGet_Text <- getSafeGet;
                                                             safeGet_Bool <- getSafeGet;
                                                             safeGet_SetUserId <- getSafeGet;
                                                             (((return Trainer <*> safeGet_TrainerId) <*> safeGet_Text) <*> safeGet_Bool) <*> safeGet_SetUserId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.Trainer"
instance SafeCopy TrainerId
    where putCopy (TrainerId arg) = contain (do {safePut_UserId <- getSafePut;
                                                 safePut_UserId arg;
                                                 return ()})
          getCopy = contain (label "Stage1Def.TrainerId:" (do {safeGet_UserId <- getSafeGet;
                                                               return TrainerId <*> safeGet_UserId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.TrainerId"
instance SafeCopy Client
    where putCopy (Client arg
                          arg
                          arg) = contain (do {safePut_ClientId <- getSafePut;
                                              safePut_Text <- getSafePut;
                                              safePut_Bool <- getSafePut;
                                              safePut_ClientId arg;
                                              safePut_Text arg;
                                              safePut_Bool arg;
                                              return ()})
          getCopy = contain (label "Stage1Def.Client:" (do {safeGet_ClientId <- getSafeGet;
                                                            safeGet_Text <- getSafeGet;
                                                            safeGet_Bool <- getSafeGet;
                                                            ((return Client <*> safeGet_ClientId) <*> safeGet_Text) <*> safeGet_Bool}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.Client"
instance SafeCopy ClientId
    where putCopy (ClientId arg) = contain (do {safePut_UserId <- getSafePut;
                                                safePut_UserId arg;
                                                return ()})
          getCopy = contain (label "Stage1Def.ClientId:" (do {safeGet_UserId <- getSafeGet;
                                                              return ClientId <*> safeGet_UserId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.ClientId"
instance SafeCopy Exercise
    where putCopy (Exercise arg
                            arg
                            arg
                            arg) = contain (do {safePut_ExerciseId <- getSafePut;
                                                safePut_TrainerId <- getSafePut;
                                                safePut_Text <- getSafePut;
                                                safePut_ExerciseId arg;
                                                safePut_TrainerId arg;
                                                safePut_Text arg;
                                                safePut_Text arg;
                                                return ()})
          getCopy = contain (label "Stage1Def.Exercise:" (do {safeGet_ExerciseId <- getSafeGet;
                                                              safeGet_TrainerId <- getSafeGet;
                                                              safeGet_Text <- getSafeGet;
                                                              (((return Exercise <*> safeGet_ExerciseId) <*> safeGet_TrainerId) <*> safeGet_Text) <*> safeGet_Text}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.Exercise"
instance SafeCopy ExerciseId
    where putCopy (ExerciseId arg) = contain (do {safePut_Integer <- getSafePut;
                                                  safePut_Integer arg;
                                                  return ()})
          getCopy = contain (label "Stage1Def.ExerciseId:" (do {safeGet_Integer <- getSafeGet;
                                                                return ExerciseId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.ExerciseId"
instance SafeCopy Program
    where putCopy (Program arg
                           arg
                           arg
                           arg
                           arg
                           arg) = contain (do {safePut_ProgramId <- getSafePut;
                                               safePut_Text <- getSafePut;
                                               safePut_ListTuple2TextText <- getSafePut;
                                               safePut_ListCircuit <- getSafePut;
                                               safePut_TrainerId <- getSafePut;
                                               safePut_SetUserId <- getSafePut;
                                               safePut_ProgramId arg;
                                               safePut_Text arg;
                                               safePut_ListTuple2TextText arg;
                                               safePut_ListCircuit arg;
                                               safePut_TrainerId arg;
                                               safePut_SetUserId arg;
                                               return ()})
          getCopy = contain (label "Stage1Def.Program:" (do {safeGet_ProgramId <- getSafeGet;
                                                             safeGet_Text <- getSafeGet;
                                                             safeGet_ListTuple2TextText <- getSafeGet;
                                                             safeGet_ListCircuit <- getSafeGet;
                                                             safeGet_TrainerId <- getSafeGet;
                                                             safeGet_SetUserId <- getSafeGet;
                                                             (((((return Program <*> safeGet_ProgramId) <*> safeGet_Text) <*> safeGet_ListTuple2TextText) <*> safeGet_ListCircuit) <*> safeGet_TrainerId) <*> safeGet_SetUserId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.Program"
instance SafeCopy ProgramId
    where putCopy (ProgramId arg) = contain (do {safePut_Integer <- getSafePut;
                                                 safePut_Integer arg;
                                                 return ()})
          getCopy = contain (label "Stage1Def.ProgramId:" (do {safeGet_Integer <- getSafeGet;
                                                               return ProgramId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.ProgramId"
instance SafeCopy Circuit
    where putCopy (Circuit arg
                           arg
                           arg
                           arg
                           arg
                           arg
                           arg
                           arg
                           arg
                           arg) = contain (do {safePut_CircuitId <- getSafePut;
                                               safePut_MaybeExerciseId <- getSafePut;
                                               safePut_MaybeChar <- getSafePut;
                                               safePut_MaybeInteger <- getSafePut;
                                               safePut_Text <- getSafePut;
                                               safePut_CircuitId arg;
                                               safePut_MaybeExerciseId arg;
                                               safePut_MaybeChar arg;
                                               safePut_MaybeInteger arg;
                                               safePut_Text arg;
                                               safePut_Text arg;
                                               safePut_Text arg;
                                               safePut_Text arg;
                                               safePut_Text arg;
                                               safePut_Text arg;
                                               return ()})
          getCopy = contain (label "Stage1Def.Circuit:" (do {safeGet_CircuitId <- getSafeGet;
                                                             safeGet_MaybeExerciseId <- getSafeGet;
                                                             safeGet_MaybeChar <- getSafeGet;
                                                             safeGet_MaybeInteger <- getSafeGet;
                                                             safeGet_Text <- getSafeGet;
                                                             (((((((((return Circuit <*> safeGet_CircuitId) <*> safeGet_MaybeExerciseId) <*> safeGet_MaybeChar) <*> safeGet_MaybeInteger) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.Circuit"
instance SafeCopy CircuitId
    where putCopy (CircuitId arg) = contain (do {safePut_Integer <- getSafePut;
                                                 safePut_Integer arg;
                                                 return ()})
          getCopy = contain (label "Stage1Def.CircuitId:" (do {safeGet_Integer <- getSafeGet;
                                                               return CircuitId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.CircuitId"
instance SafeCopy ProgramView
    where putCopy (ProgramView arg
                               arg
                               arg) = contain (do {safePut_ProgramViewId <- getSafePut;
                                                   safePut_ProgramId <- getSafePut;
                                                   safePut_ListViewNote <- getSafePut;
                                                   safePut_ProgramViewId arg;
                                                   safePut_ProgramId arg;
                                                   safePut_ListViewNote arg;
                                                   return ()})
          getCopy = contain (label "Stage1Def.ProgramView:" (do {safeGet_ProgramViewId <- getSafeGet;
                                                                 safeGet_ProgramId <- getSafeGet;
                                                                 safeGet_ListViewNote <- getSafeGet;
                                                                 ((return ProgramView <*> safeGet_ProgramViewId) <*> safeGet_ProgramId) <*> safeGet_ListViewNote}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.ProgramView"
instance SafeCopy ProgramViewId
    where putCopy (ProgramViewId arg) = contain (do {safePut_Integer <- getSafePut;
                                                     safePut_Integer arg;
                                                     return ()})
          getCopy = contain (label "Stage1Def.ProgramViewId:" (do {safeGet_Integer <- getSafeGet;
                                                                   return ProgramViewId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.ProgramViewId"
instance SafeCopy ViewNote
    where putCopy (ViewNote arg
                            arg
                            arg) = contain (do {safePut_MaybeChar <- getSafePut;
                                                safePut_MaybeInteger <- getSafePut;
                                                safePut_ExerciseId <- getSafePut;
                                                safePut_MaybeChar arg;
                                                safePut_MaybeInteger arg;
                                                safePut_ExerciseId arg;
                                                return ()})
          getCopy = contain (label "Stage1Def.ViewNote:" (do {safeGet_MaybeChar <- getSafeGet;
                                                              safeGet_MaybeInteger <- getSafeGet;
                                                              safeGet_ExerciseId <- getSafeGet;
                                                              ((return ViewNote <*> safeGet_MaybeChar) <*> safeGet_MaybeInteger) <*> safeGet_ExerciseId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage1Def.ViewNote"
instance Default Circuit
    where def = Circuit def def def def def def def def def def
instance Default CircuitId
    where def = CircuitId def
instance Default Client
    where def = Client def def def
instance Default ClientId
    where def = ClientId def
instance Default Exercise
    where def = Exercise def def def def
instance Default ExerciseId
    where def = ExerciseId def
instance Default Program
    where def = Program def def def def def def
instance Default ProgramId
    where def = ProgramId def
instance Default ProgramView
    where def = ProgramView def def def
instance Default ProgramViewId
    where def = ProgramViewId def
instance Default Trainer
    where def = Trainer def def def def
instance Default TrainerId
    where def = TrainerId def
instance Default ViewNote
    where def = ViewNote def def def
