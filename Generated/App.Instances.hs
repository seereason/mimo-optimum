instance SafeCopy Trainer
    where putCopy (Trainer arg
                           arg
                           arg
                           arg) = contain (do {safePut_UserId <- getSafePut;
                                               safePut_Text <- getSafePut;
                                               safePut_Bool <- getSafePut;
                                               safePut_SetUserId <- getSafePut;
                                               safePut_UserId arg;
                                               safePut_Text arg;
                                               safePut_Bool arg;
                                               safePut_SetUserId arg;
                                               return ()})
          getCopy = contain (label "Stage0.Trainer:" (do {safeGet_UserId <- getSafeGet;
                                                          safeGet_Text <- getSafeGet;
                                                          safeGet_Bool <- getSafeGet;
                                                          safeGet_SetUserId <- getSafeGet;
                                                          (((return Trainer <*> safeGet_UserId) <*> safeGet_Text) <*> safeGet_Bool) <*> safeGet_SetUserId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.Trainer"
instance SafeCopy Client
    where putCopy (Client arg
                          arg
                          arg) = contain (do {safePut_UserId <- getSafePut;
                                              safePut_Text <- getSafePut;
                                              safePut_Bool <- getSafePut;
                                              safePut_UserId arg;
                                              safePut_Text arg;
                                              safePut_Bool arg;
                                              return ()})
          getCopy = contain (label "Stage0.Client:" (do {safeGet_UserId <- getSafeGet;
                                                         safeGet_Text <- getSafeGet;
                                                         safeGet_Bool <- getSafeGet;
                                                         ((return Client <*> safeGet_UserId) <*> safeGet_Text) <*> safeGet_Bool}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.Client"
instance SafeCopy Exercise
    where putCopy (Exercise arg
                            arg
                            arg
                            arg) = contain (do {safePut_ExerciseId <- getSafePut;
                                                safePut_UserId <- getSafePut;
                                                safePut_Text <- getSafePut;
                                                safePut_ExerciseId arg;
                                                safePut_UserId arg;
                                                safePut_Text arg;
                                                safePut_Text arg;
                                                return ()})
          getCopy = contain (label "Stage0.Exercise:" (do {safeGet_ExerciseId <- getSafeGet;
                                                           safeGet_UserId <- getSafeGet;
                                                           safeGet_Text <- getSafeGet;
                                                           (((return Exercise <*> safeGet_ExerciseId) <*> safeGet_UserId) <*> safeGet_Text) <*> safeGet_Text}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.Exercise"
instance SafeCopy ExerciseId
    where putCopy (ExerciseId arg) = contain (do {safePut_Integer <- getSafePut;
                                                  safePut_Integer arg;
                                                  return ()})
          getCopy = contain (label "Stage0.ExerciseId:" (do {safeGet_Integer <- getSafeGet;
                                                             return ExerciseId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.ExerciseId"
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
                                               safePut_UserId <- getSafePut;
                                               safePut_SetUserId <- getSafePut;
                                               safePut_ProgramId arg;
                                               safePut_Text arg;
                                               safePut_ListTuple2TextText arg;
                                               safePut_ListCircuit arg;
                                               safePut_UserId arg;
                                               safePut_SetUserId arg;
                                               return ()})
          getCopy = contain (label "Stage0.Program:" (do {safeGet_ProgramId <- getSafeGet;
                                                          safeGet_Text <- getSafeGet;
                                                          safeGet_ListTuple2TextText <- getSafeGet;
                                                          safeGet_ListCircuit <- getSafeGet;
                                                          safeGet_UserId <- getSafeGet;
                                                          safeGet_SetUserId <- getSafeGet;
                                                          (((((return Program <*> safeGet_ProgramId) <*> safeGet_Text) <*> safeGet_ListTuple2TextText) <*> safeGet_ListCircuit) <*> safeGet_UserId) <*> safeGet_SetUserId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.Program"
instance SafeCopy ProgramId
    where putCopy (ProgramId arg) = contain (do {safePut_Integer <- getSafePut;
                                                 safePut_Integer arg;
                                                 return ()})
          getCopy = contain (label "Stage0.ProgramId:" (do {safeGet_Integer <- getSafeGet;
                                                            return ProgramId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.ProgramId"
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
          getCopy = contain (label "Stage0.Circuit:" (do {safeGet_CircuitId <- getSafeGet;
                                                          safeGet_MaybeExerciseId <- getSafeGet;
                                                          safeGet_MaybeChar <- getSafeGet;
                                                          safeGet_MaybeInteger <- getSafeGet;
                                                          safeGet_Text <- getSafeGet;
                                                          (((((((((return Circuit <*> safeGet_CircuitId) <*> safeGet_MaybeExerciseId) <*> safeGet_MaybeChar) <*> safeGet_MaybeInteger) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text) <*> safeGet_Text}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.Circuit"
instance SafeCopy CircuitId
    where putCopy (CircuitId arg) = contain (do {safePut_Integer <- getSafePut;
                                                 safePut_Integer arg;
                                                 return ()})
          getCopy = contain (label "Stage0.CircuitId:" (do {safeGet_Integer <- getSafeGet;
                                                            return CircuitId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.CircuitId"
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
          getCopy = contain (label "Stage0.ProgramView:" (do {safeGet_ProgramViewId <- getSafeGet;
                                                              safeGet_ProgramId <- getSafeGet;
                                                              safeGet_ListViewNote <- getSafeGet;
                                                              ((return ProgramView <*> safeGet_ProgramViewId) <*> safeGet_ProgramId) <*> safeGet_ListViewNote}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.ProgramView"
instance SafeCopy ProgramViewId
    where putCopy (ProgramViewId arg) = contain (do {safePut_Integer <- getSafePut;
                                                     safePut_Integer arg;
                                                     return ()})
          getCopy = contain (label "Stage0.ProgramViewId:" (do {safeGet_Integer <- getSafeGet;
                                                                return ProgramViewId <*> safeGet_Integer}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.ProgramViewId"
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
          getCopy = contain (label "Stage0.ViewNote:" (do {safeGet_MaybeChar <- getSafeGet;
                                                           safeGet_MaybeInteger <- getSafeGet;
                                                           safeGet_ExerciseId <- getSafeGet;
                                                           ((return ViewNote <*> safeGet_MaybeChar) <*> safeGet_MaybeInteger) <*> safeGet_ExerciseId}))
          version = 0
          kind = base
          errorTypeName _ = "Stage0.ViewNote"
instance Default Circuit
    where def = Circuit def def def def def def def def def def
instance Default CircuitId
    where def = CircuitId def
instance Default Client
    where def = Client def def def
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
instance Default ViewNote
    where def = ViewNote def def def
