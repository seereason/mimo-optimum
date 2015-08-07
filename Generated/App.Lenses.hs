lens_unUserId :: forall . Iso' UserId Integer
lens_unUserId = iso (\(UserId x) -> x) UserId
{-# INLINE lens_unUserId #-}
lensCircuitExercise :: forall . Lens' Circuit (Maybe ExerciseId)
lensCircuitExercise f (Circuit x
                               x
                               x
                               x
                               x
                               x
                               x
                               x
                               x
                               x) = fmap (\y -> Circuit x y x x x x x x x x) (f x)
{-# INLINE lensCircuitExercise #-}
lensCircuitId :: forall . Lens' Circuit CircuitId
lensCircuitId f (Circuit x
                         x
                         x
                         x
                         x
                         x
                         x
                         x
                         x
                         x) = fmap (\y -> Circuit y x x x x x x x x x) (f x)
{-# INLINE lensCircuitId #-}
lensCircuitIntensity :: forall . Lens' Circuit Text
lensCircuitIntensity f (Circuit x
                                x
                                x
                                x
                                x
                                x
                                x
                                x
                                x
                                x) = fmap (\y -> Circuit x x x x x y x x x x) (f x)
{-# INLINE lensCircuitIntensity #-}
lensCircuitName :: forall . Lens' Circuit (Maybe Char)
lensCircuitName f (Circuit x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x) = fmap (\y -> Circuit x x y x x x x x x x) (f x)
{-# INLINE lensCircuitName #-}
lensCircuitOrder :: forall . Lens' Circuit (Maybe Integer)
lensCircuitOrder f (Circuit x
                            x
                            x
                            x
                            x
                            x
                            x
                            x
                            x
                            x) = fmap (\y -> Circuit x x x y x x x x x x) (f x)
{-# INLINE lensCircuitOrder #-}
lensCircuitReps :: forall . Lens' Circuit Text
lensCircuitReps f (Circuit x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x) = fmap (\y -> Circuit x x x x x x y x x x) (f x)
{-# INLINE lensCircuitReps #-}
lensCircuitRest :: forall . Lens' Circuit Text
lensCircuitRest f (Circuit x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x) = fmap (\y -> Circuit x x x x y x x x x x) (f x)
{-# INLINE lensCircuitRest #-}
lensCircuitSets :: forall . Lens' Circuit Text
lensCircuitSets f (Circuit x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x
                           x) = fmap (\y -> Circuit x x x x x x x x y x) (f x)
{-# INLINE lensCircuitSets #-}
lensCircuitTempo :: forall . Lens' Circuit Text
lensCircuitTempo f (Circuit x
                            x
                            x
                            x
                            x
                            x
                            x
                            x
                            x
                            x) = fmap (\y -> Circuit x x x x x x x y x x) (f x)
{-# INLINE lensCircuitTempo #-}
lensCircuitTotal :: forall . Lens' Circuit Text
lensCircuitTotal f (Circuit x
                            x
                            x
                            x
                            x
                            x
                            x
                            x
                            x
                            x) = fmap (\y -> Circuit x x x x x x x x x y) (f x)
{-# INLINE lensCircuitTotal #-}
lensUnCircuitId :: forall . Iso' CircuitId Integer
lensUnCircuitId = iso (\(CircuitId x) -> x) CircuitId
{-# INLINE lensUnCircuitId #-}
lensClientActive :: forall . Lens' Client Bool
lensClientActive f (Client x x x) = fmap (\y -> Client x x y) (f x)
{-# INLINE lensClientActive #-}
lensClientId :: forall . Lens' Client UserId
lensClientId f (Client x x x) = fmap (\y -> Client y x x) (f x)
{-# INLINE lensClientId #-}
lensClientName :: forall . Lens' Client Text
lensClientName f (Client x x x) = fmap (\y -> Client x y x) (f x)
{-# INLINE lensClientName #-}
lensExerciseAuthor :: forall . Lens' Exercise UserId
lensExerciseAuthor f (Exercise x
                               x
                               x
                               x) = fmap (\y -> Exercise x y x x) (f x)
{-# INLINE lensExerciseAuthor #-}
lensExerciseId :: forall . Lens' Exercise ExerciseId
lensExerciseId f (Exercise x
                           x
                           x
                           x) = fmap (\y -> Exercise y x x x) (f x)
{-# INLINE lensExerciseId #-}
lensExerciseText :: forall . Lens' Exercise Text
lensExerciseText f (Exercise x
                             x
                             x
                             x) = fmap (\y -> Exercise x x x y) (f x)
{-# INLINE lensExerciseText #-}
lensExerciseTitle :: forall . Lens' Exercise Text
lensExerciseTitle f (Exercise x
                              x
                              x
                              x) = fmap (\y -> Exercise x x y x) (f x)
{-# INLINE lensExerciseTitle #-}
lensUnExerciseId :: forall . Iso' ExerciseId Integer
lensUnExerciseId = iso (\(ExerciseId x) -> x) ExerciseId
{-# INLINE lensUnExerciseId #-}
lensProgramAuthor :: forall . Lens' Program UserId
lensProgramAuthor f (Program x
                             x
                             x
                             x
                             x
                             x) = fmap (\y -> Program x x x x y x) (f x)
{-# INLINE lensProgramAuthor #-}
lensProgramCircuits :: forall . Lens' Program ([Circuit])
lensProgramCircuits f (Program x
                               x
                               x
                               x
                               x
                               x) = fmap (\y -> Program x x x y x x) (f x)
{-# INLINE lensProgramCircuits #-}
lensProgramClients :: forall . Lens' Program (Set UserId)
lensProgramClients f (Program x
                              x
                              x
                              x
                              x
                              x) = fmap (\y -> Program x x x x x y) (f x)
{-# INLINE lensProgramClients #-}
lensProgramId :: forall . Lens' Program ProgramId
lensProgramId f (Program x
                         x
                         x
                         x
                         x
                         x) = fmap (\y -> Program y x x x x x) (f x)
{-# INLINE lensProgramId #-}
lensProgramNotes :: forall . Lens' Program ([(Text, Text)])
lensProgramNotes f (Program x
                            x
                            x
                            x
                            x
                            x) = fmap (\y -> Program x x y x x x) (f x)
{-# INLINE lensProgramNotes #-}
lensProgramTitle :: forall . Lens' Program Text
lensProgramTitle f (Program x
                            x
                            x
                            x
                            x
                            x) = fmap (\y -> Program x y x x x x) (f x)
{-# INLINE lensProgramTitle #-}
lensUnProgramId :: forall . Iso' ProgramId Integer
lensUnProgramId = iso (\(ProgramId x) -> x) ProgramId
{-# INLINE lensUnProgramId #-}
lensProgramViewId :: forall . Lens' ProgramView ProgramViewId
lensProgramViewId f (ProgramView x
                                 x
                                 x) = fmap (\y -> ProgramView y x x) (f x)
{-# INLINE lensProgramViewId #-}
lensProgramViewNoteList :: forall . Lens' ProgramView ([ViewNote])
lensProgramViewNoteList f (ProgramView x
                                       x
                                       x) = fmap (\y -> ProgramView x x y) (f x)
{-# INLINE lensProgramViewNoteList #-}
lensProgramViewProgram :: forall . Lens' ProgramView ProgramId
lensProgramViewProgram f (ProgramView x
                                      x
                                      x) = fmap (\y -> ProgramView x y x) (f x)
{-# INLINE lensProgramViewProgram #-}
lensUnProgramViewId :: forall . Iso' ProgramViewId Integer
lensUnProgramViewId = iso (\(ProgramViewId x) -> x) ProgramViewId
{-# INLINE lensUnProgramViewId #-}
lensTrainerActive :: forall . Lens' Trainer Bool
lensTrainerActive f (Trainer x
                             x
                             x
                             x) = fmap (\y -> Trainer x x y x) (f x)
{-# INLINE lensTrainerActive #-}
lensTrainerClients :: forall . Lens' Trainer (Set UserId)
lensTrainerClients f (Trainer x
                              x
                              x
                              x) = fmap (\y -> Trainer x x x y) (f x)
{-# INLINE lensTrainerClients #-}
lensTrainerId :: forall . Lens' Trainer UserId
lensTrainerId f (Trainer x
                         x
                         x
                         x) = fmap (\y -> Trainer y x x x) (f x)
{-# INLINE lensTrainerId #-}
lensTrainerName :: forall . Lens' Trainer Text
lensTrainerName f (Trainer x
                           x
                           x
                           x) = fmap (\y -> Trainer x y x x) (f x)
{-# INLINE lensTrainerName #-}
lensNoteName :: forall . Lens' ViewNote (Maybe Char)
lensNoteName f (ViewNote x x x) = fmap (\y -> ViewNote y x x) (f x)
{-# INLINE lensNoteName #-}
lensNoteOrder :: forall . Lens' ViewNote (Maybe Integer)
lensNoteOrder f (ViewNote x
                          x
                          x) = fmap (\y -> ViewNote x y x) (f x)
{-# INLINE lensNoteOrder #-}
lensNoteText :: forall . Lens' ViewNote ExerciseId
lensNoteText f (ViewNote x x x) = fmap (\y -> ViewNote x x y) (f x)
{-# INLINE lensNoteText #-}
