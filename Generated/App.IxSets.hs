instance Indexable Trainer
    where empty = ixSet [Ix (empty :: forall . Map TrainerId
                                                   (Set Trainer)) (flattenWithCalcs noCalcs)]
type Trainers = IxSet Trainer
instance Indexable Client
    where empty = ixSet [Ix (empty :: forall . Map ClientId
                                                   (Set Client)) (flattenWithCalcs noCalcs)]
type Clients = IxSet Client
instance Indexable Exercise
    where empty = ixSet [Ix (empty :: forall . Map ExerciseId
                                                   (Set Exercise)) (flattenWithCalcs noCalcs)]
type Exercises = IxSet Exercise
instance Indexable Program
    where empty = ixSet [Ix (empty :: forall . Map ProgramId
                                                   (Set Program)) (flattenWithCalcs noCalcs)]
type Programs = IxSet Program
instance Indexable Circuit
    where empty = ixSet [Ix (empty :: forall . Map CircuitId
                                                   (Set Circuit)) (flattenWithCalcs noCalcs)]
type Circuits = IxSet Circuit
instance Indexable ProgramView
    where empty = ixSet [Ix (empty :: forall . Map ProgramViewId
                                                   (Set ProgramView)) (flattenWithCalcs noCalcs)]
type ProgramViews = IxSet ProgramView
