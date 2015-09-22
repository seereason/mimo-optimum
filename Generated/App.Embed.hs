instance EmbedAsChild App' ((Text, Text))
    where asChild (a,
                   b) = asChild ((elt "ol" <@ ("class" := "tuple2" :: Attr AppText
                                                                           AppText)) <<: [asChild (elt "li" <: asChild a),
                                                                                          asChild (elt "li" <: asChild b)]) :: GenChildList App'
instance EmbedAsChild App' (Maybe Char)
    where asChild x = maybe (asChild (fromStringLit "-")) (\y -> asChild y) x :: GenChildList App'
instance EmbedAsChild App' (Maybe Integer)
    where asChild x = maybe (asChild (fromStringLit "-")) (\y -> asChild y) x :: GenChildList App'
instance EmbedAsChild App' (Maybe ExerciseId)
    where asChild x = maybe (asChild (fromStringLit "-")) (\y -> asChild y) x :: GenChildList App'
instance EmbedAsChild App' (Set UserId)
    where asChild x = asChild ((elt "ol" <@ ("class" := "set" :: Attr AppText
                                                                      AppText)) <<: map (\y -> asChild (elt "li" <: asChild y)) (toAscList x)) :: GenChildList App'
instance EmbedAsChild App' ([Circuit])
    where asChild x = asChild ((elt "ol" <@ ("class" := "list" :: Attr AppText
                                                                       AppText)) <<: map (\y -> asChild (elt "li" <: asChild y)) x) :: GenChildList App'
instance EmbedAsChild App' ([ViewNote])
    where asChild x = asChild ((elt "ol" <@ ("class" := "list" :: Attr AppText
                                                                       AppText)) <<: map (\y -> asChild (elt "li" <: asChild y)) x) :: GenChildList App'
instance EmbedAsChild App' Bool
    where asChild x = asChild (show x) :: GenChildList App'
instance EmbedAsChild App' Circuit
    where asChild x = (\(Circuit _a₁
                                 _a₂
                                 _a₃
                                 _a₄
                                 _a₅
                                 _a₆
                                 _a₇
                                 _a₈
                                 _a₉
                                 _a₁₀) -> asChild ((elt "ul" <@ ("class" := "Circuit.Circuit" :: Attr AppText
                                                                                                      AppText)) <<: (map (\(fname,
                                                                                                                            x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                                  AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                         "_a\8322",
                                                                                                                                                                                                         "_a\8323",
                                                                                                                                                                                                         "_a\8324",
                                                                                                                                                                                                         "_a\8325",
                                                                                                                                                                                                         "_a\8326",
                                                                                                                                                                                                         "_a\8327",
                                                                                                                                                                                                         "_a\8328",
                                                                                                                                                                                                         "_a\8329",
                                                                                                                                                                                                         "_a\8321\8320"] [asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitId" :: Attr AppText
                                                                                                                                                                                                                                                                                                   AppText)) <: fromStringLit "Circuit Id: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitExercise" :: Attr AppText
                                                                                                                                                                                                                                                                                                         AppText)) <: fromStringLit "Circuit Exercise: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitName" :: Attr AppText
                                                                                                                                                                                                                                                                                                     AppText)) <: fromStringLit "Circuit Name: ") <: (asChild _a₃ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitOrder" :: Attr AppText
                                                                                                                                                                                                                                                                                                      AppText)) <: fromStringLit "Circuit Order: ") <: (asChild _a₄ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitRest" :: Attr AppText
                                                                                                                                                                                                                                                                                                     AppText)) <: fromStringLit "Circuit Rest: ") <: (asChild _a₅ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitIntensity" :: Attr AppText
                                                                                                                                                                                                                                                                                                          AppText)) <: fromStringLit "Circuit Intensity: ") <: (asChild _a₆ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitReps" :: Attr AppText
                                                                                                                                                                                                                                                                                                     AppText)) <: fromStringLit "Circuit Reps: ") <: (asChild _a₇ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitTempo" :: Attr AppText
                                                                                                                                                                                                                                                                                                      AppText)) <: fromStringLit "Circuit Tempo: ") <: (asChild _a₈ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitSets" :: Attr AppText
                                                                                                                                                                                                                                                                                                     AppText)) <: fromStringLit "Circuit Sets: ") <: (asChild _a₉ :: GenChildList App')),
                                                                                                                                                                                                                          asChild (((elt "span" <@ ("class" := "Circuit.Circuit.circuitTotal" :: Attr AppText
                                                                                                                                                                                                                                                                                                      AppText)) <: fromStringLit "Circuit Total: ") <: (asChild _a₁₀ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' CircuitId
    where asChild x = (\(CircuitId _a₁) -> asChild ((elt "span" <@ ("class" := "CircuitId.CircuitId._a\8321" :: Attr AppText
                                                                                                                     AppText)) <: asChild (((elt "span" <@ ("class" := "CircuitId.CircuitId.unCircuitId" :: Attr AppText
                                                                                                                                                                                                                 AppText)) <: fromStringLit "Un Circuit Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
instance EmbedAsChild App' Client
    where asChild x = (\(Client _a₁
                                _a₂
                                _a₃) -> asChild ((elt "ul" <@ ("class" := "Client.Client" :: Attr AppText
                                                                                                  AppText)) <<: (map (\(fname,
                                                                                                                        x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                              AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                     "_a\8322",
                                                                                                                                                                                                     "_a\8323"] [asChild (((elt "span" <@ ("class" := "Client.Client.clientId" :: Attr AppText
                                                                                                                                                                                                                                                                                       AppText)) <: fromStringLit "Client Id: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                 asChild (((elt "span" <@ ("class" := "Client.Client.clientName" :: Attr AppText
                                                                                                                                                                                                                                                                                         AppText)) <: fromStringLit "Client Name: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                 asChild (((elt "span" <@ ("class" := "Client.Client.clientActive" :: Attr AppText
                                                                                                                                                                                                                                                                                           AppText)) <: fromStringLit "Client Active: ") <: (asChild _a₃ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' ClientId
    where asChild x = (\(ClientId _a₁) -> asChild ((elt "span" <@ ("class" := "ClientId.ClientId._a\8321" :: Attr AppText
                                                                                                                  AppText)) <: asChild (((elt "span" <@ ("class" := "ClientId.ClientId.unClientId" :: Attr AppText
                                                                                                                                                                                                           AppText)) <: fromStringLit "Un Client Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
instance EmbedAsChild App' Exercise
    where asChild x = (\(Exercise _a₁
                                  _a₂
                                  _a₃
                                  _a₄) -> asChild ((elt "ul" <@ ("class" := "Exercise.Exercise" :: Attr AppText
                                                                                                        AppText)) <<: (map (\(fname,
                                                                                                                              x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                                    AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                           "_a\8322",
                                                                                                                                                                                                           "_a\8323",
                                                                                                                                                                                                           "_a\8324"] [asChild (((elt "span" <@ ("class" := "Exercise.Exercise.exerciseId" :: Attr AppText
                                                                                                                                                                                                                                                                                                   AppText)) <: fromStringLit "Exercise Id: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                       asChild (((elt "span" <@ ("class" := "Exercise.Exercise.exerciseAuthor" :: Attr AppText
                                                                                                                                                                                                                                                                                                       AppText)) <: fromStringLit "Exercise Author: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                       asChild (((elt "span" <@ ("class" := "Exercise.Exercise.exerciseTitle" :: Attr AppText
                                                                                                                                                                                                                                                                                                      AppText)) <: fromStringLit "Exercise Title: ") <: (asChild _a₃ :: GenChildList App')),
                                                                                                                                                                                                                       asChild (((elt "span" <@ ("class" := "Exercise.Exercise.exerciseText" :: Attr AppText
                                                                                                                                                                                                                                                                                                     AppText)) <: fromStringLit "Exercise Text: ") <: (asChild _a₄ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' ExerciseId
    where asChild x = (\(ExerciseId _a₁) -> asChild ((elt "span" <@ ("class" := "ExerciseId.ExerciseId._a\8321" :: Attr AppText
                                                                                                                        AppText)) <: asChild (((elt "span" <@ ("class" := "ExerciseId.ExerciseId.unExerciseId" :: Attr AppText
                                                                                                                                                                                                                       AppText)) <: fromStringLit "Un Exercise Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
instance EmbedAsChild App' Program
    where asChild x = (\(Program _a₁
                                 _a₂
                                 _a₃
                                 _a₄
                                 _a₅
                                 _a₆) -> asChild ((elt "ul" <@ ("class" := "Program.Program" :: Attr AppText
                                                                                                     AppText)) <<: (map (\(fname,
                                                                                                                           x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                                 AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                        "_a\8322",
                                                                                                                                                                                                        "_a\8323",
                                                                                                                                                                                                        "_a\8324",
                                                                                                                                                                                                        "_a\8325",
                                                                                                                                                                                                        "_a\8326"] [asChild (((elt "span" <@ ("class" := "Program.Program.programId" :: Attr AppText
                                                                                                                                                                                                                                                                                             AppText)) <: fromStringLit "Program Id: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Program.Program.programTitle" :: Attr AppText
                                                                                                                                                                                                                                                                                                AppText)) <: fromStringLit "Program Title: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Program.Program.programNotes" :: Attr AppText
                                                                                                                                                                                                                                                                                                AppText)) <: fromStringLit "Program Notes: ") <: (asChild _a₃ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Program.Program.programCircuits" :: Attr AppText
                                                                                                                                                                                                                                                                                                   AppText)) <: fromStringLit "Program Circuits: ") <: (asChild _a₄ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Program.Program.programAuthor" :: Attr AppText
                                                                                                                                                                                                                                                                                                 AppText)) <: fromStringLit "Program Author: ") <: (asChild _a₅ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Program.Program.programClients" :: Attr AppText
                                                                                                                                                                                                                                                                                                  AppText)) <: fromStringLit "Program Clients: ") <: (asChild _a₆ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' ProgramId
    where asChild x = (\(ProgramId _a₁) -> asChild ((elt "span" <@ ("class" := "ProgramId.ProgramId._a\8321" :: Attr AppText
                                                                                                                     AppText)) <: asChild (((elt "span" <@ ("class" := "ProgramId.ProgramId.unProgramId" :: Attr AppText
                                                                                                                                                                                                                 AppText)) <: fromStringLit "Un Program Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
instance EmbedAsChild App' ProgramView
    where asChild x = (\(ProgramView _a₁
                                     _a₂
                                     _a₃) -> asChild ((elt "ul" <@ ("class" := "ProgramView.ProgramView" :: Attr AppText
                                                                                                                 AppText)) <<: (map (\(fname,
                                                                                                                                       x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                                             AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                                    "_a\8322",
                                                                                                                                                                                                                    "_a\8323"] [asChild (((elt "span" <@ ("class" := "ProgramView.ProgramView.programViewId" :: Attr AppText
                                                                                                                                                                                                                                                                                                                     AppText)) <: fromStringLit "Program View Id: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                                asChild (((elt "span" <@ ("class" := "ProgramView.ProgramView.programViewProgram" :: Attr AppText
                                                                                                                                                                                                                                                                                                                          AppText)) <: fromStringLit "Program View Program: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                                asChild (((elt "span" <@ ("class" := "ProgramView.ProgramView.programViewNoteList" :: Attr AppText
                                                                                                                                                                                                                                                                                                                           AppText)) <: fromStringLit "Program View Note List: ") <: (asChild _a₃ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' ProgramViewId
    where asChild x = (\(ProgramViewId _a₁) -> asChild ((elt "span" <@ ("class" := "ProgramViewId.ProgramViewId._a\8321" :: Attr AppText
                                                                                                                                 AppText)) <: asChild (((elt "span" <@ ("class" := "ProgramViewId.ProgramViewId.unProgramViewId" :: Attr AppText
                                                                                                                                                                                                                                         AppText)) <: fromStringLit "Un Program View Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
instance EmbedAsChild App' Trainer
    where asChild x = (\(Trainer _a₁
                                 _a₂
                                 _a₃
                                 _a₄) -> asChild ((elt "ul" <@ ("class" := "Trainer.Trainer" :: Attr AppText
                                                                                                     AppText)) <<: (map (\(fname,
                                                                                                                           x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                                 AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                        "_a\8322",
                                                                                                                                                                                                        "_a\8323",
                                                                                                                                                                                                        "_a\8324"] [asChild (((elt "span" <@ ("class" := "Trainer.Trainer.trainerId" :: Attr AppText
                                                                                                                                                                                                                                                                                             AppText)) <: fromStringLit "Trainer Id: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Trainer.Trainer.trainerName" :: Attr AppText
                                                                                                                                                                                                                                                                                               AppText)) <: fromStringLit "Trainer Name: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Trainer.Trainer.trainerActive" :: Attr AppText
                                                                                                                                                                                                                                                                                                 AppText)) <: fromStringLit "Trainer Active: ") <: (asChild _a₃ :: GenChildList App')),
                                                                                                                                                                                                                    asChild (((elt "span" <@ ("class" := "Trainer.Trainer.trainerClients" :: Attr AppText
                                                                                                                                                                                                                                                                                                  AppText)) <: fromStringLit "Trainer Clients: ") <: (asChild _a₄ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' TrainerId
    where asChild x = (\(TrainerId _a₁) -> asChild ((elt "span" <@ ("class" := "TrainerId.TrainerId._a\8321" :: Attr AppText
                                                                                                                     AppText)) <: asChild (((elt "span" <@ ("class" := "TrainerId.TrainerId.unTrainerId" :: Attr AppText
                                                                                                                                                                                                                 AppText)) <: fromStringLit "Un Trainer Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
instance EmbedAsChild App' ViewNote
    where asChild x = (\(ViewNote _a₁
                                  _a₂
                                  _a₃) -> asChild ((elt "ul" <@ ("class" := "ViewNote.ViewNote" :: Attr AppText
                                                                                                        AppText)) <<: (map (\(fname,
                                                                                                                              x) -> asChild ((elt "li" <@ ("class" := fname :: Attr AppText
                                                                                                                                                                                    AppText)) <: x)) (zip ["_a\8321",
                                                                                                                                                                                                           "_a\8322",
                                                                                                                                                                                                           "_a\8323"] [asChild (((elt "span" <@ ("class" := "ViewNote.ViewNote.noteName" :: Attr AppText
                                                                                                                                                                                                                                                                                                 AppText)) <: fromStringLit "Note Name: ") <: (asChild _a₁ :: GenChildList App')),
                                                                                                                                                                                                                       asChild (((elt "span" <@ ("class" := "ViewNote.ViewNote.noteOrder" :: Attr AppText
                                                                                                                                                                                                                                                                                                  AppText)) <: fromStringLit "Note Order: ") <: (asChild _a₂ :: GenChildList App')),
                                                                                                                                                                                                                       asChild (((elt "span" <@ ("class" := "ViewNote.ViewNote.noteText" :: Attr AppText
                                                                                                                                                                                                                                                                                                 AppText)) <: fromStringLit "Note Text: ") <: (asChild _a₃ :: GenChildList App'))]) :: [GenChildList App']))) x :: GenChildList App'
instance EmbedAsChild App' UserId
    where asChild x = (\(UserId _a₁) -> asChild ((elt "span" <@ ("class" := "UserId.UserId._a\8321" :: Attr AppText
                                                                                                            AppText)) <: asChild (((elt "span" <@ ("class" := "UserId.UserId._unUserId" :: Attr AppText
                                                                                                                                                                                                AppText)) <: fromStringLit "_un User Id: ") <: (asChild _a₁ :: GenChildList App')))) x :: GenChildList App'
