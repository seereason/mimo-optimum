instance ListForm App' Circuit
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: Circuit))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreateCircuit :: Attr AppText
                                                                                                                                                           (URL AppURL))) <: fromStringLit "create a new Circuit")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                             [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "12" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                         AppText)) <: fromStringLit "No Circuits found")))]
                                                                                                                                                                                                                                                                             _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                          then Just x
                                                                                                                                                                                                                                                                                                          else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Circuit Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Exercise") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Name") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Order") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Rest") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Intensity") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Reps") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Tempo") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Sets") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Circuit Total") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewCircuit ((\x -> view lensCircuitId x :: CircuitId) p)) :: Attr AppText
                                                                                                                                                   (URL AppURL))) <: asChild ((\x -> view lensCircuitId x :: CircuitId) p))),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitExercise x :: Maybe ExerciseId) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitName x :: Maybe Char) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitOrder x :: Maybe Integer) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitRest x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitIntensity x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitReps x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitTempo x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitSets x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensCircuitTotal x :: Text) p))]
instance ListForm App' Client
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: Client))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreateClient :: Attr AppText
                                                                                                                                                          (URL AppURL))) <: fromStringLit "register as a Client")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                            [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "5" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                       AppText)) <: fromStringLit "No Clients found")))]
                                                                                                                                                                                                                                                                            _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                         then Just x
                                                                                                                                                                                                                                                                                                         else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Client Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Client Name") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Client Active") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: asChild ((\x -> view lensClientId x :: UserId) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensClientName x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensClientActive x :: Bool) p))]
instance ListForm App' Exercise
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: Exercise))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreateExercise :: Attr AppText
                                                                                                                                                            (URL AppURL))) <: fromStringLit "create a new Exercise")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                               [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "6" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                          AppText)) <: fromStringLit "No Exercises found")))]
                                                                                                                                                                                                                                                                               _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                            then Just x
                                                                                                                                                                                                                                                                                                            else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Exercise Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Exercise Author") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Exercise Title") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewExercise ((\x -> view lensExerciseId x :: ExerciseId) p)) :: Attr AppText
                                                                                                                                                      (URL AppURL))) <: asChild ((\x -> view lensExerciseId x :: ExerciseId) p))),
                           asChild (elt "td" <: asChild ((\x -> view lensExerciseAuthor x :: UserId) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensExerciseTitle x :: Text) p))]
instance ListForm App' Program
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: Program))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreateProgram :: Attr AppText
                                                                                                                                                           (URL AppURL))) <: fromStringLit "create a new Program")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                             [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "8" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                        AppText)) <: fromStringLit "No Programs found")))]
                                                                                                                                                                                                                                                                             _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                          then Just x
                                                                                                                                                                                                                                                                                                          else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Program Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program Title") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program Notes") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program Circuits") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program Author") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program Clients") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewProgram ((\x -> view lensProgramId x :: ProgramId) p)) :: Attr AppText
                                                                                                                                                   (URL AppURL))) <: asChild ((\x -> view lensProgramId x :: ProgramId) p))),
                           asChild (elt "td" <: asChild ((\x -> view lensProgramTitle x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensProgramNotes x :: [(Text,
                                                                                             Text)]) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensProgramCircuits x :: [Circuit]) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensProgramAuthor x :: UserId) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensProgramClients x :: Set UserId) p))]
instance ListForm App' ProgramView
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: ProgramView))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreateProgramView :: Attr AppText
                                                                                                                                                               (URL AppURL))) <: fromStringLit "create a new ProgramView")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                                     [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "5" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                AppText)) <: fromStringLit "No ProgramViews found")))]
                                                                                                                                                                                                                                                                                     _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                                  then Just x
                                                                                                                                                                                                                                                                                                                  else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Program View Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program View Program") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Program View Note List") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewProgramView ((\x -> view lensProgramViewId x :: ProgramViewId) p)) :: Attr AppText
                                                                                                                                                               (URL AppURL))) <: asChild ((\x -> view lensProgramViewId x :: ProgramViewId) p))),
                           asChild (elt "td" <: ((elt "a" <@ ("href" := AppURL (ViewProgram ((\x -> view lensProgramViewProgram x :: ProgramId) p)) :: Attr AppText
                                                                                                                                                            (URL AppURL))) <: asChild ((\x -> view lensProgramViewProgram x :: ProgramId) p))),
                           asChild (elt "td" <: asChild ((\x -> view lensProgramViewNoteList x :: [ViewNote]) p))]
instance ListForm App' Trainer
    where listForm' xs = (fieldset $ (((\xs' t -> if t == pack "cancel"
                                                   then Nothing
                                                   else Just xs') <$> mapView (\rows -> [(elt "table" <: (([asChild (elt "th" <: fromStringLit "Select")] :: [GenChildList App']) ++ headers (undefined :: Trainer))) <: rows,
                                                                                         elt "hr",
                                                                                         elt "div" <: ((elt "a" <@ ("href" := AppURL CreateTrainer :: Attr AppText
                                                                                                                                                           (URL AppURL))) <: fromStringLit "register as a Trainer")]) (foldr (\xf xsf -> ((:) <$> xf) <*> xsf) (pure []) (case xs of
                                                                                                                                                                                                                                                                              [] -> [fmap (\(()) -> Nothing) (label (elt "tr" <: ((elt "td" <@ ("colspan" := "6" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                         AppText)) <: fromStringLit "No Trainers found")))]
                                                                                                                                                                                                                                                                              _ -> map (\x -> fmap (\b -> if b
                                                                                                                                                                                                                                                                                                           then Just x
                                                                                                                                                                                                                                                                                                           else Nothing) (mapView (\xml -> [elt "tr" <: xml]) $ (mapView (\xml -> [elt "td" <: xml]) (inputCheckbox False) <++ label (columns x)))) xs))) <*> (((\d c -> fromMaybe (fromMaybe (error "No button?") d) c) <$> inputSubmit "delete selected") <*> inputSubmit "cancel"))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . (Just . catMaybes)))
          headers _ = [asChild (elt "th" <: fromStringLit "Trainer Id") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Trainer Name") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Trainer Active") :: GenChildList App',
                       asChild (elt "th" <: fromStringLit "Trainer Clients") :: GenChildList App']
          columns = \p -> [asChild (elt "td" <: asChild ((\x -> view lensTrainerId x :: UserId) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensTrainerName x :: Text) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensTrainerActive x :: Bool) p)),
                           asChild (elt "td" <: asChild ((\x -> view lensTrainerClients x :: Set UserId) p))]
