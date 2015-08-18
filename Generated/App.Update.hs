instance Updatable App ((Text, Text))
    where updForm = \(Just (a,
                            b)) -> ul <$> (\a1 a2 -> fmap (\a1 a2 -> fmap (,) a1 <*> a2) a1 <*> a2) (li <$> updForm (Just a)) (li <$> updForm (Just b))
instance Updatable App (Maybe Char)
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm (Maybe Char))
instance Updatable App (Maybe Integer)
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm (Maybe Integer))
instance Updatable App (Maybe ExerciseId)
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x)) (label (asChild (asChild (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                       0) (0,
                                                                                                                                                                                                           0)))) x))) :: AppForm ()) :: AppForm (Maybe ExerciseId))
instance Updatable App (Set UserId)
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x) :: () ->
                                                                                                                       Set UserId) (label (asChild ((((((elt "div" <@ ("style" := "font-size:10px" :: Attr AppText
                                                                                                                                                                                                           AppText)) <: fromStringLit "unsupported container type Data.Set.Base.Set") <: elt "br") <: fromStringLit ("type: " ++ "Data.Set.Base.Set Happstack.Authenticate.Core.UserId")) <: elt "br") <: fromStringLit ("value: " ++ show (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    0) (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        0)))) x)))) :: AppForm ()) :: Maybe (Set UserId) ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      AppForm (Set UserId))
instance Updatable App ([(Text, Text)])
    where updForm = \(Just xs) -> do {let elements' :: [App (AppForm ([(Text,
                                                                        Text)]))]
                                          elements' = map (\x -> updForm (Just x) >>= (return . (li . fmap (: [])))) xs;
                                      elements <- sequence elements' :: App ([AppForm ([(Text,
                                                                                         Text)])]);
                                      list <- case elements of
                                                  [] -> pure $ fmap (const []) (label (asChild (fromStringLit "(empty list)")))
                                                  _ -> pure (seqA elements) >>= (return . ol) :: App (AppForm ([(Text,
                                                                                                                 Text)]));
                                      list' <- pure $ (((\xs' t -> if t
                                                                    then xs' ++ [def]
                                                                    else xs') <$> list) <*> mapView (\form -> [(elt "span" <: form) <: fromStringLit "Append new element"]) (inputCheckbox False));
                                      return list'}
instance Updatable App ([Circuit])
    where updForm = \(Just xs) -> do {let elements' :: [App (AppForm ([Circuit]))]
                                          elements' = map (\x -> updForm (Just x) >>= (return . (li . fmap (: [])))) xs;
                                      elements <- sequence elements' :: App ([AppForm ([Circuit])]);
                                      list <- case elements of
                                                  [] -> pure $ fmap (const []) (label (asChild (fromStringLit "(empty list)")))
                                                  _ -> pure (seqA elements) >>= (return . ol) :: App (AppForm ([Circuit]));
                                      list' <- pure $ (((\xs' t -> if t
                                                                    then xs' ++ [def]
                                                                    else xs') <$> list) <*> mapView (\form -> [(elt "span" <: form) <: fromStringLit "Append new element"]) (inputCheckbox False));
                                      return list'}
instance Updatable App ([ViewNote])
    where updForm = \(Just xs) -> do {let elements' :: [App (AppForm ([ViewNote]))]
                                          elements' = map (\x -> updForm (Just x) >>= (return . (li . fmap (: [])))) xs;
                                      elements <- sequence elements' :: App ([AppForm ([ViewNote])]);
                                      list <- case elements of
                                                  [] -> pure $ fmap (const []) (label (asChild (fromStringLit "(empty list)")))
                                                  _ -> pure (seqA elements) >>= (return . ol) :: App (AppForm ([ViewNote]));
                                      list' <- pure $ (((\xs' t -> if t
                                                                    then xs' ++ [def]
                                                                    else xs') <$> list) <*> mapView (\form -> [(elt "span" <: form) <: fromStringLit "Append new element"]) (inputCheckbox False));
                                      return list'}
instance Updatable App Char
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm Char)
instance Updatable App Int
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm Int)
instance Updatable App Integer
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm Integer)
instance Updatable App Circuit
    where updForm = \x -> (\(Just (Circuit _a₁
                                           _a₂
                                           _a₃
                                           _a₄
                                           _a₅
                                           _a₆
                                           _a₇
                                           _a₈
                                           _a₉
                                           _a₁₀)) -> (ul :: AppForm Circuit ->
                                                            AppForm Circuit) <$> (\a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> ((((((((fmap (\a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> ((((((((fmap Circuit a1 <*> a2) <*> a3) <*> a4) <*> a5) <*> a6) <*> a7) <*> a8) <*> a9) <*> a10) a1 <*> a2) <*> a3) <*> a4) <*> a5) <*> a6) <*> a7) <*> a8) <*> a9) <*> a10) ((pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                            0)))) x)) (inputHidden empty :: AppForm Text))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Exercise" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Name" ++ ": ")) <: form]))) (Just _a₃)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Order" ++ ": ")) <: form]))) (Just _a₄)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Rest" ++ ": ")) <: form]))) (Just _a₅)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Intensity" ++ ": ")) <: form]))) (Just _a₆)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Reps" ++ ": ")) <: form]))) (Just _a₇)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Tempo" ++ ": ")) <: form]))) (Just _a₈)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Sets" ++ ": ")) <: form]))) (Just _a₉)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Total" ++ ": ")) <: form]))) (Just _a₁₀))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Circuit"] <> v)) :: Maybe Circuit ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       App (AppForm Circuit)
instance Updatable App CircuitId
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x)) (inputHidden empty :: AppForm Text))
instance Updatable App Client
    where updForm = \x -> (\(Just (Client _a₁
                                          _a₂
                                          _a₃)) -> (ul :: AppForm Client ->
                                                          AppForm Client) <$> (\a1 a2 a3 -> (fmap (\a1 a2 a3 -> (fmap Client a1 <*> a2) <*> a3) a1 <*> a2) <*> a3) ((pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                                                                            0)))) x)) (inputHidden empty :: AppForm Text))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Client Name" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Client Active" ++ ": ")) <: form]))) (Just _a₃))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Client"] <> v)) :: Maybe Client ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         App (AppForm Client)
instance Updatable App Exercise
    where updForm = \x -> (\(Just (Exercise _a₁
                                            _a₂
                                            _a₃
                                            _a₄)) -> (ul :: AppForm Exercise ->
                                                            AppForm Exercise) <$> (\a1 a2 a3 a4 -> ((fmap (\a1 a2 a3 a4 -> ((fmap Exercise a1 <*> a2) <*> a3) <*> a4) a1 <*> a2) <*> a3) <*> a4) ((pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                      0) (0,
                                                                                                                                                                                                                                                                                          0)))) x)) (inputHidden empty :: AppForm Text))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Exercise Author" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Exercise Title" ++ ": ")) <: form]))) (Just _a₃)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Exercise Text" ++ ": ")) <: form]))) (Just _a₄))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Exercise"] <> v)) :: Maybe Exercise ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               App (AppForm Exercise)
instance Updatable App ExerciseId
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x)) (inputHidden empty :: AppForm Text))
instance Updatable App Program
    where updForm = \x -> (\(Just (Program _a₁
                                           _a₂
                                           _a₃
                                           _a₄
                                           _a₅
                                           _a₆)) -> (ul :: AppForm Program ->
                                                           AppForm Program) <$> (\a1 a2 a3 a4 a5 a6 -> ((((fmap (\a1 a2 a3 a4 a5 a6 -> ((((fmap Program a1 <*> a2) <*> a3) <*> a4) <*> a5) <*> a6) a1 <*> a2) <*> a3) <*> a4) <*> a5) <*> a6) ((pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                                                                   0) (0,
                                                                                                                                                                                                                                                                                                                                       0)))) x)) (inputHidden empty :: AppForm Text))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program Title" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program Notes" ++ ": ")) <: form]))) (Just _a₃)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program Circuits" ++ ": ")) <: form]))) (Just _a₄)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program Author" ++ ": ")) <: form]))) (Just _a₅)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program Clients" ++ ": ")) <: form]))) (Just _a₆))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Program"] <> v)) :: Maybe Program ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                App (AppForm Program)
instance Updatable App ProgramId
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x)) (inputHidden empty :: AppForm Text))
instance Updatable App ProgramView
    where updForm = \x -> (\(Just (ProgramView _a₁
                                               _a₂
                                               _a₃)) -> (ul :: AppForm ProgramView ->
                                                               AppForm ProgramView) <$> (\a1 a2 a3 -> (fmap (\a1 a2 a3 -> (fmap ProgramView a1 <*> a2) <*> a3) a1 <*> a2) <*> a3) ((pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                       0) (0,
                                                                                                                                                                                                                                                                           0)))) x)) (inputHidden empty :: AppForm Text))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program View Program" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Program View Note List" ++ ": ")) <: form]))) (Just _a₃))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Program View"] <> v)) :: Maybe ProgramView ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                App (AppForm ProgramView)
instance Updatable App ProgramViewId
    where updForm = pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                       0) (0,
                                                                                                           0)))) x)) (inputHidden empty :: AppForm Text))
instance Updatable App Trainer
    where updForm = \x -> (\(Just (Trainer _a₁
                                           _a₂
                                           _a₃
                                           _a₄)) -> (ul :: AppForm Trainer ->
                                                           AppForm Trainer) <$> (\a1 a2 a3 a4 -> ((fmap (\a1 a2 a3 a4 -> ((fmap Trainer a1 <*> a2) <*> a3) <*> a4) a1 <*> a2) <*> a3) <*> a4) ((pure . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                   0) (0,
                                                                                                                                                                                                                                                                                       0)))) x)) (inputHidden empty :: AppForm Text))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Trainer Name" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Trainer Active" ++ ": ")) <: form]))) (Just _a₃)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Trainer Clients" ++ ": ")) <: form]))) (Just _a₄))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "Trainer"] <> v)) :: Maybe Trainer ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          App (AppForm Trainer)
instance Updatable App ViewNote
    where updForm = \x -> (\(Just (ViewNote _a₁
                                            _a₂
                                            _a₃)) -> (ul :: AppForm ViewNote ->
                                                            AppForm ViewNote) <$> (\a1 a2 a3 -> (fmap (\a1 a2 a3 -> (fmap ViewNote a1 <*> a2) <*> a3) a1 <*> a2) <*> a3) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Note Name" ++ ": ")) <: form]))) (Just _a₁)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Note Order" ++ ": ")) <: form]))) (Just _a₂)) ((\x -> updForm x >>= (return . mapView (\form -> [(elt "li" <: fromStringLit ("Note Text" ++ ": ")) <: form]))) (Just _a₃))) x >>= (return . mapView (\v -> [elt "span" <: fromStringLit "View Note"] <> v)) :: Maybe ViewNote ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     App (AppForm ViewNote)
instance Updatable App Text
    where updForm x = pure ((inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                         0) (0,
                                                                                                             0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                        0) (0,
                                                                                                                                                                                                            0)))) x)) <++ errorList :: AppForm Text)
