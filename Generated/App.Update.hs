instance Updatable App ((Text, Text))
    where updForm = \(Just (a,
                            b)) -> ul (((,) <$> ((li . (updForm . Just)) $ a)) <*> ((li . (updForm . Just)) $ b))
          updForm' = undefined . (\(Just (a,
                                          b)) -> ul (((,) <$> ((li . (updForm . Just)) $ a)) <*> ((li . (updForm . Just)) $ b)) :: Maybe ((Text,
                                                                                                                                           Text)) ->
                                                                                                                                   AppForm ((Text,
                                                                                                                                             Text))) :: Maybe ((Text,
                                                                                                                                                                Text)) ->
                                                                                                                                                        App (AppForm ((Text,
                                                                                                                                                                       Text)))
instance Updatable App (Maybe Char)
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm (Maybe Char)
          updForm' = pure . updForm
instance Updatable App (Maybe Integer)
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm (Maybe Integer)
          updForm' = pure . updForm
instance Updatable App (Maybe ExerciseId)
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x)) (label (asChild (asChild (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                               0) (0,
                                                                                                                                                                                                   0)))) x))) :: AppForm ()) :: AppForm (Maybe ExerciseId)
          updForm' = undefined . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                             0) (0,
                                                                                                                 0)))) x)) (label (asChild (asChild (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                             0) (0,
                                                                                                                                                                                                                 0)))) x))) :: AppForm ()) :: AppForm (Maybe ExerciseId) :: Maybe (Maybe ExerciseId) ->
                                                                                                                                                                                                                                                                            AppForm (Maybe ExerciseId)) :: Maybe (Maybe ExerciseId) ->
                                                                                                                                                                                                                                                                                                           App (AppForm (Maybe ExerciseId))
instance Updatable App (Set UserId)
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x) :: () ->
                                                                                                               Set UserId) (label (asChild ((((((elt "div" <@ ("style" := "font-size:10px" :: Attr AppText
                                                                                                                                                                                                   AppText)) <: fromStringLit "unsupported container type Data.Set.Base.Set") <: elt "br") <: fromStringLit ("type: " ++ "Data.Set.Base.Set Happstack.Authenticate.Core.UserId")) <: elt "br") <: fromStringLit ("value: " ++ show (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            0) (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                0)))) x)))) :: AppForm ()) :: Maybe (Set UserId) ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              AppForm (Set UserId)
          updForm' = undefined . (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                             0) (0,
                                                                                                                 0)))) x) :: () ->
                                                                                                                             Set UserId) (label (asChild ((((((elt "div" <@ ("style" := "font-size:10px" :: Attr AppText
                                                                                                                                                                                                                 AppText)) <: fromStringLit "unsupported container type Data.Set.Base.Set") <: elt "br") <: fromStringLit ("type: " ++ "Data.Set.Base.Set Happstack.Authenticate.Core.UserId")) <: elt "br") <: fromStringLit ("value: " ++ show (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          0) (0,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0)))) x)))) :: AppForm ()) :: Maybe (Set UserId) ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            AppForm (Set UserId) :: Maybe (Set UserId) ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AppForm (Set UserId)) :: Maybe (Set UserId) ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             App (AppForm (Set UserId))
instance Updatable App ([(Text, Text)])
    where updForm = \(Just xs) -> let {elements :: [AppForm ([(Text,
                                                               Text)])];
                                       elements = map (li . (fmap (: []) . (updForm . Just))) xs;
                                       list :: AppForm ([(Text, Text)]);
                                       list = ol $ (case elements of
                                                        [] -> fmap (const []) (label (asChild (fromStringLit "(empty list)")))
                                                        _ -> seqA elements);
                                       list' = ((\xs' t -> if t
                                                            then xs' ++ [def]
                                                            else xs') <$> list) <*> mapView (\form -> [(elt "span" <: form) <: fromStringLit "Append new element"]) (inputCheckbox False)}
                                   in list'
          updForm' = pure . updForm
instance Updatable App ([Circuit])
    where updForm = \(Just xs) -> let {elements :: [AppForm ([Circuit])];
                                       elements = map (li . (fmap (: []) . (updForm . Just))) xs;
                                       list :: AppForm ([Circuit]);
                                       list = ol $ (case elements of
                                                        [] -> fmap (const []) (label (asChild (fromStringLit "(empty list)")))
                                                        _ -> seqA elements);
                                       list' = ((\xs' t -> if t
                                                            then xs' ++ [def]
                                                            else xs') <$> list) <*> mapView (\form -> [(elt "span" <: form) <: fromStringLit "Append new element"]) (inputCheckbox False)}
                                   in list'
          updForm' = pure . updForm
instance Updatable App ([ViewNote])
    where updForm = \(Just xs) -> let {elements :: [AppForm ([ViewNote])];
                                       elements = map (li . (fmap (: []) . (updForm . Just))) xs;
                                       list :: AppForm ([ViewNote]);
                                       list = ol $ (case elements of
                                                        [] -> fmap (const []) (label (asChild (fromStringLit "(empty list)")))
                                                        _ -> seqA elements);
                                       list' = ((\xs' t -> if t
                                                            then xs' ++ [def]
                                                            else xs') <$> list) <*> mapView (\form -> [(elt "span" <: form) <: fromStringLit "Append new element"]) (inputCheckbox False)}
                                   in list'
          updForm' = pure . updForm
instance Updatable App Char
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm Char
          updForm' = pure . updForm
instance Updatable App Int
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm Int
          updForm' = pure . updForm
instance Updatable App Integer
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm Integer
          updForm' = pure . updForm
instance Updatable App Circuit
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Circuit"] <> v) ((\(Just (Circuit _a₁
                                                                                                         _a₂
                                                                                                         _a₃
                                                                                                         _a₄
                                                                                                         _a₅
                                                                                                         _a₆
                                                                                                         _a₇
                                                                                                         _a₈
                                                                                                         _a₉
                                                                                                         _a₁₀)) -> (ul :: AppForm Circuit ->
                                                                                                                          AppForm Circuit) ((((((((((pure Circuit <*> (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                                                                      0)))) x)) (inputHidden empty :: AppForm Text)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Exercise" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Name" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Order" ++ ": ")) <: form]) (updForm x)) (Just _a₄)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Rest" ++ ": ")) <: form]) (updForm x)) (Just _a₅)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Intensity" ++ ": ")) <: form]) (updForm x)) (Just _a₆)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Reps" ++ ": ")) <: form]) (updForm x)) (Just _a₇)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Tempo" ++ ": ")) <: form]) (updForm x)) (Just _a₈)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Sets" ++ ": ")) <: form]) (updForm x)) (Just _a₉)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Circuit Total" ++ ": ")) <: form]) (updForm x)) (Just _a₁₀)) :: Maybe Circuit ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       AppForm Circuit) x) :: Maybe Circuit ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              AppForm Circuit
          updForm' = pure . updForm
instance Updatable App CircuitId
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x)) (inputHidden empty :: AppForm Text)
          updForm' = pure . updForm
instance Updatable App Client
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Client"] <> v) ((\(Just (Client _a₁
                                                                                                       _a₂
                                                                                                       _a₃)) -> (ul :: AppForm Client ->
                                                                                                                       AppForm Client) (((pure Client <*> (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                      0) (0,
                                                                                                                                                                                                                                          0)))) x)) (inputHidden empty :: AppForm Text)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Client Name" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Client Active" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) :: Maybe Client ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AppForm Client) x) :: Maybe Client ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          AppForm Client
          updForm' = pure . updForm
instance Updatable App Exercise
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Exercise"] <> v) ((\(Just (Exercise _a₁
                                                                                                           _a₂
                                                                                                           _a₃
                                                                                                           _a₄)) -> (ul :: AppForm Exercise ->
                                                                                                                           AppForm Exercise) ((((pure Exercise <*> (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                               0) (0,
                                                                                                                                                                                                                                                   0)))) x)) (inputHidden empty :: AppForm Text)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Exercise Author" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Exercise Title" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Exercise Text" ++ ": ")) <: form]) (updForm x)) (Just _a₄)) :: Maybe Exercise ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         AppForm Exercise) x) :: Maybe Exercise ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 AppForm Exercise
          updForm' = pure . updForm
instance Updatable App ExerciseId
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x)) (inputHidden empty :: AppForm Text)
          updForm' = pure . updForm
instance Updatable App Program
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Program"] <> v) ((\(Just (Program _a₁
                                                                                                         _a₂
                                                                                                         _a₃
                                                                                                         _a₄
                                                                                                         _a₅
                                                                                                         _a₆)) -> (ul :: AppForm Program ->
                                                                                                                         AppForm Program) ((((((pure Program <*> (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                             0) (0,
                                                                                                                                                                                                                                                 0)))) x)) (inputHidden empty :: AppForm Text)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program Title" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program Notes" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program Circuits" ++ ": ")) <: form]) (updForm x)) (Just _a₄)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program Author" ++ ": ")) <: form]) (updForm x)) (Just _a₅)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program Clients" ++ ": ")) <: form]) (updForm x)) (Just _a₆)) :: Maybe Program ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        AppForm Program) x) :: Maybe Program ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               AppForm Program
          updForm' = pure . updForm
instance Updatable App ProgramId
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x)) (inputHidden empty :: AppForm Text)
          updForm' = pure . updForm
instance Updatable App ProgramView
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Program View"] <> v) ((\(Just (ProgramView _a₁
                                                                                                                  _a₂
                                                                                                                  _a₃)) -> (ul :: AppForm ProgramView ->
                                                                                                                                  AppForm ProgramView) (((pure ProgramView <*> (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                                           0) (0,
                                                                                                                                                                                                                                                               0)))) x)) (inputHidden empty :: AppForm Text)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program View Program" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Program View Note List" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) :: Maybe ProgramView ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           AppForm ProgramView) x) :: Maybe ProgramView ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      AppForm ProgramView
          updForm' = pure . updForm
instance Updatable App ProgramViewId
    where updForm = \x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                               0) (0,
                                                                                                   0)))) x)) (inputHidden empty :: AppForm Text)
          updForm' = pure . updForm
instance Updatable App Trainer
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "Trainer"] <> v) ((\(Just (Trainer _a₁
                                                                                                         _a₂
                                                                                                         _a₃
                                                                                                         _a₄)) -> (ul :: AppForm Trainer ->
                                                                                                                         AppForm Trainer) ((((pure Trainer <*> (\x -> fmap (const (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                                                           0) (0,
                                                                                                                                                                                                                                               0)))) x)) (inputHidden empty :: AppForm Text)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Trainer Name" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Trainer Active" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Trainer Clients" ++ ": ")) <: form]) (updForm x)) (Just _a₄)) :: Maybe Trainer ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AppForm Trainer) x) :: Maybe Trainer ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           AppForm Trainer
          updForm' = pure . updForm
instance Updatable App ViewNote
    where updForm = \x -> mapView (\v -> [elt "span" <: fromStringLit "View Note"] <> v) ((\(Just (ViewNote _a₁
                                                                                                            _a₂
                                                                                                            _a₃)) -> (ul :: AppForm ViewNote ->
                                                                                                                            AppForm ViewNote) (((pure ViewNote <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Note Name" ++ ": ")) <: form]) (updForm x)) (Just _a₁)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Note Order" ++ ": ")) <: form]) (updForm x)) (Just _a₂)) <*> (\x -> mapView (\form -> [(elt "li" <: fromStringLit ("Note Text" ++ ": ")) <: form]) (updForm x)) (Just _a₃)) :: Maybe ViewNote ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            AppForm ViewNote) x) :: Maybe ViewNote ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AppForm ViewNote
          updForm' = pure . updForm
instance Updatable App Text
    where updForm x = (inputText (unparse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                   0) (0,
                                                                                                       0)))) x)) `transformEither` parse (fromMaybe (error ("fromJust at " ++ show (Loc "" "" "" (0,
                                                                                                                                                                                                  0) (0,
                                                                                                                                                                                                      0)))) x)) <++ errorList :: AppForm Text
          updForm' = pure . updForm
