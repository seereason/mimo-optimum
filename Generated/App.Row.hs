instance Row Trainer
    where type AppType Trainer = App
          type AppType' Trainer = App'
          type AppURLType Trainer = URL AppURL
          type AppFormType Trainer = AppForm
          type IdType Trainer = TrainerId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetTrainerByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomeTrainers)) (\x -> do {xid <- update (CreateTrainerEvent x);
                                                                                                            seeOtherURL (AppURL (ViewTrainer xid))})) Nothing (createForm' frm :: AppFormType Trainer
                                                                                                                                                                                              (Maybe Trainer))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomeTrainersEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetTrainerByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomeTrainers)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteTrainerEvent" ++ (" " ++ show (map trainerId xs'))));
                                                                                                             _ <- mapM (update . DeleteTrainerEvent) xs';
                                                                                                             seeOtherURL (AppURL SomeTrainers)})) Nothing (listForm' recent :: AppFormType Trainer
                                                                                                                                                                                           (Maybe ([Trainer])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetTrainerByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomeTrainers)) (\x' -> do {xid' <- update (UpdateTrainerEvent x');
                                                                                                                seeOtherURL (AppURL (ViewTrainer xid'))})) Nothing (updateForm' frm :: AppFormType Trainer
                                                                                                                                                                                                   (Maybe Trainer))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "Trainer") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] <> loginForm)],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " "),
                                                                                                                                                                                asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Trainer") mUserId)),
                                                                                                                                                                                asChild (createForm (undefined :: Trainer) :: AppType Trainer
                                                                                                                                                                                                                                      ([AppType Trainer
                                                                                                                                                                                                                                                XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "Trainer") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: Trainer)),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit "Trainers:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: Trainer)),
                                                                                                                                                                                                                                                                                  asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetTrainerByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Trainer" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Trainer ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "Trainer") () $ (asChild $ ([asChild menuList,
                                                                                                            asChild (fromStringLit " "),
                                                                                                            asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update Trainer ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetTrainerByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Trainer" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Trainer ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Trainer" <> (" " <> (show $ (unTrainerId $ (trainerId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                           AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                                    AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                 AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateTrainer xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (AppURLType Trainer))) <: fromStringLit ("update this " ++ "Trainer"))))}
                                       where mkDescList x' = map (\f -> f x') [\_ -> asChild (elt "dt" <: fromStringLit "Trainer Id:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerId x :: TrainerId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Trainer Name:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerName x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Trainer Active:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerActive x :: Bool) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Trainer Clients:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerClients x :: Set UserId) x))] :: [GenChildList (AppType' Trainer)]
                                             mkDiv x' = map (\f -> f x') [] :: [GenChildList (AppType' Trainer)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Trainer
                                                                                                 Trainer)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Trainer
                                                                                                                                                                                                                                                                                                                                       (Maybe Trainer)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Trainer
                                                                                                 Trainer)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Trainer
                                                                                                                                                                                                                                                                                                                                       (Maybe Trainer)
instance Row Client
    where type AppType Client = App
          type AppType' Client = App'
          type AppURLType Client = URL AppURL
          type AppFormType Client = AppForm
          type IdType Client = ClientId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetClientByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomeClients)) (\x -> do {xid <- update (CreateClientEvent x);
                                                                                                           seeOtherURL (AppURL (ViewClient xid))})) Nothing (createForm' frm :: AppFormType Client
                                                                                                                                                                                            (Maybe Client))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomeClientsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetClientByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomeClients)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteClientEvent" ++ (" " ++ show (map clientId xs'))));
                                                                                                            _ <- mapM (update . DeleteClientEvent) xs';
                                                                                                            seeOtherURL (AppURL SomeClients)})) Nothing (listForm' recent :: AppFormType Client
                                                                                                                                                                                         (Maybe ([Client])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetClientByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomeClients)) (\x' -> do {xid' <- update (UpdateClientEvent x');
                                                                                                               seeOtherURL (AppURL (ViewClient xid'))})) Nothing (updateForm' frm :: AppFormType Client
                                                                                                                                                                                                 (Maybe Client))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "Client") () $ asChild [genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " ")] <> loginForm)],
                                                                              genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                               asChild (fromStringLit " "),
                                                                                                                                                                               asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Client") mUserId)),
                                                                                                                                                                               asChild (createForm (undefined :: Client) :: AppType Client
                                                                                                                                                                                                                                    ([AppType Client
                                                                                                                                                                                                                                              XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "Client") () $ asChild [genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: Client)),
                                                                                                                                                                                                                                                                           asChild (fromStringLit " ")]) mUserId))],
                                                                              genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " "),
                                                                                                                                                                                 asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                 asChild (elt "h1" <: fromStringLit "Clients:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: Client)),
                                                                                                                                                                                                                                                                                asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetClientByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Client" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Client ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "Client") () $ (asChild $ ([asChild menuList,
                                                                                                           asChild (fromStringLit " "),
                                                                                                           asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update Client ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                           asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetClientByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Client" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Client ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Client" <> (" " <> (show $ (unClientId $ (clientId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                        AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                                 AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                              AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateClient xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                                (AppURLType Client))) <: fromStringLit ("update this " ++ "Client"))))}
                                       where mkDescList x' = map (\f -> f x') [\_ -> asChild (elt "dt" <: fromStringLit "Client Id:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensClientId x :: ClientId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Client Name:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensClientName x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Client Active:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensClientActive x :: Bool) x))] :: [GenChildList (AppType' Client)]
                                             mkDiv x' = map (\f -> f x') [] :: [GenChildList (AppType' Client)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Client
                                                                                                 Client)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Client
                                                                                                                                                                                                                                                                                                                                      (Maybe Client)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Client
                                                                                                 Client)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Client
                                                                                                                                                                                                                                                                                                                                      (Maybe Client)
instance Row Exercise
    where type AppType Exercise = App
          type AppType' Exercise = App'
          type AppURLType Exercise = URL AppURL
          type AppFormType Exercise = AppForm
          type IdType Exercise = ExerciseId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetExerciseByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomeExercises)) (\x -> do {xid <- update (CreateExerciseEvent x);
                                                                                                             seeOtherURL (AppURL (ViewExercise xid))})) Nothing (createForm' frm :: AppFormType Exercise
                                                                                                                                                                                                (Maybe Exercise))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomeExercisesEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetExerciseByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomeExercises)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteExerciseEvent" ++ (" " ++ show (map exerciseId xs'))));
                                                                                                              _ <- mapM (update . DeleteExerciseEvent) xs';
                                                                                                              seeOtherURL (AppURL SomeExercises)})) Nothing (listForm' recent :: AppFormType Exercise
                                                                                                                                                                                             (Maybe ([Exercise])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetExerciseByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomeExercises)) (\x' -> do {xid' <- update (UpdateExerciseEvent x');
                                                                                                                 seeOtherURL (AppURL (ViewExercise xid'))})) Nothing (updateForm' frm :: AppFormType Exercise
                                                                                                                                                                                                     (Maybe Exercise))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "Exercise") () $ asChild [genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                   asChild (fromStringLit " ")] <> loginForm)],
                                                                                genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " "),
                                                                                                                                                                                 asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Exercise") mUserId)),
                                                                                                                                                                                 asChild (createForm (undefined :: Exercise) :: AppType Exercise
                                                                                                                                                                                                                                        ([AppType Exercise
                                                                                                                                                                                                                                                  XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "Exercise") () $ asChild [genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                   asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: Exercise)),
                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId))],
                                                                                genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                   asChild (fromStringLit " "),
                                                                                                                                                                                   asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                   asChild (elt "h1" <: fromStringLit "Exercises:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: Exercise)),
                                                                                                                                                                                                                                                                                    asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetExerciseByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Exercise" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Exercise ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "Exercise") () $ (asChild $ ([asChild menuList,
                                                                                                             asChild (fromStringLit " "),
                                                                                                             asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update Exercise ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                               asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetExerciseByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Exercise" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Exercise ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Exercise" <> (" " <> (show $ (unExerciseId $ (exerciseId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                              AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                                       AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                    AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateExercise xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                                        (AppURLType Exercise))) <: fromStringLit ("update this " ++ "Exercise"))))}
                                       where mkDescList x' = map (\f -> f x') [\_ -> asChild (elt "dt" <: fromStringLit "Exercise Id:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensExerciseId x :: ExerciseId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Exercise Author:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensExerciseAuthor x :: TrainerId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Exercise Title:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensExerciseTitle x :: Text) x))] :: [GenChildList (AppType' Exercise)]
                                             mkDiv x' = map (\f -> f x') [\x -> asChild (doTextFormat (getTextFormat x) (appPack (unpack ((\x -> view lensExerciseText x :: Text) x))))] :: [GenChildList (AppType' Exercise)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Exercise
                                                                                                 Exercise)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Exercise
                                                                                                                                                                                                                                                                                                                                        (Maybe Exercise)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Exercise
                                                                                                 Exercise)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Exercise
                                                                                                                                                                                                                                                                                                                                        (Maybe Exercise)
instance Row Program
    where type AppType Program = App
          type AppType' Program = App'
          type AppURLType Program = URL AppURL
          type AppFormType Program = AppForm
          type IdType Program = ProgramId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetProgramByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomePrograms)) (\x -> do {xid <- update (CreateProgramEvent x);
                                                                                                            seeOtherURL (AppURL (ViewProgram xid))})) Nothing (createForm' frm :: AppFormType Program
                                                                                                                                                                                              (Maybe Program))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomeProgramsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetProgramByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomePrograms)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteProgramEvent" ++ (" " ++ show (map programId xs'))));
                                                                                                             _ <- mapM (update . DeleteProgramEvent) xs';
                                                                                                             seeOtherURL (AppURL SomePrograms)})) Nothing (listForm' recent :: AppFormType Program
                                                                                                                                                                                           (Maybe ([Program])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetProgramByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomePrograms)) (\x' -> do {xid' <- update (UpdateProgramEvent x');
                                                                                                                seeOtherURL (AppURL (ViewProgram xid'))})) Nothing (updateForm' frm :: AppFormType Program
                                                                                                                                                                                                   (Maybe Program))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "Program") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] <> loginForm)],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " "),
                                                                                                                                                                                asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Program") mUserId)),
                                                                                                                                                                                asChild (createForm (undefined :: Program) :: AppType Program
                                                                                                                                                                                                                                      ([AppType Program
                                                                                                                                                                                                                                                XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "Program") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: Program)),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit "Programs:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: Program)),
                                                                                                                                                                                                                                                                                  asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetProgramByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Program" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Program ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "Program") () $ (asChild $ ([asChild menuList,
                                                                                                            asChild (fromStringLit " "),
                                                                                                            asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update Program ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetProgramByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Program" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Program ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Program" <> (" " <> (show $ (unProgramId $ (programId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                           AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                                    AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                 AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateProgram xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (AppURLType Program))) <: fromStringLit ("update this " ++ "Program"))))}
                                       where mkDescList x' = map (\f -> f x') [\_ -> asChild (elt "dt" <: fromStringLit "Program Id:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramId x :: ProgramId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program Title:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramTitle x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program Notes:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramNotes x :: [(Text,
                                                                                                                                                       Text)]) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program Circuits:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramCircuits x :: [Circuit]) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program Author:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramAuthor x :: TrainerId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program Clients:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramClients x :: Set UserId) x))] :: [GenChildList (AppType' Program)]
                                             mkDiv x' = map (\f -> f x') [] :: [GenChildList (AppType' Program)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Program
                                                                                                 Program)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Program
                                                                                                                                                                                                                                                                                                                                       (Maybe Program)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Program
                                                                                                 Program)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Program
                                                                                                                                                                                                                                                                                                                                       (Maybe Program)
instance Row Circuit
    where type AppType Circuit = App
          type AppType' Circuit = App'
          type AppURLType Circuit = URL AppURL
          type AppFormType Circuit = AppForm
          type IdType Circuit = CircuitId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetCircuitByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomeCircuits)) (\x -> do {xid <- update (CreateCircuitEvent x);
                                                                                                            seeOtherURL (AppURL (ViewCircuit xid))})) Nothing (createForm' frm :: AppFormType Circuit
                                                                                                                                                                                              (Maybe Circuit))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomeCircuitsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetCircuitByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomeCircuits)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteCircuitEvent" ++ (" " ++ show (map circuitId xs'))));
                                                                                                             _ <- mapM (update . DeleteCircuitEvent) xs';
                                                                                                             seeOtherURL (AppURL SomeCircuits)})) Nothing (listForm' recent :: AppFormType Circuit
                                                                                                                                                                                           (Maybe ([Circuit])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetCircuitByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomeCircuits)) (\x' -> do {xid' <- update (UpdateCircuitEvent x');
                                                                                                                seeOtherURL (AppURL (ViewCircuit xid'))})) Nothing (updateForm' frm :: AppFormType Circuit
                                                                                                                                                                                                   (Maybe Circuit))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "Circuit") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] <> loginForm)],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " "),
                                                                                                                                                                                asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Circuit") mUserId)),
                                                                                                                                                                                asChild (createForm (undefined :: Circuit) :: AppType Circuit
                                                                                                                                                                                                                                      ([AppType Circuit
                                                                                                                                                                                                                                                XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "Circuit") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: Circuit)),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit "Circuits:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: Circuit)),
                                                                                                                                                                                                                                                                                  asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetCircuitByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Circuit" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Circuit ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "Circuit") () $ (asChild $ ([asChild menuList,
                                                                                                            asChild (fromStringLit " "),
                                                                                                            asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update Circuit ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetCircuitByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Circuit" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Circuit ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Circuit" <> (" " <> (show $ (unCircuitId $ (circuitId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                           AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                                    AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                 AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateCircuit xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (AppURLType Circuit))) <: fromStringLit ("update this " ++ "Circuit"))))}
                                       where mkDescList x' = map (\f -> f x') [\_ -> asChild (elt "dt" <: fromStringLit "Circuit Id:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitId x :: CircuitId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Exercise:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitExercise x :: Maybe ExerciseId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Name:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitName x :: Maybe Char) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Order:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitOrder x :: Maybe Integer) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Rest:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitRest x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Intensity:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitIntensity x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Reps:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitReps x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Tempo:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitTempo x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Sets:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitSets x :: Text) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Circuit Total:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitTotal x :: Text) x))] :: [GenChildList (AppType' Circuit)]
                                             mkDiv x' = map (\f -> f x') [] :: [GenChildList (AppType' Circuit)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Circuit
                                                                                                 Circuit)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Circuit
                                                                                                                                                                                                                                                                                                                                       (Maybe Circuit)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType Circuit
                                                                                                 Circuit)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType Circuit
                                                                                                                                                                                                                                                                                                                                       (Maybe Circuit)
instance Row ProgramView
    where type AppType ProgramView = App
          type AppType' ProgramView = App'
          type AppURLType ProgramView = URL AppURL
          type AppFormType ProgramView = AppForm
          type IdType ProgramView = ProgramViewId
          createForm _ = do {here <- whereami;
                             frm <- updForm (Just def);
                             liftIO $ logM "Create" DEBUG ("EVENT " ++ "GetProgramViewByIdEvent");
                             reform (form here) "add" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\x -> do {xid <- update (CreateProgramViewEvent x);
                                                                                                                seeOtherURL (AppURL (ViewProgramView xid))})) Nothing (createForm' frm :: AppFormType ProgramView
                                                                                                                                                                                                      (Maybe ProgramView))}
          listForm _ = do {here <- whereami;
                           recent <- query (SomeProgramViewsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetProgramViewByIdEvent" ++ (" " ++ show recent)));
                           reform (form here) "list" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteProgramViewEvent" ++ (" " ++ show (map programViewId xs'))));
                                                                                                                 _ <- mapM (update . DeleteProgramViewEvent) xs';
                                                                                                                 seeOtherURL (AppURL SomeProgramViews)})) Nothing (listForm' recent :: AppFormType ProgramView
                                                                                                                                                                                                   (Maybe ([ProgramView])))}
          updateForm x = do {here <- whereami;
                             liftIO $ logM "Update" DEBUG ("EVENT " ++ ("GetProgramViewByIdEvent" ++ (" " ++ show x)));
                             frm <- updForm (Just x);
                             reform (form here) "update" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\x' -> do {xid' <- update (UpdateProgramViewEvent x');
                                                                                                                    seeOtherURL (AppURL (ViewProgramView xid'))})) Nothing (updateForm' frm :: AppFormType ProgramView
                                                                                                                                                                                                           (Maybe ProgramView))}
          createPage _ = do {mUserId <- currentUser;
                             appTemplate ("Add a " <> "ProgramView") () $ asChild [genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                      asChild (fromStringLit " ")] <> loginForm)],
                                                                                   genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ [asChild menuList,
                                                                                                                                                                                    asChild (fromStringLit " "),
                                                                                                                                                                                    asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a ProgramView") mUserId)),
                                                                                                                                                                                    asChild (createForm (undefined :: ProgramView) :: AppType ProgramView
                                                                                                                                                                                                                                              ([AppType ProgramView
                                                                                                                                                                                                                                                        XML]))]]]}
          listPage _ = do {mUserId <- currentUser;
                           appTemplate ("List of " <> "ProgramView") () $ asChild [genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                      asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (listForm (undefined :: ProgramView)),
                                                                                                                                                                                                                                                                                asChild (fromStringLit " ")]) mUserId))],
                                                                                   genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                      asChild (fromStringLit " "),
                                                                                                                                                                                      asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                      asChild (elt "h1" <: fromStringLit "ProgramViews:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (listForm (undefined :: ProgramView)),
                                                                                                                                                                                                                                                                                          asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetProgramViewByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("ProgramView" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "ProgramView ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {mUserId <- currentUser;
                                                   appTemplate ("Update a " <> "ProgramView") () $ (asChild $ ([asChild menuList,
                                                                                                                asChild (fromStringLit " "),
                                                                                                                asChild (maybe (elt "h1" <: fromStringLit "You Are Not Logged In") (\_ -> (elt "h1" <: fromStringLit "Update ProgramView ") <: asChild xid) mUserId)] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (updateForm x),
                                                                                                                                                                                                                                                                                                                     asChild (fromStringLit " ")]) mUserId))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetProgramViewByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("ProgramView" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "ProgramView ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("ProgramView" <> (" " <> (show $ (unProgramViewId $ (programViewId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                                       AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                                                AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                             AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateProgramView xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (AppURLType ProgramView))) <: fromStringLit ("update this " ++ "ProgramView"))))}
                                       where mkDescList x' = map (\f -> f x') [\_ -> asChild (elt "dt" <: fromStringLit "Program View Id:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramViewId x :: ProgramViewId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program View Program:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramViewProgram x :: ProgramId) x)),
                                                                               \_ -> asChild (elt "dt" <: fromStringLit "Program View Note List:"),
                                                                               \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramViewNoteList x :: [ViewNote]) x))] :: [GenChildList (AppType' ProgramView)]
                                             mkDiv x' = map (\f -> f x') [] :: [GenChildList (AppType' ProgramView)]}
          updateForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType ProgramView
                                                                                                 ProgramView)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType ProgramView
                                                                                                                                                                                                                                                                                                                                           (Maybe ProgramView)
          createForm' frm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                           then Nothing
                                                           else Just v') <$> (frm :: AppFormType ProgramView
                                                                                                 ProgramView)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppFormType ProgramView
                                                                                                                                                                                                                                                                                                                                           (Maybe ProgramView)
