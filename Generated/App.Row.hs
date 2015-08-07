instance Row Trainer
    where type AppType Trainer = App
          type IdType Trainer = UserId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "Trainer") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeTrainers)) (\x -> do {xid <- update (CreateTrainerEvent x);
                                                                                                                                                                                                                                                                                                                                                                    seeOtherURL (AppURL (ViewTrainer xid))})) Nothing (createForm :: AppForm (Maybe Trainer))),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " "),
                                                                                                                                                                                 asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Trainer") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeTrainers)) (\x -> do {xid <- update (CreateTrainerEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                             seeOtherURL (AppURL (ViewTrainer xid))})) Nothing (createForm :: AppForm (Maybe Trainer))),
                                                                                                                                                                                                                                                                                                                                     asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomeTrainersEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetTrainerByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "Trainer") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeTrainers)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteTrainerEvent" ++ (" " ++ show (map trainerId xs'))));
                                                                                                                                                                                                                                                                                                                                                                       _ <- mapM (update . DeleteTrainerEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                       seeOtherURL (AppURL SomeTrainers)})) Nothing (listForm recent :: AppForm (Maybe ([Trainer])))),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit "Trainers:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeTrainers)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteTrainerEvent" ++ (" " ++ show (map trainerId xs'))));
                                                                                                                                                                                                                                                                                                                                                                             _ <- mapM (update . DeleteTrainerEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                             seeOtherURL (AppURL SomeTrainers)})) Nothing (listForm recent :: AppForm (Maybe ([Trainer])))),
                                                                                                                                                                                                                                                                                  asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetTrainerByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Trainer" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Trainer ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe Trainer -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomeTrainers)
                                                       success (Just x') = do {xid' <- update (UpdateTrainerEvent x');
                                                                               seeOtherURL (AppURL (ViewTrainer xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update Trainer ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      Trainer -> [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe Trainer))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "Trainer") () $ (asChild $ ([asChild menuList,
                                                                                                            asChild (fromStringLit " "),
                                                                                                            asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetTrainerByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Trainer" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Trainer ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Trainer" <> (" " <> (show $ (id $ (trainerId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                  AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                           AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                        AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateTrainer xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                           (URL AppURL))) <: fromStringLit ("update this " ++ "Trainer"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Trainer Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerId x :: UserId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Trainer Name:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerName x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Trainer Active:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerActive x :: Bool) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Trainer Clients:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensTrainerClients x :: Set UserId) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Trainer)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Trainer)
instance Row Client
    where type AppType Client = App
          type IdType Client = UserId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "Client") () $ asChild [genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeClients)) (\x -> do {xid <- update (CreateClientEvent x);
                                                                                                                                                                                                                                                                                                                                                                  seeOtherURL (AppURL (ViewClient xid))})) Nothing (createForm :: AppForm (Maybe Client))),
                                                                                                                                                                                                                                                                           asChild (fromStringLit " ")]) mUserId))],
                                                                              genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                                asChild (fromStringLit " "),
                                                                                                                                                                                asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Client") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeClients)) (\x -> do {xid <- update (CreateClientEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                          seeOtherURL (AppURL (ViewClient xid))})) Nothing (createForm :: AppForm (Maybe Client))),
                                                                                                                                                                                                                                                                                                                                   asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomeClientsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetClientByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "Client") () $ asChild [genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeClients)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteClientEvent" ++ (" " ++ show (map clientId xs'))));
                                                                                                                                                                                                                                                                                                                                                                     _ <- mapM (update . DeleteClientEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                     seeOtherURL (AppURL SomeClients)})) Nothing (listForm recent :: AppForm (Maybe ([Client])))),
                                                                                                                                                                                                                                                                           asChild (fromStringLit " ")]) mUserId))],
                                                                              genElement (Nothing,
                                                                                          fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " "),
                                                                                                                                                                                 asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                 asChild (elt "h1" <: fromStringLit "Clients:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeClients)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteClientEvent" ++ (" " ++ show (map clientId xs'))));
                                                                                                                                                                                                                                                                                                                                                                          _ <- mapM (update . DeleteClientEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                          seeOtherURL (AppURL SomeClients)})) Nothing (listForm recent :: AppForm (Maybe ([Client])))),
                                                                                                                                                                                                                                                                                asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetClientByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Client" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Client ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe Client -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomeClients)
                                                       success (Just x') = do {xid' <- update (UpdateClientEvent x');
                                                                               seeOtherURL (AppURL (ViewClient xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update Client ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      Client -> [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe Client))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "Client") () $ (asChild $ ([asChild menuList,
                                                                                                           asChild (fromStringLit " "),
                                                                                                           asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
          viewPage _ xid = do {method GET;
                               mRow <- query (GetClientByIdEvent xid);
                               case mRow of
                                   Nothing -> do {notFound ();
                                                  appTemplate ("Client" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Client ") <: asChild xid) <: fromStringLit " could not be found.")}
                                   Just x -> do {ok ();
                                                 appTemplate (appPack $ ("Client" <> (" " <> (show $ (id $ (clientId $ x)))))) () $ (((((elt "div" <@ ("class" := "row" :: Attr AppText
                                                                                                                                                                                AppText)) <: menuList) <: ((elt "dl" <@ ("class" := "row-header" :: Attr AppText
                                                                                                                                                                                                                                                         AppText)) <: mkDescList x)) <: ((elt "div" <@ ("class" := "row-body" :: Attr AppText
                                                                                                                                                                                                                                                                                                                                      AppText)) <: mkDiv x)) <: (elt "div" <: ((elt "a" <@ ("href" := AppURL (UpdateClient xid) :: Attr AppText
                                                                                                                                                                                                                                                                                                                                                                                                                                        (URL AppURL))) <: fromStringLit ("update this " ++ "Client"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Client Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensClientId x :: UserId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Client Name:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensClientName x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Client Active:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensClientActive x :: Bool) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Client)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Client)
instance Row Exercise
    where type AppType Exercise = App
          type IdType Exercise = ExerciseId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "Exercise") () $ asChild [genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                   asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeExercises)) (\x -> do {xid <- update (CreateExerciseEvent x);
                                                                                                                                                                                                                                                                                                                                                                      seeOtherURL (AppURL (ViewExercise xid))})) Nothing (createForm :: AppForm (Maybe Exercise))),
                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId))],
                                                                                genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Exercise") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeExercises)) (\x -> do {xid <- update (CreateExerciseEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                                seeOtherURL (AppURL (ViewExercise xid))})) Nothing (createForm :: AppForm (Maybe Exercise))),
                                                                                                                                                                                                                                                                                                                                       asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomeExercisesEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetExerciseByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "Exercise") () $ asChild [genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                   asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeExercises)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteExerciseEvent" ++ (" " ++ show (map exerciseId xs'))));
                                                                                                                                                                                                                                                                                                                                                                         _ <- mapM (update . DeleteExerciseEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                         seeOtherURL (AppURL SomeExercises)})) Nothing (listForm recent :: AppForm (Maybe ([Exercise])))),
                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId))],
                                                                                genElement (Nothing,
                                                                                            fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                   asChild (fromStringLit " "),
                                                                                                                                                                                   asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                   asChild (elt "h1" <: fromStringLit "Exercises:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeExercises)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteExerciseEvent" ++ (" " ++ show (map exerciseId xs'))));
                                                                                                                                                                                                                                                                                                                                                                                _ <- mapM (update . DeleteExerciseEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                                seeOtherURL (AppURL SomeExercises)})) Nothing (listForm recent :: AppForm (Maybe ([Exercise])))),
                                                                                                                                                                                                                                                                                    asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetExerciseByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Exercise" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Exercise ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe Exercise -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomeExercises)
                                                       success (Just x') = do {xid' <- update (UpdateExerciseEvent x');
                                                                               seeOtherURL (AppURL (ViewExercise xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update Exercise ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      Exercise ->
                                                                      [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe Exercise))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "Exercise") () $ (asChild $ ([asChild menuList,
                                                                                                             asChild (fromStringLit " "),
                                                                                                             asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                        (URL AppURL))) <: fromStringLit ("update this " ++ "Exercise"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Exercise Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensExerciseId x :: ExerciseId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Exercise Author:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensExerciseAuthor x :: UserId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Exercise Title:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensExerciseTitle x :: Text) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [\x -> asChild (doTextFormat (getTextFormat x) (appPack (unpack ((\x -> view lensExerciseText x :: Text) x))))] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Exercise)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Exercise)
instance Row Program
    where type AppType Program = App
          type IdType Program = ProgramId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "Program") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomePrograms)) (\x -> do {xid <- update (CreateProgramEvent x);
                                                                                                                                                                                                                                                                                                                                                                    seeOtherURL (AppURL (ViewProgram xid))})) Nothing (createForm :: AppForm (Maybe Program))),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " "),
                                                                                                                                                                                 asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Program") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomePrograms)) (\x -> do {xid <- update (CreateProgramEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                             seeOtherURL (AppURL (ViewProgram xid))})) Nothing (createForm :: AppForm (Maybe Program))),
                                                                                                                                                                                                                                                                                                                                     asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomeProgramsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetProgramByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "Program") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomePrograms)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteProgramEvent" ++ (" " ++ show (map programId xs'))));
                                                                                                                                                                                                                                                                                                                                                                       _ <- mapM (update . DeleteProgramEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                       seeOtherURL (AppURL SomePrograms)})) Nothing (listForm recent :: AppForm (Maybe ([Program])))),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit "Programs:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomePrograms)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteProgramEvent" ++ (" " ++ show (map programId xs'))));
                                                                                                                                                                                                                                                                                                                                                                             _ <- mapM (update . DeleteProgramEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                             seeOtherURL (AppURL SomePrograms)})) Nothing (listForm recent :: AppForm (Maybe ([Program])))),
                                                                                                                                                                                                                                                                                  asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetProgramByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Program" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Program ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe Program -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomePrograms)
                                                       success (Just x') = do {xid' <- update (UpdateProgramEvent x');
                                                                               seeOtherURL (AppURL (ViewProgram xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update Program ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      Program -> [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe Program))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "Program") () $ (asChild $ ([asChild menuList,
                                                                                                            asChild (fromStringLit " "),
                                                                                                            asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (URL AppURL))) <: fromStringLit ("update this " ++ "Program"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Program Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramId x :: ProgramId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program Title:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramTitle x :: Text) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program Notes:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramNotes x :: [(Text,
                                                                                                                                                     Text)]) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program Circuits:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramCircuits x :: [Circuit]) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program Author:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramAuthor x :: UserId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program Clients:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramClients x :: Set UserId) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Program)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Program)
instance Row Circuit
    where type AppType Circuit = App
          type IdType Circuit = CircuitId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "Circuit") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeCircuits)) (\x -> do {xid <- update (CreateCircuitEvent x);
                                                                                                                                                                                                                                                                                                                                                                    seeOtherURL (AppURL (ViewCircuit xid))})) Nothing (createForm :: AppForm (Maybe Circuit))),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                                 asChild (fromStringLit " "),
                                                                                                                                                                                 asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a Circuit") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeCircuits)) (\x -> do {xid <- update (CreateCircuitEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                             seeOtherURL (AppURL (ViewCircuit xid))})) Nothing (createForm :: AppForm (Maybe Circuit))),
                                                                                                                                                                                                                                                                                                                                     asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomeCircuitsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetCircuitByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "Circuit") () $ asChild [genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeCircuits)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteCircuitEvent" ++ (" " ++ show (map circuitId xs'))));
                                                                                                                                                                                                                                                                                                                                                                       _ <- mapM (update . DeleteCircuitEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                       seeOtherURL (AppURL SomeCircuits)})) Nothing (listForm recent :: AppForm (Maybe ([Circuit])))),
                                                                                                                                                                                                                                                                            asChild (fromStringLit " ")]) mUserId))],
                                                                               genElement (Nothing,
                                                                                           fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                  asChild (fromStringLit " "),
                                                                                                                                                                                  asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                  asChild (elt "h1" <: fromStringLit "Circuits:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeCircuits)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteCircuitEvent" ++ (" " ++ show (map circuitId xs'))));
                                                                                                                                                                                                                                                                                                                                                                             _ <- mapM (update . DeleteCircuitEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                             seeOtherURL (AppURL SomeCircuits)})) Nothing (listForm recent :: AppForm (Maybe ([Circuit])))),
                                                                                                                                                                                                                                                                                  asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetCircuitByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("Circuit" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "Circuit ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe Circuit -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomeCircuits)
                                                       success (Just x') = do {xid' <- update (UpdateCircuitEvent x');
                                                                               seeOtherURL (AppURL (ViewCircuit xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update Circuit ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      Circuit -> [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe Circuit))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "Circuit") () $ (asChild $ ([asChild menuList,
                                                                                                            asChild (fromStringLit " "),
                                                                                                            asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (URL AppURL))) <: fromStringLit ("update this " ++ "Circuit"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Circuit Id:"),
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
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensCircuitTotal x :: Text) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Circuit)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe Circuit)
instance Row ProgramView
    where type AppType ProgramView = App
          type IdType ProgramView = ProgramViewId
          createPage _ = do {here <- whereami;
                             mUserId <- currentUser;
                             appTemplate ("Add a " <> "ProgramView") () $ asChild [genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                      asChild (fromStringLit " ")] <> (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\x -> do {xid <- update (CreateProgramViewEvent x);
                                                                                                                                                                                                                                                                                                                                                                            seeOtherURL (AppURL (ViewProgramView xid))})) Nothing (createForm :: AppForm (Maybe ProgramView))),
                                                                                                                                                                                                                                                                                asChild (fromStringLit " ")]) mUserId))],
                                                                                   genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] [asChild $ ([asChild menuList,
                                                                                                                                                                                     asChild (fromStringLit " "),
                                                                                                                                                                                     asChild (elt "h1" <: fromStringLit (maybe "You Are Not Logged In" (const "Add a ProgramView") mUserId))] ++ maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "add" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\x -> do {xid <- update (CreateProgramViewEvent x);
                                                                                                                                                                                                                                                                                                                                                                                                                                         seeOtherURL (AppURL (ViewProgramView xid))})) Nothing (createForm :: AppForm (Maybe ProgramView))),
                                                                                                                                                                                                                                                                                                                                             asChild (fromStringLit " ")]) mUserId)]]}
          listPage _ = do {recent <- query (SomeProgramViewsEvent (20 :: Int) (0 :: Int));
                           liftIO $ logM "List" DEBUG ("EVENT " ++ ("GetProgramViewByIdEvent" ++ (" " ++ show recent)));
                           here <- whereami;
                           mUserId <- currentUser;
                           appTemplate ("List of " <> "ProgramView") () $ asChild [genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := False)] [asChild $ ([asChild menuList,
                                                                                                                                                                                      asChild (fromStringLit " ")] ++ (loginForm <> maybe [asChild (fromStringLit " ")] (\_ -> [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteProgramViewEvent" ++ (" " ++ show (map programViewId xs'))));
                                                                                                                                                                                                                                                                                                                                                                               _ <- mapM (update . DeleteProgramViewEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                               seeOtherURL (AppURL SomeProgramViews)})) Nothing (listForm recent :: AppForm (Maybe ([ProgramView])))),
                                                                                                                                                                                                                                                                                asChild (fromStringLit " ")]) mUserId))],
                                                                                   genElement (Nothing,
                                                                                               fromStringLit "div") [asAttr (fromStringLit "up-authenticated" := True)] ([asChild $ ([asChild menuList,
                                                                                                                                                                                      asChild (fromStringLit " "),
                                                                                                                                                                                      asChild (fromStringLit (show mUserId)),
                                                                                                                                                                                      asChild (elt "h1" <: fromStringLit "ProgramViews:")] <> maybe [asChild (fromStringLit " ")] (const [asChild (reform (form here) "list" (maybe (seeOtherURL (AppURL SomeProgramViews)) (\xs' -> do {liftIO $ logM "Delete" DEBUG ("EVENT " ++ ("DeleteProgramViewEvent" ++ (" " ++ show (map programViewId xs'))));
                                                                                                                                                                                                                                                                                                                                                                                         _ <- mapM (update . DeleteProgramViewEvent) xs';
                                                                                                                                                                                                                                                                                                                                                                                         seeOtherURL (AppURL SomeProgramViews)})) Nothing (listForm recent :: AppForm (Maybe ([ProgramView])))),
                                                                                                                                                                                                                                                                                          asChild (fromStringLit " ")]) mUserId)] <> logoutForm mUserId)]}
          updatePage _ xid = do {mRow <- query (GetProgramViewByIdEvent xid);
                                 case mRow of
                                     Nothing -> do {notFound ();
                                                    appTemplate ("ProgramView" <> " not found.") () $ ((((elt "p" <: menuList) <: fromStringLit "ProgramView ") <: asChild xid) <: fromStringLit " could not be found.")}
                                     Just x -> do {here <- whereami;
                                                   mUserId <- currentUser;
                                                   let success :: Maybe ProgramView -> App Response
                                                       success (Nothing) = seeOtherURL (AppURL SomeProgramViews)
                                                       success (Just x') = do {xid' <- update (UpdateProgramViewEvent x');
                                                                               seeOtherURL (AppURL (ViewProgramView xid'))}
                                                       updateHeader (Just _) = (elt "h1" <: fromStringLit "Update ProgramView ") <: asChild xid
                                                       updateHeader (Nothing) = elt "h1" <: fromStringLit "You Are Not Logged In"
                                                       updateForm' :: Maybe UserId ->
                                                                      ProgramView ->
                                                                      [GenChildList App']
                                                       updateForm' (Just _) x' = [asChild (reform (form here) "update" success Nothing (updateForm x' :: AppForm (Maybe ProgramView))),
                                                                                  asChild (fromStringLit " ")]
                                                       updateForm' (Nothing) _ = [asChild (fromStringLit " ")];
                                                   appTemplate ("Update a " <> "ProgramView") () $ (asChild $ ([asChild menuList,
                                                                                                                asChild (fromStringLit " "),
                                                                                                                asChild (updateHeader mUserId)] ++ updateForm' mUserId x))}}
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (URL AppURL))) <: fromStringLit ("update this " ++ "ProgramView"))))}
                                       where mkDescList x = map (\f -> f x) [\_ -> asChild (elt "dt" <: fromStringLit "Program View Id:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramViewId x :: ProgramViewId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program View Program:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramViewProgram x :: ProgramId) x)),
                                                                             \_ -> asChild (elt "dt" <: fromStringLit "Program View Note List:"),
                                                                             \x -> asChild (elt "dd" <: asChild ((\x -> view lensProgramViewNoteList x :: [ViewNote]) x))] :: [GenChildList App']
                                             mkDiv x = map (\f -> f x) [] :: [GenChildList App']}
          updateForm v = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                        then Nothing
                                                        else Just v') <$> appForm (Just v)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "update") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe ProgramView)
          createForm = (fieldset $ (ul $ (((\v' t -> if t == pack "cancel"
                                                      then Nothing
                                                      else Just v') <$> appForm (Just def)) <*> (((\u c -> fromMaybe (fromMaybe (error "No button pushed!") u) c) <$> inputSubmit "create") <*> inputSubmit "cancel")))) `transformEitherM` maybe (return $ (Right $ Nothing)) (return . (Right . Just)) :: AppForm (Maybe ProgramView)
