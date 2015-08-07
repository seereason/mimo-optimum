route :: (AuthenticateURL ->
          RouteT AuthenticateURL (ServerPartT IO) Response) ->
         Text -> URL AppURL -> App Response
route routeAuthenticate _baseURL theurl = case theurl of
                                              Authenticate authURL -> lift $ (nestURL Authenticate $ (mapRouteT lift $ routeAuthenticate authURL))
                                              AppJs -> ok $ (toResponse $ appJs)
                                              CSS -> serveFile (asContentType "text/css") "style.css"
                                              JQuery -> serveDirectory DisableBrowsing [] "/usr/share/javascript/jquery/"
                                              JQueryUI -> serveDirectory DisableBrowsing [] "/usr/share/javascript/jquery-ui/"
                                              AppURL appurl -> case appurl of
                                                                   SomeTrainers -> listPage (undefined :: Trainer)
                                                                   ViewTrainer pid -> viewPage (undefined :: Trainer) pid
                                                                   CreateTrainer -> createPage (undefined :: Trainer)
                                                                   UpdateTrainer pid -> updatePage (undefined :: Trainer) pid
                                                                   SomeClients -> listPage (undefined :: Client)
                                                                   ViewClient pid -> viewPage (undefined :: Client) pid
                                                                   CreateClient -> createPage (undefined :: Client)
                                                                   UpdateClient pid -> updatePage (undefined :: Client) pid
                                                                   SomeExercises -> listPage (undefined :: Exercise)
                                                                   ViewExercise pid -> viewPage (undefined :: Exercise) pid
                                                                   CreateExercise -> createPage (undefined :: Exercise)
                                                                   UpdateExercise pid -> updatePage (undefined :: Exercise) pid
                                                                   SomePrograms -> listPage (undefined :: Program)
                                                                   ViewProgram pid -> viewPage (undefined :: Program) pid
                                                                   CreateProgram -> createPage (undefined :: Program)
                                                                   UpdateProgram pid -> updatePage (undefined :: Program) pid
                                                                   SomeCircuits -> listPage (undefined :: Circuit)
                                                                   ViewCircuit pid -> viewPage (undefined :: Circuit) pid
                                                                   CreateCircuit -> createPage (undefined :: Circuit)
                                                                   UpdateCircuit pid -> updatePage (undefined :: Circuit) pid
                                                                   SomeProgramViews -> listPage (undefined :: ProgramView)
                                                                   ViewProgramView pid -> viewPage (undefined :: ProgramView) pid
                                                                   CreateProgramView -> createPage (undefined :: ProgramView)
                                                                   UpdateProgramView pid -> updatePage (undefined :: ProgramView) pid
