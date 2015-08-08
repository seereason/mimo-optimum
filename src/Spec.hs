-- | This module is the authoritative source of all information
-- used to generate the Optimum web site.
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             RecordWildCards, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Spec where

import Distribution.License (License(..))
import Happstack.Authenticate.Core (UserId(UserId))
import MIMO.Base (version)
import MIMO.Spec (Spec(..))
import qualified Ports (optimum)
import Types

spec :: Spec
spec = Spec { siteName = "Optimum"
            , siteVersion = version "1.2.3"
            , siteHomepage = "homepage"
            , siteAuthor = "author"
            , siteLicense = PublicDomain
            , siteSynopsis = "synopsis"
            , siteDescription = "description"
            , siteOwner = UserId 1
            , siteDomain = "optimumhealth.com"
            , siteTestHost = Nothing
            , sitePorts = Ports.optimum
            , siteAdmin = "logic@seereason.com"
            , siteParent = "/srv"
            , siteBackupsDir = "/srv/backups"
            , siteBackupsUser = "upload"
            , siteRowTypes = [''Trainer, ''Client, ''Exercise, ''Program, ''Circuit, ''ProgramView, ''ViewNote]
            }
