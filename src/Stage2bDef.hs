{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- We usually need the symbols from Stage1Def, but not always.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Stage2bDef where

import Stage1Def
import Stage1Gen (App)
import Stage2aDef
import Stage2aGen
