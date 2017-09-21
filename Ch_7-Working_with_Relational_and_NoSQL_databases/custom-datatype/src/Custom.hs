{-# LANGUAGE TemplateHaskell #-}
module Custom where

import Database.Persist.TH
import Text.Email.Validate

data Status = Active | Inactive
            deriving (Show, Eq, Read)

derivePersistField "Status"

derivePersistField "EmailAddress"
