{-# LANGUAGE DeriveGeneric #-}

module Finance.IBAN.Germany.Core
  ( BLZRecord (..),
    BLZ,
    BIC,
    AccountNr,
    ChecksumType,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

type BIC = Text

type BLZ = Text

type AccountNr = Text

type ChecksumType = Text

data BLZRecord = BLZRecord
  { blz :: BLZ,
    blzMerkmal :: Int,
    blzDescription :: Text,
    blzPLZ :: Text,
    blzTown :: Text,
    blzShortDescription :: Text,
    blzPAN :: Text,
    blzBIC :: BIC,
    blzChecksumType :: ChecksumType,
    blzDatensatzNummer :: Int,
    blzAenderungsKennzeichen :: Text,
    blzLoeschung :: Text,
    blzNachfolgeBLZ :: Text
  }
  deriving (Eq, Read, Show, Ord, Generic)
