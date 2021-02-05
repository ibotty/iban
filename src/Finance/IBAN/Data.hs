{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Finance.IBAN.Data
  (structures
  , isCompliant
  , countryP
  , ibanStrP
  , parseStructures
  , ibanStructureByCountry
  , IBANStricture(..)
  , BBANStructure
  , toIBANElementP
  , StructElem
  , elemP
  , uniqueBBANStructures
  ) where

import Data.Text (Text, pack)
import Data.Set (Set, fromList, member)
import Data.Attoparsec.Text as P
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.ISO3166_CountryCodes (CountryCode)
import Data.Char (isLetter, toUpper)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M (Map, fromList, lookup, elems)
import Debug.Trace (traceShowId)
import GHC.Stack (HasCallStack)
import Data.Either (fromRight)

structures :: [Text]
structures = [ "AL2!n8!n16!c"
             , "AD2!n4!n4!n12!c"
             , "AT2!n5!n11!n"
             , "AZ2!n4!a20!c"
             , "BH2!n4!a14!c"
             , "BE2!n3!n7!n2!n"
             , "BA2!n3!n3!n8!n2!n"
             , "BR2!n8!n5!n10!n1!a1!c"
             , "BG2!n4!a4!n2!n8!c"
             , "CR2!n3!n14!n"
             , "HR2!n7!n10!n"
             , "CY2!n3!n5!n16!c"
             , "CZ2!n4!n6!n10!n"
             , "FI2!n6!n7!n1!n"
             , "FR2!n5!n5!n11!c2!n"
             , "GE2!n2!a16!n"
             , "DE2!n8!n10!n"
             , "GI2!n4!a15!c"
             , "GR2!n3!n4!n16!c"
             , "GT2!n4!c20!c"
             , "HU2!n3!n4!n1!n15!n1!n"
             , "IS2!n4!n2!n6!n10!n"
             , "IE2!n4!a6!n8!n"
             , "IL2!n3!n3!n13!n"
             , "IT2!n1!a5!n5!n12!c"
             , "KW2!n4!a22!n"
             , "LV2!n4!a13!c"
             , "LB2!n4!n20!c"
             , "LI2!n5!n12!c"
             , "LT2!n5!n11!n"
             , "LU2!n3!n13!c"
             , "MK2!n3!n10!c2!n"
             , "MT2!n4!a5!n18!c"
             , "MR2!n5!n5!n11!n2!n"
             , "MU2!n4!a2!n2!n12!n3!n3!a"
             , "MD2!n20!c"
             , "MC2!n5!n5!n11!c2!n"
             , "ME2!n3!n13!n2!n"
             , "NL2!n4!a10!n"
             , "NO2!n4!n6!n1!n"
             , "PK2!n4!a16!c"
             , "PS2!n4!a21!c"
             , "PL2!n8!n16n"
             , "PT2!n4!n4!n11!n2!n"
             , "RO2!n4!a16!c"
             , "QA2!n4!a21!c"
             , "SM2!n1!a5!n5!n12!c"
             , "SA2!n2!n18!c"
             , "RS2!n3!n13!n2!n"
             , "SK2!n4!n6!n10!n"
             , "SI2!n5!n8!n2!n"
             , "ES2!n4!n4!n1!n1!n10!n"
             , "SE2!n3!n16!n1!n"
             , "CH2!n5!n12!c"
             , "TN2!n2!n3!n13!n2!n"
             , "TR2!n5!n1!c16!c"
             , "AE2!n3!n16!n"
             , "GB2!n4!a6!n8!n"
             , "VG2!n4!a16!n"
             , "DK2!n4!n9!n1!n"
             , "FO2!n4!n9!n1!n"
             , "GL2!n4!n9!n1!n"
             , "DO2!n4!c20!n"
             , "EE2!n2!n2!n11!n1!n"
             , "KZ2!n3!n13!c"
             , "JO2!n4!a4!n18!c"
             ]
             
-- |Checks if character belongs to subset used by IBAN REGISTRY to describe IBAN structure
isCompliant :: Char -> Bool
isCompliant = (`member` compliantChars)

n :: [Char]
n = ['0'..'9']  
a :: [Char]
a = ['A'..'Z' ] 
c :: [Char]
c = n ++ a ++ ['a' .. 'z'] 

compliantChars :: Set Char
compliantChars = fromList $ c ++ [' ']

data Len = Fixed !Int | Max !Int deriving (Eq, Ord, Show)

data StructElem = StructElem { len :: !Len, repr :: !Char } deriving (Eq, Ord, Show)
type BBANStructure = [StructElem]
data IBANStricture = 
  IBANStricture { countryCode :: !CountryCode
                , checkDigitsStructure :: !StructElem
                , bbanStructure :: !BBANStructure
                }

instance Show IBANStricture where
  show IBANStricture{..} = mconcat [ "IBANStricture"
                                   , "\n  countryCode = ", show countryCode
                                   , "\n  checksumStructure = ", show checkDigitsStructure
                                   , "\n  bbanStructure = ", show bbanStructure
                                   ]

ibanStrP :: Parser IBANStricture
ibanStrP = do
  _countryCode <- countryP
  _checksumEl <- elemP
  _bbanEls <- many1 elemP
  return $ IBANStricture _countryCode _checksumEl _bbanEls
  <?> "IBAN structure parser"

countryP :: Parser CountryCode
countryP = do
  v <- P.count 2 (satisfy isLetter)
  maybe (fail v) pure (readMaybe v)
  <?> "Country code parser"
                
elemP :: Parser StructElem
elemP = StructElem <$> lenP  <*> reprP <?> "IBAN structure element parser" where
    reprP :: Parser Char
    reprP = satisfy (inClass "nace")
    
    lenP :: Parser Len
    lenP = (Fixed <$> nnP <* char '!') <|> (Max <$> nnP)
    
    nnP :: Parser Int
    nnP = do
      v <- many1 digit
      guard (Prelude.length v `elem` [1,2])
      return $ read v
      
reprToParser :: Char -> Parser Char
reprToParser 'n' = satisfy $ inClass n
reprToParser 'a' = satisfy $ inClass a
reprToParser 'c' = satisfy $ inClass c
reprToParser 'e' = satisfy (==' ')
reprToParser other = fail $ "No representation in IBAN structure for '" ++ [other] ++"'"

toIBANElementP :: StructElem -> Parser Text
toIBANElementP (StructElem (Fixed x) typ) = do
   v <- P.count x (reprToParser typ)
   return $ pack v

toIBANElementP (StructElem (Max x) typ) = do
  v <- P.many1 (reprToParser typ)
  guard (Prelude.length v <= x)
  return $ pack v


parseStructures :: [Text] -> Either String (M.Map CountryCode IBANStricture)
parseStructures ss = do
  res <- traverse (parseOnly ibanStrP) ss
  let pre = fmap (\struct -> (countryCode struct, struct)) res
  return $ M.fromList pre

parsedStructures :: HasCallStack => M.Map CountryCode IBANStricture
parsedStructures = fromRight
                    (error "Critical error: can't parse IBAN structures")
                    (parseStructures structures)


ibanStructureByCountry :: CountryCode -> Maybe IBANStricture
ibanStructureByCountry cc = M.lookup cc parsedStructures

uniqueBBANStructures :: Set BBANStructure
uniqueBBANStructures = fromList $ bbanStructure <$> M.elems parsedStructures