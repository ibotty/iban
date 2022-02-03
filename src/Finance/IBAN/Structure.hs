{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance.IBAN.Structure
  ( countryP,
    ibanStrP,
    parseStructures,
    IBANStructure (..),
    BBANStructure,
    toIBANElementP,
    StructElem (..),
    Repr (..),
    Len (..),
    elemP,
    ibanStructures,
    reprPred,
    isCompliant,
    lenPred,
  )
where

import Contrib.Data.ISO3166_CountryCodes (CountryCode)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Attoparsec.Text as P
import Data.Char (isLetter, toUpper)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M (Map, elems, fromList, lookup)
import Data.Set (Set, fromList, member)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Debug.Trace (traceShowId)
import GHC.Stack (HasCallStack)
import Instances.TH.Lift ()
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift, lift)
import Text.Read (readMaybe)

-- | Checks if character belongs to subset used by IBAN REGISTRY to describe IBAN structure
isCompliant :: Char -> Bool
isCompliant = (`member` compliantChars)

n :: [Char]
n = ['0' .. '9']

a :: [Char]
a = ['A' .. 'Z']

c :: [Char]
c = n ++ a ++ ['a' .. 'z']

compliantChars :: Set Char
compliantChars = fromList $ c ++ [' ']

data Len = Fixed !Int | Max !Int
  deriving (Eq, Ord, Show, Lift)

data Repr = N | A | C | E
  deriving (Eq, Ord, Show, Lift)

data StructElem = StructElem {len :: !Len, repr :: !Repr}
  deriving (Eq, Ord, Show, Lift)

type BBANStructure = [StructElem]

data IBANStructure = IBANStructure
  { countryCode :: !CountryCode,
    bbanStructure :: !BBANStructure
  }
  deriving (Eq, Ord, Show, Lift)

ibanStrP :: Parser IBANStructure
ibanStrP =
  do
    _countryCode <- countryP
    _ <- string "2!n" -- checksum
    _bbanEls <- many1 elemP
    return $ IBANStructure _countryCode _bbanEls
    <?> "IBAN structure parser"

countryP :: Parser CountryCode
countryP =
  do
    v <- P.count 2 (satisfy isLetter)
    maybe (fail v) pure (readMaybe v)
    <?> "Country code parser"

elemP :: Parser StructElem
elemP = StructElem <$> lenP <*> reprP <?> "IBAN structure element parser"
  where
    reprP :: Parser Repr
    reprP = N <$ char 'n' <|> A <$ char 'a' <|> C <$ char 'c' <|> E <$ char 'e'

    lenP :: Parser Len
    lenP = (Fixed <$> nnP <* char '!') <|> (Max <$> nnP)

    nnP :: Parser Int
    nnP = do
      v <- many1 digit
      guard (Prelude.length v `elem` [1, 2])
      return $ read v

reprToParser :: Repr -> Parser Char
reprToParser N = satisfy $ inClass n
reprToParser A = satisfy $ inClass a
reprToParser C = satisfy $ inClass c
reprToParser E = satisfy (== ' ')

toIBANElementP :: StructElem -> Parser Text
toIBANElementP (StructElem (Fixed x) typ) = do
  v <- P.count x (reprToParser typ)
  return $ pack v
toIBANElementP (StructElem (Max x) typ) = do
  v <- P.many1 (reprToParser typ)
  guard (Prelude.length v <= x)
  return $ pack v

parseStructures :: [Text] -> Either String (M.Map CountryCode IBANStructure)
parseStructures ss = do
  res <- traverse (parseOnly ibanStrP) ss
  let pre = fmap (\struct -> (countryCode struct, struct)) res
  return $ M.fromList pre

ibanStructures :: QuasiQuoter
ibanStructures =
  QuasiQuoter
    { quoteExp = parseToExpression,
      quotePat = err,
      quoteType = err,
      quoteDec = err
    }
  where
    parseToExpression =
      either (fail . show) lift
        . parseStructures
        . filter (/= "")
        . fmap trim
        . T.lines
        . T.pack
    err _ = fail "[iban|...|] can only be used as expression."
    trim = T.dropAround (== ' ')

reprPred :: Repr -> Char -> Bool
reprPred A c = 'A' <= c && c <= 'Z'
reprPred N c = '0' <= c && c <= '9'
reprPred C c = reprPred A c || reprPred N c || 'a' <= c && c <= 'z'
reprPred E c = c == ' '

lenPred :: Len -> Int -> Bool
lenPred (Fixed n) = (== n)
lenPred (Max n) = (&&) <$> (<= n) <*> (> 0)