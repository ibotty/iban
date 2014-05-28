module Main (main) where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>))
import Finance.IBAN.Germany.Core
import Data.Csv (HasHeader(HasHeader), FromRecord, decode)
import Data.Text (Text)
import Data.Vector (Vector)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
    file <- head <$> getArgs
    blzList <- either error id <$> decodeBLZList file
    let blzList' = V.map (blz &&& blzBIC) . V.filter primaryBanksFilter $ blzList
    V.mapM_ print blzList'

-- orphan instance from same package
instance FromRecord BLZRecord

primaryBanksFilter :: BLZRecord -> Bool
primaryBanksFilter = (&&) <$> (== 1) . blzMerkmal <*> (T.null . blzBIC)


decodeBLZList :: FilePath -> IO (Either String (Vector BLZRecord))
decodeBLZList = fmap (decode HasHeader) . BL.readFile

