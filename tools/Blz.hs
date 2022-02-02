module Main (main) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromRecord, HasHeader (HasHeader), decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Finance.IBAN.Germany.Core
import System.Environment (getArgs)

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
