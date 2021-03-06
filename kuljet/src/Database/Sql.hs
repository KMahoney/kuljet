module Database.Sql (Sql(..), (<+>), quoteName, quoteString, intercalate, hcat, integer) where

import Data.String
import Data.Char (isAlphaNum, isAlpha)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T


data Sql = Sql { toText :: Text }
  deriving (Show)


instance Semigroup Sql where
  (Sql a) <> (Sql b) = Sql (a <> b)

instance IsString Sql where
  fromString = Sql . T.pack

instance Monoid Sql where
  mempty = Sql ""


(<+>) :: Sql -> Sql -> Sql
(Sql a) <+> (Sql b) = Sql (a <> " " <> b)


quoteName :: Text -> Sql
quoteName s
  | safeStartChar (T.head s) && T.all safeChar s = Sql s
  | otherwise = Sql $ "\"" <> T.concatMap escape s <> "\""
  where
    escape :: Char -> Text
    escape '"' = "\\\""
    escape x = T.singleton x

    safeChar :: Char -> Bool
    safeChar c = isAlphaNum c || c `elem` ("_" :: String)

    safeStartChar :: Char -> Bool
    safeStartChar c = isAlpha c || c `elem` ("_" :: String)


quoteString :: Text -> Sql
quoteString s = Sql $ "'" <> T.concatMap escape s <> "'"
  where
    escape :: Char -> Text
    escape '\'' = "\\'"
    escape x = T.singleton x


integer :: Integer -> Sql
integer i = Sql $ T.pack $ show i


intercalate :: Sql -> [Sql] -> Sql
intercalate sep sqls = mconcat (intersperse sep sqls)


hcat :: [Sql] -> Sql
hcat = mconcat
