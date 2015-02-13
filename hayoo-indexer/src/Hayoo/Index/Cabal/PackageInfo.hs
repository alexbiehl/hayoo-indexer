module Hayoo.Index.Cabal.PackageInfo where

import           Data.Aeson (toJSON)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Version (showVersion)
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal
import           Hayoo.Index.IndexSchema
import qualified Hunt.ClientInterface as Hunt
import qualified Hunt.Common.DocDesc as Hunt

type PackageName = Text
type Version = Text

type Error = String

data PackageIdentifier = PackageIdentifier {
    pIdName :: PackageName
  , pIdVersion :: Version
  }
  deriving (Eq, Show)

data PackageDescription = PackageDescription {
    pdId           :: PackageIdentifier
  , pdUri          :: Text
  , pdAuthor       :: Text
  , pdCategory     :: Text
  , pdDependencies :: Text
  , pdHomepage     :: Text
  , pdMaintainer   :: Text
  , pdSynopsis     :: Text
  , pdDescription  :: Text
  } deriving (Eq, Show)

instance Hunt.Huntable PackageDescription where
  huntURI      = pdUri
  huntDescr    = Hunt.mkDocDesc
                 . fmap toJSON
                 . HashMap.fromList
                 . toList
  huntIndexMap = Map.fromList . toList

toList :: PackageDescription -> [(Text, Text)]
toList pd = filter (not . Text.null . snd) [
    (d'author, pdAuthor pd)
  , (d'category, pdCategory pd)
  , (d'dependencies, pdDependencies pd)
  , (d'description, pdDescription pd)
  , (d'maintainer, pdMaintainer pd)
  , (d'name, pIdName (pdId pd))
  , (d'version, pIdVersion (pdId pd))
  ]

parseCabalFile :: String -> Either Error PackageDescription
parseCabalFile content =
  case parseDesc content of
   Cabal.ParseFailed err -> Left (show err)
   Cabal.ParseOk _ desc  -> Right desc
  where
    parseDesc desc = do
      cabalGenDesc <- Cabal.parsePackageDescription desc
      let cabalDesc = Cabal.packageDescription cabalGenDesc
      return (cabalDescToHayooDesc cabalDesc)

cabalDescToHayooDesc :: Cabal.PackageDescription -> PackageDescription
cabalDescToHayooDesc desc =
  PackageDescription {
      pdId           = PackageIdentifier pkgName version
    , pdUri          = Text.pack $ Cabal.pkgUrl desc
    , pdAuthor       = Text.pack $ Cabal.author desc
    , pdCategory     = Text.pack $ Cabal.category desc
    , pdDependencies = Text.empty
    , pdHomepage     = Text.pack $ Cabal.homepage desc
    , pdMaintainer   = Text.pack $ Cabal.maintainer desc
    , pdSynopsis     = Text.pack $ Cabal.synopsis desc
    , pdDescription  = Text.pack $ Cabal.description desc
    }
  where
    package = Cabal.package desc
    pkgName = Text.pack $ Cabal.display (Cabal.pkgName package)
    version = Text.pack $ showVersion (Cabal.pkgVersion package)
