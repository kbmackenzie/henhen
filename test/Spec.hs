import HenHen.Config
    ( HenHenConfig(..)
    , Target(..)
    , TargetKey(..)
    , TargetMeta(..)
    , EggOptions(..)
    )
import HenHen.Actions (collectDependencies)
import Test.Hspec
    ( Spec
    , it
    , describe
    , shouldBe
    , hspec
    )
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Bifunctor (bimap)
import qualified Data.List as List

generateConfigDeps :: [String] -> [(String, [String])] -> HenHenConfig
generateConfigDeps topLevelDeps targetDeps = do
    let generateMeta :: [String] -> TargetMeta
        generateMeta deps = TargetMeta
            { metaDeps    = map TargetKey deps
            , metaOptions = mempty }

    let generateTarget :: [String] -> Target
        generateTarget = flip Egg (EggOptions mempty) . generateMeta
    
    let targets = map (bimap TargetKey generateTarget) targetDeps
    HenHenConfig
        { configName      = mempty
        , configSources   = mempty
        , configDataFiles = mempty
        , configSourceDir = mempty
        , configDeps      = HashSet.fromList topLevelDeps
        , configFetch     = mempty
        , configScripts   = mempty
        , configAliases   = Nothing
        , configTargets   = HashMap.fromList targets }

dependencies :: Spec
dependencies = describe "collect dependencies from config" $ do
    let collect = List.sort . HashSet.toList . collectDependencies

    describe "top level + targets have dependencies" $ do
        let topLevel = ["abc", "def", "ghi"]
        let targets  = [("foo", ["abc", "ghi"]) , ("bar", ["def", "xyz"])]
        let config   = generateConfigDeps topLevel targets

        it "should collect unique dependencies" $
            collect config `shouldBe` ["abc", "def", "ghi", "xyz"]

    describe "only targets have dependencies" $ do
        let topLevel = []
        let targets  = [("foo", ["abc"]) , ("bar", ["def", "xyz"])]
        let config   = generateConfigDeps topLevel targets

        it "should collect unique dependencies" $
            collect config `shouldBe` ["abc", "def", "xyz"]

    describe "no dependencies" $ do
        let config   = generateConfigDeps [] []

        it "should find no dependencies" $
            collect config `shouldBe` []

    describe "only targets have dependencies, and they only depend on other targets" $ do
        let topLevel = []
        let targets  = [("foo", []), ("bar", ["foo"]), ("baz", ["foo", "bar"])]
        let config   = generateConfigDeps topLevel targets

        it "should collect no dependencies" $
            collect config `shouldBe` []

main :: IO ()
main = hspec dependencies
