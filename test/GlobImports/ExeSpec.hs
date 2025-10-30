module GlobImports.ExeSpec where

import Test.Hspec
import GlobImports.Exe
import System.FilePath

spec :: Spec
spec = do
    describe "renderFile" do
        let allModelsFile =
                AllModelsFile
                    { amfModuleBase =
                        Module
                            { moduleName = "Foo.Bar"
                            , modulePath = "src/Foo/Bar.hs"
                            }
                    , amfModuleImports =
                        [ Module
                            { moduleName = "Foo.Bar.Quux"
                            , modulePath = "src/Foo/Bar/Quux.hs"
                            }
                        ]
                    }
        it "errors without a GLOB_IMPORTS_SPLICE marker" do
            (pure $! length (renderFile allModelsFile Prefix (unlines
                [ "module Foo.Bar where"
                , ""
                , "import Blah"
                ])))
                `shouldThrow` anyErrorCall
        it "works with a GLOB_IMPORTS_SPLICE marker" do
            renderFile allModelsFile Prefix
                (unlines
                    [ "module Foo.Bar where"
                    , ""
                    , "import Blah"
                    , "-- GLOB_IMPORTS_SPLICE"
                    ])
                `shouldBe`
                    unlines
                        [ "module Foo.Bar where"
                        , ""
                        , "import Blah"
                        , "import qualified Foo.Bar.Quux"
                        , "_importedModules :: [String]"
                        , "_importedModules ="
                        , "  [ \"Foo.Bar.Quux\""
                        , "  ]"
                        ]
        it "works with import qualified post" do
            renderFile allModelsFile Suffix
                (unlines
                    [ "module Foo.Bar where"
                    , ""
                    , "import Blah"
                    , "-- GLOB_IMPORTS_SPLICE"
                    ])
                `shouldBe`
                    unlines
                        [ "module Foo.Bar where"
                        , ""
                        , "import Blah"
                        , "import Foo.Bar.Quux qualified"
                        , "_importedModules :: [String]"
                        , "_importedModules ="
                        , "  [ \"Foo.Bar.Quux\""
                        , "  ]"
                        ]
        it "can handle imports after glob import splice" do
            renderFile allModelsFile Prefix
                (unlines
                    [ "module Foo.Bar where"
                    , ""
                    , "import Blah"
                    , "-- GLOB_IMPORTS_SPLICE"
                    , "import Boo"
                    , ""
                    , "baz :: Int"
                    , "baz = 3"
                    ])
                `shouldBe`
                    unlines
                        [ "module Foo.Bar where"
                        , ""
                        , "import Blah"
                        , "import qualified Foo.Bar.Quux"
                        , "import Boo"
                        , "_importedModules :: [String]"
                        , "_importedModules ="
                        , "  [ \"Foo.Bar.Quux\""
                        , "  ]"
                        , ""
                        , "baz :: Int"
                        , "baz = 3"
                        ]

    describe "mkModulePieces" do
        let
            exPath =
                "src/Foo/Bar.hs"
        it "works" do
            mkModulePieces exPath
                `shouldBe`
                    ["Foo", "Bar"]

        it "does not eat non-hs files" do
            let
                mdFile =
                    "src/Foo/README.md"
            mkModulePieces mdFile
                `shouldBe`
                    []

    describe "pathToModule" do
        let
            exPath =
                "./src/Foo/Bar.hs"
            (dir, file) =
                splitFileName exPath
        it "splits how i expect" $ do
            dir `shouldBe` "./src/Foo/"
            file `shouldBe` "Bar.hs"
        it "works" do
            pathToModule exPath
                `shouldBe`
                    Just Module
                        { moduleName =
                            "Foo.Bar"
                        , modulePath =
                            exPath
                        }
        it "pathToModule absolute path" do
            let absPath = "/Users/user/Code/project/src/Foo/Bar.hs"
            pathToModule absPath
                `shouldBe`
                    Just Module
                        { moduleName =
                            "Foo.Bar"
                        , modulePath =
                            absPath
                        }
