{-# LANGUAGE DeriveGeneric #-}


import           Control.Monad                  (zipWithM_)
import           Data.Proxy
import           Data.Text                      hiding (length, map)
import           Elm
import           GHC.Generics
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Text.Printf

data Post =
  Post {id       :: Int
       ,name     :: String
       ,age      :: Maybe Double
       ,comments :: [Comment]
       ,promoted :: Maybe Comment
       ,author   :: Maybe String}
  deriving Generic

data Comment =
  Comment {postId    :: Int
          ,text      :: Text
          ,published :: Bool}
  deriving (Generic)

instance ToElmType Post
instance ToElmType Comment

main :: IO ()
main =
  defaultMainWithOpts
    [testCase "toElmTypeSource Post"
              (testToElmTypeSource (Proxy :: Proxy Post)
                                   "test/PostType.elm")
    ,testCase "toElmTypeSource Comment"
              (testToElmTypeSource (Proxy :: Proxy Comment)
                                   "test/CommentType.elm")
    ,testCase "toElmTypeWithSources Post"
              (testToElmTypeWithSources (Proxy :: Proxy [Post])
                                        "List (Post)"
                                        ["test/PostType.elm"
                                        ,"test/CommentType.elm"])
    ,testCase "toElmDecoderSource Post"
              (testToElmDecoderSource (Proxy :: Proxy Post)
                                      "test/PostDecoder.elm")
    ,testCase "toElmDecoderSource Comment"
              (testToElmDecoderSource (Proxy :: Proxy Comment)
                                      "test/CommentDecoder.elm")
     ,testCase "toElmDecoderWithSources Post"
               (testToElmDecoderWithSources (Proxy :: Proxy [Post])
                                            "(Json.Decode.list decodePost)"
                                            ["test/PostDecoder.elm"
                                            ,"test/CommentDecoder.elm"])
    ,testCase "toElmEncoderSource Post"
              (testToElmEncoderSource (Proxy :: Proxy Post)
                                      "test/PostEncoder.elm")
    ,testCase "toElmEncoderSource Comment"
              (testToElmEncoderSource (Proxy :: Proxy Comment)
                                      "test/CommentEncoder.elm")
    ,testCase "toElmEncoderWithSources Post"
              (testToElmEncoderWithSources (Proxy :: Proxy [Post])
                                           "(JS.list << List.map encodePost)"
                                           ["test/PostEncoder.elm"
                                           ,"test/CommentEncoder.elm"])]
    mempty

outputWrapping :: String
outputWrapping = "module Main (..) where\n\n\n%s\n"

testToElmTypeSource :: ToElmType a => a -> FilePath -> IO ()
testToElmTypeSource proxy sourceFile =
  do source <- readFile sourceFile
     assertEqual "Encoding a type" source $
       printf outputWrapping (toElmTypeSource proxy)

testWithSources :: ToElmType a => (a -> (String, [String])) -> a -> String -> [FilePath] -> IO ()
testWithSources fn proxy expectedType sourceFiles =
  do expectedSources <- mapM readFile sourceFiles
     let (genType, genSources) = fn proxy
     assertEqual "Encoding a type name"
                 expectedType
                 genType
     assertEqual "Encoding a type gives the correct number of sources"
                 (length expectedSources)
                 (length genSources)
     zipWithM_ (assertEqual "Encoding a type with sources")
               expectedSources
               (map (printf outputWrapping) genSources)

testToElmTypeWithSources :: ToElmType a => a -> String -> [FilePath] -> IO ()
testToElmTypeWithSources = testWithSources toElmTypeWithSources

testToElmDecoderSource :: ToElmType a => a -> FilePath -> IO ()
testToElmDecoderSource proxy sourceFile =
  do source <- readFile sourceFile
     assertEqual "Encoding a decoder" source $
       printf outputWrapping (toElmDecoderSource proxy)

testToElmDecoderWithSources :: ToElmType a => a -> String -> [FilePath] -> IO ()
testToElmDecoderWithSources = testWithSources toElmDecoderWithSources

testToElmEncoderSource :: ToElmType a => a -> FilePath -> IO ()
testToElmEncoderSource proxy sourceFile =
  do source <- readFile sourceFile
     assertEqual "Encoding a encoder" source $
       printf outputWrapping (toElmEncoderSource proxy)

testToElmEncoderWithSources :: ToElmType a => a -> String -> [FilePath] -> IO ()
testToElmEncoderWithSources = testWithSources toElmEncoderWithSources
