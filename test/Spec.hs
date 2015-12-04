{-# LANGUAGE DeriveGeneric #-}

import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           Elm
import           GHC.Generics
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

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
    [testCase "toElmTypeSource" testToElmTypeSource
    ,testCase "toElmTypeWithSources" testToElmTypeWithSources
    ,testCase "toElmDecoderSource" testToElmDecoderSource
    ,testCase "toElmDecoderWithSources" testToElmDecoderWithSources
    ,testCase "toElmEncoderSource" testToElmEncoderSource]
    mempty

testToElmTypeSource :: Assertion
testToElmTypeSource =
  do postSource <- readFile "test/PostType.txt"
     assertEqual "Encoding a Post type" postSource $
       (toElmTypeSource (Proxy :: Proxy Post)) ++ "\n"
     commentSource <- readFile "test/CommentType.txt"
     assertEqual "Encoding a Comment type" commentSource $
       (toElmTypeSource (Proxy :: Proxy Comment)) ++ "\n"

testToElmTypeWithSources :: Assertion
testToElmTypeWithSources =
  do postSource <- readFile "test/PostType.txt"
     commentSource <- readFile "test/CommentType.txt"
     assertEqual "Encoding a [Post] type" ("List (Post)", [postSource, commentSource]) $
       fmap (fmap (++ "\n")) (toElmTypeWithSources (Proxy :: Proxy [Post]))
     assertEqual "Encoding a Primitive String type" ("String", []) $
       fmap (fmap (++ "\n")) (toElmTypeWithSources (Proxy :: Proxy String))

testToElmDecoderSource :: Assertion
testToElmDecoderSource =
  do postSource <- readFile "test/PostDecoder.txt"
     assertEqual "Encoding a Post decoder" postSource $
       (toElmDecoderSource (Proxy :: Proxy Post)) ++ "\n"
     commentSource <- readFile "test/CommentDecoder.txt"
     assertEqual "Encoding a Comment decoder" commentSource $
       (toElmDecoderSource (Proxy :: Proxy Comment)) ++ "\n"

testToElmDecoderWithSources :: Assertion
testToElmDecoderWithSources =
  do postSource <- readFile "test/PostDecoder.txt"
     commentSource <- readFile "test/CommentDecoder.txt"
     assertEqual "Encoding a [Post] decoder" ("(list decodePost)", [postSource, commentSource]) $
       fmap (fmap (++ "\n")) (toElmDecoderWithSources (Proxy :: Proxy [Post]))
     assertEqual "Encoding a Primitive String decoder" ("string", []) $
       fmap (fmap (++ "\n")) (toElmDecoderWithSources (Proxy :: Proxy String))

testToElmEncoderSource :: Assertion
testToElmEncoderSource =
  do postSource <- readFile "test/PostEncoder.txt"
     assertEqual "Encoding a Post encoder" postSource $
       toElmEncoderSource (Proxy :: Proxy Post) ++ "\n"
     commentSource <- readFile "test/CommentEncoder.txt"
     assertEqual "Encoding a Comment encoder" commentSource $
       toElmEncoderSource (Proxy :: Proxy Comment) ++ "\n"
