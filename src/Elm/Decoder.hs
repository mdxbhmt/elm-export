{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder (toElmDecoderSource, toElmDecoderSourceWith)
       where

import           Control.Monad.Reader
import           Data.List            (intercalate)
import           Elm.Common
import           Elm.Type
import           Text.Printf

decoderHeader :: String -> String
decoderHeader typeName =
  printf "%s : Json.Decode.Decoder %s\n%s =\n" fnName typeName fnName
  where fnName = "decode" ++ typeName

render :: ElmTypeExpr -> Reader Options String

render (TopLevel (DataType d s@(Sum _ _))) =
  (decoderHeader d ++) <$> do
    body <- renderSum s True
    return $ intercalate "\n"
      ["  Json.Decode.string `Json.Decode.andThen` \\s ->"
      ,body
      ,"    else"
      ,printf "      Json.Decode.fail (\"Could not decode %s from '\" ++ s ++ \"'\")" d
      ]
  where
    renderSum :: ElmTypeExpr -> Bool -> Reader Options String
    renderSum (Constructor c Unit) isFirst =
      return $ intercalate "\n"
        [printf "    %s s == \"%s\" then" (if isFirst then "if" else "else if") c
        ,printf "      Json.Decode.succeed %s" c
        ]
    renderSum (Sum a b) isFirst =
      do bodyA <- renderSum a isFirst
         bodyB <- renderSum b False
         return $ intercalate "\n"
           [bodyA
           ,bodyB
           ]
    renderSum t _ = return $ printf "<%s>" (show t)

render (TopLevel (DataType d t)) =
  (decoderHeader d ++ ) <$> render t

render (DataType d _) = return $ "decode" ++ d

render (Record n t) = printf "  Json.Decode.succeed %s\n%s" n <$> render t

render (Product (Primitive "List") (Primitive "Char")) = render (Primitive "String")

render (Product (Primitive "List") t) = printf "Json.Decode.list %s" <$> render t

render (Product (Primitive "Maybe") t) = printf "Json.Decode.maybe %s" <$> render t

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "%s\n%s" bodyX bodyY

render (Selector n t) =
  do fieldModifier <- asks fieldLabelModifier
     printf "    |: (\"%s\" := %s)" (fieldModifier n) <$> render t

render (Tuple2 x y) =
    do bodyX <- render x
       bodyY <- render y
       return $ printf "Json.Decode.tuple2 (,) %s %s" bodyX bodyY

render (Dict x y) =
  printf "Json.Decode.map Dict.fromList (Json.Decode.list (%s))" <$> render (Tuple2 x y)

render (Primitive "String") = return "Json.Decode.string"

render (Primitive "Int") = return "Json.Decode.int"

render (Primitive "Double") = return "Json.Decode.float"

render (Primitive "Float") = return "Json.Decode.float"

render (Primitive "Date") = return "Json.Decode.Extra.date"

render (Primitive "Bool") = return "Json.Decode.bool"

render (Primitive "()") =
  return $ intercalate "\n"
    ["Json.Decode.customDecoder (Json.Decode.list Json.Decode.value)"
    ,"          (\\jsonList ->"
    ,"             case jsonList of"
    ,"               [] -> Result.Ok ()"
    ,"               _  -> Result.Err \"expecting a zero-length array\")"
    ]

render (Field t) = render t

render x = return $ printf "<%s>" (show x)

toElmDecoderSourceWith :: ElmType a => Options -> a -> String
toElmDecoderSourceWith options x =
  runReader (render . TopLevel $ toElmType x) options

toElmDecoderSource :: ElmType a => a -> String
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
