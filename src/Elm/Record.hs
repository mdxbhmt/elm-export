module Elm.Record (toElmTypeSource,toElmTypeSourceWith) where

import           Control.Monad.Reader
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmTypeExpr -> Reader Options String

render (TopLevel (DataType dataTypeName record@(Record _ _))) =
  printf "type alias %s =\n  { %s\n  }" dataTypeName <$> render record

render (TopLevel (DataType d s@(Sum _ _))) =
  printf "type %s\n  = %s" d <$> render s

render (DataType d _) = return d
render (Primitive s) = return s

render (Sum x y) =
    do bodyX <- render x
       bodyY <- render y
       return $ printf "%s\n  | %s" bodyX bodyY

render (Field t) = render t

render (Selector s t) = do fieldModifier <- asks fieldLabelModifier
                           printf "%s : %s" (fieldModifier s) <$> render t

render (Constructor c Unit) = pure c

render (Constructor c t) = printf "%s %s" c <$> render t

render (Tuple2 x y) =
    do bodyX <- render x
       bodyY <- render y
       return $ printf "( %s, %s )" bodyX bodyY

render (Dict x y) =
    do bodyX <- render x
       bodyY <- render y
       return $ printf "Dict %s %s" bodyX bodyY

render (Product (Primitive "List") (Primitive "Char")) = return "String"

render (Product (Primitive "List") p@(Product _ _)) =
  printf "List (%s)" <$> render p

render (Product (Primitive "List") t) = printf "List %s" <$> render t

render (Product (Primitive "Dict") (Product k v)) =
  do keyBody <- render k
     valueBody <- render v
     return $ printf "Dict %s %s" keyBody valueBody

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf ("%s " ++ parenthesize y "%s") bodyX bodyY

render (Record n (Product x y)) =
  do bodyX <- render (Record n x)
     bodyY <- render (Record n y)
     return $ printf "%s\n  , %s" bodyX bodyY

render (Record _ s@(Selector _ _)) = render s

render Unit = return ""

toElmTypeSourceWith :: ElmType a => Options -> a -> String
toElmTypeSourceWith options x = runReader (render . TopLevel $ toElmType x) options

toElmTypeSource :: ElmType a => a -> String
toElmTypeSource = toElmTypeSourceWith defaultOptions
