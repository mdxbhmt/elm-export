module Elm.Record (toElmTypeSource, toElmTypeWithSources) where

import           Data.List   (nub)
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render (TopLevel (DataType d r@(Record _ _))) = printf "type alias %s =\n  {%s}" d (render r)
render (TopLevel (DataType d s@(Sum _ _))) = printf "type %s\n  = %s" d (render s)
render (DataType d _) = d
render (Primitive s) = s
render (Sum x y) = printf "%s\n  | %s" (render x) (render y)
render (Field t) = render t
render (Selector s t) = printf "%s : %s" s (render t)
render (Constructor c t) = printf "%s %s" c (render t)
render (Product (Primitive "List") (Primitive "Char")) = "String"
render (Product x y) = printf "%s %s" (render x) (parenthesize y (render y))
render (Record n (Product x y)) = printf "%s\n  ,%s" (render (Record n x)) (render (Record n y))
render (Record _ s@(Selector _ _)) = render s
render Unit = ""

toElmTypeSource :: ToElmType a => a -> String
toElmTypeSource = render . TopLevel . toElmType

toElmTypeWithSources :: ToElmType a => a -> (String, [String])
toElmTypeWithSources = fmap nub . go . toElmType
  where
    go t@(DataType d s) =
      let (tDecoder, tDefs) = (d, [render (TopLevel t)])
          (_, sDefs) = go s
      in (tDecoder, tDefs ++ sDefs)
    go t@(Product (Primitive "List") (Primitive "Char")) = (render (Field t), [])
    go (Product x y) =
      let (xType, xDefs) = go x
          (yType, yDefs) = go y
      in ( printf (case y of
                     Primitive _ -> "%s %s"
                     _           -> "%s (%s)") xType yType
         , xDefs ++ yDefs )
    go t@(Primitive _) = (render t, [])
    go (Record _ t) = go t
    go (Selector _ t) = go t
    go (Field t) = go t
    go t = error $ "toElmTypeWithSources: " ++ show t
