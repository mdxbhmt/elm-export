module Elm.Encoder (toElmEncoderSource, toElmEncoderWithSources) where

import           Data.List   (nub)

import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render elmType =
  case elmType of
    (TopLevel (DataType d t)) ->
      let fnName = "encode" ++ d
      in printf "%s : %s -> Json.Encode.Value\n%s x =%s" fnName d fnName (render t)
    (DataType d _) -> "encode" ++ d
    (Record _ t) -> printf "\n  Json.Encode.object\n    [ %s\n    ]" (render t)
    (Product (Primitive "List") (Primitive "Char")) -> render (Primitive "String")
    (Product (Primitive "List") t) ->
      printf "(Json.Encode.list << List.map %s)" (render t)
    (Product (Primitive "Maybe") t) ->
      printf "(Maybe.withDefault Json.Encode.null << Maybe.map %s)" (render t)
    (Product x y) ->
      printf "%s\n    , %s"
             (render x)
             (render y)
    (Selector n t) -> printf "( \"%s\", %s x.%s )" n (render t) n
    (Primitive "String") -> "Json.Encode.string"
    (Primitive "Int") -> "Json.Encode.int"
    (Primitive "Double") -> "Json.Encode.float"
    (Primitive "Float") -> "Json.Encode.float"
    (Primitive "Date") -> "Json.Encode.date"
    (Primitive "Bool") -> "Json.Encode.bool"
    (Field t) -> render t
    x -> printf "<%s>" (show x)

toElmEncoderSource :: ToElmType a => a -> String
toElmEncoderSource = render . TopLevel . toElmType

toElmEncoderWithSources :: ToElmType a => a -> (String, [String])
toElmEncoderWithSources = fmap nub . go . toElmType
  where
    go t@(DataType _ s) =
      let (tDecoder, tDefs) = (render t, [render (TopLevel t)])
          (_, sDefs) = go s
      in (tDecoder, tDefs ++ sDefs)
    go (Product (Primitive "Maybe") t) =
      let (tDecoder, tDefs) = go t
      in  (printf "(\\y -> case y of Nothing -> JS.null; Just val -> %s val)" (parenthesize t tDecoder), tDefs)
    go t@(Product (Primitive "List") (Primitive "Char")) = (render (Field t), [])
    go (Product (Primitive "List") t) =
      let (tDecoder, tDefs) = go t
      in  (printf "(JS.list << List.map %s)" (parenthesize t tDecoder), tDefs)
    go (Product x y) =
      let (_, xDefs) = go x
          (_, yDefs) = go y
      in ( undefined -- this case is used only for generating definitions (called from DataType match above)
         , xDefs ++ yDefs )
    go Unit = ("JS.null", [])
    go t@(Primitive _) = (render t, [])
    go (Record _ t) = go t
    go (Selector _ t) = go t
    go (Field t) = go t
    go t = error $ "toElmEncoder: " ++ show t
