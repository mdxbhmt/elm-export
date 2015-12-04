module Elm.Encoder (toElmEncoderSource, toElmEncoderWithSources) where

import           Data.List (intercalate, nub)
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render elmType = case elmType of
  (TopLevel (DataType d t)) ->
    let fnName = "encode" ++ d
    in printf "%s : %s -> JS.Value\n%s x =%s" fnName d fnName (render t)
  (DataType d _) -> "encode" ++ d
  (Record _ t) -> printf "\n  JS.object\n    [%s]" (render t)
  (Product x y) -> printf "%s\n    ,%s" (render x) (render y)
  (Selector n t) -> printf "(\"%s\", %s x.%s)" n (render t) n
  (Primitive "String") -> "JS.string"
  (Primitive "Int") -> "JS.int"
  (Primitive "Double") -> "JS.float"
  (Primitive "Float") -> "JS.float"
  (Primitive "Date") -> "JS.date"
  (Primitive "Bool") -> "JS.bool"
  (Field (Product (Primitive "Maybe") (Product (Primitive "List") (Primitive "Char")))) -> renderMaybeWith "JS.string"
  (Field (Product (Primitive "Maybe") t)) -> renderMaybeWith (render t)
  (Field (Product (Primitive "List") (Primitive "Char"))) -> "JS.string"
  (Field (Product (Primitive "List") t )) -> printf "(JS.list << List.map %s)" (render t)
  (Field t) -> render t
  x -> printf "<%s>" (show x)

  where renderMaybeWith :: String -> String
        renderMaybeWith renderedType =
          printf (intercalate "\n"
                    [""
                    ,"      (\\y ->"
                    ,"        case y of"
                    ,"          Just val -> %s val"
                    ,"          Nothing -> JS.null)"])
                 renderedType

toElmEncoderSource :: ToElmType a => a -> String
toElmEncoderSource = render . TopLevel . toElmType

toElmEncoderWithSources :: ToElmType a
                           => a
                           -> ( String     -- use this to call the decoder
                              , [String] ) -- a list of decoder definitions for any custom types
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
