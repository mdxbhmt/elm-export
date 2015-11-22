{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder (toElmDecoderSource, toElmDecoderWithSources) where

import           Data.List   (nub)

import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render (TopLevel (DataType d t)) =
  printf "%s : Json.Decode.Decoder %s\n%s =\n%s" fnName d fnName (render t)
  where fnName = "decode" ++ d
render (DataType d _) = "decode" ++ d
render (Record n t) = printf "  Json.Decode.succeed %s\n%s" n (render t)
render (Product (Primitive "List") (Primitive "Char")) = render (Primitive "String")
render (Product (Primitive "List") t) = printf "Json.Decode.list %s" (render t)
render (Product (Primitive "Maybe") t) = printf "Json.Decode.maybe %s" (render t)
render (Product x y) = printf "%s\n%s" (render x) (render y)
render (Selector n t) = printf "    |: (\"%s\" := %s)" n (render t)
render (Primitive "String") = "Json.Decode.string"
render (Primitive "Int") = "Json.Decode.int"
render (Primitive "Double") = "Json.Decode.float"
render (Primitive "Float") = "Json.Decode.float"
render (Primitive "Date") = "Json.Decode.date"
render (Primitive "Bool") = "Json.Decode.bool"
render (Field t) = render t
render x = printf "<%s>" (show x)

toElmDecoderSource :: ToElmType a => a -> String
toElmDecoderSource = render . TopLevel . toElmType

toElmDecoderWithSources :: ToElmType a => a -> (String, [String])
toElmDecoderWithSources = fmap nub . go . toElmType
  where
    go t@(DataType _ s) =
      let (tDecoder, tDefs) = (render t, [render (TopLevel t)])
          (_, sDefs) = go s
      in (tDecoder, tDefs ++ sDefs)
    go (Product (Primitive "Maybe") t) =
      let (tDecoder, tDefs) = go t
      in  (printf "(Json.Decode.maybe %s)" tDecoder, tDefs)
    go t@(Product (Primitive "List") (Primitive "Char")) = (render (Field t), [])
    go (Product (Primitive "List") t) =
      let (tDecoder, tDefs) = go t
      in  (printf "(Json.Decode.list %s)" tDecoder, tDefs)
    go (Product x y) =
      let (xDecoder, xDefs) = go x
          (yDecoder, yDefs) = go y
      in ( printf "%s\n  |: %s" xDecoder yDecoder
         , xDefs ++ yDefs )
    go Unit = ("Json.Decode.succeed ()", [])
    go t@(Primitive _) = (render t, [])
    go (Record _ t) = go t
    go (Selector _ t) = go t
    go (Field t) = go t
    go t = error $ "toElmDecoderWithSources: " ++ show t
