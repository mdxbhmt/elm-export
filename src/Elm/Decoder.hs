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
  printf "%s : Decoder %s\n%s = %s" fnName d fnName (render t)
  where fnName = "decode" ++ d
render (DataType d _) = "decode" ++ d
render (Record n t) = printf "%s\n  `map`   %s" n (render t)
render (Product x y) = printf "%s\n  `apply` %s" (render x) (render y)
render (Selector n t) = printf "(\"%s\" := %s)" n (render t)
render (Primitive "String") = "string"
render (Primitive "Int") = "int"
render (Primitive "Double") = "float"
render (Primitive "Float") = "float"
render (Primitive "Date") = "date"
render (Primitive "Bool") = "bool"
render (Field (Product (Primitive "Maybe") (Product (Primitive "List") (Primitive "Char")))) = "maybe string"
render (Field (Product (Primitive "Maybe") t)) = printf "maybe %s" (render t)
render (Field (Product (Primitive "List") (Primitive "Char"))) = "string"
render (Field (Product (Primitive "List") t )) = printf "list %s" (render t)
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
      in  (printf "(maybe %s)" tDecoder, tDefs)
    go t@(Product (Primitive "List") (Primitive "Char")) = (render (Field t), [])
    go (Product (Primitive "List") t) =
      let (tDecoder, tDefs) = go t
      in  (printf "(list %s)" tDecoder, tDefs)
    go (Product x y) =
      let (xDecoder, xDefs) = go x
          (yDecoder, yDefs) = go y
      in ( printf "%s\n  `apply` %s" xDecoder yDecoder
         , xDefs ++ yDefs )
    go Unit = ("succeed ()", [])
    go t@(Primitive _) = (render t, [])
    go (Record _ t) = go t
    go (Selector _ t) = go t
    go (Field t) = go t
    go t = error $ "toElmDecoderWithSources: " ++ show t
