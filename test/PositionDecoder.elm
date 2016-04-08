module Main (..) where


decodePosition : Json.Decode.Decoder Position
decodePosition =
  Json.Decode.string `Json.Decode.andThen` \s ->
    if s == "Beginning" then
      Json.Decode.succeed Beginning
    else if s == "Middle" then
      Json.Decode.succeed Middle
    else if s == "End" then
      Json.Decode.succeed End
    else
      Json.Decode.fail ("Could not decode Position from '" ++ s ++ "'")
