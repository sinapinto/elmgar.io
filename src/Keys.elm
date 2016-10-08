module Keys exposing (Keys, update)

type alias Keys =
  { space : Bool
  , w : Bool
  }

update : Int -> Keys -> Keys
update key keys =
  case key of
    87    -> { keys | w = True }
    (-87) -> { keys | w = False }
    32    -> { keys | space = True }
    (-32) -> { keys | space = False }
    _ -> keys
