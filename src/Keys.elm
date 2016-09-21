module Keys exposing (Keys, update)

type alias Keys =
  { space : Bool
  }

update : Int -> Keys -> Keys
update key keys =
  case key of
    32    -> { keys | space = True }
    (-32) -> { keys | space = False }
    _ -> keys
