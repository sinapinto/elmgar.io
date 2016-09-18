module KeyStates exposing (KeyStates, pressed, released)

type alias KeyStates =
  { space : Bool
  }

pressed : Int -> KeyStates -> KeyStates
pressed key keys =
  case key of
    32 -> { keys | space = True }
    _ -> keys

released : Int -> KeyStates -> KeyStates
released key keys =
  case key of
    32 -> { keys | space = False }
    _ -> keys
