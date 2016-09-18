module Vector exposing (Vector, (<+>), (<->), (<*>), (</>), (*>), rotate)

type alias Vector = (Float, Float)

infixl 8 <+>
infixl 8 <->
infixl 9 <*>
infixl 9 </>
infixl 9 *>

vectorOp : (Float -> Float -> Float) -> Vector -> Vector -> Vector
vectorOp op (x, y) (x2, y2) =
  ( x `op` x2
  , y `op` y2
  )

(<+>) : Vector -> Vector -> Vector
(<+>) =  vectorOp (+)

(<->) : Vector -> Vector -> Vector
(<->) =  vectorOp (-)

(<*>) : Vector -> Vector -> Vector
(<*>) =  vectorOp (*)

(</>) : Vector -> Vector -> Vector
(</>) =  vectorOp (/)


scalarOp : (Float -> Float -> Float) -> Float -> Vector -> Vector
scalarOp op scalar (x, y) =
  ( scalar `op` x
  , scalar `op` y
  )

(*>) : Float -> Vector -> Vector
(*>) = scalarOp (*)

rotate : Float -> Vector -> Vector
rotate angle vector =
  let
    (x, y) = vector
    c = cos angle
    s = sin angle
  in
    (y*s + x*c, y*c - x*s)
