module Vector
    exposing
        ( Vector
        , add
        , subtract
        , dotProduct
        , scale
        , negate
        , normalise
        , magnitude
        )


type alias Vector =
    ( Float, Float )



-- TODO: Is there a way to define this using the (+) operator?


add : Vector -> Vector -> Vector
add ( a, b ) ( x, y ) =
    ( a + x, b + y )


subtract : Vector -> Vector -> Vector
subtract ( a, b ) ( x, y ) =
    ( a - x, b - y )


dotProduct : Vector -> Vector -> Float
dotProduct ( a, b ) ( x, y ) =
    a * x + b * y


scale : Float -> Vector -> Vector
scale s ( x, y ) =
    ( s * x, s * y )


negate : Vector -> Vector
negate vector =
    scale -1 vector


normalise : Vector -> Vector
normalise ( a, b ) =
    let
        length =
            magnitude ( a, b )
    in
        ( a / length, b / length )


magnitude : Vector -> Float
magnitude ( a, b ) =
    sqrt (square a + square b)


square : number -> number
square n =
    n ^ 2
