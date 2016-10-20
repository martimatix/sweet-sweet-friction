module CircularCollision
    exposing
        ( Circle
        , Vector
        , collision
        , velocityAfterCollision
        , circleAtCollision
        )


type alias Circle =
    { cx : Float
    , cy : Float
    , radius : Float
    }


type alias Vector =
    ( Float, Float )


collision : Circle -> Circle -> Bool
collision circle1 circle2 =
    let
        dx =
            circle1.cx - circle2.cx

        dy =
            circle1.cy - circle2.cy

        distance =
            hypotenuse dx dy
    in
        distance < circle1.radius + circle2.radius



-- velocityAfterCollision formula based on response from:
-- http://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector


velocityAfterCollision : Circle -> Circle -> Vector -> Vector
velocityAfterCollision stationaryCircle movingCircle velocity =
    let
        mirror =
            unitMirrorVector stationaryCircle movingCircle
    in
        vectorSubtraction velocity (scale (2 * dotProduct velocity mirror) mirror)


circleAtCollision : Circle -> Circle -> Vector -> Circle
circleAtCollision stationaryCircle movingCircle velocity =
    let
        adjustmentVector =
            adjustmentToCollision stationaryCircle movingCircle velocity

        ( collisionPointX, collisionPointY ) =
            vectorAddition ( movingCircle.cx, movingCircle.cy ) adjustmentVector
    in
        Circle collisionPointX collisionPointY movingCircle.radius


adjustmentToCollision : Circle -> Circle -> Vector -> Vector
adjustmentToCollision stationaryCircle movingCircle velocity =
    let
        circleCentresX =
            movingCircle.cx - stationaryCircle.cx

        circleCentresY =
            movingCircle.cy - stationaryCircle.cy

        circleCentresVector =
            ( circleCentresX, circleCentresY )

        sumCircleRadii =
            stationaryCircle.radius + movingCircle.radius

        circleCentresDistance =
            hypotenuse circleCentresX circleCentresY

        negatedVelocityUnitVector =
            velocity
                |> negate
                |> normalise
    in
        scale (2 * (sumCircleRadii - circleCentresDistance)) <|
            negatedVelocityUnitVector


vectorAddition : Vector -> Vector -> Vector
vectorAddition ( a, b ) ( x, y ) =
    ( a + x, b + y )


vectorSubtraction : Vector -> Vector -> Vector
vectorSubtraction ( a, b ) ( x, y ) =
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
        magnitude =
            hypotenuse a b
    in
        ( a / magnitude, b / magnitude )


unitMirrorVector : Circle -> Circle -> Vector
unitMirrorVector stationaryCircle movingCircle =
    let
        x =
            stationaryCircle.cx - movingCircle.cx

        y =
            stationaryCircle.cy - movingCircle.cy

        magnitude =
            hypotenuse x y
    in
        ( x / magnitude, y / magnitude )


square : number -> number
square n =
    n ^ 2


hypotenuse : Float -> Float -> Float
hypotenuse a b =
    sqrt (square a + square b)
