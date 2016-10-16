module CircularCollision exposing
  ( Circle
  , circle
  , circleToCircle
  )

type Circle = Circle { cx: Float, cy: Float, radius : Float }


circle : Float -> Float -> Float -> Circle
circle centerX centerY radius =
  Circle { cx = centerX, cy = centerY, radius = radius }


circleToCircle : Circle -> Circle -> Bool
circleToCircle (Circle circle1) (Circle circle2) =
  let
    dx = circle1.cx - circle2.cx
    dy = circle1.cy - circle2.cy
    distance = sqrt ((dx * dx) + (dy * dy))
  in
    if distance < circle1.radius + circle2.radius then
      True
    else
      False
