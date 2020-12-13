module Utils exposing (
  Rectangle,
  Circle,
  Point,
  randomSmallRectangle,
  createRectangle,
  createPoint,
  pointsToHtml,
  rectangleToHtml,
  createCircle,
  calculatePi,
  circleToHtml,
  pointInsideCircle,
  pointInsideRectangle
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Random
import List

type alias Point = 
  {
    x: Float,
    y: Float
  }

type alias Rectangle =
  {
    x: Float,
    y: Float,
    width: Float,
    height: Float,
    p1: Point,
    p2: Point,
    p3: Point,
    p4: Point
  }
type alias Circle =
  {
    x: Float,
    y: Float,
    radius: Float
  }

createPoint : Float -> Float -> Point
createPoint x y = { x = x, y = y}

createRectangle : Float -> Float -> Float -> Float -> Rectangle
createRectangle x y width height = 
  {
    x = x,
    y = y,
    width = width,
    height = height,
    p1 = { x = x + (width / 2), y = y - (height / 2)},
    p2 = { x = x + (width / 2), y = y + (height / 2)},
    p3 = { x = x - (width / 2), y = y + (height / 2)},
    p4 = { x = x - (width / 2), y = y - (height / 2)}
  }


randomSmallRectangle : Float -> Float -> Rectangle
randomSmallRectangle x y = createRectangle x y 2 2 

pointsToHtml : List(Point) -> List(Html msg)
pointsToHtml points = 
  List.map (fixedSizePointToHtml) points

fixedSizePointToHtml : Point -> Html msg
fixedSizePointToHtml point = 
  pointToHtml point 2

pointToHtml : Point -> Float -> Html msg
pointToHtml point size = 
  div [ style "left" ((String.fromFloat(point.x - (size / 2))) ++ "px")
      , style "top" ((String.fromFloat(point.y - (size / 2))) ++ "px")
      , style "width" ((String.fromFloat(size)) ++ "px")
      , style "height" ((String.fromFloat(size)) ++ "px")
      , style "position" "absolute"
      , style "background-color" "purple"
      ] []


rectangleToHtml : Rectangle -> Html msg
rectangleToHtml rectangle = 
  div [ style "left" ((String.fromFloat(rectangle.x - (rectangle.width / 2))) ++ "px")
      , style "top" ((String.fromFloat(rectangle.y - (rectangle.height / 2))) ++ "px")
      , style "width" ((String.fromFloat(rectangle.width)) ++ "px")
      , style "height" ((String.fromFloat(rectangle.height)) ++ "px")
      , style "position" "absolute"
      , style "background-color" "pink"
      ] []

createCircle : Float -> Float -> Float -> Circle
createCircle x y radius =
  {
    x = x,
    y = y,
    radius = radius
  }

circleToHtml : Circle -> Html msg
circleToHtml c =
  div [
    style "position" "absolute"
    ,style "left" ((String.fromFloat(c.x - c.radius)) ++ "px")
    ,style "top" ((String.fromFloat(c.y - c.radius)) ++ "px")
    ,style "width" ((String.fromFloat(c.radius * 2)) ++ "px")
    ,style "height" ((String.fromFloat(c.radius * 2)) ++ "px")
    ,style "border-radius" ((String.fromFloat(c.radius)) ++ "px")
    ,style "background-color" "white"
  ] []

pointInsideCircle : Circle -> Point -> Bool
pointInsideCircle circle point = 
  Basics.sqrt(((point.x - circle.x)^2) + ((point.y - circle.y)^2)) < circle.radius

pointInsideRectangle : Point -> Rectangle -> Bool
pointInsideRectangle point rectangle =
  let 
    bs = (triangleArea rectangle.p1 rectangle.p2 rectangle.p3) + (triangleArea rectangle.p1 rectangle.p4 rectangle.p3)
    firstA = triangleArea point rectangle.p1 rectangle.p2
    secondA = triangleArea point rectangle.p2 rectangle.p3
    thirdA = triangleArea point rectangle.p3 rectangle.p4
    fourthA = triangleArea point rectangle.p1 rectangle.p4
 in 
  bs == (firstA + secondA + thirdA + fourthA)

triangleArea : Point -> Point -> Point -> Float
triangleArea point1 point2 point3 = 
  Basics.abs(
    ((point1.x * (point2.y - point3.y))
    + (point2.x * (point3.y - point1.y))
    + (point3.x * (point1.y - point2.y)))
    / 2
    )

calculatePi : Int -> Int -> Float
calculatePi inCircle inSquare = 4 * (Basics.toFloat inCircle) / (Basics.toFloat inSquare)
