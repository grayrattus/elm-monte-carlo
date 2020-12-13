module Counter exposing (Model)

import Utils exposing (Rectangle, Circle, Point)
import Debug exposing (log)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Html exposing (..)
import Task
import Time
import Random
import Utils exposing (
  randomSmallRectangle, 
  createRectangle, 
  pointsToHtml,
  createPoint,
  pointInsideCircle,
  calculatePi,
  rectangleToHtml)
import Utils exposing (createCircle, circleToHtml)
import List
import Utils exposing (pointInsideRectangle)


-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

defaultRadius = 300 * 2

type alias Model =
  { 
   time : Time.Posix
  , disableDrawing: Bool
  , points: List(Point)
  , circle: Circle
  , rectangle: Rectangle
  , x: Float
  , y: Float
  , pointsInCircle: Int
  , pointsInSquare: Int
  , monteCarloPi: Float
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Time.millisToPosix 0) False [] (createCircle (defaultRadius / 2) (defaultRadius / 2) (defaultRadius / 2)) (createRectangle (defaultRadius / 2) (defaultRadius / 2) defaultRadius defaultRadius) 0 0 0 0 0 
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | NewRandomX Float
  | NewRandomY Float
  | DisableDrawing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> 
      (model
      , Random.generate NewRandomX (Random.float 0 defaultRadius))

    NewRandomX newX ->
      ({ model | x = newX }
      , Random.generate NewRandomY (Random.float 0 defaultRadius))

    NewRandomY newY ->
      let
        newPoint = createPoint model.x newY
        points = List.append model.points [newPoint]
        total = List.length points
        pointInCircle = if (pointInsideCircle  model.circle newPoint) then 1 else 0 
        pointInSquare = if (pointInsideRectangle newPoint model.rectangle) then 1 else 0

        pointsInCircle = pointInCircle + model.pointsInCircle
        pointsInSquare = pointInSquare + model.pointsInSquare
        monteCarloPi = calculatePi pointsInCircle pointsInSquare
      in
        ({ model | points = points
        , y = newY
        , monteCarloPi = monteCarloPi
        , pointsInCircle = pointsInCircle
        , pointsInSquare = pointsInSquare
        }
        , Cmd.none )

    DisableDrawing -> 
      ({model | disableDrawing = not model.disableDrawing}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 0 Tick

-- VIEW

view : Model -> Html Msg
view model =
  let
    htmlCircle = [circleToHtml(model.circle)]
    htmlRectangle = [rectangleToHtml(model.rectangle)]
    shapes = htmlRectangle ++ htmlCircle ++ if model.disableDrawing then [] else (pointsToHtml model.points)
    pi = p [] [text ("PI: " ++ (String.fromFloat model.monteCarloPi))]
    pointsInCircle = p [] [text ("Inside circle: " ++ (String.fromInt model.pointsInCircle))]
    pointsInSqare = p [] [text ("Inside square: " ++ (String.fromInt model.pointsInSquare))]
    totalPoints = p [] [text ("Total: " ++ (String.fromInt (List.length model.points)))]
  in
  div [] [
    div [
      style "width" ((String.fromInt defaultRadius) ++ "px")
    ,style "height" ((String.fromInt defaultRadius) ++ "px")
    ,style "position" "relative"
    ,style "background-color" "gray"
    ] (shapes),
    pi,
    button [ onClick DisableDrawing ] [ text "Toggle drawing" ],
    pointsInCircle,
    pointsInSqare,
    totalPoints
  ]