module Counter exposing (Model)

import Utils exposing (Rectangle, Circle, Point)
import Debug exposing (log)

import Html.Attributes exposing (..)
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

defaultRadius = 50

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
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
  ( Model Time.utc (Time.millisToPosix 0) [] (createCircle 150 50 defaultRadius) (createRectangle 50 50 defaultRadius defaultRadius) 0 0 0 0 0 
  , Task.perform AdjustTimeZone Time.here
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | NewRandomX Float
  | NewRandomY Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime -> 
      (model
      , Random.generate NewRandomX (Random.float 0 200))

    NewRandomX newX ->
      ({ model | x = newX }
      , Random.generate NewRandomY (Random.float 0 100))

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

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
    htmlCircle = [circleToHtml(model.circle)]
    htmlRectangle = [rectangleToHtml(model.rectangle)]
    shapes = htmlCircle ++ htmlRectangle ++ (pointsToHtml model.points)
    pi = p [] [text ("PI: " ++ (String.fromFloat model.monteCarloPi))]
    pointsInCircle = p [] [text ("Inside circle: " ++ (String.fromInt model.pointsInCircle))]
    pointsInSqare = p [] [text ("Inside square: " ++ (String.fromInt model.pointsInSquare))]
    totalPoints = p [] [text ("Total: " ++ (String.fromInt (List.length model.points)))]
  in
  div [] [
    div [
      style "width" "200px"
    ,style "height" "100px" 
    ,style "position" "relative"
    ,style "background-color" "gray"
    ] (shapes),
    pi,
    pointsInCircle,
    pointsInSqare,
    totalPoints
  ]