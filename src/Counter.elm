module Counter exposing (Model)

import Utils exposing (Rectangle, Circle, Point)

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
  , monteCarloPi: Float
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) [] (createCircle 150 150 defaultRadius) (createRectangle 50 100 defaultRadius defaultRadius) 0 0 0 0 
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
      , Random.generate NewRandomX (Random.float 0 300))

    NewRandomX newX ->
      ({ model | x = newX }
      , Random.generate NewRandomY (Random.float 0 300))

    NewRandomY newY ->
      let
        newPoint = createPoint model.x newY
        points = List.append model.points [newPoint]
        total = List.length points
        pointInCircle = if (pointInsideCircle  model.circle newPoint) then 1 else 0 
        pointsInCircle = pointInCircle + model.pointsInCircle
        monteCarloPi = calculatePi pointInCircle total
      in
        ({ model | points = points
        , y = newY
        , monteCarloPi = monteCarloPi
        , pointsInCircle = model.pointsInCircle + pointInCircle}
        , Cmd.none )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 10 Tick


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
    pi = p [] [text (String.fromFloat model.monteCarloPi)]
  in
  div [] [
    div [
      style "width" "300px"
    ,style "height" "300px" 
    ,style "position" "relative"
    ,style "background-color" "gray"
    ] (shapes),
    pi
  ]