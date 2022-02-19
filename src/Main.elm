port module Main exposing (main)

import Acceleration
import Angle
import Axis3d exposing (Axis3d)
import Block3d
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Color.Manipulate
import Direction3d
import Duration
import Force
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Length exposing (Length, Meters)
import Mass
import Physics.Body
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World exposing (World)
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Quantity exposing (Quantity)
import Random exposing (Seed)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material
import Sphere3d
import Task
import Viewpoint3d


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { lastBallDrop : Float
    , ballsToAdd : Int
    , nextBallsToAdd : Int
    , ballSize : Length
    , ballsCollected : Int
    , highScore : Int
    , elapsedTime : Float
    , floorColor : Color.Color
    , seed : Seed
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , physicsWorld : World ( GameShape, Bool )
    }


type GameShape
    = Static (Entity WorldCoordinates)
    | ColorChanging
    | Ball Int Color.Color Seed Float


init : Int -> ( Model, Cmd Msg )
init highScore =
    ( { lastBallDrop = 0
      , ballsToAdd = 1
      , nextBallsToAdd = 2
      , ballSize = Length.meters 2
      , ballsCollected = 0
      , highScore = highScore
      , elapsedTime = 0
      , floorColor = Color.white
      , seed = Random.initialSeed 0
      , width = Pixels.pixels 0
      , height = Pixels.pixels 0
      , physicsWorld =
            Physics.World.empty
                |> Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.8) Direction3d.negativeZ
                -- Ground
                |> Physics.World.add
                    (Physics.Body.plane
                        ( ColorChanging, True )
                    )
                -- Barriers
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -12 -10 0) (Point3d.meters 12 -12 40)
                     in
                     Physics.Body.block block
                        ( Static Scene3d.nothing, True )
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -12 10 0) (Point3d.meters 12 12 40)
                     in
                     Physics.Body.block block
                        ( Static Scene3d.nothing, True )
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 10 12 0) (Point3d.meters 12 -12 40)
                     in
                     Physics.Body.block block
                        ( Static Scene3d.nothing, True )
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -10 12 0) (Point3d.meters -12 -12 40)
                     in
                     Physics.Body.block block
                        ( Static Scene3d.nothing, True )
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -12 -12 40) (Point3d.meters 12 12 41)
                     in
                     Physics.Body.block block
                        ( Static Scene3d.nothing, True )
                    )
                -- Spiller
                |> Physics.World.add
                    (let
                        sphere =
                            Sphere3d.atPoint Point3d.origin (Length.meters 3.9)
                     in
                     Physics.Body.sphere sphere
                        ( Static (Scene3d.sphereWithShadow (Scene3d.Material.matte Color.black) sphere)
                        , True
                        )
                    )
      }
    , Task.perform
        (\{ viewport } ->
            Resize (round viewport.width) (round viewport.height)
        )
        Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize Resize
        ]


port saveScore : Int -> Cmd msg


type Msg
    = Tick Float
    | Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model
                | width = Pixels.float (toFloat w)
                , height = Pixels.float (toFloat h)
              }
            , Cmd.none
            )

        MouseDown mouseRay ->
            ( case Physics.World.raycast mouseRay model.physicsWorld of
                Just raycastResult ->
                    case Physics.Body.data raycastResult.body of
                        ( Ball idClicked color seed lastImpulse, _ ) ->
                            { model
                                | physicsWorld =
                                    Physics.World.update
                                        (\body ->
                                            case Physics.Body.data body of
                                                ( Ball id _ _ _, _ ) ->
                                                    if id == idClicked then
                                                        Physics.Body.withData
                                                            ( Ball id color seed lastImpulse
                                                            , False
                                                            )
                                                            body

                                                    else
                                                        body

                                                _ ->
                                                    body
                                        )
                                        model.physicsWorld
                            }

                        _ ->
                            model

                Nothing ->
                    model
            , Cmd.none
            )

        Tick deltaMs ->
            let
                newElapsedTime : Float
                newElapsedTime =
                    model.elapsedTime + deltaMs

                partialModelUpdate : Model
                partialModelUpdate =
                    if model.ballsToAdd > 0 && newElapsedTime - model.lastBallDrop > 150 then
                        let
                            ( config, seed ) =
                                Random.step
                                    (Random.map3 (\x y hue -> { randomX = x, randomY = y, color = Color.hsl hue 1.0 0.5 })
                                        (Random.float -2 2)
                                        (Random.float -2 2)
                                        (Random.float 0 1)
                                    )
                                    model.seed

                            spawnPoint =
                                Point3d.meters config.randomX config.randomY 15
                        in
                        { model
                            | physicsWorld =
                                model.physicsWorld
                                    |> Physics.World.add
                                        (( Ball model.ballsToAdd config.color seed newElapsedTime, True )
                                            |> Physics.Body.sphere (Sphere3d.atOrigin model.ballSize)
                                            |> Physics.Body.moveTo spawnPoint
                                            |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 10))
                                            |> Physics.Body.applyImpulse
                                                (Quantity.times (Duration.seconds 0.05) (Force.newtons 500))
                                                Direction3d.negativeZ
                                                spawnPoint
                                        )
                            , ballsToAdd = model.ballsToAdd - 1
                            , lastBallDrop = newElapsedTime
                            , seed = seed
                        }

                    else
                        model

                impulseAppliedWorld : World ( GameShape, Bool )
                impulseAppliedWorld =
                    Physics.World.update
                        (\body ->
                            case Physics.Body.data body of
                                ( Ball id color seed lastImpulse, True ) ->
                                    if newElapsedTime - lastImpulse > 500 then
                                        let
                                            ( impulseDirection, nextSeed ) =
                                                Random.step
                                                    (Random.map2 Direction3d.xyZ
                                                        (Random.map Angle.degrees (Random.float 0 360))
                                                        (Random.map Angle.degrees (Random.float -180 0))
                                                    )
                                                    seed
                                        in
                                        Physics.Body.applyImpulse
                                            (Quantity.times (Duration.seconds 0.05) (Force.newtons 1000))
                                            impulseDirection
                                            (Physics.Body.originPoint body)
                                            (Physics.Body.withData ( Ball id color nextSeed newElapsedTime, True ) body)

                                    else
                                        body

                                _ ->
                                    body
                        )
                        partialModelUpdate.physicsWorld

                simulatedWorld : World ( GameShape, Bool )
                simulatedWorld =
                    Physics.World.simulate (Duration.milliseconds deltaMs) impulseAppliedWorld

                ( mixedColors, newBallsCollected ) =
                    List.foldl
                        (\body ( result, count ) ->
                            case Physics.Body.data body of
                                ( Ball _ color _ _, False ) ->
                                    ( Color.Manipulate.mix color result, count + 1 )

                                _ ->
                                    ( result, count )
                        )
                        ( model.floorColor, model.ballsCollected )
                        (Physics.World.bodies simulatedWorld)

                keptPhysicalWorld : World ( GameShape, Bool )
                keptPhysicalWorld =
                    Physics.World.keepIf (Physics.Body.data >> Tuple.second) simulatedWorld

                remainingBalls : Int
                remainingBalls =
                    List.foldl
                        (\body remaining ->
                            case Physics.Body.data body of
                                ( Ball _ _ _ _, _ ) ->
                                    remaining + 1

                                _ ->
                                    remaining
                        )
                        0
                        (Physics.World.bodies keptPhysicalWorld)
            in
            ( { partialModelUpdate
                | elapsedTime = newElapsedTime
                , floorColor = mixedColors
                , ballsCollected = newBallsCollected
                , physicsWorld = keptPhysicalWorld
                , ballsToAdd =
                    if remainingBalls == 0 then
                        partialModelUpdate.nextBallsToAdd

                    else
                        partialModelUpdate.ballsToAdd
                , nextBallsToAdd =
                    if remainingBalls == 0 then
                        partialModelUpdate.nextBallsToAdd * 2

                    else
                        partialModelUpdate.nextBallsToAdd
                , ballSize =
                    if remainingBalls == 0 then
                        Quantity.divideBy 1.25 partialModelUpdate.ballSize

                    else
                        partialModelUpdate.ballSize
              }
            , saveScore newBallsCollected
            )


view : Model -> Document Msg
view model =
    { title = "Collect Them All!"
    , body =
        [ view3dScene model
        , Html.div
            [ Html.Attributes.class "score"
            ]
            [ Html.text ("High Score: " ++ String.fromInt (max model.highScore model.ballsCollected))
            , Html.br [] []
            , Html.text ("Collected: " ++ String.fromInt model.ballsCollected)
            ]
        , Html.div
            [ Html.Attributes.class "title"
            ]
            [ Html.text "Click them all!!!"
            ]
        ]
    }


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 0 0 40
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }


view3dScene : Model -> Html Msg
view3dScene model =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (decodeMouseRay camera model.width model.height MouseDown)
        ]
        [ Scene3d.sunny
            { background = Scene3d.backgroundColor Color.black
            , camera = camera
            , clipDepth = Length.millimeter
            , dimensions =
                ( Pixels.int (round (Pixels.toFloat model.width))
                , Pixels.int (round (Pixels.toFloat model.height))
                )
            , entities =
                List.map
                    (\body ->
                        case Physics.Body.data body of
                            ( Static entity, _ ) ->
                                entity

                            ( ColorChanging, _ ) ->
                                Scene3d.quadWithShadow (Scene3d.Material.matte model.floorColor)
                                    (Point3d.meters 10 10 0)
                                    (Point3d.meters 10 -10 0)
                                    (Point3d.meters -10 -10 0)
                                    (Point3d.meters -10 10 0)

                            ( Ball _ color _ _, _ ) ->
                                Scene3d.sphereWithShadow
                                    (Scene3d.Material.matte color)
                                    (Sphere3d.atPoint (Physics.Body.originPoint body) model.ballSize)
                    )
                    (Physics.World.bodies model.physicsWorld)
            , shadows = True
            , sunlightDirection = Direction3d.negativeZ
            , upDirection = Direction3d.positiveZ
            }
        ]


decodeMouseRay :
    Camera3d Meters WorldCoordinates
    -> Quantity Float Pixels
    -> Quantity Float Pixels
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay camera3d w h rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg
                (Camera3d.ray
                    camera3d
                    (Rectangle2d.with
                        { x1 = Pixels.pixels 0
                        , y1 = h
                        , x2 = w
                        , y2 = Pixels.pixels 0
                        }
                    )
                    (Point2d.pixels x y)
                )
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)
