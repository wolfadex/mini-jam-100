module Main exposing (main)

-- import Color.Convert

import Acceleration exposing (Acceleration)
import Angle
import Axis3d
import Block3d exposing (Block3d)
import Browser exposing (Document)
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Length exposing (Meters)
import LineSegment3d
import Mass
import Physics.Body
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World exposing (World)
import Pixels
import Point3d
import Random exposing (Seed)
import Scene3d exposing (Entity)
import Scene3d.Material
import Sphere3d
import Viewpoint3d


main : Program () Model Msg
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
    , elapsedTime : Float
    , seed : Seed
    , physicsWorld : World GameShape
    }


type GameShape
    = Static (Entity WorldCoordinates)
    | Ball Color.Color
    | Player


init : () -> ( Model, Cmd Msg )
init () =
    ( { lastBallDrop = 0
      , ballsToAdd = 50
      , elapsedTime = 0
      , seed = Random.initialSeed 0
      , physicsWorld =
            Physics.World.empty
                |> Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.8) Direction3d.negativeZ
                -- Ground
                |> Physics.World.add
                    (Physics.Body.plane
                        (Static
                            (Scene3d.quadWithShadow (Scene3d.Material.matte Color.black)
                                (Point3d.meters 10 10 0)
                                (Point3d.meters 10 -10 0)
                                (Point3d.meters -10 -10 0)
                                (Point3d.meters -10 10 0)
                            )
                        )
                    )
                -- Raised ground
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 10 4 0) (Point3d.meters -4 10 2)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.white) block))
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 4 -10 0) (Point3d.meters -10 -4 2)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.white) block))
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 4 -10 0) (Point3d.meters 10 4 2)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.white) block))
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -4 -4 0) (Point3d.meters -10 10 2)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.white) block))
                    )
                -- Barriers
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -12 -10 0) (Point3d.meters 12 -12 6)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.black) block))
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -12 10 0) (Point3d.meters 12 12 6)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.black) block))
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 10 12 0) (Point3d.meters 12 -12 6)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.black) block))
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -10 12 0) (Point3d.meters -12 -12 6)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.black) block))
                    )
                -- Lip
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -4 -4 3.5) (Point3d.meters 4 4 4.1)
                     in
                     Physics.Body.block block
                        (Static (Scene3d.blockWithShadow (Scene3d.Material.matte Color.red) block))
                    )
                -- Spiller
                |> Physics.World.add
                    (let
                        sphere =
                            Sphere3d.atPoint (Point3d.meters 0 0 4.1) (Length.meters 3.9)
                     in
                     Physics.Body.sphere sphere
                        (Static (Scene3d.sphereWithShadow (Scene3d.Material.matte Color.darkGray) sphere))
                    )
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        ]


type Msg
    = NoOp
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick deltaMs ->
            let
                partialModelUpdate : Model
                partialModelUpdate =
                    if model.ballsToAdd > 0 && model.elapsedTime + deltaMs - model.lastBallDrop > 200 then
                        let
                            ( config, seed ) =
                                Random.step
                                    (Random.map3 (\x y hue -> { randomX = x, randomY = y, color = Color.hsl hue 1.0 0.5 })
                                        (Random.float -0.1 0.1)
                                        (Random.float -0.1 0.1)
                                        (Random.float 0 1)
                                    )
                                    model.seed
                        in
                        { model
                            | physicsWorld =
                                model.physicsWorld
                                    |> Physics.World.add
                                        (Physics.Body.sphere (Sphere3d.atOrigin (Length.meters 0.25)) (Ball config.color)
                                            |> Physics.Body.moveTo (Point3d.meters config.randomX config.randomY 25)
                                            |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 10))
                                        )
                            , ballsToAdd = model.ballsToAdd - 1
                            , lastBallDrop = model.elapsedTime + deltaMs
                            , seed = seed
                        }

                    else
                        model
            in
            ( { partialModelUpdate
                | elapsedTime = model.elapsedTime + deltaMs
                , physicsWorld = Physics.World.simulate (Duration.milliseconds deltaMs) partialModelUpdate.physicsWorld

                -- |> Physics.World.update
                --     (\body ->
                --         let
                --             ( entity, isPlayer ) =
                --                 Physics.Body.data body
                --             position =
                --                 Physics.Body.originPoint body
                --         in
                --         Physics.Body.withData
                --             ( Scene3d.translateBy entity, isPlayer )
                --             body
                --     )
              }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Collect Them All"
    , body = [ layout [ width fill, height fill ] (viewModel model) ]
    }


viewModel : Model -> Element Msg
viewModel model =
    column
        [ width fill, height fill ]
        [ row
            [ width fill
            , paddingXY 16 8
            , Background.color (rgb 0 0 0)
            , Font.color (rgb 0 1 0)
            ]
            [ text "Collect Them All" ]
        , view3dScene model
        ]


view3dScene : Model -> Element Msg
view3dScene model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 10 0 40
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    el [ height fill ]
        (html
            (Scene3d.sunny
                { background = Scene3d.backgroundColor Color.black
                , camera = camera
                , clipDepth = Length.millimeter
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , entities =
                    List.map
                        (\body ->
                            case Physics.Body.data body of
                                Static entity ->
                                    entity

                                Ball color ->
                                    Scene3d.sphereWithShadow
                                        (Scene3d.Material.matte color)
                                        (Sphere3d.atPoint (Physics.Body.originPoint body) (Length.meters 0.25))

                                Player ->
                                    Debug.todo "TODO"
                        )
                        (Physics.World.bodies model.physicsWorld)
                , shadows = True
                , sunlightDirection = Direction3d.negativeZ
                , upDirection = Direction3d.positiveZ
                }
            )
        )
