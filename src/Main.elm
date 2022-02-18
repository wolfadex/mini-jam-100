module Main exposing (main)

-- import Color.Convert

import Angle
import Axis3d
import Browser exposing (Document)
import Camera3d
import Color
import Direction3d
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Length
import LineSegment3d
import Physics.Body
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World exposing (World)
import Pixels
import Point3d
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
    { physicsWorld : World ( Entity WorldCoordinates, Bool )
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { physicsWorld =
            Physics.World.empty
                |> Physics.World.add
                    (Physics.Body.plane
                        ( Scene3d.quadWithShadow (Scene3d.Material.color Color.green)
                            (Point3d.meters 10 10 0)
                            (Point3d.meters 10 -10 0)
                            (Point3d.meters -10 -10 0)
                            (Point3d.meters -10 10 0)
                        , False
                        )
                    )
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


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
                        { eyePoint = Point3d.meters 20 20 20
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    el [ height fill ]
        (html
            (Scene3d.sunny
                { background = Scene3d.transparentBackground
                , camera = camera
                , clipDepth = Length.millimeter
                , dimensions = ( Pixels.int 800, Pixels.int 600 )
                , entities =
                    List.map
                        (\body ->
                            let
                                ( entity, _ ) =
                                    Physics.Body.data body
                            in
                            entity
                        )
                        (Physics.World.bodies model.physicsWorld)
                , shadows = True
                , sunlightDirection = Direction3d.negativeZ
                , upDirection = Direction3d.positiveZ
                }
            )
        )
