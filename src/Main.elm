port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Element)
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onKeyUp, onMouseMove, onResize)
import Element exposing (Element, alignLeft, alignRight, alignTop, behindContent, centerX, column, el, fill, html, padding, paddingEach, paddingXY, paragraph, px, rgb, row, scrollbarY, shrink, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Json.Decode as Decode
import Math exposing (Player, accelerate)
import Math.Vector2 exposing (vec2)
import Set exposing (Set)
import WebGL


port onPointerLock : (Bool -> msg) -> Sub msg


port ready : {} -> Cmd msg


type alias Model =
    { player : Player
    , keys : Set String
    , pointerLock : Bool
    , window : Window
    , dropdowns : Array Bool
    , sidebars :
        { left : Bool
        , right : Bool
        }
    , resolution : Float
    , edgeWidth : Float
    }


type alias Window =
    { width : Int
    , height : Int
    }


type Msg
    = Frame Float
    | KeyCode { code : String, down : Bool }
    | MouseMove Int Int
    | PointerLock Bool
    | WindowResize Int Int
    | Dropdown Int Bool
    | LeftSidebar Bool
    | RightSidebar Bool
    | Resolution Float
    | EdgeWidth Float


type alias Flags =
    { window : Window }


main : Program Flags Model Msg
main =
    Browser.document
        { init = \flags -> ( init flags, ready {} )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


init : Flags -> Model
init { window } =
    { player = Math.defaultPlayer
    , keys = Set.empty
    , pointerLock = False
    , window = window
    , dropdowns = Array.fromList [ True, False, True, False ]
    , sidebars = { left = True, right = True }
    , resolution = 0.5
    , edgeWidth = 0.1
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Frame dt ->
            let
                ifKey : String -> Float -> Float
                ifKey =
                    \code val ->
                        if Set.member code model.keys then
                            val

                        else
                            0

                space =
                    vec2
                        (ifKey "KeyD" dt - ifKey "KeyA" dt)
                        (ifKey "Space" dt - ifKey "ShiftLeft" dt)

                time =
                    ifKey "KeyW" dt - ifKey "KeyS" dt
            in
            { model | player = model.player |> Math.moveTime time |> Math.moveSpace space }

        KeyCode { code, down } ->
            if down then
                { model | keys = Set.insert code model.keys }

            else
                { model | keys = Set.remove code model.keys }

        MouseMove x y ->
            { model
                | player =
                    Math.accelerate
                        (vec2 (toFloat x / 200) (toFloat y / -200))
                        model.player
            }

        PointerLock lock ->
            { model
                | pointerLock = lock
                , keys = Set.empty
            }

        WindowResize width height ->
            { model | window = { width = width, height = height } }

        Dropdown which status ->
            { model | dropdowns = Array.set which status model.dropdowns }

        LeftSidebar status ->
            let
                sidebars =
                    model.sidebars
            in
            { model | sidebars = { sidebars | left = status } }

        RightSidebar status ->
            let
                sidebars =
                    model.sidebars
            in
            { model | sidebars = { sidebars | right = status } }

        Resolution resolution ->
            { model | resolution = resolution }

        EdgeWidth edgeWidth ->
            { model | edgeWidth = edgeWidth }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.pointerLock then
            runningSubscriptions model

          else
            Sub.none
        , onKeyDown <| Decode.map (\code -> KeyCode { code = code, down = True }) <| Decode.field "code" Decode.string
        , onKeyUp <| Decode.map (\code -> KeyCode { code = code, down = False }) <| Decode.field "code" Decode.string
        , onPointerLock PointerLock
        , onResize WindowResize
        ]


runningSubscriptions : Model -> Sub Msg
runningSubscriptions model =
    Sub.batch
        [ onMouseMove <| Decode.map2 MouseMove (Decode.field "movementX" Decode.int) (Decode.field "movementY" Decode.int)
        , -- Only process frames if actually necessary, to save CPU.
          if Set.isEmpty model.keys then
            Sub.none

          else
            onAnimationFrameDelta (\dt -> Frame (dt / 1000))
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Gravity"
    , body =
        [ Element.layout
            [ Background.color (rgb 0 0 0)
            , behindContent (viewGame model)
            , Font.size (scaled 1)
            , Font.color (rgb 0.7 0.7 0.7)
            ]
            (viewHud model)
        ]
    }


viewGame : Model -> Element msg
viewGame { player, window, resolution, edgeWidth } =
    el [ Element.pointer ] <|
        html <|
            WebGL.toHtml
                [ Html.Attributes.id "game"
                , Html.Attributes.width (round <| toFloat window.width * resolution)
                , Html.Attributes.height (round <| toFloat window.height * resolution)
                , Html.Attributes.style "width" (String.fromInt window.width)
                , Html.Attributes.style "height" (String.fromInt window.height)
                ]
                [ Math.render
                    { player = player
                    , aspect = toFloat window.width / toFloat window.height
                    , edgeWidth = edgeWidth
                    }
                ]


viewHud : Model -> Element Msg
viewHud model =
    column
        [ Element.width fill
        , Element.height (px 0)
        ]
        [ row
            [ Element.width fill
            , Element.height (px 0)
            ]
            [ viewLeftSidebar model
            , viewRightSidebar model
            ]
        , if model.pointerLock then
            Element.none

          else
            viewReminder
        ]


viewReminder : Element msg
viewReminder =
    paragraph
        [ Border.color borderColor
        , Border.width 5
        , Background.color backgroundColor
        , padding 10
        , centerX
        , Element.alignTop
        , Element.width shrink
        , Font.bold
        , Font.color (rgb 0.8 0.6 0.8)
        , Font.size (scaled 4)
        ]
        [ text "Paused" ]


viewLeftSidebar : Model -> Element Msg
viewLeftSidebar { player, dropdowns, sidebars, window } =
    let
        width =
            window.width // 9

        ( symbol, content ) =
            if sidebars.left then
                ( "◂"
                , column
                    [ Element.width shrink
                    , Background.color borderColor
                    , padding 5
                    , spacing 5
                    ]
                  <|
                    List.map2 (dropdown { width = width })
                        [ { title = "Orbit"
                          , msg = Dropdown 0
                          , content = el [] <| viewOrbit (width - 10) player
                          }
                        , { title = "Stats"
                          , msg = Dropdown 1
                          , content = viewStats player
                          }
                        ]
                    <|
                        Array.toList dropdowns
                )

            else
                ( "▸", Element.none )
    in
    row [ alignLeft, alignTop ]
        [ content
        , el
            [ Font.bold
            , Font.size (scaled 3)
            , alignTop
            , Background.color backgroundColor
            , Border.color borderColor
            , Border.widthEach { bottom = 5, top = 5, right = 5, left = 0 }
            , paddingXY 5 2
            , Element.pointer
            , Events.onClick (LeftSidebar (not sidebars.left))
            ]
            (text symbol)
        ]


viewRightSidebar : Model -> Element Msg
viewRightSidebar model =
    let
        width =
            2 * model.window.width // 9

        ( symbol, content ) =
            if model.sidebars.right then
                ( "▸"
                , column
                    [ Element.width shrink
                    , Background.color borderColor
                    , padding 5
                    , spacing 5
                    ]
                  <|
                    el [ Background.color backgroundColor, Element.width fill ]
                        (Element.link [ centerX, padding 5 ] { url = "", label = text "[src]" })
                        :: Element.textColumn
                            [ Background.color backgroundColor
                            , Font.size (scaled -1)
                            , padding 5
                            , Font.center
                            , Font.italic
                            , Element.width (px width)
                            ]
                            [ Element.paragraph [] [ text "Meant for use on a computer. " ]
                            , Element.paragraph [] [ text "Does not support Internet Explorer." ]
                            ]
                        :: List.map2 (dropdown { width = width })
                            [ { title = "Explanation"
                              , msg = Dropdown 2
                              , content = viewExplanation (model.window.height // 2)
                              }
                            , { title = "Settings"
                              , msg = Dropdown 3
                              , content = viewSettings model
                              }
                            ]
                            (List.drop 2 <| Array.toList model.dropdowns)
                )

            else
                ( "◂", Element.none )
    in
    row [ alignRight, alignTop ]
        [ el
            [ Font.bold
            , Font.size (scaled 3)
            , alignTop
            , Background.color backgroundColor
            , Border.color borderColor
            , Border.widthEach { bottom = 5, top = 5, left = 5, right = 0 }
            , paddingXY 5 2
            , Element.pointer
            , Events.onClick (RightSidebar (not model.sidebars.right))
            ]
            (text symbol)
        , content
        ]


viewOrbit : Int -> Player -> Element msg
viewOrbit size player =
    el [ Element.width (px size), Element.height (px size), centerX ] <|
        html <|
            WebGL.toHtml
                [ Html.Attributes.width size
                , Html.Attributes.height size
                ]
                [ Math.hud
                    { player = player
                    , aspect = 1
                    , edgeWidth = 0 -- doesn't matter
                    }
                ]


viewStats : Player -> Element Msg
viewStats player =
    let
        stats =
            Math.stats player
    in
    Element.textColumn
        [ Font.center
        , Element.width fill
        , padding 5
        , spacing 5
        ]
        [ el
            [ Font.bold
            , Font.underline
            , Font.size (scaled 2)
            , Font.color (rgb 0.6 0.3 0.6)
            , paddingEach { top = 10, left = 0, right = 0, bottom = 0 }
            ]
            (text "Orbital")
        , el [ Font.bold ] <| text "Energy"
        , text <| String.left 7 <| String.fromFloat stats.energy
        , el [ Font.bold ] <| text "Angular Momentum"
        , text <| String.left 7 <| String.fromFloat stats.angularMomentum
        , el [ Font.bold ] <| text "Eccentricity"
        , text <| String.left 7 <| String.fromFloat stats.eccentricity
        , el [ Font.bold ] <| text "Periapsis"
        , text <| String.left 7 <| String.fromFloat stats.periapsis
        , el [ Font.bold ] <| text "Apoapsis"
        , text <| String.left 7 <| String.fromFloat stats.apoapsis
        , el
            [ Font.bold
            , Font.underline
            , Font.size (scaled 2)
            , Font.color (rgb 0.6 0.3 0.6)
            , paddingEach { top = 15, left = 0, right = 0, bottom = 0 }
            ]
            (text "Other")
        , el [ Font.bold ] <| text "Position"
        , text <|
            String.concat
                [ String.left 7 <| String.fromFloat <| Math.Vector2.getX stats.position
                , ", "
                , String.left 7 <| String.fromFloat <| Math.Vector2.getY stats.position
                ]
        , el [ Font.bold ] <| text "Velocity"
        , text <|
            String.concat
                [ String.left 7 <| String.fromFloat <| Math.Vector2.getX stats.velocity
                , ", "
                , String.left 7 <| String.fromFloat <| Math.Vector2.getY stats.velocity
                ]
        , el [ Font.bold ] <| text "Time"
        , text <| String.left 7 <| String.fromFloat stats.time
        ]


viewExplanation : Int -> Element msg
viewExplanation height =
    Element.textColumn
        [ Element.height (px height)
        , Element.width fill
        , Element.clipY
        , scrollbarY
        , padding 5
        , spacing 15
        , Font.center
        ]
        explanation


explanation : List (Element msg)
explanation =
    [ paragraph []
        [ text "The idea of general relativity is that what we feel as a gravitational force "
        , text "is in fact not a force, but simply a result of the curvature of spacetime. "
        ]
    , paragraph []
        [ text "This is typically demonstrated with an analogy involving a heavy object on a rubber sheet. "
        , text "But that's actually a "
        , el [ Font.italic ] <| text "terrible "
        , text "picture of curved spacetime! "
        , text "It's curved, but that's about all it has going for it. "
        , text "So I'm attempting to do better. "
        ]
    , paragraph []
        [ text "In this project, I attempt to show the curved spacetime around a planet. "
        , text "To simplify the picture, I'm only showing two spacial dimensions. "
        , text "Including the time dimension, that makes three. "
        ]
    , paragraph []
        [ text "The spacial dimensions are shown as left/right and up/down. Time is shown as forward/backward. "
        ]
    , paragraph []
        [ text "I've filled the spacetime with grid-lines, to show the x, y, and t coordinates. "
        , text "The planet, at x=y=0, is shown in red. "
        , text "Far from the planet, the grid mostly looks like an ordinary cubic lattice. "
        , text "But near the planet, spacetime is strongly curved, so everything looks distorted. "
        ]
    , paragraph []
        [ text "Now click on the scene. "
        , text "This will \"lock\" the mouse pointer, and allow you to look around by moving the mouse. "
        , text "You can use the escape key to unlock the mouse pointer again. "
        ]
    , paragraph []
        [ text "When the pointer is locked, you can also move around the spacetime. "
        , text "The W and S keys will move you forward and backward. (Recall that this is the time direction.) "
        , text "A and D will move you left and right, and Space and (Left) Shift will move you up and down. "
        , text "(If you've played Minecraft, these controls should be familiar.) "
        ]
    , paragraph []
        [ text "Now, Freely falling objects follow straight paths through spacetime. "
        , text "So as you walk forward, you are following a path that a falling object might take! "
        , text "Under the \"Orbit\" panel, on the left, you can see the path in "
        , el [ Font.italic ] <| text "space, "
        , text "without the time coordinate. "
        ]
    , paragraph []
        [ text "Some paths are "
        , el [ Font.italic ] <| text "orbits, "
        , text "looping around the planet forever. "
        , text "But for other paths, the object has too much speed, and escapes the planet's gravity entirely! "
        , text "I demonstrate this using color; if you are looking at a green region, you are following an orbit. "
        , text "If you are looking at a cyan region, you are following an escape trajectory. "
        , text "Try watching what happens to the path on the orbit panel as you look around the scene. "
        ]
    , paragraph []
        [ text "I hope this project gives you a better sense of what \"curved spacetime\" is actually like." ]
    , paragraph []
        [ text "Have fun!" ]
    , el
        [ Font.bold
        , Font.underline
        , Font.size (scaled 2)
        , Font.color (rgb 0.6 0.3 0.6)
        , paddingEach { top = 15, left = 0, right = 0, bottom = 0 }
        ]
        (text "Something to notice")
    , paragraph []
        [ text "Look carefully at the timeward-pointing edges of the lattice. "
        , text "They all bend away from the planet! "
        , text "This corresponds to the fact that if you want to hover in place above a planet, "
        , text "you need to continually accelerate upward. "
        ]
    , el
        [ Font.bold
        , Font.underline
        , Font.size (scaled 2)
        , Font.color (rgb 0.6 0.3 0.6)
        , paddingEach { top = 15, left = 0, right = 0, bottom = 0 }
        ]
        (text "Amusing note")
    , paragraph []
        [ text "The visible dividing line between the green and cyan regions is actually a bug in my code. "
        , text "But it looks cool enough that I don't want to fix it! "
        ]
    ]


viewSettings : Model -> Element Msg
viewSettings { resolution, edgeWidth } =
    column
        [ Element.width fill
        , padding 5
        , spacing 5
        ]
        [ Input.slider
            [ Element.width fill
            , Element.height (px 5)
            , Background.color borderColor
            ]
            { onChange = Resolution
            , label =
                Input.labelAbove [ centerX ] <|
                    Element.textColumn [ Font.center, padding 5, spacing 5, Element.width shrink ]
                        [ el [ Font.bold ] <|
                            text <|
                                String.concat
                                    [ "resolution ("
                                    , String.fromInt <| round <| 100 * resolution
                                    , "%)"
                                    ]
                        , el [ Font.size (scaled -1) ] <| text "Performance Option"
                        ]
            , min = 0
            , max = 1
            , value = resolution
            , thumb = Input.defaultThumb
            , step = Just 0.01
            }
        , Input.slider
            [ Element.width fill
            , Element.height (px 5)
            , Background.color borderColor
            ]
            { onChange = EdgeWidth
            , label =
                Input.labelAbove [ centerX ] <|
                    el [ Font.bold ] <|
                        text <|
                            String.concat
                                [ "Lattice edge width ("
                                , String.fromFloat edgeWidth
                                , ")"
                                ]
            , min = 0.01
            , max = 0.2
            , value = edgeWidth
            , thumb = Input.defaultThumb
            , step = Just 0.001
            }
        ]


dropdown : { width : Int } -> { title : String, content : Element msg, msg : Bool -> msg } -> Bool -> Element msg
dropdown { width } { title, content, msg } open =
    let
        ( symbol, optContent ) =
            if open then
                ( "▾ ", content )

            else
                ( "▸ ", Element.none )
    in
    column
        [ Background.color backgroundColor
        , Element.width (px width)
        , padding 5
        , spacing 5
        ]
        [ el
            [ Font.bold
            , Font.size (scaled 3)
            , Events.onClick (msg (not open))
            , Element.pointer
            , centerX
            ]
            (text <| String.append symbol title)
        , optContent
        ]


scaled : Int -> Int
scaled =
    round << Element.modular 20 1.25


borderColor : Element.Color
borderColor =
    rgb 0.4 0.2 0.4


backgroundColor : Element.Color
backgroundColor =
    rgb 0.1 0.0 0.1



{-

   Todo:

   Github

-}
