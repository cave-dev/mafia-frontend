module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h1
        , i
        , li
        , nav
        , span
        , text
        , ul
        )
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type LobbyRole
    = Host
    | Player


type State
    = Viewing
    | Lobby LobbyRole
    | Playing


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


type Route
    = Game
    | Info
    | Settings
    | NotFound


routeToString : Route -> String
routeToString route =
    case route of
        Game ->
            "/game"

        Info ->
            "/info"

        Settings ->
            "/settings"

        NotFound ->
            "/not_found"


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ oneOf [ map Game top, map Game (s "game") ]
        , map Info (s "info")
        , map Settings (s "settings")
        ]


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , state : State
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , route = Maybe.withDefault NotFound (parse routeParser url)
      , state = Viewing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateState State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External ref ->
                    ( model, Nav.load ref )

        UrlChanged url ->
            ( { model | url = url, route = Maybe.withDefault NotFound (parse routeParser url) }
            , Cmd.none
            )

        UpdateState state ->
            ( { model | state = state }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


landingView : Model -> Html Msg
landingView _ =
    let
        btn : String -> String -> Msg -> Html Msg
        btn name icon event =
            button [ class "button is-primary is-large", onClick event ]
                [ span [ class "icon is-small" ]
                    [ i [ class ("fas fa-" ++ icon) ]
                        []
                    ]
                , span [] [ text name ]
                ]
    in
    div []
        [ h1 [ class "title" ] [ text "Start Playing" ]
        , btn "create" "plus" (UpdateState <| Lobby Host)
        , btn "join" "sign-in-alt" (UpdateState <| Lobby Player)
        ]


waitingView : Model -> Html Msg
waitingView _ =
    h1 [ class "title" ] [ text "Waiting" ]


infoView : Model -> Html Msg
infoView _ =
    h1 [ class "title" ] [ text "infoView" ]


notFoundView : Model -> Html Msg
notFoundView _ =
    h1 [ class "title" ] [ text "notFoundView" ]


settingsView : Model -> Html Msg
settingsView _ =
    h1 [ class "title" ] [ text "settingsView" ]


gameView : Model -> Html Msg
gameView model =
    case model.state of
        Viewing ->
            landingView model

        Lobby _ ->
            waitingView model

        Playing ->
            h1 [ class "title" ] [ text "Working on Game logic" ]


headerView : Model -> Html Msg
headerView _ =
    nav [ class "navbar is-link is-fixed-top" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href Game ] [ text "Mafia" ]
            ]
        ]


footerView : Model -> Html Msg
footerView model =
    let
        tab : Route -> String -> String -> Html Msg
        tab route icon str =
            li
                [ class
                    (if route == model.route then
                        "is-active"

                     else
                        ""
                    )
                ]
                [ a [ href route ]
                    [ span [ class "icon is-small" ]
                        [ i [ class ("fas fa-" ++ icon) ]
                            []
                        ]
                    , span []
                        [ text str ]
                    ]
                ]

        game =
            tab Game "gamepad" "Game"

        info =
            tab Info "info" "Info"

        settings =
            tab Settings "cog" "Settings"

        tabsList =
            case model.state of
                Viewing ->
                    [ info, game ]

                _ ->
                    [ info , game , settings ]
    in
    nav [ class "navbar is-fixed-bottom" ]
        [ div [ class "is-fullwidth tabs" ]
            [ ul []
                tabsList
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        mainView : Html Msg
        mainView =
            case model.route of
                Info ->
                    infoView model

                Settings ->
                    settingsView model

                Game ->
                    gameView model

                NotFound ->
                    notFoundView model
    in
    { title = "Mafia"
    , body =
        [ headerView model
        , mainView
        , footerView model
        ]
    }
