module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- INIT


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , input = ""
      , playerId = Nothing
      , plays = []
      }
    , Cmd.none
    )


type alias Model =
    { players : List Player
    , input : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { playerName : String
    , playerId : Int
    , points : Int
    }



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ headerSection model
        , playersListSection model
        , totalSection model
        , formSection model
        , playsSection model
        ]


formSection : Model -> Html Msg
formSection model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , value model.input
            , onInput Input
            , placeholder "Player Name"
            ]
            []
        , button
            [ type_ "submit"
            ]
            [ text "Save" ]
        , button
            [ type_ "button"
            , onClick Cancel
            ]
            [ text "Cancel" ]
        ]


totalSection : Model -> Html Msg
totalSection model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div
                [ style
                    [ ( "color", "green" )
                    , ( "font-size", "150%" )
                    ]
                ]
                [ text ("Score: " ++ (toString total)) ]
            ]


playersListSection : Model -> Html Msg
playersListSection model =
    div [] [ renderPlayers model.players ]


playsSection : Model -> Html Msg
playsSection model =
    div [] [ renderPlays model.plays ]


headerSection : Model -> Html Msg
headerSection model =
    header
        [ class "main-header" ]
        [ h1 [] [ text "Basketball Score Keeper" ]
        ]


renderPlays plays =
    plays
        |> List.map play
        |> List.reverse
        |> ul []


renderPlayers players =
    players
        |> List.sortBy .name
        |> List.map player
        |> ul [ style [ ( "list-style-type", "none" ) ] ]


play : Play -> Html Msg
play play =
    let
        playDescription =
            play.playerName ++ ": " ++ (toString play.points)
    in
        li []
            [ span [] [ text playDescription ]
            ]


player : Player -> Html Msg
player player =
    li []
        [ button
            [ class "edit"
            , onClick (Edit player)
            ]
            [ text "Edit Name" ]
        , span
            [ style
                [ ( "display", "inline-block" )
                , ( "width", "10em" )
                , ( "padding", "5px" )
                ]
            ]
            [ text player.name ]
        , button [ type_ "button", onClick (Score player 1) ] [ text "Free Throw" ]
        , button [ type_ "button", onClick (Score player 2) ] [ text "2 pt" ]
        , button [ type_ "button", onClick (Score player 3) ] [ text "3 pt" ]
        ]



-- UPDATE


type Msg
    = Input String
    | Save
    | Edit Player
    | Cancel
    | Delete Play
    | Score Player Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input name ->
            ( { model | input = name }, Cmd.none )

        Cancel ->
            ( { model | input = "", playerId = Nothing }, Cmd.none )

        Save ->
            if (String.isEmpty model.input) then
                ( model, Cmd.none )
            else
                ( savePlayer model, Cmd.none )

        Score player points ->
            ( score model player points, Cmd.none )

        Delete p ->
            ( deletePlay model p, Cmd.none )

        Edit player ->
            ( { model | input = player.name, playerId = Just player.id }, Cmd.none )


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            []
    in
        model


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player
                            | points = player.points + points
                        }
                    else
                        player
                )
                model.players

        play =
            Play scorer.name scorer.id points
    in
        { model | players = newPlayers, plays = play :: model.plays }


savePlayer : Model -> Model
savePlayer model =
    case model.playerId of
        Nothing ->
            addPlayer model

        Just id ->
            updatePlayer model id


updatePlayer : Model -> Int -> Model
updatePlayer model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.input }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | playerName = model.input }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , input = ""
            , playerId = Nothing
        }


addPlayer : Model -> Model
addPlayer model =
    let
        newPlayer =
            Player (newPlayerId model.players) model.input 0

        newPlayers =
            newPlayer :: model.players
    in
        { model
            | players = newPlayers
            , input = ""
        }


newPlayerId : List Player -> Int
newPlayerId players =
    List.length players



-- SUBSCRIPTIONS


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
