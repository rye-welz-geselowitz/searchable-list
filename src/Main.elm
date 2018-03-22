module Main exposing (..)

import Html exposing (Html, div, h1, img, text)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import SearchableList
import Task exposing (Task)


---- PEOPLE ----


type alias Person =
    { name : String
    , gender : String
    }


getPeople : Task Http.Error (List Person)
getPeople =
    Http.get "https://swapi.co/api/people/" (JD.at [ "results" ] (JD.list decodePerson))
        |> Http.toTask


decodePerson : Decoder Person
decodePerson =
    JDP.decode Person
        |> JDP.required "name" JD.string
        |> JDP.required "gender" JD.string



---- MODEL ----


type alias Model =
    { searchableListState : SearchableList.State
    , people : Maybe (List Person)
    , error : Bool
    }


initialModel : Model
initialModel =
    { searchableListState = SearchableList.initialState
    , people = Nothing
    , error = False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.attempt SetPeople getPeople
    )



---- UPDATE ----


type Msg
    = SetPeople (Result Http.Error (List Person))
    | SetSearchableListState SearchableList.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSearchableListState state ->
            ( { model | searchableListState = state }, Cmd.none )

        SetPeople (Ok people) ->
            ( { model | people = Just people }, Cmd.none )

        SetPeople (Err err) ->
            ( { model | error = True }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    if model.error then
        div [] [ text "GAHH!!!!! SOMETHING IS WRONG!!! Failed Http request?" ]
    else
        div []
            [ h1 [] [ text "Search a Star Wars Character!" ]
            , content model
            ]


content : Model -> Html Msg
content model =
    case model.people of
        Nothing ->
            div [] [ text "loading..." ]

        Just people ->
            div []
                [ SearchableList.view (searchBoxConfig people) model.searchableListState
                ]


searchBoxConfig : List Person -> SearchableList.Config Msg Person
searchBoxConfig people =
    { placeholder = "I'm looking for..."
    , toMsg = SetSearchableListState
    , items = people
    , displayItem = displayPerson
    , filter = filterPerson
    }


filterPerson : String -> Person -> Bool
filterPerson searchText =
    .name >> cleanString >> String.contains (cleanString searchText)


cleanString : String -> String
cleanString =
    String.toLower >> String.trim


displayPerson : Person -> Html Msg
displayPerson person =
    div []
        [ text <| person.name ++ " - " ++ person.gender
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
