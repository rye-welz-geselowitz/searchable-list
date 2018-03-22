module SearchableList exposing (Config, State, initialState, view)

import Html exposing (Attribute, Html, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events as Events
import Json.Decode as JD


type State
    = State String


type alias Config msg item =
    { placeholder : String
    , toMsg : State -> msg
    , items : List item
    , displayItem : item -> Html msg
    , filter : String -> item -> Bool
    }


initialState : State
initialState =
    State ""


view : Config msg item -> State -> Html msg
view config state =
    div []
        [ input
            [ placeholder config.placeholder
            , onInputSetSearchText config state
            , value (getSearchText state)
            ]
            []
        , filteredItemsView config state
        ]


filteredItemsView : Config msg item -> State -> Html msg
filteredItemsView config state =
    config.items
        |> List.filter (getSearchText state |> config.filter)
        |> List.map (itemView config state)
        |> ul []


itemView : Config msg item -> State -> item -> Html msg
itemView config state item =
    li [] [ config.displayItem item ]



-- HELPERS


onInputSetSearchText : Config msg item -> State -> Attribute msg
onInputSetSearchText config state =
    Events.targetValue
        |> JD.map
            (setSearchText state)
        |> JD.map config.toMsg
        |> Events.on "input"


getSearchText : State -> String
getSearchText (State searchText) =
    searchText


setSearchText : State -> String -> State
setSearchText (State _) searchText =
    State searchText
