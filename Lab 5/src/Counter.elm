
module Counter exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

main =
  Browser.sandbox { init = (Model 0 False False), update = update, view = view }

type alias Model = {
    value : Int,
    isDisabledAdd: Bool,
    isDisabledSub: Bool}

type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      if model.value >= 10 then (Model model.value True False)
      else (Model (model.value + 1) False False)

    Decrement ->
      if model.value <= -10 then (Model model.value False True)
      else (Model (model.value - 1) False False)

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Increment, disabled model.isDisabledAdd ] [ text "+" ]
    , div [] [ text (String.fromInt model.value) ]
    , button [ onClick Decrement, disabled model.isDisabledSub ] [ text "-" ]
    ]

