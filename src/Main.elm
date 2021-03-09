-----------------------
-- John Doe
-- 31.02.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List exposing (..)
import Random
import Debug
import Card exposing (..)
import Html.Attributes exposing (disabled)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool,
    gameStatus: Status,
    dealerHand: List Card
  }

startingModel : Model
startingModel =
  Model 
    []
    Card.deck
    False
    Running
    []
    
init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , drawCard NewDealerCard startingModel
  )


type Status 
  = Running 
  | Win 
  | Bust


type Msg
  = Draw
  | NewPlayerCard Card
  | ToggleDeck
  | NewGame
  | NewDealerCard Card
  | Stand


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard NewPlayerCard model
      )

    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewPlayerCard newCard ->
      let
        newHand = (model.hand ++ [newCard])
        showDeck =
          if (calculateScore newHand) >= 21 then True
          else model.showDeck
        newModel = 
          {
            model |
            hand = newHand,
            deck = (List.filter (\card -> card /= newCard) model.deck),
            showDeck = showDeck
          }
      in
      (
        {
          newModel |
          gameStatus = (getGameStatus newModel)
        }
      , Cmd.none
      )

    NewGame ->
      (
        startingModel
      , drawCard NewDealerCard model
      )

    NewDealerCard newCard ->
      let 
        newDealerHand = model.dealerHand ++ [newCard]
        newModel = 
          {
            model |
            deck = (List.filter (\card -> card /= newCard) model.deck),
            dealerHand = newDealerHand
          }
        cmd =
          if (calculateScore newDealerHand) < 17 then
            drawCard NewDealerCard newModel
          else Cmd.none
      in
        (
          newModel
        , cmd
        )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToggleDeck ->
      (
        {
          model |
          showDeck = not model.showDeck
        }
      , Cmd.none
      )

    Stand ->
      let
          newModel = 
            { model |
              showDeck = Basics.True
            }
      in
        (
          {
            newModel |
            gameStatus = (getGameStatus newModel)
          }
        , Cmd.none
        )

drawCard : (Card -> Msg) -> Model -> Cmd Msg
drawCard msg model =
  case model.deck of
    (first::rest) -> Random.generate msg (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}

headUtil: List Int -> Int
headUtil list =
    let
        listTemp = List.head list
    in
        case listTemp of
           Just x -> x
           Nothing -> 0

cardValues : List Card -> List (List Int)
cardValues cards =
  List.map cardValue cards

possibleScores : List (List Int) -> List Int
possibleScores cardValuesList=
  let
    possibleScoresHelper : List (List Int) -> List Int -> List Int -> List Int
    possibleScoresHelper list currentValues finalResultList=
      case list of
        [] -> (List.sum currentValues) :: finalResultList
        x::xs -> 
          if List.length x == 1 then
            possibleScoresHelper xs (headUtil x :: currentValues) finalResultList
          else -- Ace
            (possibleScoresHelper xs (11 :: currentValues) finalResultList) ++ 
            (possibleScoresHelper xs (1 :: currentValues) finalResultList) ++ finalResultList
  in  
    possibleScoresHelper cardValuesList [] []

calculateScore : List Card -> Int
calculateScore cards = 
    let
      possibleValues = possibleScores (cardValues cards)
      (smaller, bigger) = List.partition (\x -> x <= 21) possibleValues
      biggestWinner = headUtil (List.sortWith (\x -> \y -> if x > y then Basics.LT else Basics.GT) smaller)
      smallestLooser = headUtil (List.sort bigger)
    in
      if biggestWinner > 0 then biggestWinner
      else smallestLooser


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


getGameStatus: Model -> Status
getGameStatus model =
  let
    playerScore = calculateScore model.hand
    dealerScore = calculateScore model.dealerHand
  in
    if model.showDeck == False then Running
    else if dealerScore > 21 then Win
    else if dealerScore > playerScore then Bust
    else if playerScore <= 21 then Win
    else Bust

statusToString : Status -> String
statusToString status =
  case status of
     Running -> "Running"
     Win -> "Win"
     Bust -> "Bust"

boolToString : Bool -> String
boolToString bool =
  if bool then "True"
  else "False"

dealerScoreString : Model -> String
dealerScoreString model =
  if model.showDeck == False then String.fromInt (calculateScore (List.take 1 model.dealerHand))
  else String.fromInt (calculateScore model.dealerHand)

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    dealerCards = 
      if model.showDeck == True then
        ul [ style "font-size" "6em", style "display" "flex", style "margin" "0" ] 
          (List.map (\card -> li [ style "list-style-type" "none", style "color" (cardColor card) ] [ text (cardToUnicode card) ]) model.dealerHand)
      else 
        let
            firstCard = (Maybe.withDefault (Card Ace Diamond) (List.head model.dealerHand)) -- mode.dealerHand surely contains a card
        in
          ul [ style "font-size" "6em", style "display" "flex", style "margin" "0" ] 
            [
              li [ style "list-style-type" "none", style "color" (cardColor firstCard) ] [ text (cardToUnicode firstCard) ],
              li [ style "list-style-type" "none" ] [ text "🂠" ]
            ]
  in
    div []
      [ 
        div [] [ h1 [] [text appName, text " - ", text (statusToString model.gameStatus)] ],
        div [] [ h2 [] [text ("Your score: " ++ (String.fromInt (calculateScore model.hand)))]],
        ul [ style "font-size" "6em", style "display" "flex", style "margin" "0" ] 
          (List.map (\card -> li [ style "list-style-type" "none", style "color" (cardColor card) ] [ text (cardToUnicode card) ]) model.hand) ,
        hr [] [],
        div [] [ h2 [] [text ("Dealer score: " ++ (dealerScoreString model))]],
        dealerCards ,
        hr [] [],
        button [ onClick Draw, disabled (model.showDeck == True) ] [ text "Hit" ],
        button [ onClick Stand, disabled (model.showDeck == True) ] [ text "Stand" ],
        hr [] [],
        button [ onClick ToggleDeck ] [ text "Show Cards" ],
        button [ onClick NewGame ] [ text "New Game" ]
      ]