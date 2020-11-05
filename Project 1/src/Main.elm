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
    True
    Running
    []
    
init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , drawCard NewDealerCard startingModel
  )


type Status 
  = Running 
  | Won 
  | Bust

type Msg
  = Draw
  | NewPlayerCard Card
  | ToggleDeck
  | NewGame
  | NewDealerCard Card
  | ScoreCheck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard NewPlayerCard model
      )

    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewPlayerCard newCard ->
      (
        (Model
          (model.hand ++ [newCard])
          (List.filter (\card -> card /= newCard) model.deck)
          model.showDeck
          model.gameStatus
          model.dealerHand
        )
      , Cmd.none
      )

    NewGame ->
      (
        (Model
          []
          Card.deck
          Basics.False
          Running
          []
        )
      , drawCard NewDealerCard model
      )

    NewDealerCard newCard ->
      let 
        newDealerHand = model.dealerHand ++ [newCard]
        newDeck = (List.filter (\card -> card /= newCard) model.deck)
        cmd =
          if (calculateScore newDealerHand) < 17 then
            drawCard NewDealerCard model
          else Cmd.none
      in
        (
          (Model
            model.hand
            newDeck
            model.showDeck
            model.gameStatus
            newDealerHand
          )
        , cmd
        )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToggleDeck ->
      (
        (Model
          model.hand
          model.deck
          (Basics.not model.showDeck)
          model.gameStatus
          model.dealerHand
        )
      , Cmd.none
      )

    ScoreCheck ->
      let
        playerScore = calculateScore model.hand
        dealerScore = calculateScore model.dealerHand
        cmd = 
          if dealerScore < 17 then
            drawCard NewDealerCard model
          else Cmd.none
        gameStatus =
          if dealerScore > 21 then Won
          else if dealerScore > playerScore then Bust
          else if playerScore <= 21 then Won
          else Bust
        modelType = 
          (Model
            model.hand
            model.deck
            model.showDeck
            gameStatus
            model.dealerHand
          )
      in
        (modelType, cmd)

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

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
  in
    div []
      [ 
        div [] [ h1 [] [text appName] ],
        div [] [ h2 [] [text ("Dealer score: " ++ (String.fromInt (calculateScore model.dealerHand)))]],
        ul [ style "font-size" "6em", style "display" "flex" ] 
          (List.map (\card -> li [ style "list-style-type" "none" ] [ text (cardToUnicode card) ]) model.dealerHand) ,
        hr [] [],
        div [] [ h2 [] [text ("Your score: " ++ (String.fromInt (calculateScore model.hand)))]],
        ul [ style "font-size" "6em", style "display" "flex" ] 
          (List.map (\card -> li [ style "list-style-type" "none" ] [ text (cardToUnicode card) ]) model.hand) ,
        text (String.fromInt (List.length model.hand)),
        hr [] [],
        text (String.fromInt (List.length model.deck)),
        button [ onClick Draw, disabled (model.showDeck == False) ] [ text "Hit" ],
        button [ onClick ToggleDeck ] [ text "Stand" ]
      ]