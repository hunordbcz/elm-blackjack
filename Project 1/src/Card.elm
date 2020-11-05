-----------------------
-- Hunor Debreczeni
-- 31.02.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

type Suit = Clubs | Diamond | Hearts | Spades
type Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Ace | Jack | Queen | King
type Card = Card Face Suit

faceToString : Face -> String
faceToString face = 
    case face of
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nine"
        Ten -> "Ten"
        Ace -> "Ace"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs -> "Clubs"
        Diamond -> "Diamond"
        Hearts -> "Hearts"
        Spades -> "Spades"

cardToString : Card -> String
cardToString (Card face suit) =
    faceToString face ++ " of " ++ suitToString suit

cardValue : Card -> List Int
cardValue (Card face suit) =
    case (face, suit) of
        (Nine,_) -> [9]
        (Eight,_) -> [8]
        (Seven,_) -> [7]
        (Six,_) -> [6]
        (Five,_) -> [5]
        (Four,_) -> [4]
        (Three,_) -> [3]
        (Two,_) -> [2]
        (Ace, _) -> [1,11]
        (_,_) -> [10]

deck : List Card
deck =
    List.concatMap
        (\face -> 
            List.map 
                (\suit -> (Card face suit)) 
                [Clubs, Diamond, Hearts, Spades]
        )
        [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Ace, Jack, Queen, King]

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of 
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamond -> "🃁"
     Two -> case suit of 
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamond -> "🃂"
     Three -> case suit of 
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamond ->"🃃" 
     Four -> case suit of 
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamond -> "🃄"
     Five -> case suit of 
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamond -> "🃅"
     Six -> case suit of 
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamond -> "🃆"
     Seven -> case suit of 
       Spades ->"🂩"
       Hearts -> "🂹"
       Clubs ->  "🃙"
       Diamond -> "🃉"
     Eight -> case suit of 
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamond ->  "🃈"
     Nine -> case suit of 
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamond ->  "🃉"
     Ten -> case suit of 
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamond -> "🃊"
     Jack -> case suit of 
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamond -> "🃋"
     Queen -> case suit of 
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamond -> "🃍"
     King -> case suit of 
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamond -> "🃎"

{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (Card face suit) =
   let
     card = (Card face suit)
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s = 
       case s of
         Diamond -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]