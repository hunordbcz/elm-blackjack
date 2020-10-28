-----------------------
-- Hunor Debreczeni
-- 17.10.2020
-----------------------
module Solutions exposing (..)

import Lists exposing (..)
import Html.Attributes exposing (list)
import Html exposing (text)
import Html exposing (a)

-- Helpers Start
len : List a -> Int
len list = 
    let
        lenTail : Int -> List a -> Int
        lenTail count listTail = 
            case listTail of
            [] -> count
            x::xs -> lenTail (count + 1) xs
    in
        lenTail 0 list
-- Helpers End

-- 2.2.1 Start
type Suit = Clubs | Diamonds | Hearts | Spades
type Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Ace | Jack | Queen | King
type Card = Card Face Suit

enumSuit = [Clubs, Diamonds, Hearts, Spades]
enumFace = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Ace, Jack, Queen, King]

deck : List Card
deck =
    List.concatMap
        (\face -> 
            List.map 
                (\suit -> (Card face suit)) 
                enumSuit 
        )
        enumFace
-- 2.2.1 End

-- 2.2.2 Start
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
-- 2.2.2 End

-- 2.2.3 Start
smallestK : Int -> List Int -> List Int
smallestK k list =
    let
        sortedList = List.sort list
        smallestKUtil : Int -> Int -> List Int -> List Int -> List Int
        smallestKUtil count kIndex resultList originalList =
            if count == kIndex then resultList
            else case originalList of
               [] -> resultList
               x::xs -> smallestKUtil (count + 1) kIndex (List.append resultList [x]) xs    
    in
        smallestKUtil 0 k [] sortedList
-- 2.2.3 End

-- 2.2.4 Start
balanced : String -> Bool
balanced text =
    let
        balancedUtil : Int -> List Char -> Bool
        balancedUtil open textList =
            case textList of
                [] ->
                    if open == 0 then True
                    else False
                x::xs ->
                    case x of
                        '(' ->  balancedUtil (open + 1) xs
                        ')' ->  if open > 0 then
                                    balancedUtil (open - 1) xs
                                else False
                        _   ->  balancedUtil open xs
    in
        balancedUtil 0 (String.toList text)
-- 2.2.4 End

-- 2.2.5 Start
tailUtil: List a -> List a
tailUtil list =
    let
        listTemp = tail list
    in
        case listTemp of
           Nothing -> []
           Just x -> x

headUtil: List Int -> Int
headUtil list =
    let
        listTemp = head list
    in
        case listTemp of
           Just x -> x
           Nothing -> 0

coinChange : Int -> List Int -> Int
coinChange amount coinTypes =
    if amount == 0 then 1
    else if amount < 0 then 0
    else if (len coinTypes) <= 0 && amount >= 1 then 0
    else coinChange amount (tailUtil coinTypes) + coinChange (amount - (headUtil coinTypes)) coinTypes
-- 2.2.5 End