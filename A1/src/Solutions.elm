-----------------------
-- Hunor Debreczeni
-- 10.10.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Solutions exposing (..)

-- 1.2.1 Start
type Suit = Clubs | Diamonds | Hearts | Spades
type Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Ace | Jack | Queen | King
type Card = Card Face Suit
-- 1.2.1 End

-- 1.2.2 Start
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
        Diamonds -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"

cardToString : Card -> String
cardToString (Card face suit) =
    faceToString face ++ " of " ++ suitToString suit
-- 1.2.2 End

-- 1.2.3 Start
type Point = Point Float Float
type Line = Line Point Point

onSegment : Point -> Point -> Point -> Bool
onSegment (Point px py) (Point qx qy) (Point rx ry) =
    if  qx <= (max px rx) && 
        qx >= (min px rx) && 
        qy <= (max py ry) && 
        qy >= (min py ry) then True
    else False

orientation : Point -> Point -> Point -> Int
orientation (Point px py) (Point qx qy) (Point rx ry) =
    let
        val = (qy - py) * (rx - qx) *
              (qx - px) * (ry - qy)
    in
        if val == 0 then 0
        else if val > 0 then 1
        else 2

linesIntersect : Line -> Line -> Bool
linesIntersect (Line p1 q1) (Line p2 q2) =
    let
        o1 = orientation p1 q1 p2
        o2 = orientation p1 q1 q2
        o3 = orientation p2 q2 p1
        o4 = orientation p2 q2 q1
    in
        if o1 /= o2 && o3 /= o4 then True
        else if o1 == 0 && onSegment p1 p2 q1 then True
        else if o2 == 0 && onSegment p1 q2 q1 then  True
        else if o3 == 0 && onSegment p2 p1 q2 then  True
        else if o4 == 0 && onSegment p2 q1 q2 then  True
        else False
-- 1.2.3 End

-- 1.2.4 Start
trailingZeroes : Int -> Int
trailingZeroes nrFact =
    let
        trailingZeroesAcc : Int -> Int -> Int
        trailingZeroesAcc result nr =
            if nr == 0 then result
            else trailingZeroesAcc (result + nr // 5) (nr // 5)
    in
        trailingZeroesAcc 0 nrFact
-- 1.2.4 End