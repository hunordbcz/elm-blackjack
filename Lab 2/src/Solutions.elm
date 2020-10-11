-- Hunor Debreczeni, Group 30433
module Solutions exposing (..)

import Shapes exposing (..)
import Random

-- 2.2.1
-- fullTitle person = (ifperson.idDrthen"Dr. "else"") ++person.firstName ++" "++ person.lstName
-- fullTitle {isDr = true, firstName = "Haskell", lastName = "Curry"}

-- 2.3.2
type alias Address = {
    street : String,
    number : Int,
    city : String,
    country : String
    }

-- 2.3.3
formatAddress : Address -> String
formatAddress addr = addr.street ++ " " ++ String.fromInt addr.number ++ ", " ++ addr.city ++ ", " ++ addr.country

-- 2.5.3
type Triangle = Triangle Int Int Int

isEquilateralTriangle : Triangle -> Bool
isEquilateralTriangle (Triangle a b c) = 
    case (a - b, b - c) of
       (0, 0) -> True
       (_, _) -> False

-- 2.8.1
type Dice = One | Two | Three | Four | Five | Six

-- 2.8.2
type alias DicePair = {
        first : Dice,
        second : Dice
    }

-- 2.8.2
type Dices = Dices Dice Dice

-- 2.8.3
luckyRoll : DicePair -> String
luckyRoll dicePair = 
    case (dicePair.first, dicePair.second) of
        (Six , Six) -> "Very Lucky"
        (_ , Six) -> "Lucky"
        (Six , _) -> "Lucky"
        (_, _) -> "Meh"



-- 2.8.4
type ShapeRec
  = CircleRec { radius : Float }
  | RectangleRec { width : Float, height : Float }
  | TriangleRec { sideA : Float, sideB : Float, sideC : Float }

areaRec : ShapeRec -> Float
areaRec shape = 
    case shape of
       CircleRec circle -> pi * circle.radius * circle.radius
       RectangleRec rectangle -> rectangle.width * rectangle.height
       TriangleRec triangle -> heron triangle.sideA triangle.sideB triangle.sideC