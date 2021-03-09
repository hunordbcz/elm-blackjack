module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Main exposing (..)
import Card exposing (..)

genTest name fn input expected =
  test name <|
    \_ -> Expect.equal expected (fn input)

{-
  Uncomment the tests after you complete the `Card` module and implement the `calculateScore` function
-}
suite : Test
suite = describe "Tests" [
  -- describe "calculateScore"
  --   [  genTest "calculateScore" calculateScore [Card King Hearts] 10
  --   ,  genTest "calculateScore" calculateScore [Card Two Hearts] 2
  --   ,  genTest "calculateScore" calculateScore [Card Two Hearts, Card King Spades] 12
  --   ,  genTest "calculateScore" calculateScore [Card Ace Hearts, Card King Spades] 21
  --   ,  genTest "calculateScore" calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] 13
  --   ,  genTest "calculateScore" calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] 22
  --   ,  genTest "calculateScore" calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] 21
  --   ,  genTest "calculateScore" calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] 22
  --   ]
  ]
