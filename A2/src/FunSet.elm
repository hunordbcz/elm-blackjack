-----------------------
-- Hunor Debreczeni
-- 17.10.2020
-----------------------

module FunSet exposing (..)

type alias FunSet = Int -> Bool

contains : FunSet -> Int -> Bool
contains set elem = set elem

singletonSet : Int -> FunSet
singletonSet elem = \inputElem -> elem == inputElem

{-
Conveniece function to create a set of elements.
```elm
setOf [1, 2, 3] == union (union (singletonSet 1) (singletonSet 2)) (singletonSet 3))
setOf [1, 2, 3] == fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]
```
-}
setOf : List Int -> FunSet
setOf elems = Debug.todo "Implement this"

{-
Returns the union of 2 sets.
```elm
(union (singletonSet 1) (singletonSet 2)) 1 == True
(union (singletonSet 1) (singletonSet 2)) 1 == False
```
-}
union : FunSet -> FunSet -> FunSet
union a b = Debug.todo "Implement this"

{-
Returns the intersection of 2 sets.
```elm
(intersect (setOf [1, 2]) (setOf [1, 3])) 1 == True
(intersect (setOf [1, 2]) (setOf [1, 3])) 2 == False
```
-}
intersect : FunSet -> FunSet -> FunSet
intersect a b = Debug.todo "Implement this"

{-
Returns the difference of 2 sets.
```elm
(diff (setOf [1, 2]) (setOf [1, 3])) 1 == False
(diff (setOf [1, 2]) (setOf [1, 3])) 2 == True
```
-}
diff : FunSet -> FunSet -> FunSet
diff a b = Debug.todo "Implement this"

{-
Returns a new set, with `function` applied to each of element.
```elm
(map (\x -> x + 1) (setOf [1, 2]) 1 == False
(map (\x -> x + 1) (setOf [1, 2]) 2 == True
(map (\x -> x + 1) (setOf [1, 2]) 3 == True
```
-}
map: ( Int -> Int ) -> FunSet -> FunSet
map function set = Debug.todo "Implement this"

{-
Takes a list of sets and returns a new set, which is build by applying a fold using `operation` function.
```elm
(fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 == True
(fold intersection [setOf [1], setOf [2]]) 1 == False
(fold intersection [setOf [1], setOf [2]]) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 == True
```
-}
fold: List FunSet -> ( FunSet -> FunSet -> FunSet ) -> FunSet
fold operation sets = Debug.todo "Implement this"