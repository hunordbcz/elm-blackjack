-----------------------
-- Hunor Debreczeni
-- 25.10.2020
-----------------------
module FunSet exposing (..)

import List exposing (..)

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
setOf elems = \x -> List.any (\y -> y == x) elems

{-
Returns the union of 2 sets.
```elm
(union (singletonSet 1) (singletonSet 2)) 1 == True
(union (singletonSet 1) (singletonSet 2)) 2 == True
(union (setOf [1, 3, 4]) (setOf [1, 2])) 2 == True
(union (setOf [1, 3, 4]) (setOf [1, 2])) 5 == False
```
-}
union : FunSet -> FunSet -> FunSet
union a b = 
    \x -> a x || b x

{-
Returns the intersection of 2 sets.
```elm
(intersect (setOf [1, 2]) (setOf [1, 3])) 1 == True
(intersect (setOf [1, 2]) (setOf [1, 3])) 2 == False
```
-}
intersect : FunSet -> FunSet -> FunSet
intersect a b = 
    \x -> a x && b x

{-
Returns the difference of 2 sets.
```elm
(diff (setOf [1, 2]) (setOf [1, 3])) 1 == False
(diff (setOf [1, 2]) (setOf [1, 3])) 2 == True
```
-}
diff : FunSet -> FunSet -> FunSet
diff a b = 
    \x -> a x && not (b x)

{-
Returns a new set, with `function` applied to each of element. 
You can assume that elements are integres in the range [-1000, 1000].
```elm
(map (\x -> x + 1) (setOf [1, 2]) 1 == False
(map (\x -> x + 1) (setOf [1, 2]) 2 == True
(map (\x -> x + 1) (setOf [1, 2]) 3 == True
```
-}
map: ( Int -> Int ) -> FunSet -> FunSet
map function set = 
    (\x ->
        let
            limitMax = 1000
            mapUtil : Int -> Bool
            mapUtil currVal =
                if currVal > limitMax || currVal < -limitMax then False
                else if set currVal && (\a -> x == function a) currVal then True
                else mapUtil (function currVal)
        in
            mapUtil -limitMax
    )
{-
Takes a list of sets and returns a new set, which is build by applying a fold using `operation` function.
```elm
(fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 == True
(fold intersect [setOf [1], setOf [2]]) 1 == False
(fold intersect [setOf [1], setOf [2]]) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 == True
```
-}

fold: ( FunSet -> FunSet -> FunSet ) -> List FunSet -> FunSet
fold operation sets = 
    let
        foldUtil : FunSet -> List FunSet -> FunSet
        foldUtil val list =
            case list of
               []       -> val
               x::xs    -> foldUtil (operation val x) xs
    in
        case sets of
            []      -> \_ -> False
            x :: xs -> foldUtil x xs