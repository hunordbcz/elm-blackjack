module Solutions exposing (..)

import Html exposing (a)
import Array exposing (repeat)
import Html.Attributes exposing (checked)
import Lists
 
sum: Int -> Int -> Int -> Int
sum a b c = a + b + c

sum3: Int -> Int -> Int -> Int
sum3 a =
    let
        sum2 b c = b + c + a
    in 
        sum2 


--list = [a1, a2, a3,  ..... an]

--list.foldLeft(z)((a, b) -> a op b)

--(((z op a1) op a2) op a3 )...
--(an op..... (a2 op (a1 op z))

type alias FunSet = Int -> Bool
singletonSet : Int -> FunSet
singletonSet elem = 
    let 
        check param = elem == param
    in
        check

check1 : FunSet
check1 = \item -> item == 4
 --\inputElem -> elem == inputElem


setOf : List Int -> FunSet
setOf lst = 
    let
        check lista elem = 
            case lista of 
                [] -> False
                x::xs -> if elem == x then True 
                         else
                             check xs elem
    in
        (check lst)


-- N = 2 acc =[element]
-- N = 1 acc = [elemnt, element]
-- N = 0 acc [elemnt, elment, element] 
repeat: Int -> a -> List a
repeat number element = 
    let
        repeatHelper n lstAcc = 
            case n of 
                0 -> lstAcc
                _ -> repeatHelper (n - 1) (element::lstAcc)
    in
        repeatHelper number []


enumerate : List a -> List (Int, a)
enumerate lst = 
    let
        enumerateHelper index lstInput accResult = 
            case lstInput of
                [] -> accResult
                x::xs -> enumerateHelper (index + 1) xs ((index, x)::accResult) 
 
    in
       List.reverse (enumerateHelper 0 lst []) 
        --(enumerateHelper 0 lst []) 



countries : List (String, String)
countries = [("Romania", "Bucharest"), ("Germany", "Berlin"), ("France", "Paris")]

countriesWithCapital : List (String, String) -> (String -> Bool) -> List String 
countriesWithCapital lst checkFun = 
    case lst of
       [] -> []
       (country, capital)::xs -> if checkFun capital then
                                     country :: countriesWithCapital xs checkFun
                                 else  
                                     countriesWithCapital xs checkFun


validate : String -> Bool
validate = (\s -> (String.left 1 s) == "B")

--[8, 1, 2, 5, 9]  (x > 3)   ([1, 2], [8, 5, 9])
partition : List Int -> (Int -> Bool) -> (List Int, List Int)
partition list cond = 
    let
        partitionHelper lst accLeft accRight = 
            case lst of 
                [] -> (accLeft, accRight)
                x::xs -> if cond x then 
                            partitionHelper xs accLeft (x::accRight)
                        else
                            partitionHelper xs (x::accLeft) accRight
        (l1, l2) = (partitionHelper list [] [])
    in
        
        (List.reverse l1, List.reverse l2)


foldl : (a -> b -> b) -> b -> List a -> b

foldl op start l =
    case l of
        [] -> start
        x::xs -> foldl op (op x start) xs


all: List a -> (a -> Bool) -> Bool
all lst cond = foldl (\x-> \y -> y && cond x) True lst




any: List a -> (a -> Bool) -> Bool
any lst cond = foldl (\x-> \y -> y || cond x) False lst


--[a1, a2, a3, a4, a5, a6]

-- (((boolean op a1) op a2) op a3) op a3


--[1, 2, 3, 4]
-- > collect [Ok 1, Ok 2, Ok 3, Ok 4]
-- Ok [1, 2] : Result error (List number)
-- > collect [Ok 1, Err 2, Ok 3]
-- Err 2

collect : List (Result err ok) -> Result err (List ok)
collect list = 
    let                              --Err 3
        collectHelper lst okAcc = -- Ok [1, 2, 3, 4]
            case lst of 
                [] -> Ok okAcc
                (Ok res)::xs -> collectHelper xs (res::okAcc)
                (Err err)::xs -> Err err
    
        result = collectHelper list []
    in
             case result of 
                Ok lst -> Ok (List.reverse lst)
                _ -> result








-- 4.6.8 Start
chunks:Int -> List a -> List (List a)
chunks n l = 
    case l of
        [] -> [] 
        _ -> 
            (List.take n l) :: chunks n (Lists.drop n l) 
-- 4.6.8 End