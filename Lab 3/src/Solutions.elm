-------------------------
--  Hunor Debreczeni
--  16.10.2020
-------------------------

module Solutions exposing (..)

-- 3.5.1 Start
len : List Int -> Int
len list = 
    case list of
       [] -> 0
       x::xs -> 1 + len xs
-- 3.5.1 End

-- 3.6.1 Start
safeDiv : Int -> Int -> Maybe Int
safeDiv a b =
  if b == 0 then
    Nothing
  else
    Just (a // b)
-- 3.6.2 End

-- 3.6.3 Start
lenTail : Int -> List Int -> Int
lenTail count list = 
    case list of
       [] -> count
       x::xs -> lenTail (count + 1) xs
-- 3.6.3 End

-- 3.6.4 Start // Starts from 0, not 1
indexList : Int -> List Int -> Maybe Int
indexList i list =
    let
        indexTemp : Int -> Int -> List Int -> Maybe Int
        indexTemp current expected listInside =
            case listInside of
                [] -> Nothing
                x::xs ->
                    if current == expected then Just (x)
                    else indexTemp (current + 1) expected xs
    in
        indexTemp 0 i list
-- 3.6.4 End

-- 3.6.5 Start
daysInMonth : Month -> Int -> Int
daysInMonth month year =
  case month of
    Jan -> 31
    Feb -> if isLeapYear year then 29 else 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

isLeapYear : Int -> Bool
isLeapYear year = 
    if modBy 400 year == 0 then True
    else if modBy 100 year == 0 then False
    else if modBy 4 year == 0 then True
    else False
-- 3.6.5 End