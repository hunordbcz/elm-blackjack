
module NestedRecords exposing (..)
import Html exposing (s)

type alias Point = {x: Int, y: Int, z: Int}
type Color = Red | Green | Blue

type alias ColoredShpere = {center: Point, color: Color, radius: Int}

moveConstructor : ColoredShpere -> Int -> Int -> ColoredShpere
moveConstructor shpere dx dy =
  ColoredShpere (Point (shpere.center.x + dx) (shpere.center.y + dy) shpere.center.z) shpere.color shpere.radius

moveRec : ColoredShpere -> Int -> Int -> ColoredShpere
moveRec sphere dx dy =
  { center = 
    { x = sphere.center.x + dx,
      y = sphere.center.y + dx,
      z = sphere.center.z
    }
  , color = sphere.color
  , radius = sphere.radius
  }


moveDestructure : ColoredShpere -> Int -> Int -> ColoredShpere
moveDestructure sphere dx dy =
  let 
    { center, color, radius } = sphere
    { x, y, z } = center
  in
    { center = 
      { x = x + dx
      , y = y + dy
      , z = z
      }
    , color = color
    , radius = radius
    }

moveUpdate : ColoredShpere -> Int -> Int -> ColoredShpere
moveUpdate ({center} as sphere) dx dy =
  { sphere 
  | center = 
    { center 
    | x = center.x + dx
    , y = center.y + dy
    }
  }

