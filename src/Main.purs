module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple
import Data.Array
import Data.Maybe
import Data.Generic
import DOM (DOM)
import Partial.Unsafe (unsafePartial)

import Signal (Signal, runSignal, foldp, sampleOn, map4, merge, filterMap)
import Signal.DOM (keyPressed)
import Signal.Time (Time, second, every)

newtype Point = Point {x :: Int, y :: Int}

derive instance genericPoint :: Generic Point

instance eqPoint :: Eq Point where
    eq = gEq
instance showPoint :: Show Point where
    show = gShow

point :: Int -> Int -> Point
point x y = Point {x : x, y : y}

type Model = {loc :: Point, xd :: Int, yd :: Int, dir :: Point, size :: Int}

inputDir2 :: Eff _ (Signal Point)
inputDir2 = map (\k -> Point { x: 0 , y: if k then  5555555  else 0 }) <$> (keyPressed 68)

input2 :: Eff _ (Signal Point)
input2 = sampleOn (fps 1.0) <$> inputDir2

fps :: Time -> Signal Time
fps x = every (second/x)

step :: forall e. Partial => Point -> Eff (console :: CONSOLE | e) Model -> Eff (console :: CONSOLE| e) Model --need 2nd argument to be Eff for foldp
step dir m' = 
  do
    m <- m'
    let d = if dir /= Point {x:0,y:0}
            then dir
            else m.dir
    log "input"
    log $ show dir
    pure (m {loc = m.loc})

init :: forall e. Eff e Model
init = do
  let psize = 10
  let xmax = 25
  let ymax = 25
  pure {xd : xmax, yd : ymax, loc : point 1 1, dir : point 1 0, size : psize}

main :: forall e. Eff (console :: CONSOLE,
                       dom :: DOM | e) Unit
main = do
  log "cool"
  void $ unsafePartial do
    log "hello"
    dirSignal <- input2
    let game = foldp step init dirSignal
    runSignal (map void game)
