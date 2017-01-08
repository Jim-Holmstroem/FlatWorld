{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative

import qualified SDL
import Linear (V4(..))


data Point = Point { x :: Double
                   , y :: Double
                   }
    deriving (Show)
data Line = Line { p :: Point
                 , p' :: Point
                 }
    deriving (Show)
type Radius = Double
data Circle = Circle { r :: Radius
                     -- cp :: Point 0 0
                     }

infixl 6 .+
(.+) :: Point -> Point -> Point
(Point x y) .+ (Point x' y') = Point (x+x') (y-y')

infixl 6 .-
(.-) :: Point -> Point -> Point
(Point x y) .- (Point x' y') = Point (x-x') (y-y')

dx :: Line -> Double
dx (Line (Point x _) (Point x' _)) = x' - x 
dy :: Line -> Double
dy (Line (Point _ y) (Point _ y')) = y' - y

distance2 :: Line -> Double
distance2 l = (dx l)^2 + (dy l)^2

determinant :: Line -> Double
determinant (Line (Point x y) (Point x' y')) = x*y' - x'*y

discriminant :: Circle -> Line -> Double
discriminant (Circle r) l = r^2 * distance2 l - determinant l

intersection :: Circle -> Line -> Maybe (Point, Point)
intersection c@(Circle r) l
    | disc > 0 = Just $ (center .+ diff, center .- diff)
    | otherwise = Nothing
        where disc = discriminant c l
              det = determinant l
              d2 = distance2 l
              center = Point (det * dy l / d2) (-det * dx l / d2)
              diff = Point (((signum (dy l) * (dx l) * sqrt disc)) / d2) ((abs (dy l)* sqrt disc) / d2)
              


simulationLoop :: SDL.Renderer -> IO ()
simulationLoop renderer = do
    events <- SDL.pollEvents

    let eventIsQPressed event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False

        qPressed = not $ null $ filter eventIsQPressed events

    SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
    SDL.clear renderer



    SDL.present renderer

    unless qPressed $ simulationLoop renderer

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Simulation" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    simulationLoop renderer
