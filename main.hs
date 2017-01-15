{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Data.Functor

import qualified SDL
import Linear (V2(..),V4(..))


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
(Point x y) .+ (Point x' y') = Point (x+x') (y+y')

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
discriminant (Circle r) l = r^2 * distance2 l - (determinant l)^2

intersection :: Circle -> Line -> Maybe (Point, Point, Point)
intersection c@(Circle r) l
    | disc > 0 = Just $ (center, center .+ diff, center .- diff)
    | otherwise = Nothing
        where disc = discriminant c l
              det = determinant l
              d2 = distance2 l
              center = Point (det * dy l / d2) (-det * dx l / d2)
              diff = Point diffX diffY
              diffX = (signum (dy l) * (dx l) * sqrt disc) / d2
              diffY = (abs (dy l)* sqrt disc) / d2 

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

    SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
--    SDL.drawLine renderer (SDL.P $ V2 0 0) (SDL.P $ V2 64 64)

    SDL.P (V2 x y) <- SDL.getAbsoluteMouseLocation
    let mouse = Point (fromIntegral x) (fromIntegral y)
    let circle = Circle 512
    let line = Line (Point 550 128) (Point 16 512)

    print mouse

    renderCircle renderer circle
    renderLine renderer line

    case intersection circle line of
        (Just ps@(c, p, p')) -> do
            SDL.rendererDrawColor renderer SDL.$= V4 0 255 0 255
            renderPoint renderer c
            SDL.rendererDrawColor renderer SDL.$= V4 255 0 0 255
            renderPoint renderer p
            renderPoint renderer p'
        Nothing -> return ()

    SDL.present renderer

    unless qPressed $ simulationLoop renderer

renderPoint renderer p@(Point x y) = do
    renderLine renderer (Line p (Point (x+8) (y+8)))
    renderLine renderer (Line p (Point (x+8) (y-8)))
    renderLine renderer (Line p (Point (x-8) (y+8)))
    renderLine renderer (Line p (Point (x-8) (y-8)))
renderLine renderer (Line (Point x y)  (Point x' y')) = SDL.drawLine renderer (SDL.P $ V2 (round x) (round y)) (SDL.P $ V2 (round x') (round y'))
renderCircle renderer (Circle r) = mapM_ (\(p, p') -> renderLine renderer $ Line p p') $ zip points $ tail points
    where points = map (\t->Point (r * cos t) (r * sin t)) $ map (((2*pi)/nPoints)*) [0..nPoints]
          nPoints = 64

main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Simulation" SDL.defaultWindow

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    simulationLoop renderer
