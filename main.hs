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
data Circle = Circle {  r :: Radius
                     , cp :: Point
                     }


infixl 6 .+
(.+) :: Point -> Point -> Point
(Point x y) .+ (Point x' y') = Point (x+x') (y+y')

infixl 6 .-
(.-) :: Point -> Point -> Point
(Point x y) .- (Point x' y') = Point (x-x') (y-y')


class Reference a where
    add :: Point -> a -> a
    sub :: Point -> a -> a

instance Reference Point where
    add c p = c .+ p
    sub c p = c .- p

instance Reference Line where
    add c (Line p p') = Line (p .+ c) (p' .+ c)
    sub c (Line p p') = Line (p .- c) (p' .- c)

instance Reference Circle where
    add c (Circle r cp) = Circle r (cp .+ c)
    sub c (Circle r cp) = Circle r (cp .- c)

dx :: Line -> Double
dx (Line (Point x _) (Point x' _)) = x' - x 
dy :: Line -> Double
dy (Line (Point _ y) (Point _ y')) = y' - y

distance2 :: Line -> Double
distance2 l = (dx l)^2 + (dy l)^2

determinant :: Line -> Double
determinant (Line (Point x y) (Point x' y')) = x*y' - x'*y

discriminant :: Circle -> Line -> Double
discriminant c@(Circle r cp) l = r^2 * distance2 l' - (determinant l')^2
        where l' = sub cp l
              c' = sub cp c

intersection :: Circle -> Line -> Maybe (Point, Point, Point)
intersection c@(Circle r cp) l
    | disc > 0 = Just $ (add cp center, add cp (center .+ diff), add cp (center .- diff))
    | otherwise = Nothing
        where disc = discriminant c' l'
              det = determinant l'
              d2 = distance2 l'
              center = Point (det * dy l' / d2) (-det * dx l' / d2)
              diff = Point diffX diffY
              diffX = (signum (dy l') * (dx l') * sqrt disc) / d2
              diffY = (abs (dy l')* sqrt disc) / d2
              l' = sub cp l
              c' = sub cp c

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

    SDL.P (V2 x y) <- SDL.getAbsoluteMouseLocation
    let mouse = Point (fromIntegral x) (fromIntegral y)
    let circle = Circle 128 $ Point 400 300
    let line = Line (Point 550 128) mouse

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
renderCircle renderer (Circle r cp) = mapM_ (\(p, p') -> renderLine renderer $ Line p p') $ zip points $ tail points
    where points = map (\t->Point (cx + r * cos t) (cy + r * sin t)) $ map (((2*pi)/nPoints)*) [0..nPoints]
          nPoints = 64
          Point cx cy = cp


main :: IO ()
main = do
    SDL.initializeAll
    window <- SDL.createWindow "Simulation" SDL.defaultWindow

    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    simulationLoop renderer
