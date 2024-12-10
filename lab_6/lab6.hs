{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
-- | N-body simulation.
module Main where

import CodeWorld hiding (Vector)

main :: IO ()
main = animationOf simulateFor

-- Time scaling factor to speed up the simulation
timeScale :: Double
timeScale = 1000000  -- Simulate 1,000,000 seconds per real-time second

-- * Types
-- | Vector (components in meters).
type Vector = (Double, Double)

-- | Gravitational constant (m^3 kg^-1 s^-2)
g :: Double
g = 6.67430e-11

-- | Mass (in kilograms).
type Mass = Double

-- | Position (coordinates in meters).
type Position = Vector

-- | Velocity vector (components in meters per second).
type Velocity = Vector

-- | An astronomical body (e.g., a planet or an asteroid).
data Body = Body Mass Position Velocity Color
  deriving (Eq)

-- | A system of bodies.
data System = System [Body]

-- * Rendering
-- | Render a single body, considering visualization constants.
drawBody :: Body -> Picture
drawBody (Body mass (xPos, yPos) _vel color) = translated x y body
  where
    -- Visualize the mass as a small circle using a logarithmic scale
    body = colored color (solidCircle ((logBase 7 mass) / 10))
    -- Scale down large distances to fit on screen
    positionScale :: Double
    positionScale = 1e9  -- Adjust this value as needed
    x = xPos / positionScale
    y = yPos / positionScale

-- | Render a system of bodies on a black background.
drawSystem :: System -> Picture
drawSystem (System bodies) = pictures (map drawBody bodies)

-- * Simulation
-- | Compute the gravitational acceleration vector on the second body due to the first body
gravityAcc :: Body -> Body -> Vector
gravityAcc (Body m1 p1 _ _) (Body _ p2 _ _) = (ax, ay)
  where
    (rx, ry) = vectorBetween p1 p2
    rSquared = rx * rx + ry * ry
    rCubed = rSquared * sqrt rSquared
    factor = g * m1 / rCubed
    ax = factor * rx
    ay = factor * ry

-- | Compute the vector from the first position to the second
vectorBetween :: Position -> Position -> Vector
vectorBetween (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- | Add two vectors
addVectors :: Vector -> Vector -> Vector
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | Update both position and velocity of a body, considering gravitational effects
updateBody :: Double -> [Body] -> Body -> Body
updateBody dt bodies (Body mass pos (vx, vy) color) = 
    Body mass newPos newVel color
  where
    -- Calculate the total acceleration vector
    (ax, ay) = foldr addVectors (0, 0) [gravityAcc b (Body mass pos (vx, vy) color) | b <- bodies, b /= Body mass pos (vx, vy) color]
    
    -- Update velocity
    newVx = vx + ax * dt
    newVy = vy + ay * dt
    newVel = (newVx, newVy)
    
    -- Update position
    newPos = (fst pos + newVx * dt, snd pos + newVy * dt)

-- | Update all bodies in the system over a given time step (in seconds).
updateSystem :: Double -> System -> System
updateSystem dt (System bodies) = System (map (updateBody dt bodies) bodies)

-- | Simulate the system for a given time duration.
simulateFor :: Double -> Picture
simulateFor time = drawSystem (updateSystem (time * timeScale) solarSystem)

-- * Solar System
-- | Initial configuration of the solar system (Sun, Earth, Moon).
solarSystem :: System
solarSystem = System [earth, moon, sun]
  where
    -- The Sun is stationary at the center
    sun = Body 1.988416e30 (0, 0) (0, 0) orange
    -- The Earth is at -149 million km from the Sun and moving at 29,783 m/s
    earth = Body 5.9742e24 (-1.496e11, 0) (0, 29783) blue
    -- The Moon is 384,400 km away from Earth and orbits at 1,022 m/s
    moon = Body 7.36e22 (-1.496e11 + 384400000, 0) (0, 29783 + 1022) gray
