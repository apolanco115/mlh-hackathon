module Main(main, PongGame(..), render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

ballRadius :: Radius
ballRadius = 10

player1PaddleXPosition :: Float
player2PaddleXPosition :: Float
player1PaddleXPosition = (-120)
player2PaddleXPosition = 120

paddleHeight :: Float
paddleHeight = 86

paddleWidth :: Float
paddleWidth = 26

type Radius = Float
type Position = (Float, Float)

main :: IO ()
main = simulate window background fps initialState render update
-- | Update the game by moving the ball
-- Ignore the ViewPort argument
update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . paddleBounce . moveBall seconds

-- | Given position and radius of the ball, return whether a collision occurred
paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius = leftCollision || rightCollision
  where
    -- initial heights of the paddles and position of ball
    y1 = player1 game
    y2 = player2 game
    (x, y) = ballLoc game
    -- detect collision
    leftCollision = x <= player1PaddleXPosition && withinPaddleArea (x, y) ballRadius y1
    rightCollision = x >= player2PaddleXPosition && withinPaddleArea (x, y) ballRadius y2
    -- check to see if ball is within the paddle area
    withinPaddleArea :: Position -- ^ Position of the ball
                        -> Float -- ^ Ball radius
                        -> Float -- ^ Paddle y position
                        -> Bool -- ^ True if ball is within paddle area
    withinPaddleArea (_, ballY) radius position = topOfBall >= position - (paddleHeight / 2) && bottomOfBall <= position + (paddleHeight / 2)
      where
        bottomOfBall = ballY - radius
        topOfBall = ballY + radius   

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game {
  ballVel = (vx', vy)
} where
    -- get current velocity
    (vx, vy) = ballVel game
    -- change velocity by inverting vx and adjusting values for vx and vy
    vx' = if paddleCollision game ballRadius
          then
            -vx
          else
            -- Do nothing
            vx

-- | Given position and radius of the ball, return whether a collision occurred
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral height / 2
    bottomCollision = y + radius >= fromIntegral height / 2

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game {
  ballVel = (vx, vy')
} where
    -- old velocities
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) ballRadius
          then
            -- Update velocity
            -vy
          else
            -- Do nothing
            vy

data PongGame = Game {
  ballLoc :: (Float, Float), -- ^ Pong ball (x, y) location
  ballVel :: (Float, Float), -- ^ Pong ball (x, y) velocity
  player1 :: Float, -- ^ Left player paddle height
                    -- Zero is middle of the screen
  player2 :: Float -- ^ Right player paddle height
} deriving Show

-- | The starting state for the game of Pong
initialState :: PongGame
initialState = Game {
  ballLoc = (90, 30),
  ballVel = (30, -10),
  player1 = 80,
  player2 = -50
}

-- | Convert a game state into a picture
render :: PongGame -- ^ The game state to render
          -> Picture -- ^ A picture of this game state
render game = pictures [ball, walls,
                mkPaddle rose player1PaddleXPosition $ player1 game,
                mkPaddle orange player2PaddleXPosition $ player2 game
                ] where
                    -- the pong ball
                    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
                    ballColor = dark red
                    -- the top and bottom walls
                    wall :: Float -> Picture
                    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
                    wallColor = greyN 0.5
                    walls = pictures [wall $ fromIntegral height / 2, wall $ fromIntegral height / 2]
                    -- the paddles
                    paddleBorder = 6
                    innerPaddleHeight = paddleHeight - paddleBorder
                    innerPaddleWidth = paddleWidth - paddleBorder
                    mkPaddle :: Color -> Float -> Float -> Picture
                    mkPaddle col x y = pictures [
                      translate x y $ color col $ rectangleSolid paddleWidth paddleHeight,
                      translate x y $ color paddleColor $ rectangleSolid innerPaddleWidth innerPaddleHeight]
                    paddleColor = light (light blue)


-- | Update the ball position using its current velocity
moveBall :: Float -- ^ The number of seconds since last update
            -> PongGame -- ^ The initial game state
            -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game {
  ballLoc = (x', y')
} where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

