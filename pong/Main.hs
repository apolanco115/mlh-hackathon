module Main(main, PongGame(..), render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Network.Socket
import System.IO
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets hiding (Message)
import Network.Socket (withSocketsDo)
import Graphics.Gloss.Interface.IO.Game
import Data.Bifunctor
import Data.Biapplicative
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.List (partition)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forever)
import GHC.Generics
import Data.Serialize (Serialize, encodeLazy, decodeLazy, get)
import Control.Monad
import System.Environment
import System.Exit

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

paddleSpeed = 4.0

player1PaddleXPosition :: Float
player2PaddleXPosition :: Float
player1PaddleXPosition = (-120)
player2PaddleXPosition = 120

paddleHeight :: Float
paddleHeight = 86

paddleWidth :: Float
paddleWidth = 26

paddleYMax = 150 - paddleHeight


type Radius = Float
type Position = (Float, Float)
type PongTuple = (Float, Float, Float)
data Pong = Pong {
  xVel :: Float,
  yVel :: Float,
  pos :: Float
} deriving (Show, Read)

instance Serialize Pong

data Role = Server Int | Client String Int deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  role <- case args of ip:port:[] -> return $ Client ip (read port)
                       port:[] -> return $ Server $ read port
                       _ -> do
                               putStrLn "Invalid arguments."
                               putStrLn "Usage:"
                               putStrLn "  pong address port      - connects to an existing server"
                               putStrLn "  pong port              - creates a server"
                               exitFailure
  sendChan <- newChan
  recvMVar <- newMVar []
  gameState <- newMVar (30, -10, -30)
  _ <- forkIO $ case role of
    Server port -> do
      runServer "0.0.0.0" port $ ws gameState sendChan <=< acceptRequest
    Client ip port -> do
      withSocketsDo $ runClient ip port "/" $ ws gameState sendChan
  playIO window background fps initialState render handleKeys (update gameState sendChan)
  --TODO clean up the socket properly


update :: MVar PongTuple -> Chan Pong -> Float -> PongGame -> IO PongGame
update state chan seconds game = 
  writeState chan =<<
  updateGame state game =<<
  ((outOfBounds game . updatePaddle game . wallBounce game . paddleBounce game . moveBall seconds game))


updateGame :: MVar PongTuple -> PongGame -> IO PongGame
updateGame state game = do
  gs <- readMVar state
  let (xVel', yVel', pos') = gs
  return game {
    ballVel = (xVel', yVel'),
    player2 = pos'
  }

writeState :: Chan Pong -> PongGame -> IO PongGame
writeState chan game = do
  let (xVel', yVel') = ballVel game
  let pong = pong {
    xVel = xVel',
    yVel = yVel',
    pos = player2 game
  }
  writeChan chan pong
  return game

fromRight (Right a) = a --FIXME ignore, report or something
fromRight _ = error "deserialization error. sorry :("

instance WebSocketsData Pong where
  fromLazyByteString = fromRight . decodeLazy
  toLazyByteString = encodeLazy
  fromDataMessage (Binary bs) = fromLazyByteString bs
  fromDataMessage _ = error "Invalid websocket message"

ws :: MVar PongTuple -> Chan Pong -> Connection -> IO ()
ws gameState chan conn = do
  _ <- forkIO $ recvData conn gameState
  sendData conn chan

sendData :: Connection -> Chan Pong -> IO ()
sendData conn ch = do
  forever $ do
    m <- readChan ch
    sendBinaryData conn m
  --hClose hdl

recvData :: Connection -> MVar PongTuple -> IO ()
recvData conn gameState = do
  forever $ do
    pong <- receiveData conn
    modifyMVar_ gameState $ \_-> do
      let xVel' = xVel pong
      let yVel' = yVel pong
      let pos' = pos pong
      return (xVel', yVel', pos')
  --hClose hdl

-- | Given position and radius of the ball, return whether a collision occurred
paddleCollision :: PongGame -> Radius -> Bool
paddleCollision game radius = leftCollision || rightCollision
  where
    -- initial heights of the paddles and position of ball
    y1 = player1 game
    y2 = player2 game
    (x, y) = ballLoc game
    -- detect collision
    leftCollision = ((x - ballRadius) < (player1PaddleXPosition + (paddleWidth / 2))) && (withinPaddleArea (x, y) ballRadius y1)
    rightCollision = ((x + ballRadius) > (player2PaddleXPosition - (paddleWidth / 2))) && (withinPaddleArea (x, y) ballRadius y2)
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

outOfBounds :: PongGame -> PongGame
outOfBounds game = game {
  ballLoc = (x', y') 
} where
    (x, y) = ballLoc game

    (x', y') = if (x > fromIntegral width / 2) || (x < -fromIntegral width / 2)
          then
            -- Update velocity
            (0, 0)
          else
            -- Do nothing
            (x, y)

data PaddleMovement = PaddleUp | PaddleStill | PaddleDown deriving Show

data PongGame = Game {
  ballLoc :: (Float, Float), -- ^ Pong ball (x, y) location
  ballVel :: (Float, Float), -- ^ Pong ball (x, y) velocity
  player1 :: Float, -- ^ Left player paddle height
                    -- Zero is middle of the screen
  player2 :: Float, -- ^ Right player paddle height
  paddleMove :: PaddleMovement
} deriving (Show)

-- | The starting state for the game of Pong
initialState :: PongGame
initialState = Game {
  ballLoc = (90, 30),
  ballVel = (30, -10),
  player1 = 80,
  player2 = -30,
  paddleMove = PaddleStill
}

-- | Convert a game state into a picture
render :: Monad m => PongGame -- ^ The game state to render
          -> m Picture -- ^ A picture of this game state
render game = return $ pictures [ball, walls,
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

-- | Respond to key events.
-- handleKeys :: Event -> PongGame -> PongGame

-- -- for an 's' key press, reset the ball to the center
-- handleKeys (EventKey (Char 'w') Down _ _) game = game {
--   player1 = increment
-- } where
--     initialHeight = player1 game
--     increment = initialHeight + 5

-- handleKeys (EventKey (Char 's') Down _ _) game = game {
--   player1 = increment
-- } where
--     initialHeight = player1 game
--     increment = initialHeight - 5


movePaddle :: Float -> PaddleMovement -> Float
movePaddle py PaddleUp = py + 5
movePaddle py PaddleStill = py
movePaddle py PaddleDown = py - 5

updatePaddle :: PongGame -> PongGame
updatePaddle game = game { player1 = movePaddle (player1 game) (paddleMove game)}


handleKeys :: Monad m => Event -> PongGame -> m PongGame
handleKeys (EventKey (Char 'w') Down _ _) game = return game { paddleMove = PaddleUp }
handleKeys (EventKey (Char 'w') Up _ _) game = return game { paddleMove = PaddleStill }
handleKeys (EventKey (Char 's') Down _ _) game = return game { paddleMove = PaddleDown }
handleKeys (EventKey (Char 's') Up _ _) game = return game { paddleMove = PaddleStill }


-- handleKeys :: Event -> PongGame -> PongGame

-- -- for an 's' key press, reset the ball to the center
-- handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y')) game = game {
--   player1 = increment
-- } where
--     increment = y'

-- Do nothing for all other events.
handleKeys _ game = return game
