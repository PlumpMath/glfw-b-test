{-# LANGUAGE TemplateHaskell   #-}
import           Control.Lens
import qualified Data.Map      as Map
import           System.Random

-- The Floor data type will store all tiles on that dungeon floor

data Floor = Floor { _name      :: String
                   , _floorPlan :: FloorPlan
                   }

type Xpos = Int
type Ypos = Int
type FloorPlan = Map.Map (Xpos, Ypos) Tile
type Inventory = [Item]

-- Todo: add Tiles having 'creatures', 'traps', etc

data Tile = Tile { _sprite    :: String
                 , _inventory :: Inventory
                 }

-- Items should get thier own module

data Item = Item { _letter      :: Char
                 , _description :: String
                 }

-- Setup a few tiles to work with
-- Ideally, '_sprite' would just pick out a sprite

wall, dungeonFloor :: Tile
wall = Tile { _sprite = "wall"
            , _inventory = []
            }

dungeonFloor = Tile { _sprite = "floor"
             , _inventory = []
             }

tileMapping :: Map.Map Int Tile
tileMapping = Map.fromList $ zip [1,2..] tileList
              where
                tileList = [wall, dungeonFloor]

generateRandomRoom :: Int -> Int -> Int -> Int -> IO ([(Int, [Tile])], (Int, Int))
generateRandomRoom xMin xMax yMin yMax = do
  initialGenerator <- getStdGen
  let
      xMeasure = randomR (xMin, xMax) initialGenerator
      yMeasure = randomR (yMin, yMax) $ snd xMeasure
      generateRow :: Int -> Int -> Int -> [Tile]
      generateRow bottom width yVal
        | yVal == 1 || yVal == bottom = replicate width wall
        | otherwise = [wall] ++ (replicate (width - 2) dungeonFloor) ++ [wall]
      yDim = fst yMeasure
      xDim = fst xMeasure
      roomDimensions = (xDim, yDim)
      theRoomAs2DMatrix = zip [1,2..yDim] $ map (generateRow yDim xDim) [1,2..yDim]
  return (theRoomAs2DMatrix, roomDimensions)
