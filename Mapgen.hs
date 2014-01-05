{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Creature
import qualified Data.Map      as Map
import           System.Random
import System.FilePath
import System.Directory
import Data.Maybe (fromJust)
import Item

-- The Floor data type will store all tiles on that dungeon floor

data Floor = Floor { _floorName :: String
                   , _floorPlan :: FloorPlan
                   }

type Xpos = Int
type Ypos = Int
type FloorPlan = Map.Map (Xpos, Ypos) Tile

-- RoomStyles will be used (eventually) to define aspects of room
-- layout, as well as creatures and items inside

data RoomStyle = Arboretum | CouncilRoom | Crucible | Dining | Dormitory | Jail | Kitchen | Smithy | Storage | Temple | Tomb | TortureChamber

-- TODO: add Tiles having 'creatures', 'traps', etc

data Tile = Tile { _tileSprite  :: String
                 , _onTheGround :: Inventory
                 , _creature    :: Creature
                 }

data Room = Room { _roomStyle :: RoomStyle
                 , _xPos      :: Int
                 , _yPos      :: Int
                 , _layout    :: Layout
                 , _size      :: Int
                 }

type Layout = Map.Map (Int,Int) Tile

readLayoutFile :: String -> IO FilePath
readLayoutFile fp = do
  curDir <- getCurrentDirectory
  gotFile <- findFile [curDir </> "data" </> "myroomlayouts", curDir </> "data" </> "roomlayouts"] fp
  case gotFile of
    Just fullFP -> return fullFP
    Nothing -> return "Layout file not found!"

-- TODO: Parse the input file and generate the layout below
    
--generateLayoutFromFile :: String -> Layout
--generateLayoutFromFile fp = 
    
-- Setup a few tiles to work with
-- Ideally, '_sprite' would just pick out a sprite
          
wall, dungeonFloor :: Tile
wall = Tile { _tileSprite = "wall"
            , _onTheGround = []
            }

dungeonFloor = Tile { _tileSprite = "floor"
                    , _onTheGround = []
                    }

tileMapping :: Map.Map Int Tile
tileMapping = Map.fromList $ zip [1,2..] tileList
              where
                tileList = [wall, dungeonFloor]

-- generates a rectangular room given max and min X and Y dimentions
-- TODO: modify to allow custom default floor and wall sprites

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
