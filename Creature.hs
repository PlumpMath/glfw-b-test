{-# LANGUAGE TemplateHaskell #-}
module Creature where
import           Control.Lens
import           Data.List    (intersprse, (\\))
import qualified Data.Map     as Map
import           Item

-- Used to segregate creatures

data CreatureGenus = Human | Orc | Goblin | Troll | Elf | Dragon | Kobold

-- the 'IsType' is a tuple of genus, genus description, and sprites

type CreatureClassification = (CreatureGenus, String, [String])


-- obviously incomplete Creature type, with lens support

data Creature = Creature { _called             :: String
                         , _classification     :: CreatureClassification
                         , _sprite             :: String
                         , _inventory          :: [ItemSlot]
                         , _health             :: (Int,Int)
                         , _specialDescriptor  :: String
                         , _specialDescription :: String
                         }
              deriving (Read, Eq)

$(makeLenses ''Creature)

-- skipped showing inventory for now since I need to write a show for
-- the Item type

instance Show Creature where
    show (Creature n s i h sd) = sd ++ n ++ "\n" ++
                                 "spritepath: " ++ s ++ "\n" ++
                                 "health: " ++ show (fst h) ++ "/" ++ show (snd h)

-- inventory management functions

addItem :: ItemSlot -> Creature -> Creature
addItem itemslot creature = case findItem creature p of
                            Just (i,_) -> creature & inventory %~ over (ix i) stackItems
                            Nothing    -> checkIfLetterAvailable
    where
      stackItems i = i & stackSize +~ itemslot^.stackSize
      availLetters = (['a'..'z'] ++ ['A'..'Z']) \\ (creature & toListOf (inventory.traverse.itemLetter))
      p _ i = i^.item == itemslot^.item
      checkIfLetterAvailable = case (itemslot^.itemLetter) `elem` availLetters of
                                 True  -> creature & inventory %~ cons itemslot
                                 False -> creature & inventory %~ cons (itemslot & itemLetter .~ head availLetters)

removeItem ::  ItemSlot -> Creature -> Creature
removeItem itemslot creature = case findItem creature p of
                           Just (i,_) -> creature & inventory %~ over (ix i) deleteItem
                           Nothing    -> creature
    where
      deleteItem i = i & stackSize -~ itemslot^.stackSize
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize

findItem :: Creature -> (Int -> ItemSlot -> Bool) -> Maybe (Int, ItemSlot)
findItem creature amount = creature^.inventory & ifind amount

haveItem :: Creature -> ItemSlot -> Bool
haveItem creature itemslot = case findItem creature p of
                         Just _ -> True
                         Nothing -> False
    where
      p _ i = i^.item      == itemslot^.item
           && i^.stackSize >= itemslot^.stackSize

-- more dynamic creature interactions

transferItem :: Creature -> Creature -> ItemSlot -> Maybe (Creature, Creature)
transferItem giver taker itemSlot = if haveItem giver itemSlot
                                    then Just (removeItem itemSlot giver, addItem itemSlot taker)
                                    else Nothing

-- TODO: write the tile and floor data types before allowing tile interactions

--pickupItem :: Tile -> Creature -> ItemSlot -> (Tile, Creature)
--pickupItem giverTile taker itemSlot =



-- Creature types and descriptions

human :: CreatureClassification
human = (Human,
         "Typically between 1.3 and 2 meters in height, and 50 and 120 kilograms, these creatures utilize tools, instruments of warfare, knowledge of the arcane, and cunning to make their way in the world.",
         [])

orc :: CreatureClassification
orc = (Orc,
       "With heights between 1.7 and 2.2 meters, often weighing 75 to 130 kilograms, orcs are known to be brutish and unyielding. Though they lack much knowledge of magic, they are avid tool users, and the smarter of their kind can be sneaky and deceitful when needed.",
       [])

goblin :: CreatureClassification
goblin = (Goblin,
          "Smaller creatures, goblins range from 1.1 to 1.7 meters tall and weight between 40 and 75 kilograms. They are often silent in their movement, though their frame doesn't allow them to use heavier arms. They are often comparatively dull-witted, but agile and deft with rudimentary traps.",
          [])

troll :: CreatureClassification
troll = (Troll,
         "As the largest of the humanoids, trolls tend to stand 2.8 to 3.5 meters tall and weigh 190 to 260 kilograms. Their brawn has supplanted their mental agility, but they are often zealotous followers of a shamanistic occult. They can wield some basic magic, but tend to rely on brute strength to solve problems.",
         [])

elf :: CreatureClassification
elf = (Elf,
       "Slender and tall, elves stand roughly 1.5 to 2.2 meters tall and weigh between 50 and 75 kilograms. Creatures of enlightened ways, elves have great knowledge of the arcane, but neglect the use of tools or heavy arms."
