{-# LANGUAGE TemplateHaskell #-}
module Creature where
import           Control.Lens
import qualified Data.Map     as Map
import           Item

-- Used to segregate creatures
data CreatureGenus = Human | Orc | Goblin | Troll | Elf | Dragon | Kobold
-- the 'IsType' is a tuple of genus, genus description, and sprites
type CreatureIsType = (CreatureGenus, String, [String])

-- obviously incomplete Creature type, with lens support

data Creature = Creature { _called            :: String
                         , _sprite            :: String
                         , _inventory         :: [Item]
                         , _health            :: (Int,Int)
                         , _specialDescriptor :: String
                         }
              deriving (Read, Eq)

$(makeLenses ''Creature)

-- skipped showing inventory for now since I need to write a show for
-- the Item type

instance Show Creature where
    show (Creature n s i h sd) = sd ++ n ++ "\n" ++
                                 "spritepath: " ++ s ++ "\n" ++
                                 "health: " ++ (show $ fst h) ++ "/" ++ (show $ snd h)

