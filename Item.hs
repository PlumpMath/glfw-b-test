{-# LANGUAGE TemplateHaskell #-}
module Item where
import           Control.Lens
import qualified Data.Map     as Map

-- Item data. Very simple currently.
    
data Item = Item { _name        :: String
                 , _description :: String
                 , _weight      :: Int
                 , _valuePer    :: Int
                 }
          deriving (Read, Eq)

instance Show Item where
    show (Item name description _ _) = name ++ " - " ++ description

-- Slots are meta-items used to track inventory related stats
                                     
data ItemSlot = ItemSlot { _stackSize  :: Int
                         , _itemLetter :: Char
                         , _item       :: Item
                         }
                deriving (Read, Eq)

$(makeLenses ''Item)
$(makeLenses ''ItemSlot)

type Inventory = [Item]
