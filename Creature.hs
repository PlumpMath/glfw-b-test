{-# LANGUAGE TemplateHaskell #-}
module Creature where
import           Control.Lens
import qualified Data.Map     as Map

-- obviously incomplete Creature type, with lens support
    
data Creature = Creature { _called    :: String
                         , _sprite    :: String
                         , _inventory :: [Item]
                         , _health    :: (Int,Int)
                         }
              deriving (Read, Eq)

$(makeLenses ''Creature)

instance Show Creature where
    show (Creature name sprite inventory health) = name ++ '\n' ++
                                                   "spritepath: " ++ sprite ++ '\n' ++
                                                   "health: " ++ (fst health) ++ "/" ++ (snd health)
                                                              
