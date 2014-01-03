{-# LANGUAGE TemplateHaskell #-}
module Item where
import           Control.Lens
import qualified Data.Map     as Map

    
data Item = Item { _letter      :: Char
                 , _description :: String
                 }
          deriving (Read, Eq)

instance Show Item where
    show (Item letter description) = letter : " - " ++ description

                                     

                                     
