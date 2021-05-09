module Types
    ( Cluster (..)
     ,Sample (..) 
     ,Color (..)
     ,Pixel (..)
    ) where

data Color = Color
  { red :: Float
  , grn :: Float
  , blu :: Float
  } deriving (Eq,Ord,Show)

data Pixel = Pixel
    { x :: Int
     ,y :: Int 
     ,clr:: Color
    } deriving (Eq,Ord,Show)

data Cluster = Cluster
    {mean ::Color
     ,pix :: [Pixel]
    } deriving (Eq, Ord, Show)

data Sample = Sample
  {colors :: Int
   ,limit :: Float
   ,path :: String
  }    