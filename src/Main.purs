module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)

data BasicData = SomeConstructor
instance showBasicData :: Show BasicData where
  show SomeConstructor = "SomeConstructor"

data ProductData = OneArg Int
instance showProductData :: Show ProductData where
  show (OneArg x) = "OneArg " <> show x

data SumData = One | Two
instance showSumData :: Show SumData where
  show One = "One"
  show Two = "Two"

data Coordinates = Coordinates Int Int
instance showCoordinates :: Show Coordinates where
  show (Coordinates x y) = "Coordinates " <> show x <> " " <> show y
incrementX :: Coordinates -> Coordinates
incrementX (Coordinates x y) = Coordinates (x + 1) y
incrementY :: Coordinates -> Coordinates
incrementY (Coordinates x y) = Coordinates x (y + 1)

data DayOfWeekType = Weekday | Weekend
instance showDayOfWeekType :: Show DayOfWeekType where
  show Weekday = "Weekday"
  show _ = "Weekend"
whatToDo :: DayOfWeekType -> String
whatToDo = case _ of
  Weekday -> "work"
  Weekend -> "sleep"

newtype Id = ValidatedId String
derive instance newtypeId :: Newtype Id _
derive newtype instance showId :: Show Id
makeId :: String -> Maybe Id
makeId "" = Nothing
makeId s = Just $ wrap s

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log $ show SomeConstructor
  log $ show (OneArg 1)
  log $ show One
  log $ show Two
  log $ show (Coordinates 1 2)
  log $ show (incrementX (Coordinates 1 1))
  log $ show (incrementY (Coordinates 1 1))
  log $ whatToDo Weekday
  log $ show (makeId "")
  log $ show (makeId "abc")
