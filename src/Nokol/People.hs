module Nokol.People
  ( getFirstName
  , getManyFirstNames
  , getLastName
  , getManyLastNames
  , getFullName
  , getManyFullNames
  , getAge
  ) where
import           Control.Monad                  ( replicateM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Nokol.Lib                      ( generateManyM
                                                , getRandomIndex
                                                , getRandomInt
                                                , getRandomString
                                                , getRandomStrings
                                                )

getFirstName :: String -> IO String
getFirstName lang = do
  getRandomString lang "people" "first_name"

getManyFirstNames :: String -> Int -> IO [String]
getManyFirstNames lang count = do
  getRandomStrings lang "people" "first_name" count

getLastName :: String -> IO String
getLastName lang = do
  getRandomString lang "people" "last_name"

getManyLastNames :: String -> Int -> IO [String]
getManyLastNames lang count = do
  getRandomStrings lang "people" "last_name" count

getFullName :: String -> IO String
getFullName lang = do
  fn <- getFirstName lang
  ln <- getLastName lang
  let full_name = fn ++ " " ++ ln
  return full_name

getManyFullNames :: String -> Int -> IO [String]
getManyFullNames lang count = do
  fns <- getManyFirstNames lang count
  lns <- getManyLastNames lang count
  return $ zipWith (++) fns (map (" " ++) lns)

getAge :: String -> IO Int
getAge lang = do
  let ages = [1 .. 100]
  getRandomIndex $ length ages

getManyAges :: String -> Int -> IO [Int]
getManyAges lang count = getAge lang `generateManyM` count
