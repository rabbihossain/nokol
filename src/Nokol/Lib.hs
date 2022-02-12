{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Nokol.Lib
    ( getRandomIndex
    , getRandomInt
    , getRandomString
    , getRandomStrings
    , generateManyM
    , generateMany
    ) where
import           Control.Exception              ( SomeException
                                                , throw
                                                )
import           Control.Exception.Base         ( try )
import           Control.Monad                  ( replicateM )
import           Control.Monad.IO.Class         ( MonadIO )
import           System.Random                  ( Random(randomR)
                                                , getStdRandom
                                                )
readDataFiles :: String -> String -> String -> IO [String]
readDataFiles lang dir key =
    try (readFile ("data/" ++ lang ++ "/" ++ dir ++ "/" ++ key)) >>= \case
        Left (except :: SomeException) -> do
            print except
            error "Please check your language, object, field parameters"
            pure []
        Right content -> pure (lines content)

getRandomString :: String -> String -> String -> IO String
getRandomString lang dir key = do
    string_data  <- readDataFiles lang dir key
    random_index <- getRandomIndex $ length string_data
    return $ selectRandomString string_data random_index

getRandomStrings :: String -> String -> String -> Int -> IO [String]
getRandomStrings lang dir key count = do
    string_data <- readDataFiles lang dir key
    let length_of_data = length string_data
    random_indexes <- generateManyM (getRandomIndex length_of_data) count
    return [ string_data !! i | i <- random_indexes ]

selectRandomString :: [String] -> Int -> String
selectRandomString []  _     = ""
selectRandomString [x] _     = x
selectRandomString xs  index = xs !! index

getRandomInt :: String -> String -> String -> IO Int
getRandomInt lang dir key = do
    string_data  <- readDataFiles lang dir key
    random_index <- getRandomIndex $ length string_data
    return $ selectRandomInt string_data random_index

getRandomIntss :: String -> String -> String -> Int -> IO [Int]
getRandomIntss lang dir key count = do
    string_data <- readDataFiles lang dir key
    let length_of_data = length string_data
    random_indexes <- generateManyM (getRandomIndex length_of_data) count
    return [ read (string_data !! i) :: Int | i <- random_indexes ]


selectRandomInt :: [String] -> Int -> Int
selectRandomInt []  _     = 0
selectRandomInt [x] _     = read x :: Int
selectRandomInt xs  index = read (xs !! index) :: Int

getRandomIndex :: (MonadIO m, Random a, Num a) => a -> m a
getRandomIndex length = getStdRandom $ randomR (0, length - 1)

generateMany :: a -> Int -> [a]
generateMany func length = replicate length func


generateManyM :: Applicative m => m a -> Int -> m [a]
generateManyM func length = replicateM length func
