module Nokol.Image
    ( getImage
    , getManyImages
    ) where

getImage :: Int -> Int -> String
getImage width height =
    "https://picsum.photos/" ++ show width ++ "/" ++ show height

getManyImages :: Int -> Int -> Int -> [String]
getManyImages width height count =
    replicate count
        $  "https://picsum.photos/"
        ++ show width
        ++ "/"
        ++ show height

