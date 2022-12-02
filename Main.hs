{-# LANGUAGE LambdaCase #-}

import Control.Arrow ((>>>))

main :: IO ()
main = interact (lines >>> zzzzz)

zzzzz :: [String] -> String
zzzzz = show
