{-# LANGUAGE OverloadedStrings #-}

module Main where

import Asterius.Types ( JSString(..), fromJSString, toJSString )

import Acme.GameOfLife.Universe ( random, stringify, tick, toggle, unstringify )

main :: IO ()
main = error "built with --no-main"

getHeight :: Int
getHeight = 64

getWidth :: Int
getWidth = 64

randomUniverse :: IO JSString
randomUniverse = toJSString . stringify <$> random getHeight getWidth

tickUniverse :: JSString -> JSString
tickUniverse = toJSString . stringify . tick . unstringify getHeight getWidth . fromJSString

toggleCell :: Int -> Int -> JSString -> JSString
toggleCell row col =
    toJSString . stringify . toggle row col .
    unstringify getHeight getWidth . fromJSString

foreign export javascript "randomUniverse" randomUniverse :: IO JSString
foreign export javascript "tickUniverse"   tickUniverse   :: JSString -> JSString
foreign export javascript "getHeight"      getHeight      :: Int
foreign export javascript "getWidth"       getWidth       :: Int
foreign export javascript "toggleCell"     toggleCell     :: Int -> Int -> JSString -> JSString
