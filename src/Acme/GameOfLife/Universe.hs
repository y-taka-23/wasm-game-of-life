module Acme.GameOfLife.Universe (
      random
    , fromList
    , stringify
    , unstringify
    , tick
    , toggle
    ) where

import qualified Data.Vector   as V
import           System.Random ( getStdGen, randomR )

data Cell =
      Dead
    | Alive
    deriving (Eq, Show)

data Universe = Universe {
      height :: Int
    , width  :: Int
    , cells  :: V.Vector Cell
    } deriving (Eq, Show)

stringify :: Universe -> String
stringify univ =
    let toChar Dead  = '0'
        toChar Alive = '1'
    in V.toList $ V.map toChar $ cells univ

unstringify :: Int -> Int -> String -> Universe
unstringify h w str =
    let fromChar '0' = Dead
        fromChar '1' = Alive
        fromChar _   = error "unknown"
    in Universe h w $ V.map fromChar $ V.fromList str

fromList :: Int -> Int -> [(Int, Int)] -> Universe
fromList h w ps =
    let toIndex (r, c) = r * w + c
        toCell is i = if i `elem` is then Alive else Dead
    in Universe h w $ V.generate (h * w) (toCell (map toIndex ps))

random :: Int -> Int -> IO Universe
random h w = do
    let fromChar '0' = Dead
        fromChar '1' = Alive
        fromChar _   = error "unknown"
    gen <- getStdGen
    pure $ Universe h w $ V.map fromChar $ V.unfoldrN (h * w) (Just . randomR ('0', '1')) gen

type Rule = Int -> Cell -> Cell

conway :: Rule
conway 3 Dead  = Alive
conway _ Dead  = Dead
conway 2 Alive = Alive
conway 3 Alive = Alive
conway _ Alive = Dead

liveNeighborCount :: Universe -> Int -> Int
liveNeighborCount univ i =
    let (row, col) = (i `div` width univ, i `mod` width univ)
        neighbors = [
              (row - 1, col - 1), (row - 1, col), (row - 1, col + 1)
            , (row,     col - 1),                 (row,     col + 1)
            , (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
            ]
        wrap (r, c) = (r `mod` height univ, c `mod` width univ)
        toIndex (r, c) = r * width univ + c
        count = length . filter ((==) Alive)
    in count $ map ((V.!) (cells univ) . toIndex . wrap) neighbors

tick :: Universe -> Universe
tick univ =
    let applyRule rule i = rule (liveNeighborCount univ i)
    in univ { cells = V.imap (applyRule conway) $ cells univ }

toggle :: Int -> Int -> Universe -> Universe
toggle row col univ =
    let toggleAt i j Dead  = if i == j then Alive else Dead
        toggleAt i j Alive = if i == j then Dead else Alive
    in univ { cells = V.imap (toggleAt (row * width univ + col)) $ cells univ }
