module Board where

type Grid = [[Int]]

-- Board creation
createGrid :: Int -> Int -> Grid
createGrid cols rows = replicate rows (replicate cols 0)

playerOne :: Int
playerOne = 1

playerTwo :: Int
playerTwo = 2

-- Inserting a piece
insertDisc :: Grid -> Int -> Int -> Grid
insertDisc grid colIndex player =
  let (top, row:bottom) = splitAt (findAvailableRow grid colIndex) grid
      updatedRow = updateList (colIndex - 1) player row
  in top ++ (updatedRow : bottom)

columnAvailable :: Grid -> Int -> Bool
columnAvailable grid colIndex = any (\r -> (r !! (colIndex - 1)) == 0) grid

totalColumns :: Grid -> Int
totalColumns grid = length (head grid)

gridIsFull :: Grid -> Bool
gridIsFull grid = all (all (/= 0)) grid

-- Win condition
hasWinner :: Grid -> Int -> Bool
hasWinner grid player = or [ checkLine grid player r c dr dc
                           | r <- [0..rows-1], c <- [0..cols-1], (dr, dc) <- directions ]
  where
    rows = length grid
    cols = length (head grid)
    directions = [(0,1), (1,0), (1,1), (1,-1)]

checkLine :: Grid -> Int -> Int -> Int -> Int -> Int -> Bool
checkLine grid player r c dr dc =
  all (\i -> (grid !! ((r + i * dr) `mod` numRows)) !! ((c + i * dc) `mod` numCols) == player) [0..3]
  where
    numRows = length grid
    numCols = length (head grid)

-- Displaying the grid
gridToString :: (Int -> Char) -> Grid -> String
gridToString cellToChar grid =
  unlines [unwords [[cellToChar x] | x <- row] | row <- grid]

-- Helper utilities
updateList :: Int -> a -> [a] -> [a]
updateList index val lst = take index lst ++ [val] ++ drop (index + 1) lst

findAvailableRow :: Grid -> Int -> Int
findAvailableRow grid colIndex =
  let empties = [i | (i, row) <- zip [0..] (reverse grid), (row !! (colIndex - 1)) == 0]
  in if null empties then error "Column is full!" else (length grid - 1 - head empties)
