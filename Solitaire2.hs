module Solitaire2 where

 import System.Random
 import Data.Ord
 import Data.List
 import Data.Maybe
 import Debug.Trace
 import Solitaire1
 import MeanStats
 
 
 --Utilities
 --remove will remove a list of items from a list of lists
 remove :: Eq a => [a] -> [[a]] -> [[a]]
 remove r = map (removeFromList r)
 --removeFromList will remove an item from a list
 removeFromList :: Eq a => [a] -> [a] -> [a]
 removeFromList r = filter keep where
  keep element = element `notElem` r
 
 --deletes the first element in a list (for use in deleting extra leftover columns after moving cards due to the way I've coded the moves
 deleteFirst _ [] = [] 
 deleteFirst a (b:bc) 
  |(a == b)= bc 
  |otherwise = b : deleteFirst a bc

 -- A method that takes a board and returns a board with a King moved from the head of a column to an empty column if its legal
 kingToEmptyCol :: EOBoard -> EOBoard
 kingToEmptyCol (f,[],r) = (f,[],r) --If there are no columns then this is not possible
 kingToEmptyCol (f,c,r) 
  |(numberOfTakenCols (f,c,r) == 8) = (f,c,r)  --If there are 8 columns then this is not possible
  | otherwise = kingToEmptyColA (f,c,r)
 
 kingToEmptyColA :: EOBoard -> EOBoard
 kingToEmptyColA (f,[],r) = (f,[],r) -- Used to check for the end of the recursion
 kingToEmptyColA (f,c,r) 
  |(h == []) = kingToEmptyColA (f,t ++ [[]],r) --Append the empty columns to the back so we dont lose them when recursing
  |(isKing (x,y)) = (f,[(x,y)]:rem,r) --If the head of a column is a King then move it to the free column (removing it from its old one)
  |otherwise = kingToEmptyColA (f,t,r) --otherwise check the other heads
  where (h:t) = c 
        ((x,y):z) = h
        rem = (remove [(x,y)] c)


 --Checks the number of columns taken up already for use in other move methods
 -- ** Important to note that this ignores empty columns which is why I end up keeping empty columns at the end when I make the methods
 numberOfTakenCols :: EOBoard -> Int
 numberOfTakenCols (f,[],r) = 0 --If there are only empty columns then there arent any taken!
 numberOfTakenCols (f,c,r) = numberOfTakenColsA (f,c,r)
 
 numberOfTakenColsA :: EOBoard -> Int
 numberOfTakenColsA (f,[],r) = 8 --Checks for the end of recursion
 numberOfTakenColsA (f,c,r) 
  |(h == []) = (numberOfTakenColsA (f,t,r) - 1)  --Remove 1 from the amount taken for each column that is empty and keep recursing
  |otherwise = numberOfTakenColsA (f,t,r) --otherwise just keep recursing
  where (h:t) = c 
        ((x,y):z) = h


 -- A method that takes a board and returns a board with a King moved from the Reserves to an empty column if its legal
 kingResToEmptyColumn :: EOBoard -> EOBoard
 kingResToEmptyColumn (f,c,[]) = (f,c,[]) --If there are no Reserves then its impossible for a King to be there
 kingResToEmptyColumn (f,c,r) = kingResToEmptyColumnA(f,c,r)

 kingResToEmptyColumnA :: EOBoard -> EOBoard
 kingResToEmptyColumnA (f,c,r)
    |(numberOfTakenCols (f,c,r) == 8) = (f,c,r) --If there arent any free columns we can't move the King
    |((filter isKing r) == []) = (f,c,r) --Check to see if there are any Kings at all in the Reserves (to save time)
    |(isKing h) = (f,[h]:c,rem) --If the head of the Reserves is a King then we can legally move this King to the Empty column, removing it from the Reserves
    |otherwise = kingResToEmptyColumnA (f,c,t ++ [h]) --If it isn't a King then find the next card in the Reserves until there aren't any left to check
    where (h:t) = r
          rem = drop 1 r --because of the way we iterate through the Reserves a drop will suffice


 --A method that takes a board and returns a board with a card (non King) moved from the Reserves to the head of a column if it is a legal move
 resPCardToColHead :: EOBoard -> EOBoard
 resPCardToColHead (f,[],r) = (f,[],r) --If the Columns or  
 resPCardToColHead (f,c,[]) = (f,c,[]) -- Reserves are empty then we can't make this move
 resPCardToColHead (f,c,r)
  |(h == []) = resPCardToColHead (f,t  ++ [],r) --If the head is empty, append it to the end so we don't lose it
  |((x,y) `elem` (map sCard res)) = (f, deleteFirst [] ([(pCard (x,y)):h] ++ (remove h c)) , rem) --If the head is an sCard of one of the Reserves, move that reserve onto the head, removing it from the Reserves 
  |otherwise = (i, j ++ [h] ,k) --We want to keep the head column of all the columns no matter what happens, so append them to the end while recursing so we dont lose them
  where (h:t) = c 
        ((x,y):z) = h
        res = [x| x<-r , not (isKing x)]--Mapping the sCard of Kings breaks it
        rem = removeFromList [pCard (x,y)] r
        (i,j,k) = resPCardToColHead (f,t,r) --Some required pattern matching


 --A method that takes a board and returns a board with a Card moved from one column to the other (because its a predecessor of it) if this is legal
 pCardToOtherCol :: EOBoard -> EOBoard
 pCardToOtherCol (f,[],r) = (f,[],r) --You know the drill
 pCardToOtherCol (f,c,r)
  |(h == []) = pCardToOtherCol (f,t  ++ [h],r) --Check if the head is empty, if so append it to the tail (so we don't lose it)
  |(((x,y) `elem` sHeads)) = (f, deleteFirst [] ([(pCard (x,y)):h] ++ (remove h (remove [pCard(x,y)] c ))), r) --if the first card in the head is a successor of any of the other heads (it can't be its own successor so no worries), move that head onto this card
  |otherwise = pCardToOtherCol (i, j ++ [h] ,k) --Keep any columns that don't fit this rule while recursing
  where (h:t) = c 
        ((x,y):z) = h
        cols = [x| x<-(colHeads (f,c,r)) , not (isKing x)]--Mapping the sCard of Kings breaks it
        sHeads = (map sCard cols) --sHeads is the sCard of all the Heads in the the Columns
        (i,j,k) = pCardToOtherCol (f,t,r)
        
 
 
 --A method that takes a board and returns a list of all cards at the head of a column
 colHeads :: EOBoard -> [Card]
 colHeads (f,[],r) = []
 colHeads (f,c,r) 
  |(h == []) = colHeads (f,t,r) --If its empty you can safely ignore it
  |otherwise = (x,y) : colHeads (f,t,r) --Add any actual cards to the output list and recurse through the tail
  where (h:t) = c 
        ((x,y):z) = h
 
  --A method that takes a board and returns a list of all cards at the 2nd place of a column
 colSeconds :: EOBoard -> [Card]
 colSeconds (f,[],r) = []
 colSeconds (f,c,r)
  |(h == []) = colSeconds (f,t,r) --If its empty you can safely ignore it
  |(z == []) = colSeconds (f,t,r) --need to check theres actually a 2nd element in the column
  |otherwise = colSecondsA z : (colSeconds (f,t,r))
  where (h:t) = c 
        ((x,y):z) = h

 colSecondsA :: [Card] -> Card
 colSecondsA lis = head lis --takes the head of the 2nd element in the list so I can add it to the output list

 --A method that moves the head of a column to the reserves if theres a free column and the card behind it is a King, if this is legal
 secondKingToCol :: EOBoard -> EOBoard
 secondKingToCol (f,[],r) = (f,[],r)
 secondKingToCol (f,c,r)
  |(noFreeCol || noFreeRes) = (f,c,r) --Checks for a free column AND free reserve space (Not A Or Not B == Not (A and B))
  |(filter isKing k == []) = (f,c,r) --Check if there are any kings in the seconds elements for quickness
  |otherwise = secondKingToColA k (f,c,r)
  where  noFreeCol = (numberOfTakenCols (f,c,r) == 8) --If there are 8 columns then none are free (Remember that it ignores the empties in this calculation)
         noFreeRes = (length r == 8)
         k = colSeconds (f,c,r) --Grab all the seconds
 
 --This auxiliary takes a list of cards (the seconds) and a board so that I can check all the seconds easily
 secondKingToColA :: [Card] -> EOBoard -> EOBoard
 secondKingToColA [] (f,c,r) = (f,c,r) --No Kings, no worries
 secondKingToColA lis (f,c,r)
  |(isKing a) = (f, remove [(x,y)] c , r ++ [(x,y)]) --If theres a legal King move then remove the head thats blocking it to the reserves
  |otherwise = secondKingToColA b (f,c,r) --otherwise keep going until you find the King
  where (a:b) = lis
        (h:t) = c 
        ((x,y):z) = h

 --Similar to secondKingToCol this takes the PCard stuck behind the head
 secondPCardToCol :: EOBoard -> EOBoard
 secondPCardToCol (f,[],r) = (f,[],r)
 secondPCardToCol (f,c,r)
   |(length r == 8) = (f,c,r) --If the reserves are full then this is impossible
   |([x| x <- scnds, (x `elem` (map pCard cols))] == []) = (f,c,r) -- A list comprehension for checking all the seconds if theyre a sCard of another head, if there aren't any then we can abandon the search!
   |otherwise = secondPCardToColA scnds (f,c,r) --otherwise there definitely are some, so we should definitely keep looking
   where scnds =  colSeconds (f,c,r) --All the second elements
         cols = [m| m<-(colHeads (f,c,r)) , not (isAce m)] --Mapping the pCard of Aces breaks it
  
 --Almost identical to the previous auxiliary just checks for elem pHeads instead of isKing
 secondPCardToColA :: [Card] -> EOBoard -> EOBoard
 secondPCardToColA [] (f,c,r) = (f,c,r)
 secondPCardToColA lis (f,c,r)
  |(a `elem` (map pCard cols)) = (f, remove [(x,y)] c , r ++ [(x,y)])
  |otherwise = secondPCardToColA b (f,c,r)
  where (a:b) = lis
        (h:t) = c 
        ((x,y):z) = h
        cols = [m| m<-(colHeads (f,c,r)) , not (isAce m)]
       
        


 --A last ditch effort to salvage the game by moving the Highest Pip value reserve card to an empty column, just incase its been stuck there all game and actually has a predecessor
 resCardToEmptyColumn :: EOBoard -> EOBoard
 resCardToEmptyColumn (f,c,[]) = (f,c,[]) --No Reserves, can't work
 resCardToEmptyColumn (f,c,r)
  |(numberOfTakenCols (f,c,r) == 8) = (f,c,r) -- No free columns, can't work
  |otherwise = resCardToEmptyColumnA (f,c,r)
 
 resCardToEmptyColumnA :: EOBoard -> EOBoard 
 resCardToEmptyColumnA (f,c,r) = (f, [x]:c , removeFromList [x] r) where x = (maximumBy (comparing fst) r) --Grab the highest value card for higher probability of winning
 


 --The Score, how many cards were moved during the game
 --score :: EOBoard -> Int
 --score ([],c,r) = 0
 --score (f,c,r) = 52 - (length r)  - (length (concat c))


 --Given an EOBoard, findMoves will display the outputs of the above functions. If the function move is legal 
 --and not the same as the given EOBoard, it will be added to the list findMoves outputs. If the move is not available or 
 --it is not different to the original EOBoard, it will not be shown on the in the outputted list. findMoves outputs the 
 --list of EOBoards in order of how good of a move they are. For example, an EOBoard at the top of the list from findMoves 
 --will be better than an EOBoard move at the bottom of the list.
 findMoves :: EOBoard -> [EOBoard]
 findMoves board = [] ++ (filter (/= board) [(kingResToEmptyColumn board),(kingToEmptyCol board),(resPCardToColHead board),(pCardToOtherCol board),(secondKingToCol board),(secondPCardToCol board),(resCardToEmptyColumn board)]) 

 --Given an EOBoard, chooseMove takes that EOBoard and then passes it to findMoves to find the list of available moves. 
 --If findMoves returns with a list of no EOBoards, chooseMove will consquesntly return Nothing. If findMoves returns with a 
 --list of at least 1 EOBoard, chooseMoves will return Just the first EOBoard in the list, as that would be the best move in 
 --the list
 chooseMove :: EOBoard -> Maybe EOBoard
 chooseMove board 
  |((toFoundations board) /= board ) = chooseMove (toFoundations board)
  |(findMoves board /= []) = chooseMove (head (findMoves board))
  |otherwise = Nothing
 
 
 eOGame :: EOBoard -> Int
 eOGame board
  |isNothing nextBoard = score board
  |otherwise = eOGame (resMaybe nextBoard)
  where nextBoard = chooseMove board
        score (foundations,columns,reserve) = sum $ map length foundations


  
 
 --Given an EOBoard, score will return an Int corresponding to the number of cards that have been moved to the foundation. 
 --A winning score would be a score of 52.		 
 
 eOExpt :: Int -> Int -> (Int,Float)
 eOExpt seed numberOfGames = (0,avScore)
     where listBoards = map eODeal (take 100 (randoms (mkStdGen seed) :: [Int]))
           listScores = map eOGame listBoards
           avScore = fromIntegral (sum listScores)/ fromIntegral numberOfGames
 
  -- Maybe helper                
 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x 
