module Solitaire2 where
    import Solitaire1
    import Data.Maybe
    import Data.List
    import System.IO
    import System.Random
    
    -- Takes a card and a column, and removes that card from the column the card is in
    removeCardCols :: Card -> [[Card]] -> [[Card]]
    removeCardCols _ [] = []
    removeCardCols card (h:t)
        | elem card h         = (delete card h:t)                      -- Removes the card if it is head of the list
        | otherwise           = (h:removeCardCols card t)              -- Calls itself recursively to check the next list
    
    -- stackOfCards -> Gets a list of cards that can be moved together (E.g: 5H, 6H, 7H) from a column and returns that list
    stackOfCards :: [Card] -> [Card]
    -- Recursion stopping condition: all cards in the column have been checked
    stackOfCards [] = []
    stackOfCards cols
        -- Returns back the first card if there is only one card in this list
        | null remCards                                          = [firstCard]
        -- Add the card that was checked and check the recursively check the other card
        | not (isKing firstCard) && successor == head remCards   = firstCard:stackOfCards ((head remCards):(tail remCards))
        | otherwise                                              = [firstCard]
        where
            (firstCard:remCards)  = filter (not.null) cols    
            successor             = sCard firstCard
    
    -- kingResToEmptyCol -> Moves a king from reserve to an empty column
    kingResToEmptyCol :: EOBoard -> EOBoard
    -- Recursion stopping condition: all cards in reserves checked
    kingResToEmptyCol (fnds, cols, [])                  = (fnds, cols, [])
    kingResToEmptyCol board@(fnds, cols, (firstCard:remCards))
        -- Moves the King if:
        -- 1) The card is a King
        -- 2) There is an empty column
        | isKing firstCard && length cols < 8           = (fnds, [firstCard]:cols, remCards)
        -- Return the card previously checked and check the other reserves recursively
        | otherwise                                     = (updateFnds, updateCols, firstCard:updateRes)
        where
            (updateFnds, updateCols, updateRes)         = kingResToEmptyCol (fnds, cols, remCards)

    -- kingColToEmptyCol -> Moves a king from one column to an empty column
    kingColToEmptyCol :: EOBoard -> EOBoard
    -- Recursion stopping condition: all columns checked
    kingColToEmptyCol (fnds, [], res)                   = (fnds, [], res)
    kingColToEmptyCol board@(fnds, cols, res)
        -- Returns the old board if there are no empty columns
        | length cols == 8                                                        = board
        -- Moves the King if:
        -- 1) The card is a King
        -- 2) There is an empty column
        -- 3) The card being moved is not the only card in the column/The card is not already in an empty column
        | isKing card && length cols < 8 && last cardsToMove /= last firstCol     = (fnds, addKing:cardsToMove:remCols, res)
        -- Return the column previously checked and check the other columns recursively
        | otherwise                                                               = (updateFnds, firstCol:updateCols, updateRes)
        where
            -- Ensuring the columns being checked are not empty
            firstCol:remCols                    = filter (not.null) cols
            -- Getting the card to move which is also biggest card in the 'stack'
            card                                = last cardsToMove
            -- Getting the stack of cards to move from the selected column
            cardsToMove                         = stackOfCards firstCol
            -- Add the King into the column
            addKing                             = drop (length cardsToMove) firstCol
            -- Recursively checking the other columns
            (updateFnds, updateCols, updateRes) = kingColToEmptyCol (fnds, remCols, res)

    -- resToCols -> Moves a card from the reserves to columns
    resToCols :: EOBoard -> EOBoard
    -- Recursion stopping condition: all cards in the reserves checked
    resToCols (fnds, cols, [])                          = (fnds, cols, [])
    resToCols board@(fnds, cols, res)
        -- Moves the card if:
        -- 1) The card is one of the column's first card's predecessor
        | elem firstCard predColHeads                   = (fnds, resAddedToCols, filter (/= firstCard) res)
        -- Return the card previously checked and check the other cards in the reserves recursively
        | otherwise                                     = (updateFnds, updateCols, firstCard:updateRes)
        where
            -- Ensuring the columns being checked are not empty
            (firstCard:remCards)                        = filter (not.null) res
            (firstCol:remCols)                          = filter (not.null) cols
            -- Getting the list of column's head's predecessor to check if the card can move
            predColHeads                                = map pCard (map head cols)
            -- Returning the resulting columns:
            -- Adds the card being passed if it is the predecessor of the first card of the column
            -- Else, return the columns as is
            resAddedToCols                              = map (\column ->
                                                            if firstCard == pCard (head column)
                                                                then firstCard:column
                                                                else column) cols
            -- Recursively checking the other cards in the reserves
            (updateFnds, updateCols, updateRes)         = resToCols (fnds, cols, remCards)

    -- colToCols -> Moving one card/multiple cards from one column to another
    colToCols :: EOBoard -> EOBoard
    -- Recursion stopping condition: all columns have been checked
    colToCols (fnds, [], res)           = (fnds, [], res)
    colToCols (fnds, cols, res)
        -- Moves the card/cards to the column if:
        -- 1) The card passed is one of the column's head's predecessor
        -- 2) There is space in the reserves
        | elem card (filter (not.isAce) predColsHeads) && resAvail>0       = (fnds, resultingColumn:updateRemCols, res)
        -- Return the column previously checked and check the other columns
        | otherwise                                                        = (updateFnds, firstCol:updateCols, updateRes) 
        where
            -- Ensuring the columns being checked are not empty
            (firstCol:remCols)                      = filter (not.null) cols
            card                                    = last cardsToMove
            -- Getting the list of column's head's predecessor to check if the card can move
            predColsHeads                           = map pCard (map head (firstCol:remCols))
            resAvail                                = (8 - (length res))
            cardsToMove                             = stackOfCards firstCol
            -- Updating the column by adding the card/cards into the column
            resultingColumn                         = drop (length cardsToMove) (head(firstCol:updateRemCols))
            -- Returning the resulting column:
            -- Adds the cards to the columns if the 'biggest' card of the stack of cards to be moved
            -- is the predecessor of the card being moved to
            -- Else, return the columns as is
            updateRemCols                           = map (\col ->
                                                        if (last cardsToMove) == pCard (head col)
                                                            then cardsToMove++col
                                                            else col) remCols
            -- Recursively checking the other columns
            (updateFnds, updateCols, updateRes)     = colToCols (fnds, remCols, res)
        
    -- toRes -> Moves a card from columns to reserves
    toRes :: EOBoard -> EOBoard
    -- Recursion stopping condition: all the columns have been checked
    toRes (fnds, [], res)                           = (fnds, [], res)
    toRes (fnds, cols, res)
        -- Moves the card to reserves if:
        -- 1) There is space in the reserves
        -- 2) The card is not attached to it's predecessor (e.g: 5H, 6H)
        -- 3) The card is not a King
        | length res < 8 && length(stackOfCards (head cols)) == 1 && not (isKing card)   = (fnds, removeCardCols card cols, card:res)
        -- Return the column previously checked and check the other columns
        | otherwise                                                                      = (updateFnds, firstCol:updateCols, updateRes)
        where
            -- Ensuring that the columns being checked is not empty
            firstCol:remCols                    = filter (not.null) cols
            card                                = head firstCol
            cardColumn                          = filter (\selCol -> elem (head firstCol) selCol) (filter (\n-> (length n)>1) (firstCol:remCols))
            secondCard                          = (head cardColumn)!!1
            -- Recursively checking the other columns
            (updateFnds, updateCols, updateRes) = toRes (fnds, remCols, res)
    
    -- findMoves -> Calls all the move functions and gets all the possible moves
    findMoves :: EOBoard -> [EOBoard]
    -- Removing the resulting boards from the moves that could not be conducted
    findMoves board@(fnds, cols, res) = filter (/= board) ([kingColToEmptyCol (fnds, removeEmptyCols, res)]
        ++[kingResToEmptyCol (fnds, removeEmptyCols, res)]
        ++[colToCols (fnds, removeEmptyCols, res)]
        ++[resToCols (fnds, removeEmptyCols, res)]
        ++[toRes (fnds, removeEmptyCols, res)])
        where
            -- Removing empty columns
            removeEmptyCols = filter (not.null) cols

    -- chooseMove -> Chooses the best move from the list of boards/moves obtained from findMoves
    chooseMove :: EOBoard -> Maybe EOBoard
    -- The strategy to obtain the best move is to get the first move from the list of moves
    -- This works because all the moves it can make have been appended as such where the 
    -- best move it can make is the first move
    chooseMove board
        | null mlis        = Nothing            -- Do nothing if there is no possible moves
        -- Gets the head of the list of moves
        | otherwise        = Just bestMove
        where
            -- toFoundations is used to prevent any errors with 'pred' function due to Ace
            -- This works and removes this error as all the Aces are moved to the foundations
            -- before the moves are made. 
            mlis           = findMoves (toFoundations board)
            bestMove       = head mlis

    -- eOGame -> Conducting the game and giving the final score
    eOGame :: EOBoard -> Int
    eOGame board@(fnds, cols, res)
        -- The game is won when there are no cards in columns and reserves
        | null res && null cols                                               = 52
        -- Getting the score. The equation/algorithm to calculate the score was obtained from EOIO.hs code by Phil D Green
        | isNothing (chooseMove (toFoundations (fnds, removeEmptyCol, res)))  = (52- (length res) - (foldr (+) 0 (map length cols)))
        -- Converts Maybe EOBoards (from chooseMove) to EOBoards
        | otherwise                                                           = eOGame (resMaybe (chooseMove (toFoundations (fnds, removeEmptyCol, res))))
        where
            -- Ensuring that the columns that are being passed are not empty
            removeEmptyCol                                                    = filter (not.null) cols            
    
    resMaybe :: (Maybe a) -> a
    resMaybe (Just x) = x

    -- eOExpt -> Conducting 100 games and getting the the average scores
    eOExpt :: Int -> (Int,Float)
    eOExpt seed                 = (numberOfWins, averageScore)
        where
            -- Playing 100 games
            runGames            = map (eOGame.eODeal) [seed..(seed+99)]
            -- Getting the number of wins
            numberOfWins        = length (filter (== 52) runGames)
            -- Getting the average score of a game
            averageScore        = fromIntegral (sum runGames) / 100