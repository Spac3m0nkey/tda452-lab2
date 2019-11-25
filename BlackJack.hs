module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import Numeric 
import System.Random

-- A0 
hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

hand3 = Add (Card (Numeric 10) Hearts) hand2

sizeSteps :: [Integer]
sizeSteps = [ size hand2,
             size (Add (Card (Numeric 2) Hearts) (Add (Card Queen Spades) Empty)) 
            , 1 + size  (Add (Card Queen Spades) Empty),
            2 + size Empty
            , 2]
-- A1
-- Converts a hand to a printable string
display :: Hand -> String 
display Empty = ""
display (Add card hand) = displayCard card ++ ", " ++ display hand
    where
        displayCard :: Card -> String
        displayCard (Card (Numeric r) s) = (show r)  ++ " of " ++ (show s)
        displayCard (Card r s) = (show r)  ++ " of " ++ (show s)

-- A2 
-- Gets the value of a hand. Also checks if it should use ace value 11 or 1.
value :: Hand -> Integer
value h | aceValue 11 h > 21 = aceValue 1 h 
        | otherwise = aceValue 11 h
    where
        aceValue:: Integer -> Hand -> Integer
        aceValue v Empty = 0
        aceValue v (Add (Card r _) hand)  | r == Ace    = v + aceValue v hand
                                          | otherwise   = valueRank r + aceValue v hand 
        valueRank :: Rank -> Integer 
        valueRank (Numeric n) = n
        valueRank _           = 10

-- A3 
-- Checks if a hand has gone bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21


-- A4 
-- Hand 1 Guest , Hand 2 Bank. Gets the winner.
winner :: Hand -> Hand -> Player
winner guest bank  | gameOver bank && not(gameOver guest) = Guest  
                  | value guest > value bank && not(gameOver guest) = Guest
                  | otherwise = Bank 


--------------- PART B ----------------


-- B1 
-- Adds one hand on top of another.
(<+) :: Hand -> Hand -> Hand
Empty <+ hand = hand
hand <+ Empty = hand
(Add c h) <+ h2 = Add c (h <+ h2)

-- Property for testing that <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Property for testing that the size after <+ is the same as the combined size before. 
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

-- B2
-- Lists containing all ranks and suits
allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
allSuits = [Hearts, Spades, Diamonds, Clubs]

-- List of tuples containing all combinations of ranks and suites
rankSuits = [(r,s) | r <- allRanks, s <- allSuits]

-- Function that returns a full hand of all 52 cards.
fullDeck :: Hand
fullDeck = foldr Add Empty (map (\(r,s) -> Card r s) rankSuits)

-- B3
-- Draws a card from a deck to a hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand)) 

-- B4
-- Defines how the bank plays.
playBank :: Hand -> Hand
playBank deck = bankDraw deck Empty
    where     
        bankDraw :: Hand -> Hand -> Hand
        bankDraw deck bank  | value bank >= 16 = bank
                                                | otherwise             = let (a, b) = draw deck bank in bankDraw a b

-- B5
-- Removes a card from a specific index in the hand and returns the card and the altered hand.
getCard :: Integer -> Hand -> (Hand, Card)
getCard _ Empty = error "getCard: cannot get card from empty hand"
getCard n hand  | n < 0 && n >= (size hand) = error "getCard: index out of bounds"
                              | otherwise = getRemoveAt n hand Empty 
                                where 
                                    getRemoveAt :: Integer -> Hand -> Hand  -> (Hand, Card)
                                    getRemoveAt _ Empty out = error "getRemoveAt: out of bounds"
                                    getRemoveAt 0 (Add c h) out  =  (out <+ h, c)
                                    getRemoveAt i (Add c h) out  = getRemoveAt (i-1) h (Add c out)  

-- Shuffles a inserted hand.
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = shuffle (randomR (0, (size deck) - 1) g) deck Empty
    where 
        shuffle :: (Integer, StdGen) -> Hand -> Hand -> Hand
        shuffle _ Empty hand = hand
        shuffle (i, g') h1 h2 = 
            let (card1, card2) = getCard i h1 
                in shuffle (randomR (0, ((size h1) - 2)) g')   card1   (Add (card2) h2)

-- Checks if a card exists in a hand.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
        

-- Property for checking that a shuffled deck contains the same cards as a non shuffled deck.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Property that checks that the shuffling of the deck does not lose or gain any cards.
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)


-- B6
-- Makes us able to use runGame for IO
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation
