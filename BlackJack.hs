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

display :: Hand -> String 
display Empty = ""
display (Add card hand) = displayCard card ++ ", " ++ display hand

displayCard :: Card -> String
displayCard (Card r s) = (displayRank r)  ++ " of " ++ (show s)

displayRank :: Rank -> String
displayRank (Numeric n)  = show n
displayRank n = show n

--A2 
value :: Hand -> Integer
value h | aceValue 11 h > 21 = aceValue 1 h 
        | otherwise = aceValue 11 h

aceValue:: Integer -> Hand -> Integer
aceValue v Empty = 0
aceValue v (Add (Card r _) hand)  | r == Ace    = v + aceValue v hand
                                  | otherwise   = valueRank r + aceValue v hand 

valueRank :: Rank -> Integer 
valueRank (Numeric n) = n
valueRank _           = 10

--A3 

gameOver :: Hand -> Bool
gameOver hand = value hand > 21


--A4 
-- Hand 1 Guest , Hand 2 Bank . 
winner :: Hand -> Hand -> Player
winner guest bank  | gameOver bank && not(gameOver guest) = Guest  
                  | value guest > value bank && not(gameOver guest) = Guest
                  | otherwise = Bank 

--B1 

(<+) :: Hand -> Hand -> Hand
(<+) h1 h2 = reverseAddHands h1 (reverseAddHands h2 Empty)


reverseAddHands :: Hand -> Hand -> Hand
reverseAddHands Empty hand = hand
reverseAddHands (Add c1 h1) hand = reverseAddHands h1 (Add c1 hand)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Property
prop_size_onTopOf h1 h2 = size h1 + size h2 === size (h1 <+ h2)

--B2
allRanks = [(Numeric 2), (Numeric 3), (Numeric 4), (Numeric 5), (Numeric 6), (Numeric 7) , (Numeric 8), (Numeric 9), (Numeric 10), Jack, Queen, King, Ace]
allSuits = [Hearts, Spades, Diamonds, Clubs]

--card = [(\(r,s) -> Card r s) | r <- allRanks, s <- allSuits ]
rankSuits = [(r,s) | r <- allRanks, s <- allSuits]

rankSuitToHand :: [(Rank, Suit)] -> Hand
rankSuitToHand [] = Empty
rankSuitToHand ((r,s):rss) = Add (Card r s) (rankSuitToHand rss)

fullDeck :: Hand
fullDeck = rankSuitToHand rankSuits
--card =  (Add Card(r, s ) card | r <- allRanks, s <- allSuits) 

--B3
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand)) 

--b4
playBank :: Hand -> Hand
playBank deck = bankDraw deck Empty
    where     
        bankDraw :: Hand -> Hand -> Hand
        bankDraw deck bank  | value bank > 16 = bank
                            | otherwise       =  bankDraw (fst (draw deck bank)) (snd (draw deck bank))

--b5


getCard :: Integer -> Hand -> (Hand, Card)
getCard _ Empty = error "getCard: Wowie"
getCard n hand  | n < 0 = error "getCard: negative >:("
                | n >= (size hand) = error "getCard: Out of bounds"
                | otherwise = getRemoveAt n hand Empty (Card (Numeric 2) Hearts)
                    where 
                        getRemoveAt :: Integer -> Hand -> Hand -> Card -> (Hand, Card)
                        getRemoveAt _ Empty out card = ((reverseAddHands out Empty) , card)
                        getRemoveAt 0 (Add c h) out _ = getRemoveAt (-1) h out c
                        getRemoveAt i (Add c h) out card = getRemoveAt (i-1) h (Add c out) card 

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = shuffle (randomR (0, (size deck) - 1) g) deck Empty
    where 
        shuffle :: (Integer, StdGen) -> Hand -> Hand -> Hand
        shuffle _ Empty hand = hand
        shuffle (i, g') h1 h2 = shuffle (randomR (0, ((size h1) - 2)) g') 
            (fst (getCard i h1)) 
            (Add (snd (getCard i h1)) h2)


belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
        

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Property
prop_size_shuffle g h = size h === size (shuffleDeck g h)


-- B6

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