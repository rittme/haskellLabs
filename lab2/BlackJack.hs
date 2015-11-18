{-
  Functional Programming -- Lab Assignment 2
  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}

module BlackJack where
import Cards
import Wrapper

import Test.QuickCheck

{-
  size hand2  = size (Add (Card (Numeric 2) Hearts)(Add(Card Jack Spades) Empty))
              = 1 + size (Add(Card Jack Spades) Empty))
              = 1 + 1 + size(Empty) = 1 + 1 + 0 = 2
-}


-- empty function : returns an empty hand
empty :: Hand
empty = Empty


-- test empty properties
prop_empty :: Bool
prop_empty = size empty == 0


-- Calculates the value of a Rank
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank Ace         = 11 -- Ace = 11, by default
valueRank _           = 10


-- Calculates the value of a Card
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r


-- Calculates the number of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                                = 0
numberOfAces (Add (Card r _) hand) | r == Ace     = 1 + numberOfAces hand
                                   | otherwise    = numberOfAces hand


-- calculates the value of the hand according to the game rules
value :: Hand -> Integer
value hand | valueHand hand > 21 = valueHand hand - (10 * numberOfAces hand)
           | otherwise           = valueHand hand
-- Evaluates the value of a hand before the Aces value mutation
  where valueHand :: Hand -> Integer
        valueHand Empty           = 0
        valueHand (Add card hand) = valueCard card + valueHand hand


-- given an hand, returns whether the player is burst or not
gameOver :: Hand -> Bool
gameOver hand = value hand > 21


-- who between the guest and the bank has won ?
winner :: Hand -> Hand -> Player
winner handGuest handBank | gameOver handGuest                 = Bank
                          | gameOver handBank                  = Guest
                          | value handGuest > value handBank   = Guest
                          | otherwise                          = Bank

-- <+ : given 2 hands, puts the first on top of the second one
(<+) :: Hand -> Hand -> Hand
hand1 <+ Empty            = hand1
Empty <+ hand2            = hand2
(Add card hand1) <+ hand2 = (Add card (hand1 <+ hand2) )

-- associative property of (<+) operator
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- The size of the combined hand equals the sum of the 2 given hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- fullDeck : returns a full deck of cards
fullDeck :: Hand
fullDeck =  (handBySuit Hearts)   <+ 
            (handBySuit Spades)   <+ 
            (handBySuit Diamonds) <+ 
            (handBySuit Clubs)


-- handBySuit: returns a hand consisting of all the cards in the given suit
handBySuit :: Suit -> Hand
handBySuit s =  (Add (Card Ace s) 
                (Add (Card King s) 
                (Add (Card Queen s) 
                (Add (Card Jack s)
                (Add (Card (Numeric 10) s)
                (Add (Card (Numeric 9) s)
                (Add (Card (Numeric 8) s)
                (Add (Card (Numeric 7) s)
                (Add (Card (Numeric 6) s)
                (Add (Card (Numeric 5) s)
                (Add (Card (Numeric 4) s)
                (Add (Card (Numeric 3) s)
                (Add (Card (Numeric 2) s)
                Empty)))))))))))))

-- Draw : Given a deck and a hand, 
--        draw one card from the deck and put on the hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand             = error "draw: The deck is empty."
draw (Add card hand1) hand2 = (hand1 , (Add card hand2))


