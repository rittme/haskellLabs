{-
  Functional Programming -- Lab Assignment 2
  Group 38 - Bernardo Rittmeyer, Modou Cissé
-}

module BlackJack where
import Cards
import Wrapper

--import Test.QuickCheck
import System.Random

{-
  size hand2  = size (Add (Card (Numeric 2) Hearts)(Add(Card Jack Spades) Empty))
              = 1 + size (Add(Card Jack Spades) Empty))
              = 1 + 1 + size(Empty) = 1 + 1 + 0 = 2
-}


-- empty function : returns an empty hand
empty :: Hand
empty = Empty


-- Calculates the value of a Rank
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank Ace         = 11 -- Ace = 11, by default
valueRank _           = 10


-- Calculates the value of a Card
valueCard :: Card -> Integer
valueCard = valueRank . rank


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
(Add card hand1) <+ hand2 = Add card $ hand1 <+ hand2


-- associative property of (<+) operator
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3


-- The size of the combined hand equals the sum of the 2 given hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2


-- fullDeck : returns a full deck of cards
fullDeck :: Hand
fullDeck = fullSuit Hearts   <+
           fullSuit Spades   <+
           fullSuit Diamonds <+
           fullSuit Clubs


-- fullSuit: returns a hand consisting of all the cards in the given suit
fullSuit :: Suit -> Hand
fullSuit s =  Add (Card Ace s)   $
              Add (Card King s)  $
              Add (Card Queen s) $
              Add (Card Jack s)  $ getSuitNumerics [2..10] s


-- getSuitNumerics: get all the numeric cards from a suit
getSuitNumerics :: [Integer] -> Suit -> Hand
getSuitNumerics [] _ = Empty
getSuitNumerics (n:ns) s = Add (Card (Numeric n) s) $ getSuitNumerics ns s


-- Draw : Given a deck and a hand,
--        draw one card from the deck and put on the hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _                = error "draw: The deck is empty."
draw (Add card hand1) hand2 = (hand1 , Add card hand2)


-- playBank : Given a deck, plays for the bank according to the rules
--            and returns the bank’s final hand
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand  | value bankHand < 16 = playBank' deck' bankHand'
                         | otherwise           = bankHand
        where (deck' , bankHand') = draw deck bankHand


playBank :: Hand -> Hand
playBank deck = playBank' deck Empty


-- shuffle : shuffles a deck of cards
shuffle :: StdGen -> Hand -> Hand
shuffle gen deck = shuffle' gen deck Empty

shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' gen Empty hand = hand
shuffle' gen deck hand  = shuffle' gen' deck' (Add card' hand)
              where (deck', card', gen') = getRandomCard gen deck


-- getRandomCard : gets a random card from a deck
getRandomCard :: StdGen -> Hand -> (Hand, Card, StdGen)
getRandomCard gen deck = (hand, card, gen')
             where (gen', n) = getRandomNumber gen (size deck-1)
                   (hand, card) = pickCard n deck


-- pickCard : pick a card at the n position on the hand
pickCard :: Integer -> Hand -> (Hand, Card)
pickCard _ Empty                    = error "Can't pick card of empty."
pickCard n (Add c h) | n < 0        = error "Can't pick negative card."
                     | n > size h+0 = error "Card out of bounds."
                     | n == 0       = (h, c)
                     | otherwise    = (Add c hand, card)
                     where (hand, card) = pickCard (n - 1) h


-- generates a random integer between 0 and max
getRandomNumber :: StdGen -> Integer -> (StdGen, Integer)
getRandomNumber gen max = (gen', n)
                where (n, gen') = randomR (0, max) gen


-- shuffle properties
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty       = False
c `belongsTo` (Add card h)  = c == card || c `belongsTo` h


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h


-- prop_size_shuffle : Size not changed by shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffle g hand)


{-
 *** main : run the game **
-}
implementation = Interface
  { iEmpty = empty
  , iFullDeck = fullDeck
  , iValue = value
  , iGameOver = gameOver
  , iWinner = winner
  , iDraw = draw
  , iPlayBank = playBank
  , iShuffle = shuffle
  }
main :: IO()
main = runGame implementation
