{- 
	BlackJack
	
 cabal update
 cabal install gloss
 ghc blackjack_StartGUI.hs
 blackjack_StartGUI.exe 

 -}

import qualified Data.Map
import Data.List
import System.Random
import Text.Printf

data Card = Blank | CA | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | CJ | CQ | CK 
		  |	DA | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | DJ | DQ | DK 
		  |	HA | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 | HJ | HQ | HK 
		  |	SA | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | SJ | SQ | SK 
		  deriving (Show, Eq, Enum)
		  
type Hand = [Card]
type Deck = [Card]

-- Assigns every Card a numeric value, using pattern matching
-- For simplicities sake, we considered aces to take on the value 11
value :: Card -> Int
value CA = 11
value C2 = 2
value C3 = 3
value C4 = 4
value C5 = 5
value C6 = 6
value C7 = 7
value C8 = 8
value C9 = 9
value DA = 11
value D2 = 2
value D3 = 3
value D4 = 4
value D5 = 5
value D6 = 6
value D7 = 7
value D8 = 8
value D9 = 9
value HA = 11
value H2 = 2
value H3 = 3
value H4 = 4
value H5 = 5
value H6 = 6
value H7 = 7
value H8 = 8
value H9 = 9
value SA = 11
value S2 = 2
value S3 = 3
value S4 = 4
value S5 = 5
value S6 = 6
value S7 = 7
value S8 = 8
value S9 = 9
value _ = 10

-- Creates a deck from all possible card options - Never called on its own
initialDeck :: Deck
initialDeck = [CA .. SK]

-- Shuffles a given deck by utilizing random number generation
shuffle :: Deck -> Deck -> IO Deck
shuffle shuffled [] = return shuffled
shuffle shuffled unshuffled = do
	randomIndex 	<- randomRIO (0, length unshuffled - 1)
	let randomCard 	= unshuffled !! randomIndex
	let unshuffledTake = take randomIndex unshuffled
	let unshuffledDrop = drop (randomIndex + 1) unshuffled
  
	shuffle (randomCard:shuffled) (unshuffledTake ++ unshuffledDrop)
	
-- Calls shuffle on initialDeck
shuffleDeck :: IO Deck
shuffleDeck = shuffle [] initialDeck

-- Adds n number of cards to the hand, and removes them from the deck
draw :: Int -> Deck -> (Hand, Deck)
draw n deck = (take n deck, drop n deck)

-- Adds a single card to the hand, and removes it from the deck
hit :: Hand -> Deck -> (Hand, Deck)
hit hand deck =(hand ++ (take 1 deck), drop 1 deck)


-- Calculates the potential total of the hand
handValue :: Hand -> Int
handValue [] 	 = 0
handValue (x:xs) = value x + handValue xs

isBlackjack hand = if length hand == 2 && handValue hand == 21 then True
				   else False
				   
isWinner playerHand aiHand | handValue playerHand > handValue aiHand = True
						   | otherwise = False

-- Draws a new set of cards and passes them to the game.
playRound usingDeck = do
	currentDeck <- usingDeck
	let (playerHand, remDeck) = draw 2 currentDeck
	let (aiHand, remDeck')	  = draw 2 remDeck

	play remDeck' playerHand aiHand

-- Handles all IO, and game outcomes
play usingDeck playerHand aiHand = do
	let messagePlayer =  "Your hand is " ++ (show playerHand) ++ "."
	let messageAI = "\nThe dealer's hand is " ++ (show aiHand)
	putStrLn (messagePlayer ++ messageAI)

	if isBlackjack playerHand == True then do  -- Player got a blackjack -> Autowin
		putStrLn "You got a BlackJack! You win!"
		putStrLn "Play again? (1)Yes (2)No"
		input <- getLine
		if input == "1" then do
			playRound shuffleDeck
		else do
			putStrLn "Thanks for playing!"
	else if  (handValue playerHand) > 21 then do	-- Player busted -> Autoloss
		putStrLn "You busted! That's a loss!"
		putStrLn "Play again? (1)Yes (2)No"
		input <- getLine
		if input == "1" then do
			playRound shuffleDeck
		else do
			putStrLn "Thanks for playing!"
	else do
		putStrLn "What would you like to do? (1)New Game (2)Hit (3)Stay"
		decision <- getLine
		
		if decision == "1" then do	-- If they'd like to start with a fresh game
			playRound shuffleDeck
			
		else if decision == "2" then do -- If the player would like to hit
			let (newHand, remDeck) = hit playerHand usingDeck
			play remDeck newHand aiHand
	

		else if decision == "3" then do   -- Stay logic. If win, "you win", else "you lose" and prompts for a new game
			if (isWinner playerHand aiHand) == True then do
				putStrLn "You win!"
				putStrLn "Play again? (1)Yes (2)No"
				input <- getLine
				if input == "1" then do
					playRound shuffleDeck
				else do
					putStrLn "Thanks for playing!"
			else do
				putStrLn "Sorry you lost!"
				putStrLn "Play again? (1)Yes (2)No"
				input <- getLine
				if input == "1" then do
					playRound shuffleDeck
				else do
					putStrLn "Thanks for playing!"
		else do		-- Invalid input handling
			putStrLn "Please enter a valid decision."
			play usingDeck playerHand aiHand
			
main = playRound shuffleDeck






	
