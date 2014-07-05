{- 
	BlackJack
	
 cabal update
 cabal install gloss
 ghc blackjack_StartGUI.hs
 blackjack_StartGUI.exe 

 -}

import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Data.Map
import Data.List
import System.Random

data Game = Game {
			gameSpace :: Data.Map.Map (Int, Int) Card
}

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
-- hit :: Deck -> (Hand, Deck)
-- hit deck = (take 1 deck, drop 1 deck)

-- Calculates the potential total of the hand
handValue :: Hand -> Int
handValue (x:xs) = value x + handValue xs


playRound :: Game -> (Int, Int) -> Game
playRound (Game g) (i, j)
	| draw = newGame			-- Picks 2 new cards for the player and bot
	| otherwise = (Game g)		-- Game doesn't change
	where
		newCards = Data.Map.adjust (\_ -> 
		newGame = Game {
			board = newboard
		)


winner player bot = if bot >= player then true else false

roundOver :: Game -> Bool
roundOver (Game g) = winner game player bot

-- GUI STUFF
		
initialScreen = Data.Map.fromList [((i,j), Blank) | i <- [0..1], j <- [0..1]]

initialGame = Game initialScreen


-- Mouse and play functionality is currently junk. Just for testing purposes until more functionality
play :: Game -> Bool
play (Game g) = if length currentDeck > 52 then True else False

-- Mouse event handler | Left Click functionality
handleMouse :: Gloss.Event -> Game -> Game
handleMouse (Gloss.EventKey (Gloss.MouseButton Gloss.LeftButton) Gloss.Down _ (x, y)) game
	| roundOver game = game
	| otherwise = playRound game (i, j)
	where	i = round $ (y+256) / 256
			j = round & (x+71)


-- Ignore other mouse imputs			
handleMouse _ game = game		

main = do
	blankPicture	<-	Gloss.loadBMP "blank.bmp"
	cAPicture	<-	Gloss.loadBMP "cA.bmp"
	c2Picture	<-	Gloss.loadBMP "c2.bmp"
	c3Picture	<-	Gloss.loadBMP "c3.bmp"
	c4Picture	<-	Gloss.loadBMP "c4.bmp"
	c5Picture	<-	Gloss.loadBMP "c5.bmp"
	c6Picture	<-	Gloss.loadBMP "c6.bmp"
	c7Picture	<-	Gloss.loadBMP "c7.bmp"
	c8Picture	<-	Gloss.loadBMP "c8.bmp"
	c9Picture	<-	Gloss.loadBMP "c9.bmp"
	c10Picture	<-	Gloss.loadBMP "c10.bmp"
	cJPicture	<-	Gloss.loadBMP "c11.bmp"
	cQPicture	<-	Gloss.loadBMP "c12.bmp"
	cKPicture	<-	Gloss.loadBMP "c13.bmp"
	dAPicture	<-	Gloss.loadBMP "dA.bmp"
	d2Picture	<-	Gloss.loadBMP "d2.bmp"
	d3Picture	<-	Gloss.loadBMP "d3.bmp"
	d4Picture	<-	Gloss.loadBMP "d4.bmp"
	d5Picture	<-	Gloss.loadBMP "d5.bmp"
	d6Picture	<-	Gloss.loadBMP "d6.bmp"
	d7Picture	<-	Gloss.loadBMP "d7.bmp"
	d8Picture	<-	Gloss.loadBMP "d8.bmp"
	d9Picture	<-	Gloss.loadBMP "d9.bmp"
	d10Picture	<-	Gloss.loadBMP "d10.bmp"
	dJPicture	<-	Gloss.loadBMP "dJ.bmp"
	dQPicture	<-	Gloss.loadBMP "dQ.bmp"
	dKPicture	<-	Gloss.loadBMP "dK.bmp"
	hAPicture	<-	Gloss.loadBMP "dA.bmp"
	h2Picture	<-	Gloss.loadBMP "h2.bmp"
	h3Picture	<-	Gloss.loadBMP "h3.bmp"
	h4Picture	<-	Gloss.loadBMP "h4.bmp"
	h5Picture	<-	Gloss.loadBMP "h5.bmp"
	h6Picture	<-	Gloss.loadBMP "h6.bmp"
	h7Picture	<-	Gloss.loadBMP "h7.bmp"
	h8Picture	<-	Gloss.loadBMP "h8.bmp"
	h9Picture	<-	Gloss.loadBMP "h9.bmp"
	h10Picture	<-	Gloss.loadBMP "h10.bmp"
	hJPicture	<-	Gloss.loadBMP "hJ.bmp"
	hQPicture	<-	Gloss.loadBMP "hQ.bmp"
	hKPicture	<-	Gloss.loadBMP "hK.bmp"
	sAPicture	<-	Gloss.loadBMP "sA.bmp"
	s2Picture	<-	Gloss.loadBMP "s2.bmp"
	s3Picture	<-	Gloss.loadBMP "s3.bmp"
	s4Picture	<-	Gloss.loadBMP "s4.bmp"
	s5Picture	<-	Gloss.loadBMP "s5.bmp"
	s6Picture	<-	Gloss.loadBMP "s6.bmp"
	s7Picture	<-	Gloss.loadBMP "s7.bmp"
	s8Picture	<-	Gloss.loadBMP "s8.bmp"
	s9Picture	<-	Gloss.loadBMP "s9.bmp"
	s10Picture	<-	Gloss.loadBMP "s`0.bmp"
	sJPicture	<-	Gloss.loadBMP "sJ.bmp"
	sQPicture	<-	Gloss.loadBMP "sQ.bmp"
	sKPicture	<-	Gloss.loadBMP "sK.bmp"
	
	-- maps cards to pictures
	let cardPictures = Data.Map.fromList [(CA, cAPicture), (C2, c2Picture), (C3, c3Picture), (C4, c4Picture), (C5, c5Picture), (C6, c6Picture), (C7, c7Picture), 
										 (C8, c8Picture), (C9, c9Picture), (C10, c10Picture), (CJ, cJPicture), (CQ, cQPicture), (CK, cKPicture),
										 (DA, dAPicture), (D2, d2Picture), (D3, d3Picture), (D4, d4Picture), (D5, d5Picture), (D6, d6Picture), (D7, d7Picture), 
										 (D8, d8Picture), (D9, d9Picture), (D10, d10Picture), (DJ, dJPicture), (DQ, dQPicture), (DK, dKPicture),
										 (HA, hAPicture), (H2, h2Picture), (H3, h3Picture), (H4, h4Picture), (H5, h5Picture), (H6, h6Picture), (H7, h7Picture), 
										 (H8, h8Picture), (H9, h9Picture), (H10, h10Picture), (HJ, hJPicture), (HQ, hQPicture), (HK, hKPicture),
										 (SA, sAPicture), (S2, s2Picture), (S3, s3Picture), (S4, s4Picture), (S5, s5Picture), (S6, s6Picture), (S7, s7Picture), 
										 (S8, s8Picture), (S9, s9Picture), (S10, s10Picture), (SJ, sJPicture), (SQ, sQPicture), (SK, sKPicture), (Blank, blankPicture)]
				
	let displayCards (i,j) card = Gloss.translate ((fromIntegral j :: Float)*288-100) ((fromIntegral i :: Float)*288-160) $ cardPictures Data.Map.! cell
	
	let displayScreen (Game gamespace) = Gloss.Pictures $ Data.Map.elems $ Data.Map.mapWithKey displayCards gamespace
	
	let display game = displayScreen game
	
	
	Gloss.play
			(Gloss.InWindow "Blackjack" (1024,576) (0,0))  --Windowed mode, "title", resolution, starting position
			Gloss.black
			1											  --Updates per second
			initialGame									  --Starting game state
			display
			handleMouse
			(\f g -> g)									  --For animations | Not in use here.




















	
