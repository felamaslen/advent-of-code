import Text.Printf

inputFile = "./input.txt"

data Player = Player { position :: Int
                     , score :: Int }

data Game = Game { player1 :: Player 
                 , player2 :: Player
                 , turn :: Int }

printGameTile :: Player -> Player -> Int -> String
printGameTile (Player pos0 score0) (Player pos1 score1) i
  | pos0 == i && pos1 == i = "[(1->"++show score0++"),(2->"++show score1++")] "
  | pos0 == i = "(1->"++show score0++") "
  | pos1 == i = "(2->"++show score1++") "
  | otherwise = "_ "

instance Show Game where
  show (Game p1 p2 turn) = "Player "++show (turn+1)++" to move: "++
    concatMap (printGameTile p1 p2) [1..10]

data Die = Die { faceValue :: Int
               , roll :: Int -> Int
               , numRolls :: Int }

instance Show Die where
  show (Die faceValue _ numRolls) = "value="++show faceValue++", numRolls="++show numRolls

initGame :: [String] -> Game
initGame [l0, l1] = Game (Player pos0 0) (Player pos1 0) 0
  where pos0 = read ((last.words) l0) :: Int
        pos1 = read ((last.words) l1) :: Int

rollDie :: Die -> Die
rollDie (Die faceValue roll numRolls) = Die (roll faceValue) roll (numRolls+1)

playPlayerTurn :: Player -> Int -> Player
playPlayerTurn (Player position score) moves = Player nextPosition nextScore
  where nextPosition = mod (position + moves - 1) 10 + 1
        nextScore = score + nextPosition

playGameTurn :: Game -> Int -> Game
playGameTurn game steps = Game nextPlayer1 nextPlayer2 nextTurn
  where Game player1 player2 turn = game
        (nextPlayer1, nextPlayer2) =
          if turn == 0
            then (playPlayerTurn player1 steps, player2)
            else (player1, playPlayerTurn player2 steps)
        nextTurn = mod (turn+1) 2

winningPlayer :: Int -> Game -> Int
winningPlayer finishingScore (Game player1 player2 _)
  | score player1 >= finishingScore = 1
  | score player2 >= finishingScore = 2
  | otherwise = 0

playGameStepSimple :: Die -> Game -> (Die, Game)
playGameStepSimple die game = (rolled2, nextGame)
  where rolled0 = rollDie die
        rolled1 = rollDie rolled0
        rolled2 = rollDie rolled1
        sumRolls = (sum . map faceValue) [rolled0, rolled1, rolled2]
        nextGame = playGameTurn game sumRolls

playGameSimple :: Die -> Game -> (Die, Game)
playGameSimple die game
  | winner == 0 = playGameSimple nextDie nextGame
  | otherwise = (die, game)
  where Game player1 player2 turn = game
        winner = winningPlayer 1000 game
        (nextDie, nextGame) = playGameStepSimple die game

playGameDeterministic = playGameSimple (Die 0 (\ p -> mod p 100 + 1) 0)

-- Map of 3-throw sum to number of permutations of the given sum
numDiracThrows = zip ([3,4,5,6,7,8,9]::[Int]) ([1,3,6,7,6,3,1]::[Int])

playGameDirac :: Game -> Int
playGameDirac game = playGameLoop game 1 0
  where
    playGameLoop :: Game -> Int -> Int -> Int
    playGameLoop prevGame numOfThisBranch prev = next
      where
        playWithThrows :: (Int, Int) -> Int
        playWithThrows (sumThrows, numBranches)
          | winner == 0 = playGameLoop nextGame numOfThisGame 0
          | winner == 1 = numOfThisGame
          | otherwise = 0
          where
            numOfThisGame = numBranches * numOfThisBranch
            nextGame = playGameTurn prevGame sumThrows
            winner = winningPlayer 21 nextGame
        next = foldr ((+) . playWithThrows) prev numDiracThrows                  

task1 :: Game -> IO ()
task1 game = do
  let (die, finishedGame) = playGameDeterministic game
  let result = numRolls die * min ((score.player1) finishedGame) ((score.player2) finishedGame)
  printf "Task 1: score=%d\n" result

task2 :: Game -> IO ()
task2 game = do
  let result = playGameDirac game
  printf "Task 2: result=%d\n" result

main = do
  content <- readFile inputFile
  let game = (initGame.lines) content
  task1 game
  task2 game
