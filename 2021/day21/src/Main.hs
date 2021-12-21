import Text.Printf

inputFile = "./input.txt"

data Player = Player { position :: Int
                     , score :: Int }

data Game = Game { player1 :: Player 
                 , player2 :: Player
                 , turn :: Int }

data Die = Die { faceValue :: Int
               , roll :: Int -> Int
               , numRolls :: Int }

printGameTile :: Player -> Player -> Int -> String
printGameTile (Player pos0 score0) (Player pos1 score1) i
  | pos0 == i && pos1 == i = "[(1->"++show score0++"),(2->"++show score1++")] "
  | pos0 == i = "(1->"++show score0++") "
  | pos1 == i = "(2->"++show score1++") "
  | otherwise = "_ "

instance Show Game where
  show (Game p1 p2 turn) = "Player "++show (turn+1)++" to move: "++
    concatMap (printGameTile p1 p2) [1..10]

initGame :: [String] -> Game
initGame [l0, l1] = Game (Player pos0 0) (Player pos1 0) 0
  where pos0 = read ((last.words) l0) :: Int
        pos1 = read ((last.words) l1) :: Int

rollDie :: Die -> Die
rollDie (Die faceValue roll numRolls) = Die (roll faceValue) roll (numRolls+1)

playTurn :: Player -> Int -> Player
playTurn (Player position score) moves = Player nextPosition nextScore
  where nextPosition = mod (position + moves - 1) 10 + 1
        nextScore = score + nextPosition

playGameStep :: Die -> Game -> (Die, Game)
playGameStep die game = (rolled2, Game nextPlayer1 nextPlayer2 nextTurn)
  where Game player1 player2 turn = game
        rolled0 = rollDie die
        rolled1 = rollDie rolled0
        rolled2 = rollDie rolled1
        sumRolls = (sum . map faceValue) [rolled0, rolled1, rolled2]
        (nextPlayer1, nextPlayer2) =
          if turn == 0
            then (playTurn player1 sumRolls, player2)
            else (player1, playTurn player2 sumRolls)
        nextTurn = mod (turn+1) 2

playGame :: Die -> Game -> (Die, Game)
playGame die game
  | maxScore >= 1000 = (die, game)
  | otherwise = playGame nextDie nextGame
  where Game player1 player2 turn = game
        maxScore = max (score player1) (score player2)
        (nextDie, nextGame) = playGameStep die game

deterministicDie :: Die
deterministicDie = Die 0 (\ p -> mod p 100 + 1) 0

playGameDeterministic = playGame deterministicDie

task1 :: Game -> IO ()
task1 game = do
  let (die, finishedGame) = playGameDeterministic game
  let result = numRolls die * min ((score.player1) finishedGame) ((score.player2) finishedGame)
  printf "Task 1: score=%d\n" result

main = do
  content <- readFile inputFile
  let game = (initGame.lines) content
  task1 game
