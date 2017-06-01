import Data.List
import Data.List.Split
import Data.Char
import Data.Ord
import System.IO

data Jogador = O | X | V | D deriving (Eq,Ord)
type Tabuleiro = [[Jogador]]

instance Show Jogador where
    show O = "O"
    show X = "X"
    show D = "D"
    show V = " "

data Tree = No Tabuleiro Int [Tree] | Nil -- tab, chance de vitória, [tabuleiros]

instance Show Tree where
    show (Nil) = "Nil"
    show (No b i t) = (">> No: " ++ show b ++ ", peso: " ++ show i ++ show t ++ "\n")



----------- FUNÇÕES AUXILIARES

-- retorna o oponente
turnplayer :: Jogador -> Jogador
turnplayer player = if player == O then X else O

-- retorna de quem é a jogada em determinada célula do tabuleiro
getPlayerInPosition :: Int -> Int -> [[Jogador]] -> Jogador
getPlayerInPosition line column board = ((!!) ( (!!) board (line-1)) (column-1))


-- mostra o tabuleiro atual para a próxima jogada
showBoard :: [[Jogador]] -> IO ()
showBoard t = putStr ("   1 2 3\n1 " ++ show (head t) ++ "\n2 " ++ show (head (tail t)) ++ "\n3 " ++ show (last t) ++ "\n")

-- mostra o tabuleiro caso algum jogador vença a partida
winBoard :: [[Jogador]] -> Jogador -> IO ()
winBoard t player = putStr ("*--------------*\n*---- GGWP ----*\n*--------------*\n\n"++show (head t) ++ "\n" ++ show (head (tail t)) ++ "\n" ++ show (last t) ++ "\n\n" ++ show player ++ " WIN!\n*--------------*\n")

-- mostra o tabuleiro caso o jogo dê empate
drawBoard :: [[Jogador]] -> IO ()
drawBoard t = putStr ("\n DRAW! \n" ++ (show (head t)) ++ "\n" ++ (show (head (tail t))) ++ "\n" ++ (show (last t)) ++ "\n\n")

-- retorna o score de um nó da árvore
getScore :: Tree -> Int
getScore (No _ i _) = i

-- retorna o primeiro elemento de uma dupla
getFirst :: (a, b) -> a
getFirst (a,b) = a



---------- VERIFICAÇÃO 

-- coloca todas as condições de vitória em uma lista
getAllWinCases :: [[Jogador]] -> [[Jogador]]
getAllWinCases t = t++ transpose t ++ [verifyd t] ++ [verifyd' t]

-- funções que pegam as diagonais
verifyd :: [[Jogador]] -> [Jogador]
verifyd t = [(head ((!!) t 0))] ++ [(head (tail ( (!!) t 1)))] ++ [(last ( (!!) t 2))]
verifyd' :: [[Jogador]] -> [Jogador]
verifyd' t = [(last ((!!) t 0))] ++ [(head (tail ( (!!) t 1)))] ++ [(head ( (!!) t 2))]

-- faz a verificação. retorna a letra correspondente ao jogador que venceu, D em caso de empate e V se ainda há alguma jogada possível
verify :: [[Jogador]] -> [[Jogador]] -> Jogador
verify [] b = if ( all (==True) (map (all (/=V)) b )) == True then D else V
verify (x:xs) b = if verify' x == True then (head ((!!) (x:xs) 0)) else verify xs b

verify' :: [Jogador] -> Bool
verify' x = if ((!!) x 0) == ((!!) x 1) && ((!!) x 1) == ((!!) x 2) && ((!!) x 1) /= V then True else False



---------- MOVIMENTOS 

-- inicio do movimento (verifica a linha)
movement :: [[Jogador]] -> Jogador -> Int -> Int -> [[Jogador]]
movement board player line column
    | line == 1 = mov1 board player column
    | line == 2 = mov2 board player column 
    | line == 3 = mov3 board player column 

-- cada função representa uma linha
mov1 :: [[Jogador]] -> Jogador -> Int -> [[Jogador]]
mov1 board player column = [(m' ((!!) board 0) player column)] ++ [((!!) board 1)] ++ [( (!!) board 2)]
mov2 board player column = [((!!) board 0)] ++ [(m' ((!!) board 1) player column)] ++ [( (!!) board 2)]
mov3 board player column = [((!!) board 0)] ++ [( (!!) board 1)] ++ [(m' ((!!) board 2) player column)]

-- verifica a coluna e retorna a nova linha
m' :: [Jogador] -> Jogador -> Int -> [Jogador]
m' line player column
    | column == 1 = if (head (line) == V) then [player] ++ [(head (tail line))] ++ [(last line)] else line
    | column == 2 = if (head (tail line) == V) then [(head line)] ++ [player] ++ [(last line)] else line
    | column == 3 = if (last (line) == V) then [(head line)] ++ [(head (tail line))] ++ [player] else line
    | otherwise = []



---------- JOGABILIDADE 

-- função do jogo entre dois jogadores
pvp board player = do
    showBoard board
    hFlush stdout
    putStr("\n" ++ show player ++ ": line and column: ")
    hFlush stdout
    l <- getLine
    c <- getLine
    let tmp = movement board player (read l :: Int) (read c :: Int)
    let board = tmp
    let v = verify (getAllWinCases board) (getAllWinCases board)
    if(v == D) then putStr ("DRAW\n") else
        if(v /= V) then winBoard board player else
            pvp board (turnplayer player)

-- função para o jogo contra a IA
pvia board player = do
    putStr ("\n")
    showBoard board
    if player == X then do
        hFlush stdout
        putStr ("\n" ++ show player ++ ": line and column: ")
        hFlush stdout
        l <- getLine
        c <- getLine
        let tmp = movement board player (read l :: Int) (read c :: Int)
        let board = tmp
        let v = verify (getAllWinCases board) (getAllWinCases board)
        if(v == D) then drawBoard board else
            if(v /= V) then winBoard board player else
                pvia board (turnplayer player)
    else do
        let tree = minimax board X
        let boards = getAllBoards tree
        let board = getFirst ( getBestBoard boards)
        let v = verify (getAllWinCases board) (getAllWinCases board)
        if(v == D) then drawBoard board else
            if(v /= V) then winBoard board player else
                pvia board (turnplayer player)




----------- MINIMAX

-- retorna score 1 caso seja um tabuleiro em que a IA vença, -1 caso ela perca e 0 em caso de empate
setScore :: [[Jogador]] -> Int
setScore b 
    | (verify (getAllWinCases b) (getAllWinCases b)) == O = 1
    | (verify (getAllWinCases b) (getAllWinCases b)) == X = (-1)
    | otherwise = 0

-- gera a lista de tabuleiros possíveis a partir de um tabuleiro inicial (v)
generateBoards :: [[Jogador]] -> Jogador -> [[[Jogador]]]
generateBoards board player = generateBoards' 1 1 board player
    where
        generateBoards' 4 _ _ _ = []
        generateBoards' i j board player = if (getPlayerInPosition i j board == V) then ([movement board player i j] ++ (if j == 3 then  generateBoards' (i+1) 1 board player else generateBoards' i (j+1) board player)) else (if j == 3 then  generateBoards' (i+1) 1 board player else generateBoards' i (j+1) board player)

-- transforma um tabuleiro em uma lista de jogadores
concatenateBoard :: [[Jogador]] -> [Jogador]
concatenateBoard [] = []
concatenateBoard (b:bs) = b ++ concatenateBoard(bs)

-- recebe uma árvore e retorna a lista dos tabuleiros possíveis na próxima jogada indicando seu score
getAllBoards :: Tree -> [([[Jogador]], Int)]
getAllBoards (No b i t) = getAllBoards' t
    where
        getAllBoards' [] = []
        getAllBoards' ((No b i t):xs) = [(b, i)] ++ getAllBoards' xs

-- retorna o tabuleiro com o maior score
getBestBoard :: [([[Jogador]], Int)] -> ([[Jogador]],Int)
getBestBoard boards = maximumBy (comparing snd) (getWinner (getBestScores [] boards))
    where
        getBestScores [] ((x,y):xs) = getBestScores [(x,y)] xs
        getBestScores x [] = x
        getBestScores ((a,b):bs) ((x,y):xs) = if (y > b) then getBestScores [(x,y)] xs else if (y == b) then getBestScores (((a,b):bs)++[(x,y)]) xs else getBestScores ((a,b):bs) xs
        getWinner [] = []
        getWinner ((x,y):xs) = if (verify (getAllWinCases x) (getAllWinCases x)) == O then [(x, y+1)] ++ getWinner xs else [(x,y)] ++ getWinner xs

-- caso haja essa condição de derrota, retorna esse tabuleiro, porém com o movimento da IA na posição do movimento de vitória do jogador, bloqueando essa condição de vitória
changeBoard :: [[Jogador]] -> [[Jogador]] -> [[Jogador]]
changeBoard actual [] = actual
changeBoard actual new = chunksOf 3 (changeBoard' (concatenateBoard actual) (concatenateBoard new))
    where
        changeBoard' [] [] = []
        changeBoard' (x:xs) (y:ys) = if x == y then [x] ++ changeBoard' xs ys else [(turnplayer x)] ++ changeBoard' xs ys

-- cria a árvore de possibilidades de jogadas e retorna a raíz de cada subárvore
minimax :: [[Jogador]] -> Jogador -> Tree
minimax board player = if (verify (getAllWinCases board) (getAllWinCases board)) == V then (No board ((getSubscore board player)) (generateTree' board (turnplayer player)))
    else (No board ((setScore board)) [Nil]) 

-- gera a subárvore para cada raiz gerada na função acima
generateTree' :: [[Jogador]] -> Jogador -> [Tree]
generateTree' b p = map (\t -> minimax t p) (generateBoards b p)

-- retorna o score de um determinado tabuleiro para um jogador
getSubscore :: [[Jogador]] -> Jogador -> Int
getSubscore board player = getLocalScore (turnplayer player) (map (getScore) (generateTree' board (turnplayer player)))

-- retorna o maior valor para a IA e o menor para o jogador
getLocalScore :: Jogador -> [Int] -> Int
getLocalScore player scoreboards = if player == O then maximum scoreboards else minimum scoreboards

---------- FUNÇÃO PRINCIPAL
main = do
    putStr("\n1. PvP\n2. PvIA\n>> ")
    play <- getLine
    if(play == "1") then pvp [ [V,V,V], [V,V,V], [V,V,V] ] X else if (play == "2") then (pvia [ [V,V,V], [V,V,V], [V,V,V] ] X) else putStr("Enter a valid option\n")