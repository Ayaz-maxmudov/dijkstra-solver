module Main where

import qualified Dijkstra
import           System.IO (getContents)

-- | Пропускаем пустые строки и комментарии (начинающиеся с '#')
isCommentOrEmpty :: String -> Bool
isCommentOrEmpty s =
  case dropWhile (==' ') s of
    ('#':_) -> True
    ""      -> True
    _       -> False

-- | Парсинг строки "u v w" в кортеж (Vertex, Vertex, Weight)
parseEdge :: String -> (Dijkstra.Vertex, Dijkstra.Vertex, Dijkstra.Weight)
parseEdge line =
  case words line of
    [u, v, w] -> (read u, read v, read w)
    _         -> error $ "Неверный формат ребра: " ++ show line

-- | Парсинг строки "start goal" в пары вершин
parseStartGoal :: String -> (Dijkstra.Vertex, Dijkstra.Vertex)
parseStartGoal line =
  case words line of
    [s, g] -> (read s, read g)
    _      -> error $ "Неверный формат start/goal: " ++ show line

main :: IO ()
main = do
  contents <- getContents
  let
    allLines   = lines contents
    validLines = filter (not . isCommentOrEmpty) allLines

  case validLines of
    (nLine : rest) ->
      let n     = read nLine :: Int
          (edgeLs, afterEdges) = splitAt n rest
          edges = map parseEdge edgeLs
      in case afterEdges of
           (sgLine : _) ->
             let (startV, goalV) = parseStartGoal sgLine
                 graph           = Dijkstra.buildGraph edges
             in case Dijkstra.dijkstra graph startV goalV of
                  Nothing -> putStrLn "Путь не найден"
                  Just (path, dist) -> do
                    putStrLn $ "Длина пути: " ++ show dist
                    putStrLn $ "Сам путь: " ++ unwords (map show path)
           _ -> putStrLn "Ошибка: не указаны вершины start и goal."
    _ -> putStrLn "Ошибка формата: ожидается число ребер в первой строке."
