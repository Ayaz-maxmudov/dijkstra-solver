-- src/Dijkstra.hs

module Dijkstra
  ( Vertex
  , Weight
  , Graph
  , buildGraph
  , dijkstra
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set       as Set
import qualified Data.List      as List
import           Data.Maybe      (fromMaybe)

-- | Тип вершины (Int)
type Vertex = Int

-- | Вес ребра (Int)
type Weight = Int

-- | Представление графа:
--   Map от вершины к списку (сосед, вес)
type Graph = Map.Map Vertex [(Vertex, Weight)]

-- «Бесконечно большое» расстояние
inf :: Int
inf = maxBound `div` 2

-- | Вставка неориентированного ребра (u, v, w) в Graph
insertEdge :: (Vertex, Vertex, Weight) -> Graph -> Graph
insertEdge (u, v, w) gr =
  let
    neighboursU = Map.findWithDefault [] u gr
    gr1         = Map.insert u ((v, w) : neighboursU) gr
    neighboursV = Map.findWithDefault [] v gr1
    gr2         = Map.insert v ((u, w) : neighboursV) gr1
  in gr2

-- | Построить Graph из списка троек (u, v, w)
buildGraph :: [(Vertex, Vertex, Weight)] -> Graph
buildGraph = foldr insertEdge Map.empty

-- | Инициализация начальных расстояний: всем вершинам inf, кроме start → 0
initialDistances :: Set.Set Vertex -> Vertex -> Map.Map Vertex Weight
initialDistances verts start =
  let base = Map.fromSet (const inf) verts
  in Map.insert start 0 base

-- | Выбрать из unvisited вершину с минимальным dist
pickMin :: Map.Map Vertex Weight -> Set.Set Vertex -> Maybe Vertex
pickMin distMap unvisited =
  let
    vs  = Set.toList unvisited
    cmp u v = compare (Map.findWithDefault inf u distMap)
                      (Map.findWithDefault inf v distMap)
  in case vs of
       [] -> Nothing
       xs -> Just (List.minimumBy cmp xs)

-- | Основная рекурсивная часть алгоритма Дейкстры
dijkstra' :: Graph
          -> Set.Set Vertex
          -> Map.Map Vertex Weight
          -> Map.Map Vertex Vertex
          -> Vertex
          -> (Map.Map Vertex Weight, Map.Map Vertex Vertex)
dijkstra' graph unvisited distMap prevMap goal =
  case pickMin distMap unvisited of
    Nothing -> (distMap, prevMap)
    Just u  ->
      let du = Map.findWithDefault inf u distMap
      in if du == inf || u == goal
         then (distMap, prevMap)  -- либо все оставшиеся недостижимы, либо дошли до цели
         else
           let
             neighbours = Map.findWithDefault [] u graph

             -- Для каждого соседа (v,w) проверяем relaks: alt = du + w
             relax (dAcc, pAcc) (v, w) =
               let alt = du + w
                   dv  = Map.findWithDefault inf v dAcc
               in if alt < dv
                    then ( Map.insert v alt dAcc
                         , Map.insert v u pAcc )
                    else (dAcc, pAcc)

             (distNext, prevNext) = foldl relax (distMap, prevMap) neighbours
             unvisNext           = Set.delete u unvisited
           in dijkstra' graph unvisNext distNext prevNext goal

-- | Восстановление пути по prevMap (от goal к start), возвращает список вершин
buildPath :: Map.Map Vertex Vertex -> Vertex -> Vertex -> [Vertex] -> [Vertex]
buildPath prevMap start current acc
  | current == start = start : acc
  | otherwise =
      case Map.lookup current prevMap of
        Nothing   -> current : acc  -- если предшественника нет (значит это start)
        Just prev -> buildPath prevMap start prev (current : acc)

-- | Внешняя обёртка: вызывает dijkstra', проверяет результат и строит путь
dijkstra :: Graph -> Vertex -> Vertex -> Maybe ([Vertex], Weight)
dijkstra graph start goal =
  let
    verts         = Map.keysSet graph
    dist0         = initialDistances verts start
    prev0         = Map.empty
    (dFinal, pFinal) = dijkstra' graph verts dist0 prev0 goal
    distGoal      = Map.lookup goal dFinal
  in case distGoal of
       Nothing   -> Nothing
       Just dVal
         | dVal >= inf -> Nothing
         | otherwise   ->
             let path = buildPath pFinal start goal []
             in Just (reverse path, dVal)
