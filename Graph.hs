module Graph where
import Data.List
import Data.Ord

data Vertex = Vertex Integer deriving (Show, Ord, Eq)
data Weight = Weight {w :: Integer} deriving (Show, Ord, Eq)
data Edge = Edge { weight :: Weight, begin :: Vertex, end :: Vertex } deriving (Show, Ord, Eq)
data Path = Path { path :: [Vertex], distance :: Integer } deriving (Show, Ord, Eq)

-- Adjacency List Graph Representation
data Graph = Graph { adjacencyList :: [(Vertex, [Edge])] } deriving (Show, Ord, Eq)

-- "Infinity" used for dijkstra's initial distance to vertex and potential
-- unreachable vertices.
infinity = 9223372036854775807

-- Returns all vertices of a given graph.
vertices :: Graph -> [Vertex]
vertices graph = map fst $ adjacencyList graph

-- Dijkstra's algorithm (Anthony's implementation)
dijkstra :: Graph -> Vertex -> Vertex -> Maybe Path
dijkstra graph a b = helper (initPaths a graph) [a]
    where helper shortestPaths visited 
            | a /= b && (not (b `elem` (concatMap (\(v, e) -> map end e) (adjacencyList graph)))) = Nothing
            | (last visited) == b = Just (getShortestPath b shortestPaths)
            | otherwise = helper newShortestPaths newVisited
                where newShortestPaths = map (updatePath graph visited shortestPaths) shortestPaths
                      newVisited = visited ++ [fst $ argmin (distance . snd) (filter (\(v, p) -> (not (v `elem` visited))) newShortestPaths)]

-- Updates a single ShortestPath path. It either uses a newly found shorter
-- path or leaves the previously found shortest path.
updatePath :: Graph -> [Vertex] -> [(Vertex, Path)] -> (Vertex, Path) -> (Vertex, Path)
updatePath graph visited shortestPaths (v, p) = if (not (v `elem` visited)) && (isAdjacent v lastV adjList) && ((distance lastVPath) + currentWeight < (distance p)) 
                                                then (v, (Path ((path lastVPath) ++ [v]) ((distance lastVPath) + currentWeight))) 
                                                else (v, p)
    where lastV = last visited
          adjList = adjacencyList graph
          currentWeight = getWeightOfEdge v lastV adjList
          lastVPath = getShortestPath lastV shortestPaths

-- Returns the accumulated shortest path to the end vertex, calculated by the
-- current call to the dijkstra algorithm.
getShortestPath :: Vertex -> [(Vertex, Path)] -> Path
getShortestPath v shortestPaths = snd $ head $ filter (\(vertex, _) -> vertex == v) shortestPaths

-- Returns the weight of the edge (u, v), using a graphs adjacency list.
-- TODO fix function parameter ordering (как му беше думата на английски?)
getWeightOfEdge :: Vertex -> Vertex -> [(Vertex, [Edge])] -> Integer
getWeightOfEdge v u adjList = if null getAdjacentVertex then 0 else w $ weight $ head $ getAdjacentVertex 
    where getAdjacentVertex = filter (\(Edge _ _ vertex) -> vertex == v) $ getAdjacent u adjList

-- Checks whether there is an edge (u, v).
isAdjacent :: Vertex -> Vertex -> [(Vertex, [Edge])] -> Bool
isAdjacent v u adjList = v `elem` (map end (getAdjacent u adjList))

-- Gets all the edges that have a beggining vertex v.
-- e.g. (v, v0), (v, v1), ... ,(v, vn)
getAdjacent :: Vertex -> [(Vertex, [Edge])] -> [Edge]
getAdjacent v adjList = snd $ head $ filter (\(vertex, p) -> vertex == v) adjList

-- Gets the initial "Paths" used to store the state of the dijkstra algorithm.
-- The paths contain information that describes the current shortest path to
-- each vertex from the initial vertex. The initial vertex has a trivial path
-- and distance zero to itself.
initPaths :: Vertex -> Graph -> [(Vertex, Path)]
initPaths initVert graph = [(initVert, Path [initVert] 0)] ++ (map (\v -> (v, Path [] infinity)) $ delete initVert $ vertices graph)

-- Returns the element of the list that minimizes the function f(x), where x is in the list.
argmin :: (Ord a, Ord b) => (a -> b) -> [a] -> a
argmin f = minimumBy (comparing f)

-- -- Dijkstra algorithm (Dimitar's implementation)
-- -- TODO Check unreachable path
-- dijkstra :: Graph -> Vertex -> Vertex -> Path
-- dijkstra graph a b = helper [a] (getInitDistances a graph) (getInitPrevVertices graph)
--   where helper visited distances prevVertices
--           | (last visited) == b = getResultPath a b prevVertices distances
--           | otherwise =
--               let adjacentList = map (\(v, edgeList) -> (v, map end edgeList)) $ adjacencyList graph
--                   verticesList = map fst adjacentList
--                   current = last visited
--                   currentDistance = lookup current distances
--                   adjacentNeighbors = lookup current adjacentList
--                   edgeWeight v1 v2 = getEdgeWeight graph v1 v2
--                   withShorterDistances = filter (\v -> lookup v distances >= --TODO Make readable
--                                                        (currentDistance + (edgeWeight current v)))
--                                                 adjacentNeighbors
--                   updatedDistances = map (\(v, d) -> if v `elem` withShorterDistances
--                                                      then (v, currentDistance + (edgeWeight current v))
--                                                      else (v, d))
--                                          distances
--                   updatedPrevVertices = map (\(v, p) -> if v `elem` withShorterDistances
--                                                         then (v, current)
--                                                         else (v, p))
--                                             prevVertices
--                   distancesUnvisited = filter (\(v, d) -> not $ v `elem` visited) distances
--                   unvisitedWithSmallestDistance = fst $ argmin snd $ distancesUnvisited
--                   updatedVisited = visited ++ unvisitedWithSmallestDistance
--               in helper updatedVisited updatedDistances updatedPrevVertices
-- 
-- 
-- -- Returns а "vertex -> distance to zeroth vertex" map that is used by the dijkstra
-- -- pathfinding algorithm. The initial distance values are all infinity except for
-- -- the zeroVertex which has a distance to itself equal to 0.
-- -- Example: [(zeroVertex, 0), (v1, infinity), ... ,(vn, infinity)]
-- getInitDistances zeroVertex graph = map (\v -> if v == zeroVertex
--                                                then (v, 0)
--                                                else (v, infinity)) $
--                                         vertices graph
-- 
-- -- Returns an initial "vertex -> previous vertex" map with that is used by the dijkstra
-- -- algorithm. All the vertices point to themselves i.e. if the map has a key that is
-- -- the vertex A, then that key points to the vertex value A.
-- getInitPrevVertices graph = map (\v -> (v, v)) $ vertices graph
-- 
-- -- Returns a graph path from the beginVertex to the endVertex by using a
-- -- (vertex -> previous vertex) map, generated by the dijkstra algorithm.
-- getResultPath beginVertex endVertex prevVertices distances = helper [] 0 endVertex
--   where helper path sumDistance currentVertex
--           | currentVertex == beginVertex = Path { path=path, distance=sumDistance }
--           | helper ([lookup current prevVertices] ++ path)  -- TODO само дистанция на последното
--                    ((lookup current distances) + sumDistance)
--                    (lookup current prevVertices)
-- 
-- -- Returns the weight of the edge (v1, v2) in a given graph.
-- -- TODO FIX w $
-- getEdgeWeight graph v1 v2 = w $ weight $ find (\e -> (begin e) == v1 && (end e) == v2) 
--                                               (edges graph)
-- 
-- edges :: Graph -> [Edge]
-- edges graph = nub $ concatMap snd $ adjacencyList graph


-- This is the graph from this video https://www.youtube.com/watch?v=8Ls1RqHCOPw&t=17s
loadGraph = Graph [
    (Vertex 1, [
        (Edge (Weight 20) (Vertex 1) (Vertex 2)),
        (Edge (Weight 80) (Vertex 1) (Vertex 4)),
        (Edge (Weight 90) (Vertex 1) (Vertex 7))]),
    (Vertex 2, [
        (Edge (Weight 10) (Vertex 2) (Vertex 6))]),
    (Vertex 3, [
        (Edge (Weight 20) (Vertex 3) (Vertex 8)),
        (Edge (Weight 50) (Vertex 3) (Vertex 6)),
        (Edge (Weight 10) (Vertex 3) (Vertex 4))]),
    (Vertex 4, [
        (Edge (Weight 10) (Vertex 4) (Vertex 3)),
        (Edge (Weight 20) (Vertex 4) (Vertex 7))]),
    (Vertex 5, [
        (Edge (Weight 30) (Vertex 5) (Vertex 7)),
        (Edge (Weight 50) (Vertex 5) (Vertex 2))]),
    (Vertex 6, [
        (Edge (Weight 10) (Vertex 6) (Vertex 3)),
        (Edge (Weight 40) (Vertex 6) (Vertex 4))]),
    (Vertex 7, [
        (Edge (Weight 20) (Vertex 7) (Vertex 1))]),
    (Vertex 8, [])
    ]

-- TODO Used for testing (remove later)
-- main = do
--     print $ initPaths (Vertex 1) loadGraph
--     print $ dijkstra loadGraph (Vertex 1) (Vertex 8)
