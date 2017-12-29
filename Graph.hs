module Graph (
    Vertex,
    Weight,
    Edge,
    Path,
    Graph,
    AdjacencyList,
    getShortestPath,
    vertices,
    plus,
    existsEdge,
    edgesFromVertex,
    edgeWeight
) where

import Data.List
import Data.Ord

data Vertex a = Vertex a deriving (Show, Ord, Eq)

data Weight a = Weight a deriving (Show, Ord, Eq)

data Edge v w = 
    Edge { 
        weight :: Weight w, 
        begin :: Vertex v, 
        end :: Vertex v 
    } deriving (Show, Eq)

data Path v w = 
    Path { 
        path :: [Vertex v], 
        distance :: Weight w 
    } deriving (Show, Ord, Eq)

-- Adjacency List Graph Representation
type AdjacencyList v w = [(Vertex v, [Edge v w])]
data Graph v w = 
    Graph { 
        adjacencyList :: AdjacencyList v w
    } deriving (Show, Eq)

getShortestPath :: (Ev v, Ord v, Num w, Ord w) => Graph v w -> Vertex v -> Vertex v -> Maybe (Path v w)
getShortestPath = dijkstra 

-- Dijkstra's algorithm (Anthony's implementation)
dijkstra :: (Eq v, Ord v, Num w, Ord w) => Graph v w -> Vertex v -> Vertex v -> Maybe (Path v w)
dijkstra graph a b = helper (initPaths a graph) [a]
    where helper shortestPaths visited 
            | a /= b && (not (b `elem` (concatMap (\(v, e) -> map end e) (adjacencyList graph)))) = Nothing
            | (last visited) == b = Just (shortestDijkstraPath b shortestPaths)
            | otherwise = helper newShortestPaths newVisited
                where newShortestPaths = map (updatePath graph visited shortestPaths) shortestPaths
                      newVisited = visited ++ [fst $ argmin (distance . snd) (filter (\(v, p) -> (not (v `elem` visited))) newShortestPaths)]

-- Updates a single ShortestPath path. It either uses a newly found shorter
-- path or leaves the previously found shortest path.
updatePath :: (Eq v, Num w, Ord w) => Graph v w -> [Vertex v] -> [(Vertex v, Path v w)] -> (Vertex v, Path v w) -> (Vertex v, Path v w)
updatePath graph visited shortestPaths (v, p) = if (not (v `elem` visited)) && (existsEdge v lastV adjList) && ((distance lastVPath) `plus` currentWeight < (distance p)) 
                                                then (v, (Path ((path lastVPath) ++ [v]) ((distance lastVPath) `plus` currentWeight))) 
                                                else (v, p)
    where lastV = last visited
          adjList = adjacencyList graph
          currentWeight = edgeWeight v lastV adjList
          lastVPath = shortestDijkstraPath lastV shortestPaths

-- "Infinity" used for dijkstra's initial distance to vertex and potential
-- unreachable vertices.
infinity :: (Num w) => Weight w
infinity = Weight (fromIntegral 9223372036854775807)

-- Returns all vertices of a given graph.
vertices :: Graph v w -> [Vertex v]
vertices graph = map fst $ adjacencyList graph

-- Adds weights together.
plus :: (Num a) => Weight a -> Weight a -> Weight a
(Weight first) `plus` (Weight second) = Weight (first + second)

-- Returns the accumulated shortest path to the end vertex, calculated by the
-- current call to the dijkstra algorithm.
shortestDijkstraPath :: (Eq v) => Vertex v -> [(Vertex v, Path v w)] -> Path v w
shortestDijkstraPath v shortestPaths = snd $ head $ filter (\(vertex, _) -> vertex == v) shortestPaths

-- Returns the weight of the edge (u, v), using a graphs adjacency list.
-- TODO fix function parameter ordering (как му беше думата на английски?)
edgeWeight :: (Eq v, Num w) => Vertex v -> Vertex v -> [(Vertex v, [Edge v w])] -> Weight w
edgeWeight v u adjList = if null getAdjacentVertex then (Weight 0) else weight $ head $ getAdjacentVertex 
    where getAdjacentVertex = filter (\(Edge _ _ vertex) -> vertex == v) $ getAdjacent u adjList

-- Checks whether there is an edge (u, v).
existsEdge :: (Eq v) => Vertex v -> Vertex v -> [(Vertex v, [Edge v w])] -> Bool
existsEdge v u adjList = v `elem` (map end (getAdjacent u adjList))

-- Gets all the edges that have a beggining vertex v.
-- e.g. (v, v0), (v, v1), ... ,(v, vn)
getAdjacent :: (Eq v) => Vertex v -> [(Vertex v, [Edge v w])] -> [Edge v w]
getAdjacent v adjList = snd $ head $ filter (\(vertex, p) -> vertex == v) adjList

edgesFromVertex :: (Eq v) => Vertex v -> Graph v w -> [Edge v w]
edgesFromVertex v graph = getAdjacent v (adjacencyList graph)

-- Gets the initial "Paths" used to store the state of the dijkstra algorithm.
-- The paths contain information that describes the current shortest path to
-- each vertex from the initial vertex. The initial vertex has a trivial path
-- and distance zero to itself.
initPaths :: (Eq v, Num w) => Vertex v -> Graph v w -> [(Vertex v, Path v w)]
initPaths initVert graph = [(initVert, Path [initVert] (Weight 0))] ++ (map (\v -> (v, Path [] infinity)) $ delete initVert $ vertices graph)

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
loadGraph1 = Graph [
    (Vertex 'A', [
        (Edge (Weight 20) (Vertex 'A') (Vertex 'B')),
        (Edge (Weight 80) (Vertex 'A') (Vertex 'D')),
        (Edge (Weight 90) (Vertex 'A') (Vertex 'G'))]),
    (Vertex 'B', [
        (Edge (Weight 10) (Vertex 'B') (Vertex 'F'))]),
    (Vertex 'C', [
        (Edge (Weight 20) (Vertex 'C') (Vertex 'H')),
        (Edge (Weight 50) (Vertex 'C') (Vertex 'F')),
        (Edge (Weight 10) (Vertex 'C') (Vertex 'D'))]),
    (Vertex 'D', [
        (Edge (Weight 10) (Vertex 'D') (Vertex 'C')),
        (Edge (Weight 20) (Vertex 'D') (Vertex 'G'))]),
    (Vertex 'E', [
        (Edge (Weight 30) (Vertex 'E') (Vertex 'G')),
        (Edge (Weight 50) (Vertex 'E') (Vertex 'B'))]),
    (Vertex 'F', [
        (Edge (Weight 10) (Vertex 'F') (Vertex 'C')),
        (Edge (Weight 40) (Vertex 'F') (Vertex 'D'))]),
    (Vertex 'G', [
        (Edge (Weight 20) (Vertex 'G') (Vertex 'A'))]),
    (Vertex 'H', [])
    ]

loadGraph2 = Graph [
    (Vertex 'A', [
        (Edge (Weight 20.0) (Vertex 'A') (Vertex 'B')),
        (Edge (Weight 80.0) (Vertex 'A') (Vertex 'D')),
        (Edge (Weight 90.0) (Vertex 'A') (Vertex 'G'))]),
    (Vertex 'B', [
        (Edge (Weight 10.0) (Vertex 'B') (Vertex 'F'))]),
    (Vertex 'C', [
        (Edge (Weight 20.0) (Vertex 'C') (Vertex 'H')),
        (Edge (Weight 50.0) (Vertex 'C') (Vertex 'F')),
        (Edge (Weight 10.0) (Vertex 'C') (Vertex 'D'))]),
    (Vertex 'D', [
        (Edge (Weight 10.0) (Vertex 'D') (Vertex 'C')),
        (Edge (Weight 20.0) (Vertex 'D') (Vertex 'G'))]),
    (Vertex 'E', [
        (Edge (Weight 30.0) (Vertex 'E') (Vertex 'G')),
        (Edge (Weight 50.0) (Vertex 'E') (Vertex 'B'))]),
    (Vertex 'F', [
        (Edge (Weight 10.0) (Vertex 'F') (Vertex 'C')),
        (Edge (Weight 40.0) (Vertex 'F') (Vertex 'D'))]),
    (Vertex 'G', [
        (Edge (Weight 20.0) (Vertex 'G') (Vertex 'A'))]),
    (Vertex 'H', [])
    ]

-- TODO Used for testing (remove later)
main = do
     -- print $ dijkstra loadGraph1 (Vertex 1) (Vertex 8)
     print $ dijkstra loadGraph2 (Vertex 'A') (Vertex 'H')
