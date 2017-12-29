module Graph where
import Data.List
import Data.Ord

data Vertex = Vertex Integer deriving (Show, Ord, Eq)
data Weight = Weight {w :: Integer} deriving (Show, Ord, Eq)
data Edge = Edge { weight :: Weight, begin :: Vertex, end :: Vertex } deriving (Show, Ord, Eq)
data Path = Path { path :: [Vertex], distance :: Integer } deriving (Show, Ord, Eq)

-- Adjacency List Graph Representation
data Graph = Graph { adjacencyList :: [(Vertex, [Edge])] } deriving (Show, Ord, Eq)

-- int max bound
infinity = 9223372036854775807

-- all vertices of a graph
vertices :: Graph -> [Vertex]
vertices graph = map fst $ adjacencyList graph

-- dijkstra algorithm => returns Maybe Path between two vertices
dijkstra :: Graph -> Vertex -> Vertex -> Maybe Path
dijkstra graph a b = helper (initPaths a graph) [a]
    where helper shortestPaths visited 
            | a /= b && (not (b `elem` (concatMap (\(v, e) -> map end e) (adjacencyList graph)))) = Nothing
            | (last visited) == b = Just (getShortestPath b shortestPaths)
            | otherwise = helper newShortestPaths newVisited
                where newShortestPaths = map (updatePath graph visited shortestPaths) shortestPaths
                      newVisited = visited ++ [fst $ argmin (distance . snd) (filter (\(v, p) -> (not (v `elem` visited))) newShortestPaths)]

-- updates a single ShortestPath path 
updatePath :: Graph -> [Vertex] -> [(Vertex, Path)] -> (Vertex, Path) -> (Vertex, Path)
updatePath graph visited shortestPaths (v, p) = if (not (v `elem` visited)) && (isAdjacent v lastV adjList) && ((distance lastVPath) + currentWeight < (distance p)) 
                                                then (v, (Path ((path lastVPath) ++ [v]) ((distance lastVPath) + currentWeight))) 
                                                else (v, p)
    where lastV = last visited
          adjList = adjacencyList graph
          currentWeight = getWeightOfEdge v lastV adjList
          lastVPath = getShortestPath lastV shortestPaths

-- gets path of v from the list of shortest paths in the dijkstra algorithm
getShortestPath :: Vertex -> [(Vertex, Path)] -> Path
getShortestPath v shortestPaths = snd $ head $ filter (\(vertex, _) -> vertex == v) shortestPaths

-- returns weight of edge connecting u to v in the ajdacency list
getWeightOfEdge :: Vertex -> Vertex -> [(Vertex, [Edge])] -> Integer
getWeightOfEdge v u adjList = if null getAdjacentVertex then 0 else w $ weight $ head $ getAdjacentVertex 
    where getAdjacentVertex = filter (\(Edge _ _ vertex) -> vertex == v) $ getAdjacent u adjList

-- if v is adjacent to u in the adjacency list
isAdjacent :: Vertex -> Vertex -> [(Vertex, [Edge])] -> Bool
isAdjacent v u adjList = v `elem` (map end (getAdjacent u adjList))

-- get all edges adjacent to v in the adjacency list
getAdjacent :: Vertex -> [(Vertex, [Edge])] -> [Edge]
getAdjacent v adjList = snd $ head $ filter (\(vertex, p) -> vertex == v) adjList

-- initializes all paths in the shortestPaths list in the dijkstra algorithm
initPaths :: Vertex -> Graph -> [(Vertex, Path)]
initPaths initVert graph = [(initVert, Path [initVert] 0)] ++ (map (\v -> (v, Path [] infinity)) $ delete initVert $ vertices graph)

argmin :: (Ord a, Ord b) => (a -> b) -> [a] -> a
argmin f = minimumBy (comparing f)


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

main = do
    print $ initPaths (Vertex 1) loadGraph
    -- print $ dijkstra loadGraph (Vertex 1) (Vertex 8)