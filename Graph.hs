module Graph where
import Data.List
import Data.Ord

data Vertex = Vertex Integer deriving (Show, Ord, Eq)
data Weight = Weight {w :: Integer} deriving (Show, Ord, Eq)
data Edge = Edge { weight :: Weight, begin :: Vertex, end :: Vertex } deriving (Show, Ord, Eq)
data Path = Path { path :: [Vertex], distance :: Integer } deriving (Show, Ord, Eq)

-- Adjacency List Graph Representation
data Graph = Graph { adjacencyList :: [(Vertex, [Edge])] } deriving (Show, Ord, Eq)

infinity = 100000

vertices :: Graph -> [Vertex]
vertices graph = map fst $ adjacencyList graph

edges :: Graph -> [Edge]
edges graph = nub $ concatMap snd $ adjacencyList graph

dijkstra :: Graph -> Vertex -> Vertex -> Path
dijkstra graph a b = helper [a] (initPaths a graph) 0
    where helper visited shortestPaths totalDistance 
            | (last visited) == b = Path visited totalDistance
            | otherwise = helper newVisited newShortestPaths newTotalDistance
                where newShortestPaths = map (updatePath graph visited totalDistance) shortestPaths
                      newVisited = visited ++ [fst $ argmin (distance . snd) newShortestPaths]
                      newTotalDistance = totalDistance + (getWeightOfEdge (last newVisited) (last visited) (adjacencyList graph))

-- updates a single ShortestPath path 
updatePath :: Graph -> [Vertex] -> Integer -> (Vertex, Path) -> (Vertex, Path)
updatePath graph visited totalDistance (v, p) = if (isAdjacent v lastV adjList) && (totalDistance + currentWeight <= (distance p)) 
                                                then (v, (Path (visited ++ [v]) (totalDistance + currentWeight))) 
                                                else (v, p)
    where lastV = last visited
          adjList = adjacencyList graph
          currentWeight = getWeightOfEdge v lastV adjList


-- returns weight of edge connecting u to v
getWeightOfEdge :: Vertex -> Vertex -> [(Vertex, [Edge])] -> Integer
getWeightOfEdge v u adjList = w $ weight $ head $ filter (\(Edge _ _ vertex) -> vertex == v) $ getAdjacent u adjList

-- if v is adjacent to u
isAdjacent :: Vertex -> Vertex -> [(Vertex, [Edge])] -> Bool
isAdjacent v u adjList = v `elem` (map end (getAdjacent u adjList))

-- get all edges adjacent to v
getAdjacent :: Vertex -> [(Vertex, [Edge])] -> [Edge]
getAdjacent v adjList = snd $ head $ filter (\(vertex, p) -> vertex == v) adjList

initPaths :: Vertex -> Graph -> [(Vertex, Path)]
initPaths initVert graph = map (\v -> (v, Path [v] (-1))) $ delete initVert $ vertices graph

argmin :: (Ord a, Ord b) => (a -> b) -> [a] -> a
argmin f = minimumBy (comparing f)

loadGraph = Graph [
    (Vertex 1, [
        (Edge (Weight 3) (Vertex 1) (Vertex 3)), 
        (Edge (Weight 5) (Vertex 1) (Vertex 0))]),
    (Vertex 0, [
        (Edge (Weight 2) (Vertex 0) (Vertex 2))]),
    (Vertex 2, [
        (Edge (Weight 9) (Vertex 2) (Vertex 1))]),
    (Vertex 3, [
        (Edge (Weight 1) (Vertex 3) (Vertex 0))])
    ]



main = do
    -- so far my only working example :(
    print $ dijkstra loadGraph (Vertex 1) (Vertex 0)