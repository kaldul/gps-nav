import Graph

type Coordinates = (Double, Double)
type Location = Vertex Coordinates
type Road = Edge Location (Weight RoadWeight)
type Directions = Path

data TrafficSeverity = None | Light | Medium | Heavy | Extreme deriving (Show, Eq, Ord)

-- TODO not really average
averageTraffic :: TrafficSeverity -> TrafficSeverity -> TrafficSeverity
averageTraffic t1 t2 = max t1 t2

trafficCoefficient :: TrafficSeverity -> Double
trafficCoefficient None = 1.0;
trafficCoefficient Light = 1.1;
trafficCoefficient Medium = 2.0;
trafficCoefficient Heavy = 5.0;
trafficCoefficient Extreme = 10.0;

data RoadWeight =
    RoadWeight {
        distance :: Double,
        traffic :: TrafficSeverity,
        toll :: Double
    } deriving (Show, Eq)

instance Addable RoadWeight where
    (RoadWeight di1 tr1 to1) `plus` (RoadWeight di2 tr2 to2) = 
        RoadWeight (di1 + di2) (averageTraffic tr1 tr2) (to1 + to2)

data ShortestBy = Time | Distance | Toll

comparePathsBy :: ShortestBy -> (RoadWeight -> RoadWeight -> Ordering)
comparePathsBy Time =
    (\ (RoadWeight d1 t1 _) (RoadWeight d2 t2 _) ->
        (d1 * (trafficCoefficient t1)) `compare` (d2 * (trafficCoefficient t2)))
comparePathsBy Distance = 
    (\ (RoadWeight d1 _ _) (RoadWeight d2 _ _) -> d1 `compare` d2)
comparePathsBy Toll =
    (\ (RoadWeight _ _ t1) (RoadWeight _ _ t2) -> t1 `compare` t2)

findShortestPathBy :: ShortestBy -> Location -> Location
findShortestPathBy criterion pointA pointB =
    getShortestPathBy (comparePathsBy criterion) pointA pointB