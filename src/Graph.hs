module Graph where

data Edge v = Edge v v deriving (Show)
type Path v = [Edge v]
data Graph v = Graph [Edge v] deriving (Show)

data Base i e = Base [(i, e)] deriving (Show)

type City = String
type Country = String
type Continent = String
type CityId = Int

-- | Удаление дубликатов в списке
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs
------------------------------------------------------------------------
-- | Case-based reasoning:
-- | база авиаперелетов
-- | "похожий путь", состоящий из id из таблицы перелетов
-- | id города вылета
-- | id города прилета
cbr :: [Flights] -> [Edge CityId] -> CityId -> CityId -> [Edge CityId]
cbr flights path from to = right flights (left flights path from) to
------------------------------------------------------------------------
-- | Есть ли прямой перелет между двумя точками
-- | база авиаперелетов
-- | id первого города
-- | id второго города
areConnected :: [Flights] -> CityId -> CityId -> Bool
areConnected flights x y = if null (filter (\z -> idfrom z == x && idto z == y) flights) then False else True              
-----------------------------------------------------------------------          
-- | Левая подгонка
-- | база авиаперелетов
-- | путь, состоящий из id из таблицы перелетов
left :: [Flights] -> [Edge CityId] -> CityId -> [Edge CityId]
left flights p c = [(\(Edge f _) -> (Edge c f)) (head (new))] ++ (new)
	where
		new = dropWhile (\(Edge f _) -> not (areConnected flights c f)) p
-----------------------------------------------------------------------
-- | Правая подгонка
-- | база авиаперелетов
-- | путь, состоящий из id из таблицы перелетов
right :: [Flights] -> [Edge Int] -> Int -> [Edge Int]
right flights p c = reverse ([(\(Edge _ t) -> (Edge t c)) (head (new))] ++ (new))
	where
		new = dropWhile (\(Edge _ t) -> not (areConnected flights t c)) (reverse p)	
-----------------------------------------------------------------------
-- | Вычисляет схожесть векторов
similarity :: (Eq a, Eq a1, Floating a2) => (a2, a2, a2, a2, a1, a) -> (a2, a2, a2, a2, a1, a) -> a2
similarity x y = (scalar x y) / ((magnitude x) * (magnitude y))
	where
		scalar (x1, x2, x3, x4, x5, x6) (y1, y2, y3, y4, y5, y6) = (x1 * y1 + x2 * y2 + x3 * y3 + x4 * y4 + (if x5 == y5 then 1 else 0) + (if x6 == y6 then 1 else 0))
		magnitude v = sqrt (scalar v v)
-----------------------------------------------------------------------
-- | Ищет путь из первого города в другой
path :: City -> City -> IO ()
path s1 s2 = do
	conn <- connect connectInfo
	from <- selectcityname s1 conn
	to <- selectcityname s2 conn
	collection <- selectcollection conn
	cities <- selectcities conn
	flights <- selectflights conn
	
	let f = (head from)
	let t = (head to)
	let c = collection
	let p = cities
	simple <- selectflight (cityid t) (cityid f) conn
	let straight = map (\x -> name (cityById p x)) [idfrom (head simple), idto (head simple)]	
	let bool1 = not (null simple)
	let fromCollection = filter (\x -> fromid x == cityid f && toid x == cityid t) c
	let candidate = head (sortOn (\x -> 1 - similarity (attributes (cityById p (fromid x)) (cityById p (toid x))) (attributes f t)) c)	
	edges <- selectcollfl (collectionid candidate) conn
	let solution = cbr flights (map (\x -> (Edge (idfrom x) (idto x))) (map (\x -> head (flightById flights (flid x))) edges)) (cityid (cityByName p (name f))) (cityid (cityByName p (name t)))
	let bool2 = not (null fromCollection)
	resc <- selectcollfl (collectionid (head fromCollection)) conn
	let colec = removeDuplicates (foldl1 (++) (map (\x -> [idfrom (head x), idto (head x)]) (map (\x -> flightById flights (flid x)) resc)))	
	let bool3 = not (null solution)
	if bool1
		then (print straight)
		else if bool2
			then print colec
			else if bool3
				then print solution
				else putStrLn "Cannot find the solution"
	where
		normalize1 x = (x + 180) / 720
		normalize2 y = (y + 90) / 360
		attributes x y = (normalize1 (latitude x), normalize2 (latitude x), normalize1 (latitude y), normalize2 (latitude y), country x, country y)
		cityById g id = head (filter (\x -> cityid x == id) g)
		cityByName g s = head (filter (\x -> name x == s) g)
		flightById g id = filter (\x -> flightid x == id) g
