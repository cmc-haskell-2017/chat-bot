module Extra where
--
-- Author: Komissarov Andrey
--
import System.IO

__split_s :: String -> String -> [String]
__split_s y x = do
    --print ( y ++"_"++ x )
    if [(head x)] == " "
    then if (length x) > 1
        then [y]++(__split_s "" (tail x))
        else [""]
    else if (length x) > 1
        then if x == "\n"
        	then __split_s y (tail x)
        	else __split_s (y++[(head x)]) (tail x)
        else [y++x]

split_n :: String -> [String]
split_n x = __split_s "" x

parse_msg:: String -> [String] -- парсит сообщение из стремного вида в массив с тучей атрибутов
parse_msg st=[st]

smth :: [Integer] -> Integer -> Integer
smth az x = if length(az)>1
			then smth (tail az) (x+(head az))
			else (x+(head az))

--------------------------------------------------------
--  ЭТО ГЛАВНЫЙ ОБРАБОТЧИК СООБЩЕНИЙ
roadget :: String -> String -> String
roadget from body = do
	show (smth (map (\x -> read x :: Integer) (split_n body)) 0)
--------------------------------------------------------
