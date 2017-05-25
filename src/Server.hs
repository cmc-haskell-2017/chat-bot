module Server where
--
-- Author: Komissarov Andrey
--
import Extra

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)

run :: IO ()
run = do
  sock <- socket AF_INET Stream 0   -- создается сокет
  setSocketOption sock ReuseAddr 1  -- настройка парамметров
  bind sock (SockAddrInet 8080 iNADDR_ANY)  -- связка с портом
  listen sock 2                     -- делаем его слушающим
  chan <- newChan
  forkIO $ fix $ \loop -> do
    (_, msg) <- readChan chan
    loop
  mainLoop sock chan 0
 
type Msg = (Int, String)
 
mainLoop :: Socket -> Chan Msg -> Int -> IO ()  -- обработчик соединений
mainLoop sock chan msgNum = do
  conn <- accept sock               -- принимаем входяещее соединение
  forkIO (runConn conn chan msgNum) -- в новую нить его
  mainLoop sock chan $! msgNum + 1  -- запускаем ожидалку следующего соединения

looop :: Int -> String -> String-> Handle ->IO() -- reader читает из сокета и обрабатывает
looop flag from st hdl = do
    line <- hGetLine hdl
    --putStrLn (">>"++line)
    case flag of
      1 -> looop 0 line st hdl -- ожидаем имя отправителя
      2 -> if line == "."     -- читаем тело сообщения
          then do hPutStrLn hdl ("HELO\nFROM\nRobot\nTO\nbroadcast\nDATA\n"++(roadget from st)++"\n.\n")------ ТУТ ЗАПУСКАЕМ ВМЕСТО ++st++ ФУНКЦИЮ ОБРАБОТЧИК СМСКИ ++(roadget st)++
                  putStrLn ("--"++from++": "++st++"--") >> looop 0 from "" hdl -- else, continue looping. 
          else looop flag from (st++line++"\n") hdl -- else, continue looping.
      0 -> case line of -- ожидаем команду
        "FROM" -> looop 1 from st hdl -- ща будет имя отправителя
        "DATA" -> looop 2 from "" hdl -- ща будет тело сообщения
        "QUIT" -> putStrLn "Покеда!" -- валим!
        _ -> looop flag from "" hdl -- ни че не понял, нужно бооольше информации


runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO () -- изначально здесь делились на две нити, читает сокет и пишет в сокет
runConn (sock, _) chan msgNum = do                        -- сейчас мы только читаем, а потом уже пишем
	let broadcast msg = writeChan chan (msgNum, msg)
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering
	handle (\(SomeException _) -> return ()) $ (looop 0 "smb" "" hdl)
	hClose hdl                             -- close the handle
