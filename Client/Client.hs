module Main where
--
-- Author: Komissarov Andrey
--
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)


__split_s :: String -> String -> [String]
__split_s y x = do
	--print ( y ++"_"++ x )
	if [(head x)] == "\n"
	then if (length x) > 1
		then [y]++(__split_s "" (tail x))
		else [""]
	else if (length x) > 1
		then __split_s (y++[(head x)]) (tail x)
		else [y++x]

split_s :: String -> [String]
split_s x = __split_s "" x

getsock :: IO Socket
getsock = do 			
	addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "8080")
	addr <- return (head addrinfos)
	--sock <- socket (addrFamily addr) Stream defaultProtocol
	--sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
	sock <- socket (addrFamily addr) Stream defaultProtocol
	setSocketOption sock KeepAlive 1
	connect sock (addrAddress (head addrinfos))
	--h <- socketToHandle sock ReadWriteMode
	--hSetBuffering h NoBuffering
	return (sock)

_send :: Socket -> String -> String -> IO Int
_send sock name msg = do
	print_line msg
	send sock ("HELO\n"++"FROM "++name++"\nTO broadcast\nDATA\n"++msg++"\n.\n")

get_line :: IO String
get_line = getLine

print_line :: String -> IO ()
print_line st = putStrLn st

read_msg :: String -> Socket -> IO Int
read_msg st sock = do
	line <- get_line
	print_line (("@"++line++"@"))
	case line of 
		"_send" -> _send sock "MyName" st
		"_reset" -> read_msg "" sock
		_ -> read_msg (st++line++"\n") sock

_loop :: Socket -> IO () -- writer
_loop sock = do
	st <- read_msg "" sock
	--_send sock "MyName" st
	_loop sock


looop :: Bool-> String-> Handle ->IO() -- reader
looop flag st hdl = do
    --line <- liftM init (hGetLine hdl)
    line <- hGetLine hdl
    print_line ("@@"++line)
    case line of
      "DATA" -> looop True "" hdl -- else, continue looping.
      "." ->  print_line ("---TruTuTu----\n"++st) >> looop False "" hdl -- else, continue looping.
      "QUIT" -> print_line "Bye!" -- If an exception is caught, send a message and break the loop
      _ -> do case flag of 
              True -> looop flag (st++line++"\n") hdl -- else, continue looping.
              _ -> looop flag "" hdl

main :: IO ()
main = do
	sock <- getsock
	--_send sock "MyName" "smth\n"
	hdl <- socketToHandle sock ReadWriteMode
	looop False "" hdl
	--thrd <- forkIO $ handle (\(SomeException _) -> return ()) $ 
	--_loop sock
	--sClose sock
