import           Control.Concurrent
import qualified System.Timer as T

main :: IO ()
main = do
  t <- T.create
  T.time t "cat" (act "cat" 5)
  T.time t "dog" (act "dog" 2)
  r <- T.result t
  putStrLn $ "Time Result: " ++ show r

act     :: String -> Int -> IO ()
act s n = do
  putStrLn $ "Starting action " ++ s
  putStrLn   "Oh so sleepy... "
  pause n
  putStrLn $ "(" ++ show n ++ " seconds later)"
  putStrLn $ "Finished action " ++ s

pause :: Int -> IO ()
pause = threadDelay . (10^6 *)
