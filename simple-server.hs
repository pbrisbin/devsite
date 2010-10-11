--
-- pbrisbin 2010
--
import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Loaded" >> withServer (run 3000)
