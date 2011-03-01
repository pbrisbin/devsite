import Controller
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = putStrLn "Loaded" >> withServer (run 3000)
