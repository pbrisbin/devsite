import Controller
import Network.Wai.Handler.FastCGI

main :: IO ()
main = withServer run
