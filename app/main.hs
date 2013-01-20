import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseNothing)
import Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseNothing) makeApplication
