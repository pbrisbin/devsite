import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication

-- I don't need the stupid Extra
parseNothing :: Monad m => a -> b -> m ()
parseNothing _ _ = return ()
