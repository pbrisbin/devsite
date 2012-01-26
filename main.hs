import Prelude
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (getApplication)

main :: IO ()
main = defaultMain (fromArgs parseNothing) getApplication

-- I don't need the stupid Extra
parseNothing :: Monad m => a -> b -> m ()
parseNothing _ _ = return ()
