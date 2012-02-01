module Helpers.Admin
    ( requireAdmin
    , maybeAdmin
    ) where

import Import
import Control.Monad (unless)

requireAdmin :: Handler ()
requireAdmin = do
    (_, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied "User is not an admin"

maybeAdmin :: Handler Bool
maybeAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Just (_,u) -> userAdmin u
        _          -> False
