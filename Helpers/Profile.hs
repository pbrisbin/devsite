module Helpers.Profile
    ( profileForm
    , saveProfile
    ) where

import Import

data ProfileForm = ProfileForm
    { pfUsername :: Maybe Text
    , pfEmail    :: Maybe Text
    }

profileForm :: User -> Form ProfileForm
profileForm u = renderBootstrap $ ProfileForm
    <$> aopt textField   "User name"
        { fsTooltip = Just "comments are attributed to this username"
        } (Just $ userName u)

    <*> aopt emailField  "Email address"
        { fsTooltip = Just "never displayed, only used to find your gravatar"
        } (Just $ userEmail u)

saveProfile :: UserId -> ProfileForm -> Handler ()
saveProfile uid pf = do
    runDB $ update uid 
        [ UserName  =. pfUsername pf
        , UserEmail =. pfEmail    pf
        ]

    redirect ProfileR
