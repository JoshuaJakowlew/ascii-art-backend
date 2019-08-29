module Config where

import           Web.Spock
import           Web.Spock.Config

data Session = EmptySession
data AppState = EmptyState

type Api = SpockM () Session AppState ()
type ApiAction a = SpockAction () Session AppState a
