module AppM where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Routing.PushState (PushStateInterface)

newtype AppM t
  = AppM (ReaderT Env Aff t)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance appMFunctor :: Functor AppM

derive newtype instance appMApply :: Apply AppM

derive newtype instance appMApplicative :: Applicative AppM

derive newtype instance appMBind :: Bind AppM

derive newtype instance appMMonad :: Monad AppM

derive newtype instance appMAsk :: MonadAsk Env AppM

derive newtype instance appMReader :: MonadReader Env AppM

derive newtype instance appMMonadEffect :: MonadEffect AppM

derive newtype instance appMMonadAff :: MonadAff AppM

type Env
  = { routingInterface :: PushStateInterface }
