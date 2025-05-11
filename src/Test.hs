{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Test where




-- class Embedding k a r | k r -> a where
--   embed :: a -> r

-- geom :: Embedding "geom" a r => a -> r
-- geom = embed @"geom"

-- scale :: Embedding "scale" a r => a -> r
-- scale = embed @"scale"



-- newtype Media m a = Media {runMedia :: m a}
--   deriving (Functor, Applicative, Monad)



-- data Pos s = Pos{ time :: s, xyPos :: (s, s)}
--   deriving Show

-- instance Num s => Embedding "geom" (s,s) (Pos s) where
--   embed p = Pos 0 p


-- instance (Embedding "scale" s x, Embedding "geom" x (Media m a))
--   =>  Embedding "scale" s (Media m a) where
--   embed = embed @"geom" . embed @"scale"


-- instance Num s => Embedding "scale" s (s,s) where
--   embed s = (s,0)


-- class Monad m => ChangeReturn c m cr | c m -> cr
-- instance Monad m => ChangeReturn c (Media m) ()


-- class MkChange c m | m -> c where
--   -- | Turns a synchronizer into a media
--   change :: c -> m

-- type ChangeMonad c m cr = (Monad m, MkChange c (m cr), ChangeReturn c m cr)



-- instance (MkChange c (m a), ChangeReturn c (Media m) a) => MkChange c (Media m a) where
--   change = Media . change



-- instance MkChange (Pos Float) (IO ()) where
--   change = print

-- blub :: (Monad m, MkChange (Pos Float) (m ())) =>  Media m  ()
-- blub = do
--   scale 3
--   geom (2,2)
--   geom (4,4)
--   return ()


-- instance (Embedding "geom" s c, MkChange c (Media m a)) => Embedding "geom" s (Media m a) where
--   embed = change . geom




