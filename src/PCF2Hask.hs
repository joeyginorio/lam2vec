{- PCF2Hask.hs
   ===========
   Interpreting PCF terms in the (idealized) category Hask. -}

import PCF
import Prelude hiding (id, (.), curry)

{- ============================== CCC Machinery ============================= -}

infixr 9 .
class CCC cat where
  id    :: cat a a                               -- identity morphism
  (.)   :: cat b c -> cat a b -> cat a c         -- composition of morphisms

  toT   :: cat a ()                              -- terminal morphism
  fromT :: a -> cat () a                         -- "element" morphism

  prod :: cat a b -> cat a c -> cat a (b,c)      -- product morphism
  exl  :: cat (a,b) a                            -- extract left morphism
  exr  :: cat (a,b) b                            -- extract right morphism

  apply   :: cat ((a -> b), a) b                 -- apply morphism
  curry   :: cat (a,b) c -> cat a (b -> c)       -- curry morphism


{- ================================== Hask ================================== -}

instance CCC (->) where
  id  = \x     -> x
  (.) = \g f x -> g $ f x

  toT   = \_ -> ()
  fromT = \x -> const x

  prod = \f g x -> (f x, g x)
  exl  = fst
  exr  = snd

  apply   = \(f,x) -> f x
  curry   = \f -> \x -> \y -> f (x,y)


{- ================================- PCF2Hask =============================== -}

denote :: (CCC cat) => Context -> Term -> (cat Type Type)
denote _ (TmUnit)  = fromT TyUnit . toT
denote _ (TmTrue)  = fromT TyBool . toT
denote _ (TmFalse) = fromT TyBool . toT
