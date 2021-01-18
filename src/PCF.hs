{- PCF.hs
   ======
   Defines syntax and semantics of PCF. -}

{- TODO:

(i) Define Term datatype
    - Pretty print

(ii) Define Type datatype
    - Pretty print

(iii) Define typechecker
    - Use a transformer stack: either + reader

(iv) Define evaluator
    - Use a transformer stack: either + reader

NOTE: Try and use combinators to design these components. Avoid explicit recursion
      where possible.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

{- ================================= Syntax ================================= -}

type Id = String

data Term = TmUnit                -- Unit               {Introduction forms}
          | TmTrue                -- True
          | TmFalse               -- False
          | TmVar  Id             -- Variables
          | TmProd Term Term      -- Products
          | TmFun  Id Type Term   -- Functions
          | TmLet  Term Term      -- Let statements     {Elimination forms}
          | TmIf   Term Term Term -- If statements
          | TmFst  Term           -- First projection
          | TmSnd  Term           -- Second projection
          | TmApp  Term Term      -- Application
          deriving (Show, Eq)

data Type = TyUnit                -- Unit
          | TyBool                -- Booleans
          | TyProd Type Type      -- Products
          | TyFun  Type Type      -- Functions
          deriving (Show, Eq)

type Binding = (Id, Type)         -- e.g. x :: Bool => (x,Bool)
type Context = [Binding]


{- ================================= Syntax ================================= -}

data Error = EVar  Id         -- Variable not in context
           | ELet  Term       -- First term isn't Unit type
           | EIf1  Term       -- First term isn't Bool type
           | EIf2  Term Term  -- Second and third term aren't the same type
           | EProd Term       -- Term isn't product type
           | EFun1 Term Term  -- Second term not valid iput to first term
           | EFun2 Term       -- First term isn't a funtion
           deriving (Show)

-- Typecheck type = Reader + Either monad stack
-- (i) Reader passes around the context
-- (ii) Either passes around informative typecheck errors

type TcType = ReaderT Context (Either Error) Type

tyCheck :: Term -> TcType
tyCheck (TmUnit)           = return TyUnit
tyCheck (TmTrue)           = return TyBool
tyCheck (TmFalse)          = return TyBool
tyCheck (TmVar x)          = do ctx <- ask
                                ty  <- find x
                                return ty
tyCheck (TmProd tm1 tm2)   = do ty1 <- tyCheck tm1
                                ty2 <- tyCheck tm2
                                return $ TyProd ty1 ty2
tyCheck (TmFun x ty1 tm)   = do ty2 <- tyCheck tm
                                return $ TyFun ty1 ty2
tyCheck (TmLet tm1 tm2)    = do ty1 <- tyCheck tm1
                                lift $ if ty1 == TyUnit
                                         then Left $ ELet tm1
                                         else Right ()
                                ty2 <- tyCheck tm2
                                return ty2
tyCheck (TmIf tm1 tm2 tm3) = do ty1 <- tyCheck tm1
                                lift $ if ty1 == TyBool
                                         then Left $ EIf1 tm1
                                         else Right ()
                                ty2 <- tyCheck tm2
                                ty3 <- tyCheck tm3
                                lift $ if ty2 == ty3
                                         then Left $ EIf2 tm2 tm3
                                         else Right ()
                                return ty3
tyCheck (TmFst tm)         = do ty <- tyCheck tm
                                lift $ case ty of
                                         (TyProd _ _) -> Right ()
                                         _            -> Left $ EProd tm
                                return ty
tyCheck (TmSnd tm)         = do ty <- tyCheck tm
                                lift $ case ty of
                                         (TyProd _ _) -> Right ()
                                         _            -> Left $ EProd tm
                                return ty
tyCheck (TmApp tm1 tm2)    = do ty1 <- tyCheck tm1
                                ty2 <- tyCheck tm2
                                lift $ case ty1 of
                                         (TyFun ty11 ty12)
                                           | ty11 == ty2 -> Right ty12
                                           | otherwise   -> Left $ EFun1 tm1 tm2
                                         _                 -> Left $ EFun2 tm1

find :: Id -> TcType
find x = do ctx <- ask
            lift $ case lookup x ctx of
                     Nothing -> Left $ EVar x
                     Just ty -> Right ty


