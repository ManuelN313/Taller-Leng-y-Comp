{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Control.Applicative ( liftA, liftA2 )

type Var = String
type Σ   = Var -> Int
type MaybeInt  = Maybe Int
type MaybeBool = Maybe Bool

-- Ω ≈ (Σ' + Z × Ω + Z → Ω)⊥ 
data Ω = Normal Σ | Abort Σ | Out (Int, Ω) | In (Int -> Ω)

data Expr a where
  -- Expresiones enteras
  Const :: Int -> Expr MaybeInt                                    -- n
  Var   :: Var -> Expr MaybeInt                                    -- v
  Neg   :: Expr MaybeInt -> Expr MaybeInt                          -- -e
  Plus  :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeInt         -- e + e'
  Minus :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeInt         -- e - e'
  Mul   :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeInt         -- e * e'
  Div   :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeInt         -- e / e'
  -- Comparacion entera
  Eq    :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeBool        -- e = e'
  Neq   :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeBool        -- e /= e'
  Lt    :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeBool        -- e < e'
  Gt    :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeBool        -- e > e'
  LtE   :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeBool        -- e <= e'
  GtE   :: Expr MaybeInt -> Expr MaybeInt -> Expr MaybeBool        -- e >= e'
  -- Expresiones booleanas
  CBool :: Bool -> Expr MaybeBool                                  -- b
  Not   :: Expr MaybeBool -> Expr MaybeBool                        -- not b
  And   :: Expr MaybeBool -> Expr MaybeBool -> Expr MaybeBool      -- and b b'
  Or    :: Expr MaybeBool -> Expr MaybeBool -> Expr MaybeBool      -- or b b'
  -- Comandos Básicos
  Skip   :: Expr Ω                                                 -- skip
  Assign :: Var -> Expr MaybeInt -> Expr Ω                         -- v := e
  Seq    :: Expr Ω -> Expr Ω -> Expr Ω                             -- c ; c'
  If     :: Expr MaybeBool -> Expr Ω -> Expr Ω -> Expr Ω           -- if b then c else c'
  Newvar :: Var -> Expr MaybeInt -> Expr Ω -> Expr Ω               -- newvar v := e in e'
  While  :: Expr MaybeBool -> Expr Ω -> Expr Ω                     -- while b do c
  -- Comandos Fallas
  Fail   :: Expr Ω                                                 -- fail
  Catch  :: Expr Ω -> Expr Ω -> Expr Ω                             -- catch c with c'
  -- Comandos IO
  SysOut   :: Expr MaybeInt -> Expr Ω                              -- !e
  SysIn    :: Var -> Expr Ω                                        -- ?v

-- Funciones auxiliares

fix :: (a -> a) -> a
fix f = f (fix f)

update :: Σ -> Var -> Int -> Σ
update σ v n v' = if v == v' then n else σ v'

rest :: Σ -> Var -> (Σ -> Σ)
rest σ v σ' v'= if v == v' then σ v else σ' v'

fBig :: Expr MaybeBool -> Expr Ω -> (Σ -> Ω) -> (Σ -> Ω)
fBig b c f σ =
  case sem b σ of
    Just False -> Normal σ
    Just True  -> f *. sem c σ
    Nothing    -> Abort σ

(⊥⊥) :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
(⊥⊥) = liftA2

(⊥) :: (a -> b) -> (Maybe a -> Maybe b)
(⊥) = liftA

(>>==) :: (Maybe a, Σ) -> (a -> Ω) -> Ω
(>>==) (Nothing, σ) _ = Abort σ
(>>==) (Just n, _)  f = f n

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ)  = f σ
(*.) _ (Abort σ)   = Abort σ
(*.) f (Out (n,ω)) = Out (n, f *. ω)
(*.) f (In g)      = In ((f *.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ)  = Normal $ f σ
(†.) f (Abort σ)   = Abort $ f σ
(†.) f (Out (n,ω)) = Out (n, f †. ω)
(†.) f (In g)      = In ((f †.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ)  = Normal σ
(+.) f (Abort σ)   = f σ
(+.) f (Out (n,ω)) = Out (n, f +. ω)
(+.) f (In g)      = In ((f +.) . g)

-- Semántica de evaluación

class DomSem dom where
  sem :: Expr dom -> Σ -> dom

instance DomSem MaybeInt where
  sem (Const a) _ = Just a
  sem (Var v) σ = Just (σ v)
  sem (Neg e) σ = (negate ⊥) (sem e σ)
  sem (Plus e1 e2) σ = ((+) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Minus e1 e2) σ = ((-) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Mul e1 e2) σ = ((*) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Div e1 e2) σ = if (sem e2 σ) == Just 0
                      then Nothing
                      else (div ⊥⊥) (sem e1 σ) (sem e2 σ)

instance DomSem MaybeBool where
  sem (Eq e1 e2) σ = ((==) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Neq e1 e2) σ = ((/=) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Lt e1 e2) σ = ((<) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Gt e1 e2) σ = ((>) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (LtE e1 e2) σ = ((<=) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (GtE e1 e2) σ = ((>=) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (CBool b) σ = Just b
  sem (Not e) σ = (not ⊥) (sem e σ)
  sem (And e1 e2) σ = ((&&) ⊥⊥) (sem e1 σ) (sem e2 σ)
  sem (Or e1 e2) σ = ((||) ⊥⊥) (sem e1 σ) (sem e2 σ)

instance DomSem Ω where
  sem Skip σ = Normal σ
  sem (Assign v e) σ = (sem e σ, σ) >>== (Normal . update σ v)
  sem (Seq c1 c2) σ = (sem c2) *. (sem c1 σ)
  sem (If b c1 c2) σ = (sem b σ, σ) >>== (\bv -> if bv then sem c1 σ else sem c2 σ)
  sem (Newvar v e c) σ = (sem e σ, σ) >>== (\val ->
    let σ' = update σ v val
        restore = rest σ v
    in restore †. sem c σ')
  sem (While b c) σ = fix (fBig b c) σ
  sem Fail σ = Abort σ
  sem (Catch c1 c2) σ = (sem c2) +. (sem c1 σ)
  sem (SysOut e) σ = (sem e σ, σ) >>== (\val -> Out (val, Normal σ))
  sem (SysIn v) σ = In (Normal . update σ v)
-- Funciones de evaluación de dominio

class Eval dom where
  eval :: Expr dom -> Σ -> IO ()

instance Eval MaybeInt where
  eval e = print . sem e

instance Eval MaybeBool where
  eval e = print . sem e

instance Eval Ω where
  eval e = unrollOmega . sem e
    where unrollOmega :: Ω -> IO ()
          unrollOmega (Normal _)   = return ()
          unrollOmega (Abort _)    = putStrLn "Abort"
          unrollOmega (Out (n, ω)) = print n >> unrollOmega ω
          unrollOmega (In f)       = getLine >>= unrollOmega . f . read

-- Funciones de prueba
main = do
  eval (Neg (Const 5)) (\_ -> 0)
  eval (Plus (Const (-2)) (Const 6)) (\_ -> 0)
  eval (Minus (Const 3) (Const 6)) (\_ -> 0)
  eval (Mul (Const 9) (Const 1)) (\_ -> 0)
  eval (Div (Const 10) (Const (-2))) (\_ -> 0)
  eval (Div (Const 7) (Const 0)) (\_ -> 0)
  eval (Plus (Const 7) (Var "x")) (\_ -> 0)
  eval (Minus (Var "x") (Const 3)) (\v -> if v == "x" then 5 else 0)
  eval (Mul (Var "x") (Var "y")) (\v -> if v == "x" then 2 else if v == "y" then 3 else 0)
  eval (Mul (Var "x") (Div (Const 15) (Var "y"))) (\v -> if v == "x" then 2 else if v == "y" then 5 else 0)
  eval (Eq (Var "x") (Const 5)) (\v -> if v == "x" then 5 else 0)
  eval (Lt (Var "x") (Const 5)) (\v -> if v == "x" then 3 else 0)
  eval (Gt (Var "x") (Const 5)) (\v -> if v == "x" then 3 else 0)
  eval (LtE (Var "x") (Const 5)) (\v -> if v == "x" then 5 else 0)
  eval (GtE (Var "x") (Const 5)) (\v -> if v == "x" then 5 else 0)  
  eval (CBool True) (\_ -> 0)
  eval (Not (CBool False)) (\_ -> 0)
  eval (And (CBool True) (CBool False)) (\_ -> 0)
  eval (Or (CBool True) (CBool False)) (\_ -> 0)

ej1 :: Expr Ω
ej1 = While (Lt (Var "x") (Const 10)) $
            Seq (SysOut $ Var "x")
                (Assign "x" (Plus (Var "x") (Const 1)))

eval_ej1 :: IO ()
eval_ej1 = eval ej1 (\_ -> 0)

ej2 :: Expr Ω
ej2 = While (Lt (Var "y") (Const 10)) $
            Seq (Seq (Seq (SysIn "x")
                          (SysOut $ Var "x")
                     )
                     (SysOut $ Var "y")
                )
                (Assign "y" (Plus (Var "y") (Const 1)))

eval_ej2 :: IO ()
eval_ej2 = eval ej2 (\_ -> 0)

ej3 :: Expr Ω
ej3 = Seq (Seq (SysIn "x")
               (Newvar "x" (Const 10)
                       (SysOut $ Var "x")
               )
          )
          (SysOut $ Var "x")

eval_ej3 :: IO ()
eval_ej3 = eval ej3 (\_ -> 0)