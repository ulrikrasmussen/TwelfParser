module Language.Twelf.IntSyn where

import           Control.Monad.IO.Class
import           Control.Monad.Catch
import qualified Data.Map as M

import qualified Language.Twelf.AST as AST
import           Language.Twelf.AST hiding (Mode(..), Decl, DDefn)
import           Language.Twelf.Reconstruct
import           Language.Twelf.TwelfServer

type Var = String
type TypeName = String
type ConstName = String

data Mode = Input | Output | Unrestricted
  deriving (Show)
data Meta = Meta
    {
      metaImplicit :: Bool,
      metaMode :: Mode
    }
 deriving (Show)

defMeta :: Meta
defMeta = Meta False Unrestricted

type Binding = (Maybe Var, Meta, A)
type Arg = M

isImplicit :: Binding -> Bool
isImplicit (_, m, _) = metaImplicit m

defBinding :: Var -> A -> Binding
defBinding v a = (if v == "_" then Nothing else Just v, defMeta, a)

newtype K = K [Binding]         -- {v1:a1}{v2:a2} ... {vn:an} type
newtype A = A ([Binding], P)    -- {v1:a1}{v2:a2} ... {vn:an} P
newtype P = P (TypeName, [Arg]) -- a m1 m2 .. mn
newtype M = M ([Binding], R)    -- [v1:a1][v2:a2]...[vn:an] R
newtype R = R (Root, [Arg])     -- x m1 m2 .. mn
data Root = RVar Var A
          | RConst ConstName

showBinding :: Bool -> Binding -> String
showBinding typeLevel (mv, m, a) =
    (case mv of
       Nothing -> if typeLevel then p (show a) ++ " -> " else delim (show a)
       Just v -> delim (v ++ " : " ++ show a)) ++ " "
  where p s = "(" ++ s ++ ")"
        c False s = "{" ++ s ++ "}"
        c True s = "{{" ++ s ++ "}}"
        b False s = "[" ++ s ++ "]"
        b True s = "[[" ++ s ++ "]]"
        delim = (if typeLevel then c else b) (metaImplicit m)

showArg :: M -> String
showArg m = " (" ++ show m ++ ")"

instance Show K where
    show (K bs) = concatMap (showBinding True) bs ++ "type"

instance Show A where
    show (A (bs, p)) = concatMap (showBinding True) bs ++ show p

instance Show P where
    show (P (a, as)) = a ++ concatMap showArg as

instance Show M where
    show (M (bs, r)) = concatMap (showBinding False) bs ++ show r

instance Show R where
    show (R (r, as)) = show r ++ concatMap showArg as

instance Show Root where
    show (RVar v a) = v ++ " : " ++ show a
    show (RConst c) = c

dpar :: Bool -> String -> String -> (String -> String)
dpar False l r s = l ++ s ++ r
dpar True l r s = l ++ l ++ s ++ r ++ r

data Decl =
    DFamily TypeName K
    | DConst ConstName A
    | DDefn ConstName A M
  deriving (Show)

type Ctx = M.Map String A

toKind :: Ctx -> Term -> K
toKind _ TType = K []
toKind ctx (TArrow t1 t2) = let K as' = toKind ctx t2
                            in K $ defBinding "_" (toType ctx t1):as'
toKind ctx (TPi v t1 t2) =
    let t1' = toType ctx t1
        K as' = toKind (M.insert v t1' ctx) t2
    in K $ defBinding v t1':as'
toKind _ t = error $ "Invalid kind: " ++ show t

toType :: Ctx -> Term -> A
toType ctx (TArrow t1 t2) = let A (as', p) = toType ctx t2
                            in A (defBinding "_" (toType ctx t1):as', p)
toType ctx (TPi v t1 t2) =
    let t1' = toType ctx t1
        A (as', p) = toType (M.insert v t1' ctx) t2
    in A (defBinding v t1':as', p)
toType ctx t = A ([], toAtomType ctx t)

toAtomType :: Ctx -> Term -> P
toAtomType ctx (TConst c) = if M.member c ctx
                            then error $ "Unexpected variable '" ++ c
                                     ++ "'. Expected constant."
                            else P (c, [])
toAtomType ctx (TApp t1 t2) = let P (v, as) = toAtomType ctx t1
                              in P (v, as ++ [toTerm ctx t2])
toAtomType _ t = error $ "Invalid atomic type: " ++ show t

toTerm :: Ctx -> Term -> M
toTerm ctx (TLam v t1 t2) =
    let t1' = toType ctx t1
        M (as, r) = toTerm (M.insert v t1' ctx) t2
    in M $ (defBinding v t1':as, r)
toTerm ctx t = M ([], toAtomTerm ctx t)

toAtomTerm :: Ctx -> Term -> R
toAtomTerm ctx (TConst v) = R (maybe (RConst v) (RVar v) $ M.lookup v ctx, [])
toAtomTerm ctx (TVar v) = case M.lookup v ctx of
                            Nothing -> error $ "Unbound variable: " ++ v
                            Just t -> R (RVar v t, [])
toAtomTerm ctx (TApp t1 t2) =
    let R (r, as) = toAtomTerm ctx t1
    in R (r, as ++ [toTerm ctx t2])
toAtomTerm _ t = error $ "Invalid atomic term: " ++ show t


termArity :: Term -> Int
termArity (TArrow _ t2) = 1 + termArity t2
termArity (TPi _ _ t2) = 1 + termArity t2
termArity (TLam _ _ t2) = 1 + termArity t2
termArity (TAscribe t1 _) = termArity t1
termArity _ = 0

inferImplicitK :: Term -> K -> K
inferImplicitK t (K as) = K $ map (f True) imps ++ map (f False) rest
    where
      (imps, rest) = splitAt (length as - termArity t) as
      f b (v, m, a) = (v, m {metaImplicit = b}, a)

inferImplicitA :: Term -> A -> A
inferImplicitA t (A (as, p)) = A (map (f True) imps ++ map (f False) rest, p)
    where
      (imps, rest) = splitAt (length as - termArity t) as
      f b (v, m, a) = (v, m {metaImplicit = b}, a)

extract :: (MonadIO m, MonadMask m) =>
           [AST.Decl]
        -> TwelfMonadT m (M.Map String Decl)
extract ds = do
  ds' <- reconstruct ds
  return $ foldl extract' M.empty (zip ds ds')
      where
        extract' sig (DDecl n t, DDecl _ t')
            | TType <- conc t' =
                       M.insert n
                            (DFamily n $ inferImplicitK t $ toKind M.empty t')
                            sig
            | otherwise =
                       M.insert n
                             (DConst n $ inferImplicitA t $ toType M.empty t')
                             sig
        extract' sig (AST.DDefn _ n a _, AST.DDefn _ _ a' m') =
          M.insert n
            (DDefn n (inferImplicitA a $ toType M.empty a') (toTerm M.empty m'))
            sig
        extract' sig (DMode a ms, _) =
            M.update (\(DFamily n (K as)) -> Just $ DFamily n (K $ zipWith aux as ms)) a sig
                where aux (v, met, a') (m, _) = (v, met { metaMode = extractMode m }, a')
        extract' sig _ = sig

        extractMode AST.Input = Input
        extractMode AST.Output = Output
        extractMode AST.Unrestricted = Unrestricted
