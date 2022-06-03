{-# LANGUAGE FlexibleInstances #-}
module Language.Twelf.AST
       (Ident
       , Term(..)
       , Fixity(..)
       , Assoc(..)
       , Mode(..)
       , CallPattern
       , Order(..)
       , Decl(..)
       , showTerm
       , showSig
       , showSig'
       , conc
       , root)
where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding (Mode, (<>))

type Ident = String
data Term = TArrow Term Term
          | TAscribe Term Term
          | TVar Ident
          | TConst Ident
          | TPi Ident Term Term
          | TLam Ident Term Term
          | TApp Term Term
          | THole
          | TType
  deriving (Eq)

-- Dumb showing
instance Show Term where
    show TType = "type"
    show (TArrow t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2
    show (TAscribe t1 t2) = "(" ++ show t1 ++ " : " ++ show t2 ++ ")"
    show (TVar ident) = ident
    show (TConst ident) = ident
    show (TPi ident t1 t2) = "{" ++ ident ++ " : " ++ show t1 ++ "} " ++ show t2
    show (TLam ident t1 t2) = "[" ++ ident ++ " : " ++ show t1 ++ "]" ++ show t2
    show (TApp t1 t2) = "((" ++ show t1 ++ ") " ++ show t2 ++ ")"
    show THole = "_"

conc :: Term -> Term
conc (TArrow _ t2) = conc t2
conc (TPi _ _ t2) = conc t2
conc t = t

root :: Term -> Term
root (TApp t1 _) = root t1
root t = t

isModedFamDecl :: ModedFamilies -> Term -> Bool
isModedFamDecl mf t | TConst x <- root (conc t) = S.member x mf
isModedFamDecl _ _ = False

data Fixity = FixPrefix | FixInfix Assoc | FixPostfix
  deriving (Eq)

data Assoc = AssocLeft | AssocRight | AssocNone
  deriving (Show, Eq)

data Mode = Input | Output | Unrestricted
  deriving (Show)

type CallPattern = (Ident, [Ident])

data Order = Single Ident
           | Mutual [Ident]
           | Lexicographic [Order]
           | Simultaneous [Order]
  deriving (Show)

data Decl = DDecl String Term
          | DDefn Bool String Term Term
          | DPrefix Integer String
          | DPostfix Integer String
          | DInfix Assoc Integer String
          | DName String String
          | DMode String [(Mode, Ident)]      -- Short mode
          | DFMode [(Mode, Ident, Term)] Term -- Full mode
          | DBlock Ident [(Ident, Term)] [(Ident, Term)]
          | DWorlds [Ident] [CallPattern]
          | DTotal Order [CallPattern]

instance Show Decl where
    show (DDecl ident t1) = ident ++ " : " ++ show t1 ++ "."
    show (DDefn abbr ident t1 t2) =
        (if abbr then "%abbrev " else "") ++ ident ++ " : "
                                          ++ show t1 ++ " = " ++ show t2 ++ "."
    show (DPrefix prec ident) = "%prefix " ++ show prec ++ " " ++ ident ++ "."
    show (DPostfix prec ident) = "%postfix " ++ show prec ++ " " ++ ident ++ "."
    show (DInfix ass prec ident) = "%infix " ++ show ass
                                   ++ " " ++ show prec ++ " " ++ ident ++ "."
    show (DName id1 id2) = "%name " ++ id1 ++ " " ++ id2 ++ "."
    show _ = ""

type Precedence = Int
type ModedFamilies = S.Set String
type OpTable = M.Map String (Precedence, Fixity)
type Ctx = (Precedence, Fixity, Position)
data Position = PosLeft | PosRight
  deriving (Eq)

isUnambiguous :: Precedence -> Fixity -- parent precedence, fixity
              -> Precedence -> Fixity -- child precedence, fixity
              -> Position             -- child position
              -> Bool
isUnambiguous parentPrec parentFix childPrec childFix childPos =
    childPrec > parentPrec
    || childFix == parentFix
       && (case childFix of
             FixInfix AssocLeft  -> childPos == PosLeft
             FixInfix AssocRight -> childPos == PosRight
             FixInfix AssocNone  -> False
             _                   -> True)

isUnambiguous' :: Maybe Ctx  -- ctx: parent prec, fix, child pos
               -> Precedence -- child prec
               -> Fixity     -- child fix
               -> Bool
isUnambiguous' Nothing _ _ = True
isUnambiguous' (Just (parentPrec, parentFix, childPos)) childPrec childFix =
    isUnambiguous parentPrec parentFix childPrec childFix childPos

extractOp :: OpTable -> Term -> Maybe (String, Precedence, Fixity, [Term])
extractOp ot t = do
  r@(_, _, fixity, as) <- extractOp' t
  case (fixity, length as) of
    (FixInfix _, 2) -> return r
    (FixPrefix, 1) -> return r
    (FixPostfix, 1) -> return r
    _ -> Nothing
    where
      extractOp' (TConst ident) = do
        (prec, fixity) <- M.lookup ident ot
        return (ident, prec, fixity, [])
      extractOp' (TApp t1 t2) = do
        (op, prec, fixity, as) <- extractOp' t1
        return (op, prec, fixity, t2:as)
      extractOp' _ = Nothing

type OpData = (String, Precedence, Fixity, [Term])
opName :: OpData -> String
opName (n,_,_,_) = n

opPrec :: OpData -> Precedence
opPrec (_,p,_,_) = p

opFix :: OpData -> Fixity
opFix (_,_,f,_) = f

opArgs :: OpData -> [Term]
opArgs (_,_,_,a) = a

showTerm :: Term -> String
showTerm t = PP.render $ prettyT False Nothing defaultOpTable t

defaultOpTable :: OpTable
defaultOpTable = M.fromList
                 [("->", (-1, FixInfix AssocRight))
                 ,("<-", (-1, FixInfix AssocLeft))
                 ,(":", (-2, FixInfix AssocNone))]

showSig :: [Decl] -> String
showSig ds = render $ prettyS S.empty defaultOpTable ds

showSig' :: ModedFamilies -> OpTable -> [Decl] -> String
showSig' mf ot decls = render $ prettyS mf ot decls

prettyS :: ModedFamilies -> OpTable -> [Decl] -> PP.Doc
prettyS _ _ [] = empty
prettyS mf ot (d:ds) = prettyD mf ot d $+$ prettyS mf' ot' ds
    where
          mf' = case d of
                  DMode ident _ -> S.insert ident mf
                  _ -> mf

          ot' = case d of
              (DPrefix p op) -> M.insert op (fromIntegral p, FixPrefix) ot
              (DPostfix p op) -> M.insert op (fromIntegral p, FixPostfix) ot
              (DInfix assoc p op) -> M.insert op (fromIntegral p, FixInfix assoc) ot
              _ -> ot

prettyD :: ModedFamilies -> OpTable -> Decl -> PP.Doc
prettyD mf ot d = prettyD' d <> char '.'
    where prettyD' (DDecl ident t1) = text ident
                                      <+> colon
                                      <+> prettyT (isModedFamDecl mf t1) Nothing ot t1
          prettyD' (DDefn abbr ident ta tb) =
              (if abbr then text "%abbrev" else empty)
              $+$ (text ident
                   <+> (if ta /= THole then colon <+> prettyT False Nothing ot ta else empty)
                   <+> equals <+> prettyT False Nothing ot tb)
          prettyD' (DPrefix p op) = text "%prefix" <+> integer p <+> text op
          prettyD' (DPostfix p op) = text "%postfix" <+> integer p <+> text op
          prettyD' (DMode fam args) =
              text "%mode" <+> text fam <+> hsep (map prettyModeArg args)
          prettyD' decl = error $ "Not implemented: " ++ show decl

          prettyModeArg (m, ident) = op <> text ident
              where op = text $ case m of
                                  Input -> "+"
                                  Output -> "-"
                                  Unrestricted -> "*"

prettyT :: Bool -> Maybe Ctx -> OpTable -> Term -> PP.Doc
prettyT revArr mctx f t
    | Just opData <- extractOp f t = par (opPrec opData) (opFix opData) $ prettyTOp opData
    | otherwise                    = prettyT'
    where
      par childPrec childFix | isUnambiguous' mctx childPrec childFix = id
                             | otherwise                              = parens
      prettyTOp od
          | FixInfix{} <- opFix od, [a1, a2] <- opArgs od =
              sep [prettyT revArr (mctx' PosLeft) f a1
                  ,text (opName od)
                   <+> prettyT False (mctx' PosRight) f a2]
          | FixPrefix{} <- opFix od, [a1] <- opArgs od =
              text (opName od) <+> prettyT False (mctx' PosRight) f a1
          | FixPostfix{} <- opFix od, [a1] <- opArgs od =
              prettyT False (mctx' PosLeft) f a1 <+> text (opName od)
          | otherwise = error $ "Incompatible number of arguments"
          where mctx' = \p -> Just (opPrec od, opFix od, p)

      prettyT' =
          case t of
            TArrow t1 t2 ->
                if revArr
                then par (-1) (FixInfix AssocLeft)
                     $ prettyTOp ("<-", (-1), FixInfix AssocLeft, [t2, t1])
                else par (-1) (FixInfix AssocRight)
                     $ prettyTOp ("->", (-1), FixInfix AssocRight, [t1, t2])
            TAscribe t1 t2 -> par (-2) (FixInfix AssocNone)
                              $ prettyTOp (":", (-2), FixInfix AssocNone, [t1, t2])
            TVar ident -> text ident
            TConst ident -> text ident
            TPi ident t1 t2 -> par (-3) FixPrefix
                               $ hsep [braces $ fmtAscript ident t1
                                     ,prettyT False (Just ((-3), FixPrefix, PosRight)) f t2]
            TLam ident t1 t2 -> par (-3) FixPrefix
                                $ hsep [brackets $ fmtAscript ident t1
                                      ,prettyT False (Just ((-3), FixPrefix, PosRight)) f t2]
            TApp t1 t2 -> par maxBound (FixInfix AssocLeft)
                          $ prettyT False (Just (maxBound, FixInfix AssocLeft, PosLeft)) f t1
                            <+>
                            prettyT False (Just (maxBound, FixInfix AssocLeft, PosRight)) f t2
            THole -> text "_"
            TType -> text "type"

      fmtAscript ident THole  = text ident
      fmtAscript ident ta     = text ident <> colon <> prettyT False Nothing f ta
