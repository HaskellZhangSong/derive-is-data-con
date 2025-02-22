{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Derive.IsDataCon (
    derive_is
    ) 
where

import Language.Haskell.TH
import Control.Monad

type TypeName = Name
type ConName = Name

nameToFunc :: ConName -> Int -> Dec
nameToFunc name numOfTyArg = FunD fname [isClause, defaultClause]
  where
    fname = mkName $ "is" ++ ((if (isNameOp name) then (infixToStringName . mkName) else id) $ nameBase name)
#if __GLASGOW_HASKELL__ >= 902
    isClause = Clause [ConP name [] (replicate numOfTyArg WildP)] (NormalB (ConE 'True)) []
#else
    isClause = Clause [ConP name  (replicate numOfTyArg WildP)] (NormalB (ConE 'True)) []
#endif
    defaultClause = Clause [WildP] (NormalB (ConE 'False)) []

infixToStringName :: Name -> String
infixToStringName name = (concatMap charToString (nameBase name))

isNameOp :: Name -> Bool
isNameOp n = elem (head (nameBase n)) "~!@#$%^&*-+=|\\/<>:?.[]"

charToString :: Char -> String
charToString '~' = "Tilde"
charToString '!' = "Bang"
charToString '@' = "At"
charToString '#' = "Hash"
charToString '$' = "Dollar"
charToString '%' = "Percent"
charToString '^' = "Caret"
charToString '&' = "And"
charToString '*' = "Star"
charToString '-' = "Minus"
charToString '+' = "Plus"
charToString '=' = "Equal"
charToString '|' = "Pipe"
charToString '\\' = "Backslash"
charToString '/' = "Slash"
charToString '<' = "Lt"
charToString '>' = "Gt"
charToString ':' = "Colon"
charToString '?' = "Question"
charToString '.' = "Dot"
charToString x   = error $ show x ++ " is not a valid operator symbol"

conToIsFunc :: Con -> [Dec]
conToIsFunc (NormalC name ts)  = [nameToFunc name (length ts)]
conToIsFunc (RecC name vbts)   = [nameToFunc name (length vbts)]
conToIsFunc (InfixC _ name _)  = [nameToFunc name 2]
conToIsFunc (ForallC _ _ con)  = conToIsFunc con
conToIsFunc (GadtC ns ts _)    = map (uncurry nameToFunc) [(n, length ts) | n <- ns]
conToIsFunc (RecGadtC ns ts _) = map (uncurry nameToFunc) [(n, length ts) | n <- ns]

nameToSig :: ConName -> TypeName -> Int -> Q [Dec]
nameToSig fn tn tnNumOfVars = do
    ns <- replicateM tnNumOfVars (newName "a")
    let vars = map VarT ns
    let fname = mkName $ "is" ++ ((if (isNameOp fn) then (infixToStringName . mkName) else id) $ nameBase fn)
    return [SigD fname (AppT (AppT ArrowT (foldl AppT (ConT tn) vars)) (ConT ''Bool))]

conToSig :: TypeName -> Int -> Con -> Q [Dec]
conToSig tn numOfTyVars (NormalC name _)  = nameToSig name tn numOfTyVars
conToSig tn numOfTyVars (RecC name _)   = nameToSig name tn numOfTyVars
conToSig tn numOfTyVars (InfixC _ name _)  = nameToSig name tn numOfTyVars
conToSig tn numOfTyVars (ForallC _ _ con)  = conToSig tn numOfTyVars con
conToSig tn numOfTyVars (GadtC ns _ _)    = fmap concat $ mapM (\fn -> nameToSig fn tn numOfTyVars) ns
conToSig tn numOfTyVars (RecGadtC ns _ _) = fmap concat $ mapM (\fn -> nameToSig fn tn numOfTyVars) ns

derive_is :: Name -> Q [Dec]
derive_is name = do
  info <- reify name
  case info of
    TyConI (DataD _ _ tvbs _ cons _) -> do 
                                sigs <- fmap concat $ mapM (conToSig name (length tvbs)) cons
                                let defs = concatMap conToIsFunc cons
                                return $ sigs ++ defs
    i -> error $ show i ++ " is not data type"