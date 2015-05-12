{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures #-}

module TH where

import Transport

import Data.Aeson
import Data.Aeson.TH
import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )

-- | Generates FromJSON' instance declarations for the given
-- data type, with support for GADTs, specifically the 'FromJSON' instance
--  for 'Transport :: (* -> *) -> *', parameterized by the given GADT.
deriveFromJSONTransport 
           :: Options
           -- ^ Encoding options.
           -> Name
           -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
           -- instances.
           -> Q [Dec]
deriveFromJSONTransport o n = do
        decls <- deriveFromJSON o n
        runIO (print decls)
        let decls' = map addTransport decls
        runIO (putStrLn $ pprint decls)
        return decls'

addTransport :: Dec -> Dec
addTransport (InstanceD cxt ty funs) = InstanceD [] (addTransportType ty) $ map addTransportFun funs
addTransport other = other

addTransportType :: Type -> Type
addTransportType (AppT cls (AppT gadt (VarT _))) = AppT cls (AppT (ConT ''Transport) gadt)
addTransportType ty = error $ show ty

addTransportFun :: Dec -> Dec
addTransportFun (FunD nm cls) = FunD nm $ map addTransportClause cls
addTransportFun other = other

addTransportClause :: Clause -> Clause
addTransportClause (Clause ps body decls) = Clause ps (addTransportBody body) decls

addTransportBody :: Body -> Body
addTransportBody (NormalB exp) = NormalB $ addTransportExp exp
addTransportBody (GuardedB ges) = GuardedB 
        [ case g of
            o@(NormalG (VarE o_wise)) | o_wise == 'otherwise -> (g,e)
            _ -> (g,addTransportCons e) 
        | (g,e) <- ges ]

addTransportExp :: Exp -> Exp
addTransportExp (LamE ps e)  = LamE ps $ addTransportExp e
addTransportExp (CaseE e ms) = CaseE e $ map addTransportMatch ms
addTransportExp (DoE ss)      = DoE $ map addTransportStmt ss
addTransportExp other = other 

addTransportMatch :: Match -> Match
addTransportMatch (Match p body ds) = Match p (addTransportBody body) ds

addTransportStmt :: Stmt -> Stmt
addTransportStmt (NoBindS e) = NoBindS (addTransportExp e)
addTransportStmt other = other

addTransportCons :: Exp -> Exp
addTransportCons e = AppE (AppE (VarE 'fmap) (ConE 'Transport)) e