{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module JRPC.TH.Internal where

import JRPC.Types
import Language.Haskell.TH
import GHC.TypeLits

data MethodHandler m = forall f . ToMethod f m => MethodHandler f

class IsMethodHandler (a :: Symbol) m where
  methodHandler :: MethodHandler m

handlerToMethod :: forall m . Applicative m => MethodHandler m -> Method m
handlerToMethod (MethodHandler a) = Method (mkMethod a)

tagMethod :: String -> Name -> Name -> Q [Dec]
tagMethod l t_ name_ = pure 
  [ InstanceD
      Nothing
      []
      (AppT (AppT (ConT ''IsMethodHandler) (LitT (StrTyLit l))) (ConT t_))
      [ FunD 'methodHandler
          [ Clause [] (NormalB (AppE (ConE 'MethodHandler) (VarE name_))) [] 
          ]
      ]
  ]

reifyMethods :: Name -> Q Exp
reifyMethods n = fmap (ListE . fmap f) instances_
  where
    instances_ = reifyInstances ''IsMethodHandler [VarT (mkName "a"), ConT n]
    f (InstanceD _ _ (AppT (AppT _ (LitT (StrTyLit t))) _) _) = TupE $ fmap Just
        [ LitE $ StringL t
        , AppE (VarE 'handlerToMethod)
            $ AppTypeE (VarE 'methodHandler) (LitT $ StrTyLit t)
        ]
    f _ = error "Not instance" 
