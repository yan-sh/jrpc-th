module JRPC.TH where

import qualified JRPC.TH.Internal as I
import Language.Haskell.TH

tagMethod :: String -> Name -> Name -> Q [Dec]
tagMethod = I.tagMethod
{-# INLINE tagMethod #-}

reifyMethods :: Name -> Q Exp
reifyMethods = I.reifyMethods 
{-# INLINE reifyMethods #-}
