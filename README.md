# JRPC-TH: A library to tag and reify the methods via TH

# Example

```haskell
import JRPC.Server
import JRPC.TH
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V

newtype App a = App {runApp :: IO a}
  deriving newtype (Functor, Applicative, Monad)

methodMap :: MethodMap App
methodMap = fromList methodList 
  where methodList = $(reifyMethods ''App)

reverseMethod :: Param "string" -> App (Either CustomError Value)
reverseMethod param = pure do
  case getParam param of
    Just (String s) -> Right $ String $ T.reverse s
    Nothing -> Left $ makeCustomError "empty" Nothing 400
    _ -> Left $ makeCustomError "wrong json" Nothing 400

main :: IO ()
main = runApp do

  print =<< do
    runOnValue methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "id" .= Number 1
      , "method" .= String "reverse"
      , "params" .= object ["string" .= String "123"]
      ]

  print =<< do
    runOnValue methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "id" .= Number 1
      , "method" .= String "reverse"
      , "params" .= Array (V.fromList [String "123"])
      ]

  print =<< do
    runOnValue methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "id" .= Number 1
      , "method" .= String "reverse"
      ]

  print =<< do
    runOnValue methodMap Nothing $ object
      [ "jsonrpc" .= String "2.0"
      , "method" .= String "reverse"
      , "params" .= Array (V.fromList [String "123"])
      ]
```

```console
$ cabal run
Just (Object (fromList [("id",Number 1.0),("jsonrpc",String "2.0"),("result",String "321")]))
Just (Object (fromList [("id",Number 1.0),("jsonrpc",String "2.0"),("result",String "321")]))
Just (Object (fromList [("error",Object (fromList [("code",Number (-32600.0)),("data",Null),("message",String "Invalid request")])),("id",Null),("jsonrpc",String "2.0")]))
Nothing
```
