{-# LANGUAGE TemplateHaskell #-}
module Example.Commands where
import Example.Types
import Commands.Syntax

import Language.Haskell.TH


reified = $(stringE . show . syntaxT =<< reify ''Command)


-- $ cabal build && cabal exec runhaskell sources/Example/Commands.hs
main :: IO ()
main = do
 putStrLn ""
 print reified
