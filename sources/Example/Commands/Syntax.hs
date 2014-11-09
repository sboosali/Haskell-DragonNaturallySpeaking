{-# LANGUAGE TemplateHaskell #-}
module Example.Commands.Syntax where
import Commands.Syntax

import Language.Haskell.TH


reified = $(stringE . show . syntaxT =<< reify ''Command)


-- $ cabal build && cabal exec runhaskell sources/Example/Commands/Syntax.hs
main :: IO ()
main = do
 putStrLn ""
 print reified
