{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

module Strang.Interpreter (interpretProgram) where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec.Prim hiding ((<|>))
import Control.Arrow
import Data.List
import Strang.Types
import Strang.Parsers(programParser)
import Strang.Commands(collapseCommands,leftMap)

putIn :: Functor m => a -> m b -> m (a, b)
putIn x = fmap (const x &&& id)

parseProgram :: Text -> Either String (InputMode, [AnyCommand])
parseProgram pgm = leftMap show (parse programParser "" pgm)

interpretProgram :: Text -> IO ()
interpretProgram cmd = let programAndModeOrErr = parseProgram cmd
                           compiledProgramOrErr :: Either String (InputMode, Text -> Text)
                           compiledProgramOrErr = programAndModeOrErr >>= (\(m, c) -> putIn m ((\f -> foldl' T.append T.empty . f) <$> collapseCommands c))
                           errorHandling err = putStrLn $ "Compilation error: " ++ err
                           in either errorHandling (\(mode, cmd) -> mode >>= (sequence_ . fmap (print . cmd))) compiledProgramOrErr
