{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification #-}

module Strang.Interpreter (interpretProgram) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec.Prim hiding ((<|>))
import Control.Arrow
import Data.List
import Strang.Command
import Strang.Parsers(programParser)
import Strang.Base(leftMap,printCommand)

putIn :: Functor m => a -> m b -> m (a, b)
putIn x = fmap (const x &&& id)

parseProgram :: Text -> Either String (InputMode, [AnyCommand])
parseProgram pgm = leftMap (\e -> "Parsing error: " ++ show e) (parse programParser "" pgm)

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType AnyCommand { runAny = c@Command { inTy = StringTy, outTy = ot } } = Right $ composeCommands c (printCommand ot)
withProgramType AnyCommand { runAny = Command { inTy = ct } } = Left $ "Expected program to have input type Text, found input type " ++ show ct

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "No command!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)

interpretProgram :: Text -> IO ()
interpretProgram cmd = let programAndModeOrErr = parseProgram cmd
                           composedProgramOrErr :: Either String (InputMode, Text -> Text)
                           composedProgramOrErr = programAndModeOrErr >>= (\(m, c) -> putIn m ((\f -> foldl' T.append T.empty . f) <$> collapseCommands c))
                           errorHandling err = putStrLn $ "Compilation error: " ++ err
                           in either errorHandling (\(mode, prog) -> mode >>= (sequence_ . fmap (TIO.putStrLn . prog))) composedProgramOrErr
