{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe  #-}
{-# LANGUAGE OverloadedStrings #-}

module Strang.Interpreter (interpretProgram,runProgram) where

import           Control.Arrow
import           Data.List
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Strang.Base      (leftMap, printCommand)
import           Strang.Command
import           Strang.Input
import           Strang.Parsers   (programAndModeParser, programParser)
import           Text.Parsec.Prim hiding ((<|>))

putIn :: Functor m => a -> m b -> m (a, b)
putIn x = fmap (const x &&& id)

parseProgram :: Text -> Either String [AnyCommand]
parseProgram pgm = leftMap (\e -> "Parsing error: " ++ show e) (parse programParser "" pgm)

parseProgramAndMode :: Text -> Either String (InputMode,[AnyCommand])
parseProgramAndMode mode = leftMap (\e -> "Parsing error: " ++ show e) (parse programAndModeParser "" mode)

withProgramType :: AnyCommand -> Either String (Command Text Text)
withProgramType AnyCommand { runAny = c@Command { inTy = StringTy, outTy = ot } } = Right $ composeCommands c (printCommand ot)
withProgramType AnyCommand { runAny = Command { inTy = ct } } = Left $ "Expected program to have input type Text, found input type " ++ show ct

collapseCommands :: [AnyCommand] -> Either String (Text -> CommandResult Text)
collapseCommands [] = Left "No command!"
collapseCommands cmds = runCommand <$> (typecheckCommands cmds >>= withProgramType)

interpretProgram :: Text -> Either String (Text -> Text)
interpretProgram cmd = let programOrErr = parseProgram cmd
                           composedProgramOrErr :: Either String (Text -> Text)
                           composedProgramOrErr = programOrErr >>= (\c -> ((\f -> foldl' T.append T.empty . f) <$> collapseCommands c)) in
                           composedProgramOrErr

runProgram :: Text -> IO ()
runProgram cmd = let programAndModeOrErr = parseProgramAndMode cmd
                     composedProgramOrErr :: Either String (InputMode, Text -> Text)
                     composedProgramOrErr = programAndModeOrErr >>= (\(m, c) -> putIn m ((\f -> foldl' T.append T.empty . f) <$> collapseCommands c))
                     errorHandling err = putStrLn $ "Compilation error: " ++ err
                       in either errorHandling (\(mode, prog) -> interpretInputMode mode >>= (TIO.putStrLn . prog)) composedProgramOrErr
