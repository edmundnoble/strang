{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Strang.Interpreter (interpretProgram) where

import           Control.Arrow
import           Data.List
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Strang.Base      (leftMap, printCommand)
import           Strang.Input
import           Strang.Command
import           Strang.Parsers   (programParser)
import           Text.Parsec.Prim hiding ((<|>))

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
                           in either errorHandling (\(mode, prog) -> interpretInputMode mode >>= (TIO.putStrLn . prog)) composedProgramOrErr
