{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExistentialQuantification #-}

module Strang.Interpreter (interpretProgram,InterpreterOptions,OutputOptions) where

  import Data.Text (Text)
  import qualified Data.Text.IO as TIO
  import Text.Parsec.Prim hiding ((<|>))
  import Strang.Command
  import Strang.Parsers(programParser)
  import Strang.Prelude(leftMap)
  import Control.Monad.Writer.Strict hiding (sequence)

  parseProgram :: Text -> Either String (InputMode, AnyCommand)
  parseProgram pgm = leftMap (\e -> "Parsing error: " ++ show e) (parse programParser "" pgm)

  data InterpreterOptions = InterpreterOptions { outputOptions :: OutputOptions }
  data OutputOptions = OutputOptions { showLog :: Bool }

  printCompilationError :: String -> IO ()
  printCompilationError = putStrLn . ("Compilation error: " ++)

  interpretProgram :: Text -> InterpreterOptions -> IO ()
  interpretProgram cmd opts =
    either printCompilationError id $ do
      modeAndCmd <- parseProgram cmd
      let (mode, cmd) = modeAndCmd
      composedProgram <- runCommand <$> withProgramType cmd
      pure $ mode >>= (sequence_ . fmap (showProgramOutput (outputOptions opts) . composedProgram))

  showProgramOutput :: OutputOptions -> CommandResult Text -> IO ()
  showProgramOutput _ = TIO.putStrLn . fst . runWriter
