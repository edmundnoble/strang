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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Strang.Interpreter (interpretProgram,InterpreterOptions,OutputOptions,showLog,outputOptions) where

  import Data.Text (Text)
  import qualified Data.Text.IO as TIO
  import Text.Parsec.Prim hiding ((<|>))
  import Strang.Command
  import Strang.Parsers(programParser)
  import Strang.Prelude(leftMap)
  import Control.Monad.Writer.Strict hiding (sequence)
  import Data.Default
  import Control.Lens

  parseProgram :: Text -> Either String (InputMode, AnyCommand)
  parseProgram pgm = leftMap (\e -> "Parsing error: " ++ show e) (parse programParser "" pgm)

  data InterpreterOptions = InterpreterOptions { _outputOptions :: OutputOptions }
  data OutputOptions = OutputOptions { _showLog :: Bool }
  makeLenses ''InterpreterOptions
  makeLenses ''OutputOptions

  instance Default OutputOptions where
    def = OutputOptions { _showLog = False }

  instance Default InterpreterOptions where
    def = InterpreterOptions def

  printCompilationError :: String -> IO ()
  printCompilationError = putStrLn . ("Compilation error: " ++)

  valueOr :: (a -> b) -> Either a b -> b
  valueOr _ (Right r) = r
  valueOr f (Left l) = f l

  interpretProgram :: Text -> InterpreterOptions -> IO ()
  interpretProgram cmd opts =
    valueOr printCompilationError $ do
      modeAndCmd <- parseProgram cmd
      let (mode, cmd) = modeAndCmd
      composedProgram <- runCommand <$> withProgramType cmd
      pure $ mode >>= (sequence_ . fmap (showProgramOutput (_outputOptions opts) . composedProgram))

  showProgramOutput :: OutputOptions -> CommandResult Text -> IO ()
  showProgramOutput _ = TIO.putStrLn . fst . runWriter
