{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification,OverloadedStrings,FunctionalDependencies #-}

module Strang.Parsers (programParser) where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import Data.Functor
import Data.Maybe
import Data.Monoid
import Control.Applicative.Alternative hiding (many)
import Strang.Command
import Strang.Prelude
import Control.Arrow

-- A Strang command is a series of characters.
-- It starts with a mode character, 'l' for line mode or 't' for text mode.
modeParser :: Parser InputMode
modeParser = consumingParser <|> pure (pure . T.pack <$> getLine) where
                consumingParser = try (char 'l' $> (pure . T.pack <$> getLine)) <|> try (char 't' $> (pure . T.pack <$> getContents))

stringSeparator :: Char
stringSeparator = '/'

paramSeparator :: Char
paramSeparator = '$'

data Argument a = Literal { argumentType :: ParamTy a, argumentValue :: a }
                | Param { name :: Text }

-- This is for passing strings to commands.
singleArg :: ParamTy a -> Parser (Argument a)
singleArg StringTy = (Param <$> surroundedBy anyChar (char paramSeparator)) <|>
                     (Literal StringTy <$> surroundedBy anyChar (char stringSeparator)) where
                surroundedBy p q = T.pack <$> (q *> manyTill p (try q))
singleArg _ = error "No argument types other than Text supported (yet)"

(>*<) :: Applicative m => m a -> m b -> m (a, b)
(>*<) = liftA2 (,)

-- Collapses an error from an Either into the outer Parser.
collapseError :: Show e => Parser (Either e a) -> Parser a
collapseError p = p >>= either (fail . show) pure

-- Existentials being tricky. Parser alternation operator, converting the second operand
-- to an AnyCommand.
(<||>) :: Parser AnyCommand -> Parser (Command a b) -> Parser AnyCommand
ab <||> cd = ab <|> fmap AnyCommand cd

data FunctionBindings = FunctionBindings { builtins :: FKList FunctionBinding, userFunctionBindings :: FKList FunctionBinding }

instance Monoid FunctionBindings where
  FunctionBindings {builtins=bs1,userFunctionBindings=us1} `mappend` FunctionBindings{builtins=bs2,userFunctionBindings=us2} = FunctionBindings {builtins = bs1 <> bs2, userFunctionBindings = us1 <> us2}
  mempty = FunctionBindings mempty mempty

instance Monoid (FKList f) where
  mempty = FK KNil
  mappend = forgetIn2 (forgetOut2 (+|+))

parserForFunctionBinding :: FunctionBindings -> FKList Argument -> FunctionBinding args -> Parser AnyCommand
parserForFunctionBinding bs inargs FunctionBinding { bindingName = n, bindingArguments = args, commandFromFunctionBinding = body } = do
  string (T.unpack n)
  as <- parserForUserArgs args
  collapseError . return $ applyFunctionBinding as body as

parserForUserArgs :: KList NamedParamTy args -> Parser (KList Argument args)
parserForUserArgs KNil = return KNil
parserForUserArgs (npt :-: ks) = (:-:) <$> (singleArg . forgetName) npt <*> parserForUserArgs ks

applyFunctionBinding :: KList Argument args -> (HList args -> Either ParseError AnyCommand) -> Either ParseError AnyCommand
applyFunctionBinding = undefined

userCommandFromArgs :: KList NamedParamTy args -> FunctionBindings -> Text -> Either ParseError (HList args -> AnyCommand)
userCommandFromArgs argList fbs body = undefined

forgetName :: NamedParamTy a -> ParamTy a
forgetName (NamedParamTy pTy _) = pTy

parseParam :: ParamTy a -> Parser a
parseParam StringTy = try (char stringSeparator) *> (T.pack <$> many (satisfy (/= stringSeparator)))
parseParam unknown = error $ "Don't know how to parse values of type " ++ show unknown

foldParsers :: KList Parser args -> Parser (HList args)
foldParsers KNil = pure HNil
foldParsers (pa :-: kl) = HCons <$> pa <*> foldParsers kl

parseArgs :: KList ParamTy args -> Parser (HList args)
parseArgs args = foldParsers $ kmap parseParam args

commandParserForFunctionBindings :: FunctionBindings -> FKList NamedParamTy -> Parser AnyCommand
commandParserForFunctionBindings fbs@FunctionBindings { builtins = (FK bs), userFunctionBindings = (FK us) } params =
  foldl (<|>) parserZero (klistElim (parserForFunctionBinding fbs) bs) <|>
  foldl (<|>) parserZero (klistElim (parserForFunctionBinding fbs) us)

-- TODO: introduce new argument types
bindingArgParser :: Parser (FKList NamedParamTy)
bindingArgParser = do
  arg <- singleArgParse
  let thisParam = uncurry NamedParamTy arg
  fNextParams <- option (FK KNil) . try $ do
    try $ char '/'
    bindingArgParser
  return $ case fNextParams of
    FK nextParams -> FK $ thisParam :-: nextParams

singleArgParse :: Parser (ParamTy Text, Text)
singleArgParse = (const StringTy &&& id) <$> (T.pack <$> many alphaNum)

equationParser :: FKList FunctionBinding -> FKList FunctionBinding -> Parser (FKList TypedValue -> FFunctionBinding)
equationParser bs us = do
  t <- (T.pack <$> many1 alphaNum) >*< bindingArgParser <* many1 space <* char '=' <* many1 space
  let (name, ba) = t
  case ba of
    (FK bindingArgs) -> do
      cmd <- compoundCommandParser FunctionBindings { userFunctionBindings = us, builtins = bs } bindingArgs
      let binding a = FunctionBinding name bindingArgs (cmd a)
      return binding

data CommandWithArgs args = CommandWithArgs (KList TypedValue args -> AnyCommand)

compoundCommandParser :: FunctionBindings -> KList NamedParamTy args -> Parser (CommandWithArgs args)
compoundCommandParser bs tas = many1 (commandParserForFunctionBindings bs tas)

data Exists f = forall a. Exists (f a)

accumFK :: forall f i. (FKList f -> Parser (Exists f)) -> FKList f -> Parser i -> Parser (FKList f)
accumFK f s sep = let ourParser = try (f s) in do
  newStateF <- ourParser
  case newStateF of
    Exists newState -> do
      nextResult <- optionMaybe (try (sep *> accumFK f (newState -:- s) sep))
      return $ newState -:- fromMaybe (FK KNil) nextResult

allFunctionBindingsParser :: FunctionBindings -> Parser FunctionBindings
allFunctionBindingsParser bs = do
  userFunctionBindings <- accumFK (equationParser (builtins bs)) (userFunctionBindings bs) (many space)
  return FunctionBindings{builtins=builtins bs,userFunctionBindings=userFunctionBindings}

programParser :: Parser (InputMode, [AnyCommand])
programParser = do
  mode <- modeParser
  userFunctionBindings <- option (mempty :: FunctionBindings) (try $ allFunctionBindingsParser (FunctionBindings (FK KNil) base))
  many space
  commands <- compoundCommandParser userFunctionBindings (FK KNil)
  many space
  eof
  return (mode, commands)
