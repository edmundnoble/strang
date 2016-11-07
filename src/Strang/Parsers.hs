{-# LANGUAGE LiberalTypeSynonyms,ImpredicativeTypes,FlexibleContexts,DataKinds,TypeFamilies,RankNTypes,TupleSections,NamedFieldPuns,GADTs,MonoLocalBinds,ScopedTypeVariables,PolyKinds,TypeOperators,UndecidableInstances,FlexibleInstances,InstanceSigs,DefaultSignatures,Trustworthy,ExistentialQuantification,OverloadedStrings #-}

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
import Strang.Lists
import Strang.Iso
import Strang.Exists(Exists(..))
import Control.Arrow
import Control.Monad

-- A Strang command is a series of characters.
-- It starts with a mode character, 'l' for line mode or 't' for text mode.
modeParser :: Parser InputMode
modeParser = (<$>) (T.pack <$>) <$> (consumingParser <|> pure multiLine) where
                consumingParser = try (char 'l' $> multiLine) <|> try (char 't' $> sequence [getContents])
                multiLine = sequence $ repeat getLine

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

newtype LoadedBindings = LoadedBindings (forall args. FunctionBinding args -> Parser (CompoundCommand args))

parserForFunctionBinding :: FunctionBindings -> LoadedBindings
parserForFunctionBinding bs = LoadedBindings $ \b -> case b of
  FunctionBinding { bindingName = n, bindingArguments = args, commandFromFunctionBinding = body } -> do
    string (T.unpack n)
    as <- parserForUserArgs args
    (collapseError . return) $ applyFunctionBinding as body

parserForUserArgs :: KList NamedParamTy args -> Parser (KList Argument args)
parserForUserArgs KNil = return KNil
parserForUserArgs (npt :-: ks) = (:-:) <$> (singleArg . forgetName) npt <*> parserForUserArgs ks

applyFunctionBinding :: KList Argument args -> (HList args -> Either ParseError AnyCommand) -> Either ParseError (CompoundCommand args)
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

commandParserForFunctionBindings :: FunctionBindings -> KList NamedParamTy args -> Parser (CompoundCommand args)
commandParserForFunctionBindings fbs@FunctionBindings { builtins = bs, userFunctionBindings = us } params =
  undefined
  -- foldl (<|>) parserZero (klistElim (parserForFunctionBinding fbs) bs) <|>
  -- foldl (<|>) parserZero (klistElim (parserForFunctionBinding fbs) us)

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

equationParser :: FKList FunctionBinding -> FKList FunctionBinding -> Parser (Exists FunctionBinding)
equationParser bs us = do
  t <- (T.pack <$> many1 alphaNum) >*< bindingArgParser <* many1 space <* char '=' <* many1 space
  let (name, ba) = t
  case ba of
    (FK bindingArgs) -> do
      c <- compoundCommandParser FunctionBindings { userFunctionBindings = us, builtins = bs } bindingArgs
      let (AnyCommandWithArgs cmd) = c
      let binding = FunctionBinding name bindingArgs (pure . to . cmd)
      return $ Exists binding

data CommandWithArgs i o args = CommandWithArgs (HList args -> Command i o)
data AnyCommandWithArgs args = AnyCommandWithArgs (HList args -> Exists (Uncurry Command))
data CompoundCommand :: [*] -> * where
  ApplyArg :: forall (args :: [*]) (a :: *). NamedParamTy a -> CompoundCommand args -> CompoundCommand (a ': args)
  PureCommand :: forall (args :: [*]). AnyCommand -> CompoundCommand args -> CompoundCommand args
  EmptyCommand :: forall (args :: [*]). CompoundCommand args

data CompoundCommandLit inargs funargs = CompoundCommandList {
  compoundCommandLitArgs :: KList NamedParamTy inargs,
  compoundCommandLitCommand :: CompoundCommand funargs
}

combineCompoundCommands :: CompoundCommand args -> CompoundCommand args -> Either String (CompoundCommand args)
combineCompoundCommands = undefined

finalizeCommand :: CompoundCommand args -> AnyCommandWithArgs args
finalizeCommand = undefined

compoundCommandParser :: FunctionBindings -> KList NamedParamTy args -> Parser (AnyCommandWithArgs args)
compoundCommandParser bs tas = do
  a <- many1 (commandParserForFunctionBindings bs tas)
  let (h:cmds) = a
  command <- (collapseError . return) $ foldM combineCompoundCommands h cmds
  return $ finalizeCommand command

accumFK :: forall f i. (FKList f -> Parser (Exists f)) -> FKList f -> Parser i -> Parser (FKList f)
accumFK f s sep = let ourParser = try (f s) in do
  newStateF <- ourParser
  case newStateF of
    Exists newState -> do
      nextResult <- optionMaybe (try (sep *> accumFK f (newState ~:~ s) sep))
      return $ newState ~:~ fromMaybe (FK KNil) nextResult

allFunctionBindingsParser :: FunctionBindings -> Parser FunctionBindings
allFunctionBindingsParser bs = do
  userFunctionBindings <- accumFK (equationParser (builtins bs)) (userFunctionBindings bs) (many space)
  return FunctionBindings{builtins=builtins bs,userFunctionBindings=userFunctionBindings}

programParser :: Parser (InputMode, AnyCommand)
programParser = do
  mode <- modeParser
  userFunctionBindings <- option (FunctionBindings mempty mempty) (try $ allFunctionBindingsParser (FunctionBindings (FK KNil) base))
  many space
  cmds <- compoundCommandParser userFunctionBindings KNil
  let (AnyCommandWithArgs cmdf) = cmds
  let commands = to $ cmdf HNil
  many space
  eof
  return (mode, commands)
