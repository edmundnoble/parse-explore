{-# language DeriveFunctor #-}
{-# language LambdaCase #-}

module Parseccy where

import Control.Applicative
import Control.Monad

data R t a
        = DoneConsumedInput a t
        | FailConsumedInput String t
        | Fail String
        deriving Functor

newtype Parser t a = Parser
        {
        runParser ::
                t -> R t a
        }

anyChar = Parser $ \case
        x:xs -> ConsumedInput xs (Done x)
        _ -> Fail "no char"

instance Functor (Parser t) where
        fmap f (Parser p) = Parser (fmap f . p)

instance Applicative (Parser t) where
        pure = return
        (<*>) = ap

instance Monad (Parser t) where
        return a = Parser (const (Done a))
        (Parser pa) >>= f = Parser $ \t ->
                runParser pa t

