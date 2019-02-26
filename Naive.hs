{-# language LambdaCase #-}

module Naive where

import Control.Applicative
import Control.Monad

newtype Parser t a = Parser { runParser :: t -> Maybe (a, t) }

anyChar :: Parser String Char
anyChar = Parser $ \case
        [] -> Nothing
        h:t -> Just (h,t)

instance Functor (Parser t) where
        fmap f (Parser p) = Parser $ \t ->
                fmap (\(a, t') -> (f a, t')) (p t)

instance Applicative (Parser t) where
        pure = return
        (<*>) = ap

instance Monad (Parser t) where
        return a = Parser $ \t -> Just (a, t)
        (Parser p) >>= f = Parser $ \t ->
                p t >>= \(a, t) ->
                        runParser (f a) t

instance Alternative (Parser t) where
        empty = Parser (const Nothing)
        (Parser p) <|> (Parser p') = Parser $ \t ->
                case p t of
                        Nothing -> p' t
                        Just r -> Just r

rep _ 0 = return ()
rep c n = Parser $ \case
        x:xs | c == x -> runParser (rep c (n-1)) xs
        _ -> Nothing

cs = rep 'c'
bs = rep 'b'

anbncn n = Parser $ \case
        'a':xs -> runParser (anbncn (n+1)) xs
        xs | n > 0 -> runParser (bs n *> cs n) xs
        [] -> Nothing

