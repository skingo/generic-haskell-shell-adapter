#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}

import Shell
import Monads
import Base

createtest :: (FileManip :≺: f, Moving :≺: f, Misc :≺: f) => Filepath -> Term f ()
createtest path = do touch "foo"
                     sleep 1
                     cp "foo" path
                     sleep 1
                     rm "foo"

movetest :: (Moving :≺: f, DirManip :≺: f) => String -> String -> Term f ()
movetest from to = do mkdir "inner_test"
                      mv (fromString from) (fromString ("inner_test/" ++ to))

rmrest :: (DirManip :≺: f,FileManip :≺: f) => [Filepath] -> Term f ()
rmrest paths = foldr1 (>>) $ fmap remove paths
        where remove path | '/' `elem` (show path) = rm path
                          | otherwise              = rmdir path

combined :: Term (FileManip :+: Moving :+: DirManip :+: Misc :+: Output :+: GeneralInfo) ()
combined = do createtest "bar"
              movetest "bar" "baz"
              files <- ls "."
              view files
              sleep 1
              rmrest ["inner_test/baz", "inner_test"]
              files <- ls "."
              view files

main :: IO ()
main = exec combined
