{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}

import Base
import Monads
import Shell

lstest :: (GeneralInfo :≺: f) => Term f ShellPath
lstest = ls "."

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

testest :: Term (FileManip :+: (Sudo FileManip)) ()
testest = do touch "bar"
             sudo (touch "baz" :: Term FileManip ())

--  testest' :: Term (FileManip :+: (Sudo Moving)) ()
--  testest' = do touch "bar"
              --  sudo (mv "bar" "baz" :: Term Moving ())

cdtest :: Term Misc () 
cdtest = cd "test/"

wctest :: Term (GeneralInfo :+: Output) ()
wctest = do count <- wc "runexample.hs"
            echo count

main :: IO ()
main = exec (do paths <- lstest :: Term (FileManip :+: DirManip :+: Output :+: GeneralInfo :+: Moving :+: Misc :+: Sudo FileManip) ShellPath
                view paths
                mkdir "xxtest"
                touch "xxfoobarfile"
                sleep 2
                lsfirst <- ls "xxtest"
                view lsfirst
                mv "xxtest" "xtest"
                sleep 2
                rmdir "xtest"
                rm "xxfoobarfile"
                lstest
                echo "foobar"
                path <- pwd
                wordcount <- wc "../meeting_03/topics"
                echo wordcount
                echo $ show path
                sudo (touch "test/foobaz" :: Term FileManip ())
                )
