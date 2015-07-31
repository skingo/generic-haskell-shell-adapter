{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}

module Shell (ShellPath
             , Filepath
             , fromString
             , fromFilepath
             , GeneralInfo
             , home
             , pwd
             , ls
             , wc
             , du
             , Output
             , echo
             , view
             , DirManip
             , mkdir
             , rmdir
             , FileManip
             , touch
             , rm
             , Moving
             , mv
             , cp
             , Misc
             , sleep
             , cd
             , Sudo
             , sudo
             , exec
             ) where

import Base
import Monads
import qualified Turtle as T
import Data.Text hiding (words,unwords,length,take,tail,lines,concatMap)
import Data.String

type ShellPath = T.Shell T.FilePath
type Filepath = T.FilePath

----------- path information

data GeneralInfo a =
    Home (T.FilePath -> a)
    | Pwd (T.FilePath -> a)
    | LS T.FilePath (ShellPath -> a)
    | Wc T.FilePath (String -> a)
    | Du Filepath (Integer -> a)
    deriving (Functor)

home :: (GeneralInfo :≺: f) => Term f T.FilePath
home = inject $ Home Pure

pwd :: (GeneralInfo :≺: f) => Term f T.FilePath
pwd = inject $ Pwd Pure

ls :: (GeneralInfo :≺: f) => T.FilePath -> Term f ShellPath
ls path = inject $ LS path Pure

wc :: (GeneralInfo :≺: f) => T.FilePath -> Term f String
wc path = inject $ Wc path Pure

du :: (GeneralInfo :≺: f) => Filepath -> Term f Integer
du path = inject $ Du path Pure

instance Exec GeneralInfo where
    execAlgebra (Home f) = T.home >>= f
    execAlgebra (Pwd f) = T.pwd >>= f
    --  execAlgebra (Pwd f) = T.pwd >>= print . show >> f
    execAlgebra (LS path f) = f (T.ls path)
    --  execAlgebra (LS path f) = T.view (T.ls path) >> f
    --  execAlgebra (Wc path f) = do let filepath = tail (words (show path) !! 1)
                                 --  let filepath' = take (length filepath - 1) filepath
    execAlgebra (Wc path f) = do let filepath = fromFilepath path
                                 content <- readFile filepath
                                 let linenum = length $ lines content
                                 let wordnum = length $ words content
                                 let charnum = length content
                                 f $ unwords $ fmap show [linenum, wordnum, charnum]
    execAlgebra (Du path f) = T.du path >>= f

fromFilepath :: (IsString a) => T.FilePath -> a
fromFilepath path = let filepath = tail (words (show path) !! 1)
                        filepath' = take (length filepath - 1) filepath
                    in fromString filepath'

----------- output

data Output a =
    Echo Text a
    | Output ShellPath a
    deriving (Functor)

echo :: (Output :≺: f) => String -> Term f ()
echo text = inject $ Echo (fromString text) (Pure ())

view :: (Output :≺: f) => ShellPath -> Term f ()
view path = inject $ Output path (Pure ())

instance Exec Output where
    execAlgebra (Echo text f) = T.echo text >> f
    execAlgebra (Output path f) = T.view path >> f

----------- directory manipulation

data DirManip a =
    Mkdir T.FilePath a
    | Rmdir T.FilePath a
    deriving (Functor,Eq,Show)

mkdir :: (DirManip :≺: f) => T.FilePath -> Term f ()
mkdir path = inject $ Mkdir path (Pure ())

rmdir :: (DirManip :≺: f) => T.FilePath -> Term f ()
rmdir path = inject $ Rmdir path (Pure ())

instance Exec DirManip where
    execAlgebra (Mkdir path f) = T.mkdir path >> f
    execAlgebra (Rmdir path f) = T.rmdir path >> f

----------- file manipulation

data FileManip a =
    Touch T.FilePath a
    | Remove T.FilePath a
    deriving (Functor,Eq)

touch :: (FileManip :≺: f) => T.FilePath -> Term f ()
touch path = inject $ Touch path (Pure ())

rm :: (FileManip :≺: f) => T.FilePath -> Term f ()
rm path = inject $ Remove path (Pure ())

instance Exec FileManip where
    execAlgebra (Touch path f) = T.touch path >> f
    execAlgebra (Remove path f) = T.rm path >> f

----------- moving

data Moving a =
    Move T.FilePath T.FilePath a
    | Copy T.FilePath T.FilePath a
    deriving Functor

cp :: (Moving :≺: f) => T.FilePath -> T.FilePath -> Term f ()
cp from to = inject $ Copy from to (Pure ())

mv :: (Moving :≺: f) => T.FilePath -> T.FilePath -> Term f ()
mv from to = inject $ Move from to (Pure ())

instance Exec Moving where
    execAlgebra (Move from to f) = T.mv from to >> f
    execAlgebra (Copy from to f) = T.cp from to >> f

----------- miscellaneous

data Misc a =
    Sleep T.NominalDiffTime a
    | Cd Filepath a
   deriving Functor

sleep :: (Misc :≺: f) => T.NominalDiffTime -> Term f ()
sleep time = inject $ Sleep time (Pure ())

cd :: (Misc :≺: f) => Filepath -> Term f ()
cd path = inject $ Cd path (Pure ())

instance Exec Misc where
    execAlgebra (Sleep time f) = T.sleep time >> f
    execAlgebra (Cd path f) = T.cd path >> f

----------- sudo

data Sudo t a = Sudo (Term t ()) a
              | NoSudo (Term t ()) a
    deriving Functor

sudo :: ((Sudo a) :≺: f,Functor a) => Term a () -> Term f ()
sudo command = inject $ Sudo command (Pure ())

instance Exec (Sudo Moving) where
    execAlgebra (Sudo t f) =
        case t of
             Impure (Move from to _) -> doSudo ["mv", fromFilepath from, fromFilepath to] f
             Impure (Copy from to _) -> doSudo ["cp", fromFilepath from, fromFilepath to] f

instance Exec (Sudo FileManip) where
    execAlgebra (Sudo t f) =
        case t of
             Impure (Touch path _) -> doSudo ["touch", fromFilepath path] f
             Impure (Remove path _) -> doSudo ["rm", fromFilepath path] f

instance Exec (Sudo DirManip) where
    execAlgebra (Sudo t f) =
        case t of
             Impure (Mkdir path _) -> doSudo ["mkdir", fromFilepath path] f
             Impure (Rmdir path _) -> doSudo ["rmdir", fromFilepath path] f

doSudo :: [Text] -> IO a -> IO a
doSudo command f = T.proc "sudo" command T.empty >> f

--  instance (f :≺: g) => (Sudo f) :≺: (Sudo g) where
    --  inj (Sudo f t) = Sudo (fmap inj f) t
-- maybe use foldTerm instead of fmap?

----------- administrative overhead...

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

class Functor f => Exec f where
    execAlgebra :: f (IO a) -> IO a

instance (Exec f, Exec g) => Exec (f :+: g) where
    execAlgebra (Inl f) = execAlgebra f
    execAlgebra (Inr g) = execAlgebra g

