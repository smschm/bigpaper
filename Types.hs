module Types where
import qualified Data.Set as S
import qualified Data.Map as M

type Str = String
type Ident = Str

type Progress = Either Str Str
-- where Left is Not Done, Right is Done

data Todo = Todo {
    name :: Str,
    prereqs :: S.Set Ident,
    softprs :: S.Set Ident,
    progress :: Progress,
    priority :: Int,
    wheres :: [Str],
    context :: Maybe Str
} deriving (Eq, Show)

data Spec = Done Str | NotDone Str | PRef [Ident] | SPRef [Ident] | --Depends [Int] |
    Where Str | Context Str | Priority Int | Name Str
    deriving (Eq, Show)

type BP = M.Map Ident Todo

defaultT = Todo "" S.empty S.empty (Left "") 1 [] Nothing
