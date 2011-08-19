module Types where
import qualified Data.Set as S
import qualified Data.Map as M

type Str = String
type Ident = Str

-- | Left x is not done, Right x is done; with reason/commentary x
type Progress = Either Str Str

-- | One TODO item
data Todo = Todo {
    name     :: Str,         -- ^ The name of the task, i.e. "get some milk"
    prereqs  :: S.Set Ident, -- ^ 'Hard' prerequisites
    softprs  :: S.Set Ident, -- ^ 'Soft' prerequisites, containing suggestions
    progress :: Progress,    -- ^ Progress on the task
    priority :: Int,         -- ^ An arbitrary numerical priority
    wheres   :: [Str],       -- ^ Where is the task primarily done?
    context  :: Maybe Str    -- ^ The context or project this task belongs to
} deriving (Eq)

instance Show Todo where
    show t = (show (priority t)) ++ ":\t" ++ (name t)

data Spec = Done Str | NotDone Str | PRef [Ident] | SPRef [Ident] | --Depends [Int] |
    Where Str | Context Str | Priority Int | Name Str
    deriving (Eq, Show)

-- | The 'big paper': a map from identification keys to TODO items
type BP = M.Map Ident Todo

defaultT = Todo "" S.empty S.empty (Left "") 1 [] Nothing
