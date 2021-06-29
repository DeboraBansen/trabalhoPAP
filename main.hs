------------------------------------------------------
--Debora Bansen 
------------------------------------------------------
import Text.ParserCombinators.Parsec

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show

type Name = String

type Unifier = [(Name, Type)]


parseType :: Parser Type     -- type: function | atom
parseType=
  try parseFun <|> parseAtom

parseAtom :: Parser Type    -- atom: int | var |paren
parseAtom=
  try parseInt <|>parseVar <|>parseParen

parseInt :: Parser Type      -- int: "Int"
parseInt=do
  string "Int"
  return TypeInt

parseVar :: Parser Type      -- var: lowercase+
parseVar = do
  name <- many1 lower
  return (TypeVar name)

parseFun :: Parser Type      -- fun: atom "->" type
parseFun=do
  x <-parseAtom
  string "->"
  y <-parseType
  return (TypeArrow x y)

parseParen :: Parser Type    -- paren: "(" type ")"
parseParen=do
  char '('
  name<- parseType
  char ')'
  return (name)

-----------------------------------------------------------------------------
occursCheck :: Name -> Type -> Bool
occursCheck x (TypeInt) =
  False
occursCheck x (TypeVar y) =
  x == y 
occursCheck x (TypeArrow a b) =
  if ((occursCheck x) a)==False then
    (occursCheck x) b
  else
    True


------------------------------------------------------------------------------
compose :: Unifier -> Unifier -> Unifier
compose a b=
  let substOnTuple (name, parseType) =
        (name, subst a parseType)

  in fmap substOnTuple b



-----------------------------------------------------------------------------
subst :: Unifier -> Type -> Type
subst x (TypeInt) =
  TypeInt

subst x (TypeVar a) =
  case lookup a x of
    Just b ->
      b
    
    Nothing ->
      TypeVar a

subst x (TypeArrow a b) =
  TypeArrow (subst x a)(subst x b)




------------------------------------------------------------------------------
unify :: Type -> Type -> Maybe Unifier
unify (TypeInt)(TypeInt)=
    Just []

unify (TypeVar a)(TypeVar b)|a==b=
  Just[]
  

unify (TypeArrow a b)(TypeArrow c d)=
  case unify a c of
    Just t1->
      case unify (subst t1 b)(subst t1 d) of
        Just t2->
          Just(compose t2 t1)
        Nothing->
          Nothing
    Nothing->
      Nothing
--}
unify (TypeVar a) b=
  if occursCheck a b then
    Nothing
  else
    Just [(a,b)]

unify b (TypeVar a) =
  if occursCheck a b then
    Nothing
  else
    Just [(a, b)]

unify _ _ =
  Nothing


--------------------------------------------------------------------------
main::IO()
main=do
  putStrLn "Digite um termo:"
  a <- getLine
  let Right term_a = parse parseType "<stdin>" a

  putStrLn "Digite outro termo:"
  getLine 
  b <- getLine
  let Right term_b = parse parseType "<stdin>" b
  
  putStrLn "Unificação:"
  
  print $ unify term_a term_b  
  

