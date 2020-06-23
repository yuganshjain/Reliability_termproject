import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Char
import System.IO


data Formula =
  Constant Float |
  Variable String |
  Operation Char [Formula]
    deriving (Eq, Ord, Show)

opAdd = Operation '+'
opMul = Operation '*'

toFormula :: Sch -> Formula
toFormula (And l) = opMul $ map toFormula l
toFormula (Or l)  = oneMin $ opMul $ map (oneMin.toFormula) l
toFormula (Var s) = Variable s
toFormula (Const n) = Constant n

oneMin f = opAdd [Constant 1, opMul [Constant (-1), f]]

doOp :: Char -> Float -> Float -> Float
doOp '+' = (+)
doOp '*' = (*)

neutralElem :: Char -> Float
neutralElem '*' = 1
neutralElem '+' = 0

convolOp :: Int -> (a->a) -> (a->a)
convolOp 0 f = id
convolOp n f | n `mod` 2 == 0 = convolOp (n `div` 2) (f.f)
convolOp n f = f.(convolOp (n-1) f)

simplify x = sortForm $ (convolOp 100 (simpVar.mergeOps.distribute)) x

sortForm :: Formula -> Formula
sortForm (Operation t f) = Operation t $ sort $ map sortForm f
sortForm a = a

simpVar :: Formula -> Formula
simpVar (Operation '+' f) =
  let
  reducer elem dict =
    let dictAdd key val = Map.alter (Just.(+val).(fromMaybe 0)) key dict in
    case elem of
      Operation '*' [(Constant c),ff] -> dictAdd ff c
      Operation '*' ((Constant c):ff) -> dictAdd (Operation '*' ff) c
      f -> dictAdd f 1
  vars = foldr reducer Map.empty $ map simpVar f
  makeFormula (form, coeff) =
    if coeff==1 then form
    else Operation '*' [Constant coeff, form]
  formsVars = map makeFormula $ filter ((/=0).snd) $ Map.toList vars
  in
    Operation '+' formsVars
simpVar a = a

mergeOps (Operation typ f) =
  let
  reducer elem (val,r) =
    case elem of
      Operation t ((Constant c):ff) | t == typ -> (doOp typ val c, ff++r)
      Operation t ff | t == typ -> (val, ff++r)
      Constant c -> (doOp typ c val, r)
      f -> (val, f:r)
  in
    case (foldr reducer (neutralElem typ, []) $ map mergeOps f) of
      (finVal, []) -> Constant finVal
      (finVal, [oneFormula]) | finVal == neutralElem typ -> oneFormula
      (finVal, ff)  | finVal == neutralElem typ -> Operation typ ff
      (finVal, ff) -> Operation typ $ (Constant finVal):ff

mergeOps a = a

distribute (Operation '*' f) =
  let
  reducer elem r =
    case elem of
      Operation '+' ff ->
        concatMap (\x -> map (x:) r) ff
      f -> map (f:) r
  in Operation '+' $ map (Operation '*') $ foldr reducer [[]] $ map distribute f

distribute (Operation t f) = Operation t $ map distribute f
distribute x = x


parenthesize (Operation typ f) = "(" ++ (toString (Operation typ f)) ++ ")"
parenthesize a = toString a

toString (Operation '+' f) = 
  let plusSimple ff = case ff of
                        Operation '*' (Constant (-1) : rest) -> "- (" ++ toString (opMul rest) ++ ") "
                        o -> "+ " ++ toString o ++ " "
  in dropWhile (=='+') $ concatMap plusSimple f
toString (Operation '*' [x]) = toString x
toString (Operation '*' f) = intercalate "*" $ map parenthesize f
toString (Variable s) = "p_" ++ s
toString (Constant n) | n == fromInteger(round n) = show $ round n
toString (Constant n) = show n

data Sch = Or [Sch] | And [Sch] | Var String | Const Float deriving Show
parseSch = fst.parseAnd
parseAnd s =
  let (v,rr) = parseOr s in
    case rr of
      (' ':rrr) -> let (vv,b) = parseAnd rrr in (And [v,vv], b)
      _ -> (v,rr)

parseOr s =
  let (v,rr) = parseElem s in
    case rr of
      ('/':rrr) -> let (vv,b) = parseOr rrr in (Or [v,vv], b)
      _ -> (v,rr)

parseElem ('(':s) = let (a,b) = span (/=')') s in (parseSch a, drop 1 b)
parseElem s = case reads s of
                [(n,r)] | 0 < n && n < 1 -> (Const n, r)
                _ -> let (name, rest) = span isAlphaNum s
                     in (Var name, rest)

main = do
    interact
      (toString.simplify.toFormula.parseSch)
