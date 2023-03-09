module JavaScript where
import Data.List(intercalate)

-- # JavaScript AST

-- Variables and identifiers are strings
type Id = String
-- Operators are represented as string
type Op = String

-- Representation of JavaScript expressions
data JSExpr =
   Function [Id] JSBlock
   -- ^ a function binds a list of variables into the scope of a JSBlock
   -- e.g., JavaScript code `function (x, y) { block }`
   -- is represented as
   --    Function ["x", "y"] blockRepresentation

  | Var Id
  -- ^ A variable, e.g., a variable x is represented as (Var "x")

  | BinOp Op JSExpr JSExpr
  -- ^ A binary operation
  --  e.g. x + y is represented as
  -- BinOp "+" (Var "x") (Var "y")

  | Num Integer
  -- ^ Numerical constant
  -- e.g., 42 is represented as
  -- Num 42

  | JString String
  -- ^ String constant

  | Apply JSExpr [JSExpr]
  -- ^ Function application
  -- e.g., f(x, 0) is represented as
  -- Apply (Var "f") [(Var "x", Num 0)]

  | Property Id Id
  -- ^ Property getter
  -- e.g., object.fst is represented as
  -- Property "object" "fst"

  | List [JSExpr]
  -- ^ List constant
  -- e.g., [1,2,3] is represented as
  -- List [Num 1, Num 2, Num 3]

  | Object [(Id, JSExpr)]
  -- ^ Object constructor (also can be thought as making a dictionary in JS)
  -- e.g., {fst: 42, snd: 10}
  -- is represented as
  -- Object [("fst", Num 42), ("snd", Num 10)]

  | Null
  -- ^ Represents the null constant
  -- e.g. null is represented as
  -- Null
  deriving Show

data JSStmt =
     Decl Id JSExpr                                      -- Declaration statement
     -- e.g., `var x = 42` represented as
     -- Decl "x" (Num 42)

   | IfThenElse JSExpr JSBlock JSBlock
   -- e.g., `if (guard) then { block1 } else { block2 }
   -- represented as
   -- IfThenElse guarRepr block1repr block2repr
  deriving Show

data JSBlock =
     Next JSStmt JSBlock
     -- ^ Represents sequential composition of a statement before a block
     -- e.g., var x = 42; block   is
     -- `Next (Decl "x" (Num 42)) ...` then the representation of the lbock
   | Return JSExpr
     -- ^ Represents a return statement at the end of a block
   | Nil
     -- ^ End of a block with no return.
  deriving Show

-- Helpers
-- Create a pair object
jsPair :: JSExpr -> JSExpr -> JSExpr
jsPair e1 e2 = Object [("fst", e1), ("snd", e2)]

-- # Compilation functions for JavaScript AST

compileExpr :: JSExpr -> String
compileExpr Null = "null"
compileExpr (Var x) = x
compileExpr (Num n) = show n
compileExpr (Function names block) =
  "function(" ++ intercalate "," names ++ "){ " ++ compileBlock block ++ "}"
compileExpr (BinOp op e1 e2) =
  "(" ++ compileExpr e1 ++ " " ++ op ++ " " ++ compileExpr e2 ++ ")"
compileExpr (Apply e args) =
  compileExpr e ++ "(" ++ (intercalate "," (map compileExpr args)) ++ ")"
compileExpr (Object attrs) =
  "{" ++ intercalate "," (map (\(id, e) -> id ++ ": " ++ compileExpr e) attrs) ++ "}"
compileExpr (Property id1 id2) =
  id1 ++ "." ++ id2
compileExpr (JString s) = "\"" ++ s ++ "\""
compileExpr (List js) = "[" ++ intercalate "," (map compileExpr js) ++ "]"

compileStmt :: JSStmt -> String
compileStmt (Decl x expr) = "var " ++ x ++ " = " ++ compileExpr expr
compileStmt (IfThenElse e b1 b2) =
  "if (" ++ show e ++ "){ "
    ++ compileBlock b1
    ++ "} else {"
    ++ compileBlock b2 ++ "}"

compileBlock :: JSBlock -> String
compileBlock Nil = ""
compileBlock (Return e) = "return " ++ compileExpr e ++ ";"
compileBlock (Next s b) = compileStmt s ++ ";\n" ++ compileBlock b

