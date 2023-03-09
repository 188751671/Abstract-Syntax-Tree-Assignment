# v1.0.1p

- No change to the brief, but a slight change to pack.zip in JavaScript.hs:
the `compileExpr` function now puts parentheses around compiled BinOps, e.g.
`compileExpr (BinOp "+" (Num 1) (Num 2)) = "(1 + 2)"`. This should not
affect any solutions, but is slightly nicer for experimentation.

# v1.0.1

- Added a little more explanation on the Room representation on p.1
  and in Task 1 (a).

# v1.0.0

- Initial version