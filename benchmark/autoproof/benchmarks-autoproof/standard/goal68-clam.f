Inductive List = nil Unit | cons {Int, List} | symlist Int;

len = fix (lambda f: List -> Int.
lambda l: List.
match l with
  nil _ -> 0
| cons {h, t} -> + 1 (f t)
end);

ins = 
fix (\f: Int -> List -> List.
\n: Int.
\a: List.
match a with
  nil _ -> cons {n, nil unit}
| cons {h, t} -> if (<= n h) then cons {n, cons {h, t}} else cons {h, f n t} 
end);


lhs = \xs: List. len (ins 5 xs);
rhs = \xs: List. + 1 (len xs);