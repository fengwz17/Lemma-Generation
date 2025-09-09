Inductive List = nil Unit | cons {Int, List} | symlist Int;


mem = fix (
lambda f: Int -> List -> Bool.
\n: Int.
lambda l: List.
match l with
  nil _ -> false
| cons {h, t} -> if (== n h) then true else (f n t)
end);


cunion = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> if (mem h m) then (f m t) else cons {h, f m t}
| nil _ -> m
end
);

lhs = \xs: List.\ys: List. or (not (mem 42 xs)) (mem 42 (cunion xs ys));
rhs = \xs: List.\ys: List. true;


