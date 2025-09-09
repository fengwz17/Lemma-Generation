Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

eq = (lambda a: Iint.
lambda b: Iint.
if (== (_to_int a) (_to_int b)) then 1 else 0
);

append = 
fix (lambda f: List -> List -> List.  lambda m: List. lambda l: List. 
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

count = 
fix (lambda f: Iint -> List -> Int.
lambda x: Iint.
lambda l: List.
match l with
  cons {h, t} -> + (eq h x) (f x t)
| nil _ -> 0
end
);


lhs = \n: Iint. \l: List. \m: List. + 1 (count n l);
rhs = \n: Iint. \l: List. \m: List. count n (cons {n, l});
