Inductive Nat = zero Unit | succ Nat | symnat Int;
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

count = 
fix (lambda f: List -> Int.
lambda l: List.
match l with
  cons {h, t} -> + (eq h (cint 0)) (f t)
| nil _ -> 0
end
);

snoc = 
fix (lambda f: Iint -> List -> List. lambda m: Iint.  lambda l: List. 
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> cons {m, nil unit}
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> snoc h (f t)
| nil _ -> nil unit
end
);

lhs = \l:List. count l;
rhs = \l:List. count (rev l);
