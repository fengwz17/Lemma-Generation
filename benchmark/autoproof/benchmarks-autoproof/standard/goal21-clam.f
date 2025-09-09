Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Cnat = czero List | csucc {List, Cnat} | symcnat Int;


len = fix (lambda f: List -> Nat.
lambda l: List.
match l with
  nil _ -> zero unit
| cons {h, t} -> succ (f t)
end);


snoc = 
fix (lambda f: Iint -> List -> List. lambda m: Iint. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> cons {m, nil unit}
end
);

rotonce = 
lambda l: List.
match l with
 nil _ -> nil unit
| cons {h, t} -> snoc h t
end;

maprot =
fix (\f: Cnat -> Cnat.
\cn: Cnat.
match cn with
  czero l0 -> czero (rotonce l0)
| csucc {c0, ct} -> csucc {rotonce c0, f ct}
end);


gen_rotate =
fix (\f: List -> Nat -> Cnat.
\l: List.
\n: Nat.
match n with
   zero _ -> czero l
|  succ n0 -> csucc {l, maprot (f l n0)}
end);

exec_rotate =
fix (lambda f: Cnat -> List. 
lambda cn:Cnat.
match cn with
  czero a -> a
| csucc {c0, cn0} -> f cn0
end
);

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);



lhs = \xs: List. \ys: List. exec_rotate (gen_rotate (append ys xs) (len xs));
rhs = \xs: List. \ys: List. append xs ys;