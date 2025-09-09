Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

len = fix (lambda f: List -> Nat.
lambda l: List.
match l with
  nil _ -> zero unit
| cons {h, t} -> succ (f t)
end);

less = fix(\f: Nat -> Nat -> Bool.
\n: Nat.
\m: Nat.
match {n, m} with
  {zero _, zero _} -> false
| {zero _, succ m0} -> true
| {succ n0, zero _} -> false
| {succ n0, succ m0} -> f n0 m0
end);

ins = 
fix (\f: Nat -> List -> List.
\n: Nat.
\a: List.
match a with
  nil _ -> cons {n, nil unit}
| cons {h, t} -> if (less n h) then cons {n, cons {h, t}} else cons {h, f n t} 
end);

sort = 
fix (\f: List -> List. 
\xs: List.
match xs with
  nil _ -> nil unit
| cons {h, t} -> ins h (f t)
end);

lhs = \xs: List. len xs;
rhs = \xs: List. len (sort xs);