Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

drop =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match {n, l} with
    {zero _, nil _} -> nil unit
  | {zero _, cons {h, t}} -> cons {h, t}
  | {succ n0, nil _} -> nil unit
  | {succ n0, cons {h, t}} -> f n0 t
end
);

lhs = \x: Nat.\y: Nat. \xs: List. \n: Nat. drop x (drop y xs);
rhs = \x: Nat.\y: Nat. \xs: List. drop y (drop x xs);