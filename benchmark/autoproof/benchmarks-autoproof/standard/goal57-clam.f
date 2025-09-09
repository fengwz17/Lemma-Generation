Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

drop =
fix (lambda f: Nat -> List -> List. lambda n:Nat. lambda l:List.
match {n, l} with
    {zero _, nil _} -> nil unit
  | {zero _, cons {h, t}} -> cons {h, t}
  | {succ n0, nil _} -> nil unit
  | {succ n0, cons {h, t}} -> f n0 t
end
);

lhs = \x: Nat.\y: Nat.\z: Nat. \xs: List. drop (succ x) (drop y (drop (succ z) (cons {zero unit, cons {zero unit, xs}})));
rhs = \x: Nat.\y: Nat.\z: Nat. \xs: List. drop x (drop y (drop z xs));