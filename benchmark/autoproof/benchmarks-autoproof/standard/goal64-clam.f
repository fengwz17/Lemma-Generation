Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

len = 
fix (\f: List -> Nat.
\n: List.
match n with
  nil _ -> zero unit
| cons {h, t} -> succ (f t)
end);

even = 
fix (\f: Nat -> Bool.
\n: Nat.
match n with
  zero _ -> true
| succ n0 -> not (f n0)
end);


lhs = \a: Nat. \b: Nat. \l0: List. \l1: List. even (len (append l0 l1));
rhs = \a: Nat. \b: Nat. \l0: List. \l1: List. even (len (append (cons {a, cons {b, l0}}) l1));