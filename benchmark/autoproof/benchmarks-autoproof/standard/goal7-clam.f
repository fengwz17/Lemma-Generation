Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;

plus = fix (lambda f: Nat -> Nat -> Nat.
lambda x: Nat.
\y: Nat.
match y with
  zero _ -> x
| succ y0 -> succ (f x y0)
end);

qrev = 
fix (\f: List -> List -> List.
\l1: List.
\l2: List.
match l2 with
  nil _ -> l1
| cons {h, t} -> f (cons {h, l1}) t
end);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);

lhs = \l0: List. \l1: List. (length (qrev l0 l1));
rhs = \l0: List. \l1: List. plus (length l0) (length l1);
