Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiInt Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Llist = cnil List | ccons {Iint, List, Llist} | symLlist Int;

nat_to_int = fix (lambda f: Nat -> Int.
lambda x: Nat.
match x with
  zero _ -> 0
| succ y -> + 1 (f y)
end);


snoc = 
fix (lambda f: Iint -> List -> List. lambda m: Iint. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> cons {m, nil unit}
end
);

mapsnoc =
fix (\f: Iint -> Llist -> Llist.
\a: Iint.
\cl: Llist.
match cl with
  cnil l0 -> cnil (snoc a l0)
| ccons {ch, c0, ct} -> ccons {ch, snoc a c0, f a ct}
end);


gen_qrev = 
fix (\f: List -> List -> Llist.
\a: List.
\l: List.
match l with
  nil _ -> cnil a
| cons {h, t} -> ccons {h, a, mapsnoc h (f a t)}
end);

exec_qrev = 
fix (\f: Llist -> List.
\cl: Llist.
match cl with
  cnil a -> a
| ccons {h, h0, t} -> f t
end);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> snoc h (f t)
| nil _ -> nil unit
end
);

lhs = \l1: List.exec_qrev (gen_qrev (nil unit) (exec_qrev (gen_qrev (nil unit) l1)));
rhs = \l1: List. l1;
