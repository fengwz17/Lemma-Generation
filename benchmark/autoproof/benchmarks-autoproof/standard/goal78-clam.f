Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive List = nil Unit | cons {Nat, List} | symlist Int;
Inductive Llist = cnil List | ccons {Nat, List, Llist} | symLlist Int;


snoc = 
fix (lambda f: Nat -> List -> List. lambda m: Nat. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> cons {m, nil unit}
end
);

mapsnoc =
fix (\f: Nat -> Llist -> Llist.
\a: Nat.
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

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List.
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> snoc h (f t)
| nil _ -> nil unit
end
);

lhs = \l0: List. \l1: List. rev (exec_qrev (gen_qrev (rev l0) l1));
rhs = \l0: List. \l1: List. append l1 l0;
