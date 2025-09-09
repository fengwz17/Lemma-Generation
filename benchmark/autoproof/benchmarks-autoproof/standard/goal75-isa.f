Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

_to_int = (lambda x: Iint.
match x with
  cint a -> a
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


filter =
fix (lambda f: List -> List. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> if (< (_to_int h) 0) then cons {h, (f t)} else (f t)
end
);

lhs = \xs:List. rev (filter xs);
rhs = \xs:List. filter (rev xs);
