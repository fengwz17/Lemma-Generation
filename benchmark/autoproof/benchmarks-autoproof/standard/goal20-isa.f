Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;


_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);


less = (lambda a: Iint. lambda b: Iint.
(< (_to_int a) (_to_int b))
);

inserta = 
fix (lambda f: Iint -> List -> List. lambda n:Iint. lambda l:List.
    match l with
        nil _ -> cons {n, nil unit}
      | cons {h,t} -> if (less n h) 
                        then cons {n, cons {h, t}} 
                        else cons {h, f n t}
    end
);

sort = 
fix (lambda f:List -> List. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> inserta h (f t)
end
);

length = 
fix (lambda f: List -> Nat.
lambda l: List.
match l with
  cons {h, t} -> succ (f t)
| nil _ -> zero unit
end
);

lhs = \l:List. length (sort l);
rhs = \l:List. length l;