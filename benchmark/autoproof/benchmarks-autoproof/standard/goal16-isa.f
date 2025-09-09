Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

append = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
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

isnil = 
(lambda l:List.
    match l with
        nil _ -> true
      | cons _ -> false
    end
);

last =
fix (lambda f:List -> Iint. lambda l:List. 
    match l with
        nil _ -> cint 0
      | cons {h, t} -> if (isnil t) then h else (f t)
    end 
);

lhs = \x:Iint. last (cons {x, nil unit});
rhs = \x:Iint. x;