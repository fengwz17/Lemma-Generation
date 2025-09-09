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

isnil = 
(lambda l:List.
    match l with
        nil _ -> true
      | cons {h, t} -> false
    end
);

butlast = 
fix (lambda f:List -> List. lambda l:List.
    match l with
        nil _ -> nil unit 
      | cons {h, t} -> if (isnil t) then (nil unit) else (cons {h, (f t)})
    end
);


lhs = \xs:List. \x:Iint. butlast (append xs (cons {x, nil unit}));
rhs = \xs:List. \x:Iint. xs;