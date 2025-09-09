Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;

x = 
fix (lambda f: List -> List -> List. lambda l: List. lambda m: List. 
match l with
  cons {h, t} -> cons {h, (f t m)}
| nil _ -> m
end
);

y = 
fix (
lambda f:List -> (Iint -> List) -> List.
lambda z:List.
lambda y2: (Iint -> List).
    match z with
        nil _ -> nil unit
      | cons {z2, xs} -> x (y2 z2) (f xs y2)
    end
);

lhs = \m:List. \f:Iint->List. \g:Iint->List. y (y m f);
rhs = \m:List. \f:Iint->List. \g:Iint->List. y m (\z:Iint. y (f z) g);