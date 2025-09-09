Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;


interleave = 
fix (lambda f: List -> List -> List. lambda x: List. lambda y: List. 
match x with
  cons {z, xs} -> cons {z, (f y xs)}
| nil _ -> y
end
);

evens = 
fix (
lambda f:List->List.
lambda x:List.
    match x with
        nil _ -> x 
      | cons {y, xs} -> cons y (odds xs)
    end);

odds = fix (lambda f:List->List. lambda x:List.
    match x with
        nil _ -> x
        | cons {y, xs} -> evens xs 
    end
);


lhs = \xs:List. interleave (evens xs) (odds xs);
rhs = \xs:List. xs;