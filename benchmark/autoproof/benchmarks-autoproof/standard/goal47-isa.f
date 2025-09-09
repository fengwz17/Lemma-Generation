Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Tree = node {Int, Tree, Tree} | leaf Unit | symtree Int;

max = (lambda x:Int. lambda y:Int.
    if (< x y) then y else x
);

height = 
fix (lambda f:Tree -> Int. lambda t:Tree.
    match t with
        leaf _ -> 0
      | node {x, l, r} -> (+ 1 (max (f l) (f r)))
    end
);

mirror = 
fix (lambda f:Tree -> Tree. lambda t:Tree.
    match t with
        leaf _ -> leaf unit
      | node {x, l, r} -> node {x, (f r), (f l)}
    end
);


lhs = \a:Tree. height (mirror a);
rhs = \a:Tree. height a;