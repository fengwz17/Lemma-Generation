Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Tree = leaf Iint | bin {Tree, Tree} | symtree Int;

append = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List. 
match l with
  cons {h, t} -> cons {h, (f m t)}
| nil _ -> m
end
);

len = 
fix (lambda f: List -> Int.
lambda l: List.
match l with
  cons {h, t} -> + 1 (f t)
| nil _ -> 0
end
);

_to_int = \x: Iint. 
  match x with
    cint a -> a
  end;

sum = 
fix (lambda f: List -> Int.
lambda l: List.
match l with
  cons {h, t} -> + (_to_int h) (f t)
| nil _ -> 0
end
);


count = 
fix (lambda f: List -> Int.
lambda l: List.
match l with
  cons {h, t} -> if >= (_to_int h) 0 then + 1 (f t) else (f t)
| nil _ -> 0
end
);

flatten = fix (\f: Tree -> List.
\t: Tree.
match t with
  leaf a -> cons {a, (nil unit)}
| bin {xl, xr} -> append (f xl) (f xr)
end);

sumtree = fix (\f: Tree -> Int.
\t: Tree.
match t with
  leaf a -> _to_int a
| bin {xl, xr} -> + (f xl) (f xr)
end);

lentree = fix (\f: Tree -> Int.
\t: Tree.
match t with
  leaf a -> 1
| bin {xl, xr} -> + (f xl) (f xr)
end);

counttree = fix (\f: Tree -> Int.
\t: Tree.
match t with
  leaf a -> if >= (_to_int a) 0 then 1 else 0
| bin {xl, xr} -> + (f xl) (f xr)
end);

rev = fix (lambda f: List -> List. lambda l: List.
match l with
  cons {h, t} -> append (cons {h, nil unit}) (f t)
| nil _ -> nil unit
end
);


less = (lambda a: Iint. lambda b: Iint.
(< (_to_int a) (_to_int b))
);

linsert = 
fix (lambda f: Iint -> List -> List. lambda n:Iint. lambda l:List.
    match l with
        nil _ -> cons {n, nil unit}
      | cons {h,t} -> if (< (_to_int n) (_to_int h)) 
                        then cons {n, cons {h, t}} 
                        else cons {h, (f n t)}
    end
);

sort = 
fix (lambda f:List -> List. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> linsert h (f t)
end
);

insapp = 
fix (lambda f: List -> List -> List. lambda m: List. lambda l: List. 
match l with
  cons {h, t} -> linsert h (f m t)
| nil _ -> m
end
);

tsort = fix(\f: Tree -> List.
\t: Tree.
match t with
  leaf a -> cons {a, nil unit}
| bin {xl, xr} -> insapp (f xl) (f xr)
end);

lhs = \l0: Tree. count (tsort l0);
rhs = \l0: Tree. counttree l0;