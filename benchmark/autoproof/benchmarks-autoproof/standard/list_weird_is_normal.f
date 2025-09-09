Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Iint = cint Int | symiint Int;
Inductive List = nil Unit | cons {Iint, List} | symlist Int;
Inductive Llist = nill Unit | consl {List, Llist} | symllist Int;

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

map =
fix (lambda f: (Iint -> List) -> List -> Llist. lambda lf: Iint-> List. lambda l:List.
match l with
    nil _ -> nill unit
  | cons {h, t} -> consl {(lf h), (f lf t)}
end
);

concat =
fix (
lambda f:Llist->List.
lambda z:Llist.
  match z with
      nill _ -> nil unit
    | consl {y2, xs} -> x y2 (f xs)
  end
);

lmap =
fix (lambda f: (Iint -> Iint) -> List -> List. lambda lf: Iint-> Iint. lambda l:List.
match l with
    nil _ -> nil unit
  | cons {h, t} -> cons {(lf h), (f lf t)}
end
);

weirdconcat = 
fix (
lambda f:Llist->List.
lambda z:Llist.
    match z with
        nill _ -> nil unit
      | consl {y2, xss} -> 
            match y2 with
                nil _ -> (f xss)
              | cons {z2, xs} -> cons {z2, (f (consl {xs, xss}))}
            end
    end
);

lhs = \y:Llist. concat y;
rhs = \y:Llist. weirdconcat y;