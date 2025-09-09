Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;

_to_int = (lambda x: Iint.
match x with
  cint a -> a
end
);

sum = fix (
lambda f: Int->Int.
lambda x: Int.
    if (== 0 x) then 0 else (+ (f (- x 1)) x)
);

cubes = 
fix (
lambda f:Int->Int.
lambda x:Int.
    if (== 0 x) then 0 else (+ (f (- x 1)) (* (* x x) x))
);

lhs = \n:Iint. cubes (_to_int n);
rhs = \n:Iint. (* (sum (_to_int n)) (sum (_to_int n)));