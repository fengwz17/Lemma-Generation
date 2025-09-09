Inductive Iint = cint Int | symiint Int;
Inductive Nat = zero Unit | succ Nat | symnat Int;
Inductive Bin = one Unit | zeroand Bin | oneand Bin | symbin Int;

s = 
fix (
lambda f:Bin -> Bin.
lambda x : Bin.
    match x with
        one _ -> zeroand x
      | zeroand xs -> oneand xs
      | oneand ys -> zeroand (f ys)
    end
);

plus2 = 
fix (
lambda f: Bin -> Bin -> Bin.
lambda x: Bin.
lambda y:Bin.
    match x with
        one _ -> (s y)
      | zeroand z ->
          match y with
            one -> (s x)
          | zeroand ys -> zeroand (f z ys)
          | oneand xs -> oneand (f z xs)
          end
      | oneand x2 ->
          match y with
            one _ -> (s x)
          | zeroand zs -> oneand (f x2 zs)
          | oneand ys2 -> zeroand (s (f x2 ys2))
          end
end
);

times =
fix (
lambda f: Bin -> Bin -> Bin.
lambda x: Bin.
lambda y:Bin.
    match x with 
        one _ -> y
      | zeroand xs1 -> zeroand (f xs1 y)
      | oneand xs12 -> plus2 (zeroand (f xs12 y)) y
    end
);

plus = 
fix (
lambda f: Bin -> Bin -> Bin.
lambda x: Bin.
lambda y:Bin.
    match x with
        one _ -> (s y)
      | zeroand z ->
          match y with
            one -> (s x)
          | zeroand ys -> zeroand (f z ys)
          | oneand xs -> oneand (f z xs)
          end
      | oneand x2 ->
          match y with
            one _ -> (s x)
          | zeroand zs -> oneand (f x2 zs)
          | oneand ys2 -> zeroand (s (f x2 ys2))
          end
end
);


bin_to_int = 
fix (
lambda f: Bin -> Int.
lambda x: Bin.
match x with
    one _ -> 1
  | zeroand xs -> + (f xs) (f xs)
  | oneand ys -> + (+ 1 (f ys)) (f ys)
end
);


lhs = \x:Bin. \y:Bin. bin_to_int (plus x y);
rhs = \x:Bin. \y:Bin. + (bin_to_int x) (bin_to_int y);