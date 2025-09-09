Inductive Iint = cint Int | symiint Int;
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

lhs = \n:Bin. bin_to_int (s n);
rhs = \n:Bin. + 1 (bin_to_int n);
