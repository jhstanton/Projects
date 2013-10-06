{-
  Complex Numbers Algebra
-}

-- data type for complex numbers
data Complex = Comp Double Double

instance Show Complex where
  show (Comp r i) = concat [show r, " + ", show i, "i"]

-- helper functions to remove individual value from Complex
real (Comp r _) = r
imag (Comp _ i) = i

neg (Comp r i) = (Comp (- r) (- i))

add (Comp r1 i1) (Comp r2 i2) = (Comp (r1 + r2) (i1 + i2))
sub x y = add x (neg y)

mul (Comp r1 i1) (Comp r2 i2) = 
  (Comp (r1 * r2 - i1 * i2) (i1 * r2 + r1 * i2))
divide (Comp r1 i1) (Comp r2 i2) =
  (Comp ((r1*r2 + i1*i2)/(r2*r2 + i2*i2)) ((i1*r2 - r1*i2)/
                                           (r2*r2 + i2*i2)))
