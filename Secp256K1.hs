-- Magic numbers from http://www.secg.org/sec2-v2.pdf SS. secp256k1


-- The p from F_p
p = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1


-- The curve y^2 = x^3 + 7
isOnCurve x y = y^2 `mod` p == (x^3 + 7) `mod` p
computeY x = sqrt (x^3 + 7)


-- Generator values
gX = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
gY = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
g = (gX, gY, 1)


-- Double a point
-- "dbl-2009-l"
-- https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#doubling-dbl-2009-l
doubleJacobian (x, y, z) = (x' `mod` p, y' `mod` p, z' `mod` p)
    where
        a = x^2
        b = y^2
        c = b^2
        d = 2 * ((x + b)^2 - a - c)
        e = 3 * a
        f = e^2
        x' = f - 2 * d
        y' = e * (d - x') - 8 * c
        z' = 2 * y * z


-- Add two points
-- "add-1998-cmo"
-- https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-1998-cmo
addJacobians j1@(x1, y1, z1) j2@(x2, y2, z2) =
    if j1 == j2
        then doubleJacobian j1
        else (x3 `mod` p, y3 `mod` p, z3 `mod` p)
            where
                z1z1 = z1^2
                z2z2 = z2^2
                u1 = x1 * z2z2
                u2 = x2 * z1z1
                s1 = y1 * z2 * z2z2
                s2 = y2 * z1 * z1z1
                h = u2 - u1
                i = (2 * h)^2
                j = h * i
                r = 2 * (s2 - s1)
                v = u1 * i
                x3 = r^2 - j - 2 * v
                y3 = r * (v - x3) - 2 * s1 * j
                z3 = ((z1 + z2)^2 - z1z1 - z2z2) * h


-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
-- https://rosettacode.org/wiki/Modular_inverse#Haskell
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
 

-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then mkPos i else 0
  where mkPos x = if x < 0 then x + m else x


-- Jacobian to Affine coordinates conversion
fromJacobian (x, y, z) = (x' `mod` p, y' `mod` p)
    where
        x' = x * (modInv (z^2) p)
        y' = y * (modInv (z^3) p)


-- Perform k*G with p=G, n=k
scale p n 
    | n == 0    = (0, 0, 0)
    | n == 1    = p
    | even      = scale (doubleJacobian p) (n `div` 2)
    | otherwise = addJacobians p $ scale p (n - 1)
    where even = n `mod` 2 == 0
