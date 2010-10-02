module Vec3 where

type Precision = Float

data Vec3  = Vec3 Precision Precision Precision
	deriving(Show, Eq)
instance Num Vec3 where
		(+) (Vec3 a b c) (Vec3 x y z)=Vec3 (a+x) (b+y) (c+z)
		(-) (Vec3 a b c) (Vec3 x y z)=Vec3 (a-x) (b-y) (c-z)
		(*) (Vec3 a b c) (Vec3 x y z)=Vec3 (a*x) (b*y) (c*z)
		abs v = Vec3 l l l
			where l = len v
		signum = id
		fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)
	
(%%) :: Vec3 -> Precision -> Vec3
(%%) (Vec3 x y z) s = Vec3 (x/s) (y/s) (z/s)

(^-) :: Vec3 -> Vec3
(^-) (Vec3 x y z) = Vec3 (1/x) (1/y) (1/z)   

(***) :: Vec3 -> Precision -> Vec3
(***) (Vec3 x y z) p = Vec3 (x*p) (y*p) (z*p)

(*.) :: Vec3 -> Vec3 -> Precision
(*.) (Vec3 x y z) (Vec3 a b c) = (x*a) + (y*b) + (z*c)

(*#) :: Vec3 -> Vec3 -> Vec3
(*#) (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) = Vec3 (a2*b3 - a3*b2) (a3*b1 -a1*b3) (a1*b2 - a2*b1)

len'2:: Vec3 -> Precision
len'2 (Vec3 x y z) = x*x + y*y + z*z 

len = sqrt . len'2

normalize :: Vec3 -> Vec3
normalize v = v *** q
	where q = 1.0 / (len v)
