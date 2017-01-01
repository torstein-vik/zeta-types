module DynamicMonomials where

import TannakianSymbols

{-data dynamicMonomial m = Monomial m (Polynomial Integer)

instance (CMult m) => CMult (dynamicMonomial m) where
    e = Monomial e e
    Monomial coeff1 exp1 * Monomial coeff2 exp2 = Monomial (coeff1 * coeff2) (exp1 + exp2)
    
instance (Eq m) => Eq (dynamicMonomial m) where
    Monomial coeff1 exp1 == Monomial coeff2 exp2 = coeff1 == coeff2 && exp1 == exp2
    
{-instance (Read m) => ProtoRead (dynamicMonomial)
    pRead string = -}-}