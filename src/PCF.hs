{- PCF.hs
   ======
   Defines syntax and semantics of PCF. -}

{- TODO:

(i) Define Term datatype
    - Make Term an instance of Foldable
    - Pretty print

(ii) Define Type datatype
    - Make Type an instance of Foldable
    - Pretty print

(iii) Define typechecker
    - Use a transformer stack: either + reader

(iv) Define evaluator
    - Use a transformer stack: either + reader

NOTE: Try and use combinators to design these components. Avoid explicit recursion
      where possible. 
-}
