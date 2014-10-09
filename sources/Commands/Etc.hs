module Commands.Etc where


-- | like @maybe@ or @either@
-- <https://hackage.haskell.org/package/bool-extras-0.4.0/docs/src/Data-Bool-Extras.html#bool>
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

-- | smart constructor factory
-- makes a @smart@ constructor for a newtype @b@ over type @a@
-- the @input@ must satisfy the @predicate@ to reach the @constructor@  
smart :: (a -> String) -> (a -> b) -> (a -> Bool) -> (a -> b)
smart messenger constructor predicate input = bool (error (messenger input)) (constructor input) (predicate input)
