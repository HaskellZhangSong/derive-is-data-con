# derive-is-data-con

This package will generate data constructor predicate functions

```haskell
    derive_is ''Bool
  ======>
    isFalse :: Bool -> Bool
    isTrue :: Bool -> Bool
    isFalse False = True
    isFalse _ = False
    isTrue True = True
    isTrue _ = False
```

Above code will generate isTrue and isFalse functions. The function is 'is' + data constructor. 

For operators, there is a function that translates to string

```haskell
charToString :: Char -> String
charToString '~' = "Tilde"
charToString '!' = "Bang"
charToString '@' = "At"
charToString '#' = "Hash"
charToString '$' = "Dollar"
charToString '%' = "Percent"
charToString '^' = "Caret"
charToString '&' = "And"
charToString '*' = "Star"
charToString '-' = "Minus"
charToString '+' = "Plus"
charToString '=' = "Equal"
charToString '|' = "Pipe"
charToString '\\' = "Backslash"
charToString '/' = "Slash"
charToString '<' = "Lt"
charToString '>' = "Gt"
charToString ':' = "Colon"
charToString '?' = "Question"
charToString '.' = "Dot"
-- charToString '[' = "LSquareBracket" -- I do not want to support derive_is ''[]. One should use null function instead.
-- charToString ']' = "RSquareBracket"
```

For example `(:=:)` will be translated into `isColonStarColon` function

For gadt data type

```haskell
data Gadt a b where
    (:*:), (:=:)  :: a -> b -> Gadt a b
```

The following code will be generated:

```haskell
    derive_is ''Gadt 
  ======>
    isColonStarColon :: Gadt a_a54E a_a54F -> Bool
    isColonEqualColon :: Gadt a_a54G a_a54H -> Bool
    isColonStarColon ((:*:) _ _) = True
    isColonStarColon _ = False
    isColonEqualColon ((:=:) _ _) = True
    isColonEqualColon _ = False
```