### Definition Jargon: 

- Pure Functional Language
- Lazy (Not evaluated until asked to)
- Statically Typed

Pure - 

- Everything is immutable 
- No **side-effects**

What does it mean to be a functional language? 

- Evaluating Expressions - over - Executing Instructions
- ????? Functions are first-class, that is, functions are values that can be used in exactly the same ways as any other sort of values ???????

????? Advantages - ??????

- **Equational reasoning** & **Refactoring**
- **Parallelism**
- **No side-effects**



### Comments: 

```haskell
-- This is a comment 
{-
	This is a multiline Comment
}
```

**No in-line comment alongside code (unlike python)**

### Variable Declarations: 

```haskell
x1 :: Int
x2 :: Integer
x3 :: Double
x4 :: Bool
x5 :: Char
x6 :: String
```



### GHCi: 

- **:?** ; for listing out all the list of commands

### Arithmetic: 

- No implicit type conversion 
  - That is if you want to add a *double* and an *integer* type you have to convert the integer type to a double, and then you can add those two.
  - fromIntegral, fromDouble ... etc. 

- Backticks (`) makes an operator **infix** -

  ```haskell
  ex1 = mod 42 7
  ex2 = 42 `mod` 7
  ```

### Boolean Logic:

> not, &&, ||, ==, /= (not equal), <=, >=, >, <

- Unary logical operator - 
  **if b then t else f**

### Defining Basic Functions: 

#### Guards: 

```haskell
hailstone :: Int -> Int 
hailstone n
 | n `mod` 2 == 0 = n `div` 2
 | otherwise = 3*n + 1
```

The clauses are checked from top to bottom, and if the lhs is true then rhs is evaluated and the evaluation ends for the guard block. 

#### Multiple Arguments: 

Func :: Arg1Type -> Arg2Type -> Arg3Type ... -> ResultType

### More Data Types:

#### Pairs/Tuples: 

```haskell
p :: (Int, Char)
```

#### Lists: 

```haskell
s1,s2,s3 :: [Int]
s1 = [2,3,5,7,11]
s2 = [1..100]
s3 = [0,7..100]
```

* Includes the extremes.

##### Constructing Lists: 

- **Cons** operator (:) -
  - 1 : [] ; Adds 1 to the empty list.
  - 1 : 2 : 3 : [] ; Adds 1,2 and 3 to the list consecutively.

##### Passing a list to a functions:

- Argument ->**(x:xs)** ; (x is the head, xs is the tail [tail being everything apart from the head]) ; Here the list is being sent using the cons operator ; x is the first element of the list and xs is the rest of the list (***x & xs are just names ; syntactically the first variable references to the first element and the second to the rest of the list***). This can be extended to reference to multiple elements at the beginning of the list **(a1:a2:a3:_)** ; a1, a2, a3 are the first 3 elements of the list & the underscore serves a similar purpose as that in python.

##### Extend list: (Concatenation)

- ++ ; "Hello " ++ "World!!" ; It will print "Hello World!!".



You don't always have to type-initialize a function. You can just define the function for what it does, and the interpreter will typecast it accordingly. For example -

> func x y = x + y
>Interpreter: func :: Int -> Int -> Int  