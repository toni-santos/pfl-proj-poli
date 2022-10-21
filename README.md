# pfl-proj1

### Description

The goal of this project was to develop an haskell program able to normalize, add, multiply, derive polynomials and parsing polynomials from a string input, despite not being the main goal.

### Representations

2 types were created with the purpose of representing polynomials and monomials, their components. This approach allowed us to have powerful simple structures in the form of monomials, which made facilitated the implementation of operations. Type aliases were also created for variables (`Var = Char`), coefficients and exponents (`type Coef = Int`, `type Exp = Int`), literals (`type Lit = (Var, Exp)`) and group of literals (`type Lits = [Lit]`). These aliases were created as to make code clearer and were not elevated into `datatype` or `data` as their are used throughout the code as simply lists/tuples/ints, without any need for implementing specific operations over them.

These approaches were chosen as they were the most similar to the mathematical definition of both of these concepts.

#### Monomials

The structure chosen for the representation of the monomial was:

`data Monomial = Monomial Coef Lits`

Which essentially allows us to have a representation of `(Coef, Lits)` and also get access to the properties of haskell tuples.

Monomials implement their own sum, multiplication and derivation. This effectively makes them independent parts of the polynomial which simplifies operations of the polynomial.

####  Polynomials

Polynomials are simply represented as a list of monomials:

`newtype Polynomial = Polynomial [Monomial] deriving (Eq)`

As previously stated the selection of this representation comes from its similarity to the mathematical definition of polynomial and monomial. This approach is also helpful as the operations between monomials, which may only be sum or subtraction are stored in the coefficient of each monomial, making normailzing/simplifying a polynomial a matter of matching literals of the monomials and summing their coefficients, whilst keeping the same literal. Multiplication between monomials is also a matter of multiplying all combinations between the monomials of each polynomial.

This simple representation of a polynomial in tandem with the powerful and robust monomial representation enunciated previously makes for an understandable and mathematically adequate implementation.

### Implementations

#### Addition/Subtraction
#### Multiplication
#### Derivation
#### Parsing

### Examples
