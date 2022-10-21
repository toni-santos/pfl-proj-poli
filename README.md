# pfl-proj1

### Description

The goal of this project was to develop an haskell program able to normalize, add, multiply, derive polynomials and parsing polynomials from a string input.

### Representations

2 types were created with the purpose of representing polynomials and monomials, their components. This approach allowed us to have powerful simple structures in the form of monomials, which made facilitated the implementation of operations. Type aliases were also created for variables (`Var = Char`), coefficients and exponents (`type Coef = Int`, `type Exp = Int`), literals (`type Lit = (Var, Exp)`) and group of literals (`type Lits = [Lit]`). These aliases were created as to make code clearer and were not elevated into `datatype` or `data` as their are used throughout the code as simply lists/tuples/ints, without any need for implementing specific operations over them.

These approaches were chosen as they were the most similar to the mathematical definition of both of these concepts.

#### Monomials

The structure chosen for the representation of the monomial is:

`data Monomial = Monomial Coef Lits`

Which essentially allows us to have a representation of `(Coef, Lits)` and also get access to the properties of haskell tuples.

Monomials implement their own sum, multiplication and derivation. This effectively makes them independent parts of the polynomial which simplifies operations of the polynomial.

####  Polynomials

Polynomials are simply represented as a list of monomials:

`newtype Polynomial = Polynomial [Monomial] deriving (Eq)`

As previously stated the selection of this representation comes from its similarity to the mathematical definition of polynomial and monomial. This approach is also helpful as the operations between monomials, which may only be sum or subtraction are stored in the coefficient of each monomial, making normailzing/simplifying a polynomial a matter of matching literals of the monomials and summing their coefficients, whilst keeping the same literal. Multiplication between monomials is also a matter of multiplying all combinations between the monomials of each polynomial.

This simple representation of a polynomial in tandem with the powerful and robust monomial representation enunciated previously makes for an understandable and mathematically adequate implementation.

### Implementations

#### Normalization

Normalization of a polynomial is simply the sum of all monomials that are able to be summed, where as the normalization of a monomial entails, simplifying the literals and removing useless literals.

Relevant functions:
- `normPoly :: [Monomial] -> [Monomial]`
- `normMonom :: Monomial -> Monomial`
- `reduceLits :: Lits -> Lits`
- `fixLits :: Lits -> Lits`
- `removeUseless :: Lits -> Lits`

#### Addition/Subtraction

Addition of polynomials was implemented by mapping the addition of monomials for the list of monomials, which is exactly the definition of a polynomial. The latter is done by summing the coefficients and keeping the literals of one of the monomials, therefore this operation assumes that the monomials being summed are summable, that is, they have the same literals.

With this in mind, the implementation of polynomials had to be able to figure out which monomials should be summed, meaning that our final implementation of the sum of polynomials must sort and group together the monomials to be summed and then sum the groups of monomials previously found; this process is exactly the same as the one of normalizing a polynomial, therefore a sum of polynomials is simply the normalization of the 2 polynomials.

Subtraction is handled inherently by the operation of sum of monomials, since subtractions are treated the same way a sum would, `x + (-y)`, simply summing coefficients and keeping the literals.

Due to the nature of our implementation we had to create our own sum functions that would correctly process `[[Monomial]] -> [Monomial]`.

Relevant functions:
- `addPoly :: Polynomial -> Polynomial -> Polynomial`
- `addMonom :: Monomial -> Monomial -> Monomial`
- `normPoly :: [Monomial] -> [Monomial]`
- `sum' :: [[Monomial]] -> [Monomial]`
- `sum'' :: [Monomial] -> Monomial`

#### Multiplication

Multiplication of polynomials is based on the same principles of addition, that being that values from the polynomial are mapped to the multiplication of monomials. However, unlike addition, there is no need to worry about any conditions and we must simply multiply all combinations of monomials of the 2 operands. Multiplication of monomials is simple, concatenate the literals, summing the exponents when the same variable is present, and multiplying the coefficients by each other.

Just as with addition, we needed to implement our own product functions.

Relevant functions:
- `multiplyPoly :: Polynomial -> Polynomial -> Polynomial`
- `multiplyMonom :: Monomial -> Monomial -> Monomial`
- `calcMultCoef :: Lits -> Lits`
- `prod' :: [[Monomial]] -> [Monomial]`
- `prod' :: [Monomial] -> Monomial`


#### Derivation

Derivation of polynomials works just the same as the previous operations, mapping each monomial to its derivative. Seeing as derivation is an operation that is tightly related to monomials, as when applied to a polynomial the changes only affect certain monomials, which means that these are the basis of the operation, the main logic of a derivation is done in these units.

The derivation operation of a monomial is separated in two parts, the coefficient, where we simply multiply the current coefficient by the exponent of the current deriving variable, and the literals, these must be handled more carefully, as a derivation may leave a monomial without any variables and this must be taken into account within our programs logic. 

This implementation of derivation uses partial derivatives, meaning that when deriving a polynomial by a variable, monomials that do not have that variable in their literals will become 0.

Relevant functions:
- `derivePoly :: Polynomial -> Var -> Polynomial`
- `deriveMonom :: Monomial -> Var -> Monomial`
- `calcDervLits :: Lits -> Var -> Lits`
- `calcDervCoef :: Coef -> Lits -> Var -> Coef`


#### Parsing

Parsing of polynomials is assured by several functions that decompose a string into several monomials and aggregate them into a single polynomial.

This process starts by splitting the input string into by `+` and `-`, keeping in mind that `-` must be appended to the beginning of the next "string" monomial, as to store the subtraction operation. In this step `+`, ` `, `(`, `)` and `\n` characters are ignored, as to perserve only the necessary characters to construct the monomials.
Afterwards, with the newly created `[String]`, monomials are parsed, which requires some steps and much care, the coefficients are found and the literals are returned based on the circumstances, per example, the string `"x"` should have a coefficient of 1 and a literal composed of the literal x and the exponent 1, despite the fact none of this information is explicitly provided. It is important to note that at this point the notion of an "empty" literal is introduced, being represented by `[('_', 0)]`, which is used to signify 0 degree monomials/absence of variables.
With an initial set of monomials created and normalized, fixes on certain edge cases, p.e coefficient is 0, are done and the final polynomial is normalized.

Parsing a monomial works exactly the same way as a polynomial, except that in the end, as a monomial is a polynomial with exactly one element, the first element of the list is selected.

This parsing algorithm and method incurs on some limitations mainly in which strings we are able to accept, the following are not supported:
- `"2x² * 3y²"`, coefficients must always be at the beginning of the monomial and simplified, this restriction is in place as we are unable to determine the context in which `*` is being used and doing so escapes the scope of this assignment. Furthermore, in mathematics, a polynomial must not have multiplications between its monomials.

We should also note that in order to extend the `read` function of the Monomial and Polynomial classes and keep their simmetry, we also implemented `show` functions that are responsible for displaying polynomials and monomials in an easy to read fashion. These take into account several cases just like the parsing functions before them.

Relevant functions:
- `splitByList :: (String, [String], String) -> String -> [String]`
- `findMonomCoef :: String -> Coef`
- `findMonomExps :: String -> [Var]`
- `findMonomVars :: String -> [Var]`
- `findMonomLit :: String -> [Var]`
- `parseMonomLit :: (String, Lits) -> Lits`
- `parseMonom :: String  -> Monomial`
- `addLits :: Lits -> Lit`
- `fixLits :: Lits -> Lits`
- `reduceLits :: Lits -> Lits`
- `removeUseless :: Lits -> Lits`
- `createMonom :: Monomial -> Monomial`
- `parsePoly :: [String] -> [Monomial]`
- `parseStrPoly :: String -> Polynomial`
- `parseStrMonom :: String -> Monomial`
- (+ show functions)

### Examples

These examples are suited to be run on ghci:

> Testing Monomial creation
> > Representing 2x²
`Monomial 2 [('x', 2)]`
> > Representing 2x²y²
`Monomial 2 [('x', 2), ('y', 2)]`
> > Representing 2x⁴
`Monomial 2 [('x', 2), ('x', 2)]`

> Testing Monomial creation (from string)
> > Representing 2x²
`read "2*x^2"::Monomial`
`read "2x2"::Monomial`
> > Representing 2x²y²
`read "2*x^2*y^2"::Monomial`
`read "2x2y2"::Monomial`

> Testing Polynomial creation
> > Representing 2x²
`Polynomial [Monomial 2 [('x', 2)]]`
> > Representing 2x² + 2x²y²
`Polynomial [Monomial 2 [('x', 2), ('y', 2)] + Monomial 2 [('x', 2)]]`
> > Representing 4x²y² 
`Polynomial [Monomial 2 [('x', 2), ('y', 2)] + Monomial 2 [('x', 2), ('y', 2)]]`
> > Representing 4x⁴y⁴ 
`Polynomial [Monomial 2 [('x', 2), ('y', 2)] * Monomial 2 [('x', 2), ('y', 2)]]`

> Testing Polynomial creation (from string)
> > Representing 2x²
`read "2x^2"::Polynomial`
> > Representing 2x² + 2x²y²
`read "2x^2 + 2x2*y^2"::Polynomial`
> > Representing 4x²y² 
`read "2x2y2 + 2x2y2"::Polynomial`
> > Representing 4x⁴y⁴ 
`read "2x2*y2 * 2x2y^2"::Polynomial`

> Testing Monomial addition
> > Result 2x²
`(read "x2"::Monomial) + (read "x2"::Monomial)`

> Testing Monomial multiplication
> > Result 4x²
`(read "2x2"::Monomial) * (read "2x2"::Monomial)`

> Testing Monomial derivation
> > Result 2x
`(read "x2"::Monomial) \/ 'x'`

> Testing Polynomial addition
> > Result 2x² + 6y²
`(read "x2 + 3y2"::Polynomial) + (read "x2 + 3y2"::Polynomial)`
> > Result 4 + 2x - 4x² 
`(read "-2x2 + x + 2"::Polynomial) + (read "-2x2 + x + 2"::Polynomial)`
> > Result 2 + x + xy² - 2x³y² - 2y² + z
`(read "-2y2 + x + 2"::Polynomial) + (read "-2x3y2 + z + xy2"::Polynomial)`

> Testing Polynomial multiplication
> > Result 6x²y² + x⁴ + 9y⁴
`(read "x2 + 3y2"::Polynomial) * (read "x2 + 3y2"::Polynomial)`
> > Result 4 + 4x - 7x² - 4x³ + 4x⁴
`(read "-2x2 + x + 2"::Polynomial) * (read "-2x2 + x + 2"::Polynomial)`
> > Result 2 + x + xy² - 2x³y² - 2y² + z
`(read "-2y2 + x + 2"::Polynomial) * (read "-2x3y2 + z + xy2"::Polynomial)`

> Testing Polynomial derivation
> > Result 2x
`(read "x2 + 3y2"::Polynomial) \/ 'x'`
> > Result "" (Nothing)
`(read "-2x2 + x + 2"::Polynomial) \/ 'y'`
> > Result 1 - 4x²y
`(read "-2x2y2 + -2x + y"::Polynomial) \/ 'y'`
