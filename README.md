# mafi-lambda-calculus

University project in Haskell. LC term reducing is not feature-complete, nor is it bug-free. Just as a heads up. I did not invest much time into it. :)

The parser, including the lambda calculus quasi quoter (see [here](https://wiki.haskell.org/Quasiquotation) for what quasi quotation is) should be complete and is possibly bug-free - the test suite is not comprehensive however.

This project enables you to do things like this in your source code:

```haskell
[λ| (λx. x λy. x y) 5 |]
```

which during design/compile time evaluates to (and is therefore equivalent to writing):

```haskell
App (Abs "x" (App (Var "x") (Abs "y" (App (Var "x") (Var "y"))))) (Var "5")
```

Cool, right?
