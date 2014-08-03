# simplebool

The pure simply typed lambda calculus (with Booleans).

## Examples

Well-typed examples:

* `ex1.sb` - Top-level Boolean literal
* `ex2.sb` - That other Boolean literal
* `ex3.sb` - Simple application on a Boolean
* `ex4.sb` - Simple conditional
* `ex5.sb` - More lambda application
* `ex6.sb` - Simple application on a lambda abstraction
* `ex7.sb` - Nested applications of lambda abstractions

Ill-typed examples:

* `ex8.sb` - AppArrowTypeExpected
* `ex9.sb` - IfGuardNotBool
* `ex10.sb` - ArrowParamTypeMismatch
* `ex11.sb` - IfArmsTypeMismatch
