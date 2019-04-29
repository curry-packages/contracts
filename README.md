contract: Some auxiliaries for contract checking
================================================

This package contains libraries to deal with contracts as described
in this paper:

Sergio Antoy, Michael Hanus:
[Contracts and Specifications for Functional Logic Programming](http://dx.doi.or
g/10.1007/978-3-642-27694-1_4),
Proc. of the 14th International Symposium on Practical Aspects of
Declarative Languages (PADL 2012),
Springer LNCS 7149, pp. 33-47, 2012

* Library `Contract.Names` contains some operations to define and manipulate
  the names of contracts (i.e., specification and pre/postconditions)
  in a Curry program.
* Library `Contract.Usage` contains some operations to check the correct usage
  of contracts (i.e., the occurrences and types of specification and
  pre/postconditions) in a FlatCurry program.
* Library `Test.Contract` defines some auxiliaries to check
  contracts based on specifications or pre- and postconditions
  provided in a Curry module. It is used by the Curry preprocessor
  to translate contracts into dynamic assertions.

--------------------------------------------------------------------------
