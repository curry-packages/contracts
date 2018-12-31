setfunctions: encapsulate non-deterministic computations
========================================================

This package contains an implementation of set functions.
The general idea of set functions is described in:

> S. Antoy, M. Hanus: Set Functions for Functional Logic Programming
> Proc. 11th International Conference on Principles and Practice
> of Declarative Programming (PPDP'09), pp. 73-82, ACM Press, 2009

Intuition: If `f` is an n-ary function, then `(setn f)` is a set-valued
function that collects all non-determinism caused by f (but not
the non-determinism caused by evaluating arguments!) in a set.
Thus, `(setn f a1 ... an)` returns the set of all
values of `(f b1 ... bn)` where `b1`,...,`bn` are values
of the arguments `a1`,...,`an` (i.e., the arguments are
evaluated "outside" this capsule so that the non-determinism
caused by evaluating these arguments is not captured in this capsule
but yields several results for `(setn...)`.
Similarly, logical variables occuring in `a1`,...,`an` are not bound
inside this capsule (in PAKCS they cause a suspension until
they are bound).

The set of values returned by a set function is represented
by an abstract type 'Values' on which several operations are
defined in this module. Actually, it is a multiset of values,
i.e., duplicates are not removed.

The handling of failures and nested occurrences of set functions
is not specified in the previous paper. Thus, a detailed description
of the semantics of set functions as implemented in this library
can be found in the paper

> J. Christiansen, M. Hanus, F. Reck, D. Seidel:
> A Semantics for Weakly Encapsulated Search in Functional Logic Programs
> Proc. 15th International Conference on Principles and Practice
> of Declarative Programming (PPDP'13), pp. 49-60, ACM Press, 2013

Restrictions of the PAKCS implementation of set functions:

1. The set is a multiset, i.e., it might contain multiple values.
2. The multiset of values is completely evaluated when demanded.
   Thus, if it is infinite, its evaluation will not terminate
   even if only some elements (e.g., for a containment test)
   are demanded. However, for the emptiness test, at most one
   value will be computed
3. The arguments of a set function are strictly evaluated before
   the set functions itself will be evaluated.

--------------------------------------------------------------------------
