------------------------------------------------------------------------
--- This module contains some operations to check the correct usage of
--- contracts (i.e., the occurrences and types of specification and
--- pre/postconditions) in a FlatCurry program.
---
--- @author Michael Hanus
--- @version November 2020
------------------------------------------------------------------------

module Contract.Usage ( checkContractUsage ) where

import Data.List         ( (\\), find )

import FlatCurry.Goodies ( argTypes, resultType )
import FlatCurry.Types

import Contract.Names

--- Checks the intended usage of contracts, i.e., whether
--- contracts types correspond to types of functions.
--- The parameter are the module and the list of names and (FlatCurry) types
--- of all functions defined in this module.
--- The result is a list of error messages for qualified function names.
checkContractUsage :: String -> [(String,TypeExpr)] -> [(QName,String)]
checkContractUsage mn allopsforall =
  let allops       = map (\ (n,t) -> (n, stripForall t)) allopsforall
      allopsnames  = map fst allops
      specops      = map (\ (n,t) ->
                            (fromSpecName (decodeContractName n), t))
                         (filter (isSpecName . fst) allops)
      preops       = map (\ (n,t) ->
                            (fromPreCondName (decodeContractName n), t))
                         (filter (isPreCondName . fst) allops)
      postops      = map (\ (n,t) ->
                            (fromPostCondName (decodeContractName n), t))
                         (filter (isPostCondName . fst) allops)
      nonfailops   = map (\ (n,t) ->
                            (fromNonFailName (decodeContractName n), t))
                         (filter (isNonFailName . fst) allops)
      onlyprecond  = map fst preops     \\ allopsnames
      onlypostcond = map fst postops    \\ allopsnames
      onlyspec     = map fst specops    \\ allopsnames
      onlynonfail  = map fst nonfailops \\ allopsnames
      errmsg   = "No implementation for this "
      preerrs  = map (\ n -> ((mn, toPreCondName n), errmsg ++ "precondition"))
                     onlyprecond
      posterrs = map (\ n -> ((mn, toPostCondName n),errmsg ++ "postcondition"))
                     onlypostcond
      specerrs = map (\ n -> ((mn, toSpecName n), errmsg ++ "specification"))
                     onlyspec
      nferrs   = map (\ n -> ((mn, toNonFailName n),
                              errmsg ++ "non-fail condition"))
                     onlynonfail
   in preerrs ++ posterrs ++ specerrs ++ nferrs ++
      checkNonFailTypes  mn allops nonfailops ++
      checkPreTypes      mn allops preops ++
      checkPostTypes     mn allops postops ++
      checkSpecTypes     mn allops specops

checkNonFailTypes :: String -> [(String,TypeExpr)] -> [(String,TypeExpr)]
                  -> [(QName,String)]
checkNonFailTypes mn allops nfops = concatMap checkNonFailTypeOf nfops
 where
  checkNonFailTypeOf (n,t) =
    maybe (notFoundError "non-fail condition" (mn,n))
          (\ (_,ft) -> checkNonFailType n t ft)
          (find (\ (f,_) -> f == n) allops)

  checkNonFailType n pt ft
    | resultType pt /= TCons ("Prelude","Bool") []
    = illegalTypeError "Non-fail condition" (mn, toNonFailName n)
    | argTypes pt /= argTypes ft
    = wrongTypeError "non-fail condition" (mn, toNonFailName n)
    | otherwise
    = []
 
checkPreTypes :: String -> [(String,TypeExpr)] -> [(String,TypeExpr)]
              -> [(QName,String)]
checkPreTypes mn allops preops = concatMap checkPreTypeOf preops
 where
  checkPreTypeOf (n,t) =
    maybe (notFoundError "precondition" (mn,n))
          (\ (_,ft) -> checkPreType n t ft)
          (find (\ (f,_) -> f == n) allops)

  checkPreType n pt ft
    | resultType pt /= TCons ("Prelude","Bool") []
    = illegalTypeError "Precondition" (mn, toPreCondName n)
    | argTypes pt /= argTypes ft
    = wrongTypeError "precondition" (mn, toPreCondName n)
    | otherwise
    = []
 
checkPostTypes :: String -> [(String,TypeExpr)] -> [(String,TypeExpr)]
               -> [(QName,String)]
checkPostTypes mn allops postops = concatMap checkPostTypeOf postops
 where
  checkPostTypeOf (n,t) =
    maybe (notFoundError "postcondition" (mn,n))
          (\ (_,ft) -> checkPostType n t ft)
          (find (\ (f,_) -> f == n) allops)

  checkPostType n pt ft
    | resultType pt /= TCons ("Prelude","Bool") []
    = illegalTypeError "Postcondition" (mn, toPostCondName n)
    | argTypes pt /= argTypes ft ++ [resultType ft]
    = wrongTypeError "postcondition" (mn, toPostCondName n)
    | otherwise
    = []

checkSpecTypes :: String -> [(String,TypeExpr)] -> [(String,TypeExpr)]
               -> [(QName,String)]
checkSpecTypes mn allops specops = concatMap checkSpecTypeOf specops
 where
  checkSpecTypeOf (n,t) =
    maybe (notFoundError "specification" (mn,n))
          (\ (_,ft) -> checkSpecType n t ft)
          (find (\ (f,_) -> f == n) allops)

  checkSpecType n pt ft
    | pt /= ft
    = wrongTypeError "specification" (mn, toSpecName n)
    | otherwise
    = []

notFoundError :: String -> QName -> [(QName,String)]
notFoundError cond qn =
  [(qn, "Operation for " ++ cond ++ " not found!")]

illegalTypeError :: String -> QName -> [(QName,String)]
illegalTypeError cond qn = [(qn, cond ++ " has illegal type")]

wrongTypeError :: String -> QName -> [(QName,String)]
wrongTypeError cond qn = [(qn, "Type of " ++ cond ++ " does not match")]

-- Strip outermost `ForallType` since this quantification is not relevant
-- for our checks.
stripForall :: TypeExpr -> TypeExpr
stripForall texp = case texp of
  ForallType _ te  -> stripForall te
  _                -> texp

------------------------------------------------------------------------
