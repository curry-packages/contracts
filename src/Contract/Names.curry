------------------------------------------------------------------------
--- This module contains some operations to define and manipulate
--- the names of contracts (i.e., specification and pre/postconditions)
--- in a Curry program.
---
--- @author Michael Hanus
--- @version April 2019
------------------------------------------------------------------------

module Contract.Names
  ( isSpecName, toSpecName, toSpecQName, fromSpecName
  , isPreCondName, toPreCondName, toPreCondQName, fromPreCondName
  , isPostCondName, toPostCondName, toPostCondQName, fromPostCondName
  , isNonFailName, toNonFailName, toNonFailQName, fromNonFailName
  , decodeContractQName, decodeContractName
  , encodeContractQName, encodeContractName
  )  where

import Data.Char  ( isAlphaNum )
import Data.List  ( isPrefixOf, isSuffixOf )
import Numeric    ( readHex )

------------------------------------------------------------------------

--- Is this the name of a specification?
isSpecName :: String -> Bool
isSpecName f = "'spec" `isSuffixOf` f

--- Transform a name into a name of the corresponding specification
--- by adding the suffix "'spec".
toSpecName :: String -> String
toSpecName = (++"'spec")

--- Transform a qualified name into a name of the corresponding specification
--- by adding the suffix "'spec".
toSpecQName :: (String,String) -> (String,String)
toSpecQName (mn,fn) = (mn, toSpecName fn)

--- Drop the specification suffix "'spec" from the name:
fromSpecName :: String -> String
fromSpecName f =
  let rf = reverse f
   in reverse (drop (if take 5 rf == "ceps'" then 5 else 0) rf)

--- Is this the name of a precondition?
isPreCondName :: String -> Bool
isPreCondName f = "'pre" `isSuffixOf` f

--- Transform a name into a name of the corresponding precondition
--- by adding the suffix "'pre".
toPreCondName :: String -> String
toPreCondName = (++ "'pre")

--- Transform a qualified name into a name of the corresponding precondition
--- by adding the suffix "'pre".
toPreCondQName :: (String,String) -> (String,String)
toPreCondQName (mn,fn) = (mn, toPreCondName fn)

--- Drop the precondition suffix "'pre" from the name:
fromPreCondName :: String -> String
fromPreCondName f =
  let rf = reverse f
   in reverse (drop (if take 4 rf == "erp'" then 4 else 0) rf)

--- Is this the name of a precondition?
isPostCondName :: String -> Bool
isPostCondName f = "'post" `isSuffixOf` f

--- Transform a name into a name of the corresponding prostcondition
--- by adding the suffix "'post".
toPostCondName :: String -> String
toPostCondName = (++"'post")

--- Transform a qualified name into a name of the corresponding postcondition
--- by adding the suffix "'post".
toPostCondQName :: (String,String) -> (String,String)
toPostCondQName (mn,fn) = (mn, toPostCondName fn)

--- Drop the postcondition suffix "'post" from the name:
fromPostCondName :: String -> String
fromPostCondName f =
  let rf = reverse f
   in reverse (drop (if take 5 rf == "tsop'" then 5 else 0) rf)

--- Is this the name of a precondition?
isNonFailName :: String -> Bool
isNonFailName f = "'nonfail" `isSuffixOf` f

--- Transform a name into a name of the corresponding prostcondition
--- by adding the suffix "'post".
toNonFailName :: String -> String
toNonFailName = (++"'nonfail")

--- Transform a qualified name into a name of the corresponding postcondition
--- by adding the suffix "'post".
toNonFailQName :: (String,String) -> (String,String)
toNonFailQName (mn,fn) = (mn, toNonFailName fn)

--- Drop the postcondition suffix "'nonfail" from the name:
fromNonFailName :: String -> String
fromNonFailName f =
  let rf = reverse f
   in reverse (drop (if take 8 rf == "liafnon'" then 8 else 0) rf)

------------------------------------------------------------------------
--- Transforms a qualified operation name starting with `op_xh1...hn'`, where
--- each `hi` is a two digit hexadecimal number, into the name
--- of corresponding to the ord values of `h1...hn`.
--- For instance, `op_x263E'nonfail` is transformed into `&>'nonfail`.
decodeContractQName :: (String,String) -> (String,String)
decodeContractQName (mn,fn) = (mn, decodeContractName fn)

--- Transforms an operation name starting with `op_xh1...hn'`, where
--- each `hi` is a two digit hexadecimal number, into the name
--- of corresponding to the ord values of `h1...hn`.
--- For instance, `op_x263E'nonfail` is transformed into `&>'nonfail`.
decodeContractName :: String -> String
decodeContractName fn
  | "op_x" `isPrefixOf` fn && not (null fntail) = fromHex [] (drop 4 fnop)
  | otherwise                                   = fn
 where
  (fnop,fntail) = break (==''') fn

  fromHex s ""  = reverse s ++ fntail
  fromHex _ [_] = fn
  fromHex s (c1:c2:cs) = case readHex [c1,c2] of
    [(i,"")] -> fromHex (chr i : s) cs
    _        -> fn

--- Transforms a qualified operation name of the form `fn'tail` into a valid
--- identifier. Thus, if `fn` contains any non-alphanumeric characters,
--- the name will be transformed into `op_xh1...hn'`, where
--- each `hi` is the two digit hexadecimal number corresponding to the
--- i-th character of `fn`.
--- For instance, `&>'nonfail` is transformed into `op_x263E'nonfail`.
encodeContractQName :: (String,String) -> (String,String)
encodeContractQName (mn,fn) = (mn, encodeContractName fn)

--- Transforms an operation name of the form `fn'tail` into a valid
--- identifier. Thus, if `fn` contains any non-alphanumeric characters,
--- the name will be transformed into `op_xh1...hn'`, where
--- each `hi` is the two digit hexadecimal number corresponding to the
--- i-th character of `fn`.
--- For instance, `&>'nonfail` is transformed into `op_x263E'nonfail`.
encodeContractName :: String -> String
encodeContractName fn
  | null rop || null rsuf ||
    all (\c -> isAlphaNum c || c == ''' || c == '_') rop
   = fn
  | otherwise
  = "op_x" ++ concatMap toHex (reverse (tail rop)) ++ ''' : reverse rsuf
 where
  (rsuf,rop) = break (==''') (reverse fn)

  toHex c = let n = ord c in [toHexDigit (n `div` 16), toHexDigit (n `mod` 16)]

  toHexDigit i = if i<10 then chr (ord '0' + i)
                    else chr (ord 'A' + i - 10)

---------------------------------------------------------------------------
