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
  )  where

import List ( isSuffixOf )

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

------------------------------------------------------------------------
