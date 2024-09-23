------------------------------------------------------------------------------
-----                                                                    -----
-----     Opt: Options for Syrup                                         -----
-----                                                                    -----
------------------------------------------------------------------------------

module Language.Syrup.Opt where

import Data.Set

data Option
  = Quiet
  deriving (Eq, Ord)

type Options = Set Option
