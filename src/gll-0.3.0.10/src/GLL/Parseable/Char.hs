
-- | Exports an instance for 'Parseable' 'Char' 
-- that assumes '$' and '#' never appear in the inpur string.
module GLL.Parseable.Char () where

import GLL.Types.Abstract

-- | Assumes '$' and '#' never appear in the inpur string.
instance Parseable Char where
    eos = '$'
    eps = '#'
    matches = (==)
