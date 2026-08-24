{-# LANGUAGE CPP #-}
module CompatibilityCppEdge where

-- The formatter must reject this before preprocessing changes the source.
#include "example.h"
