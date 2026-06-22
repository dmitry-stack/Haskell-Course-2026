module Optimizer where

import AST

optimize :: Query -> Query


optimize (Filter c1 (Filter c2 q)) = 
  optimize (Filter (And c1 c2) q)


optimize (Project cols1 (Project cols2 q)) =
  optimize (Project cols1 q)



optimize (Filter cond (Join left right joinCond)) =
  Join (optimize (Filter cond left)) (optimize right) joinCond

optimize (Project cols q)       = Project cols (optimize q)
optimize (Filter cond q)        = Filter cond (optimize q)
optimize (Join left right cond) = Join (optimize left) (optimize right) cond
optimize (From tableName)       = From tableName