module Main (main) where

--TODO: Error handling throughout.

-- The understanding of TIM file formats is derived from 
-- https://moddingwiki.shikadi.net/wiki/The_Incredible_Machine.
-- In particular:
-- https://moddingwiki.shikadi.net/wiki/TIM_Resource_Format
-- https://moddingwiki.shikadi.net/wiki/The_Incredible_Machine_Level_Format

-- We reinvent the wheel on many things to keep dependencies low.
-- Ordinarily, I would trust others to have done much more performance optimizing
-- than I could or would. However, all TIM files are so small, this doesn't matter.

import qualified Data.ByteString as B

main :: IO ()
main = do
  putStrLn "hello world"
