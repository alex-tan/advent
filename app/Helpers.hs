module Helpers where

(|>) :: a -> (a -> b) -> b
x |> f = f x
