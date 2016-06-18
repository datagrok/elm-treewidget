module RoseTree where
{-| Naive RoseTree implementation in Elm

This is my first attempt at using Elm. I don't know what I'm doing LOL.

Heavily relied on https://github.com/TheSeamau5/elm-rosetree for reference. I
don't what the benefit is of using LazyList.

I also implement lens, to reach in and update bits of the tree.



-}

import Graphics.Element exposing (..)

import Html exposing (li, text, ul)
import Html.Attributes exposing (class)
import Svg

type RoseTree a
  = Rose a (List (RoseTree a))

singleton a = Rose a []

root : RoseTree a -> a
root (Rose a _) = a

children : RoseTree a -> List (RoseTree a)
children (Rose _ c) = c

addChild : RoseTree a -> RoseTree a -> RoseTree a
addChild child (Rose a c) =
      Rose a (child :: c)

-- given a tree of strings, return a ul structure of list items
toul : RoseTree String -> Svg.Svg
toul (Rose a c) = ul [] [li [] (text a :: List.map toul c)]

data1 : RoseTree String
data1 = List.foldr (\x->addChild (singleton x)) (singleton "foo") ["bar","baz","quux"]

data2 = Rose "big" [data1, data1, data1]

main =
      toul data2
