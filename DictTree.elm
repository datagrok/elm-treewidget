module DictTree exposing (DictTree, addPath, empty, root)

{-| A tree (or DAG) where each node has a value and an unordered set of children. 
-}

import Dict


{-| Represents a tree of Nodes and their child nodes.
-}
type DictTree a
    = DictTreeNode (Dict.Dict a (DictTree a))


{-| Given a DictTree, return the Dict that contains its children.
-}
root : DictTree a -> Dict.Dict a (DictTree a)
root (DictTreeNode a) =
    a

{-| Create an empty DictTree.
-}
empty : DictTree a
empty =
    DictTreeNode Dict.empty


{-| Given a list of keys, create nodes and sub-nodes sufficient to add those
keys as a path in the tree.
-}
addPath : List comparable -> DictTree comparable -> DictTree comparable
addPath path (DictTreeNode t) =
    case path of
        [] ->
            DictTreeNode t

        p :: path ->
            DictTreeNode (Dict.insert p (addPath path (Maybe.withDefault (DictTreeNode Dict.empty) (Dict.get p t))) t)
