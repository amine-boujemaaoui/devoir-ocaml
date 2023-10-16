open Process

type 'a tree =
  | Leaf of 'a process
  | Node of 'a transition * 'a tree list
