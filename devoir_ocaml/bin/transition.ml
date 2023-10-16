open Action
open Pre_process
open Process

type 'a transition =
  | ActionTransition of 'a action
  | SilentTransition
 