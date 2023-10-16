type 'a action =
  | Act of 'a
  | CoAct of 'a
  | Silent

type 'a pre_process =
  'a action * 'a process

type 'a process =
  | Skip
  | Pre of 'a pre_process
  | Plus of 'a pre_process * 'a pre_process
  | Parallel of 'a process * 'a process
