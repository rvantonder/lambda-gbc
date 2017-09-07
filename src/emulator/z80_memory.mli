open Bap.Std

(** Provides memory as a map (tree) *)
class memory_map : Z80_image.t -> Options.t -> object('s)
    method save : word -> word -> 's
    method load : word -> word option
  end

class memory_array : Z80_image.t -> Options.t -> object('s)
    method save : word -> word -> 's
    method load : word -> word option
  end

class memory_table : Z80_image.t -> Options.t -> object('s)
    method save : word -> word -> 's
    method load : word -> word option
  end
