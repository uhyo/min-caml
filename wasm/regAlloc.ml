open Asm

(* WebAssembly doesn't have registers;
 * instead it has unlimited number of local variables.
 * Wow!
 *)
let f prog = prog
