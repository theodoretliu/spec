(module
  ;; Auxiliary definition

  ;; Syntax

  (import "spectest" "print_i32" (func $print_i32 (param i32)))
  (import "spectest" "global_taint_i32" (global $global_taint_i32 i32))
  (func
    (call $print_i32 (global.get $global_taint_i32)))
)
