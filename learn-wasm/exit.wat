(module
 (import "wasi_snapshot_preview1" "proc_exit" (func (param i32)))
 (func (export "_start")
  (i32.const 4) 
  (call 0)))
