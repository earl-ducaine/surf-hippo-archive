; Note: If you want to compile this package, the order is important.
;       It's recommended to use the following function:

(defun compile-pro-syntax ()
  (compile-file "rfi.lsp")
  (compile-file "pro2lisp.lsp")
  (compile-file "lisp2pro.lsp")
; (compile-file "rfi-load.lsp") 
)



; Loads rfi.lsp with Prolog-like syntax.

(load "rfi")
(load "lisp2pro")
(load "pro2lisp")
;(load "patches.lsp")
