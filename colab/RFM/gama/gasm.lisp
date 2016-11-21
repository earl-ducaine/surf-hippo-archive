;;;
;;;
;;; gasm.lsp  -- general assembler
;;;
;;; (c) Michael Sintek      1/1992
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; expects gaux, gcla, gmem, gmht


;;; main functions
;;;;;;;;;;;;;;;;;;


(defun gassem (&rest asm-lists) ; lisp command
  (gasm asm-lists)
  T)

(defun gasm (asm-lists)
  (gasm.assem (gasm.asm-lists-transform asm-lists)))

(defun gasm.asm-lists-transform (asm-lists)
  ; expand file names
  (mapcar #'(lambda (gasm)
              (cond ((listp gasm) gasm)
                    ((stringp gasm) (gasm.get-code-from-file gasm))
                    (T (gerror "gasm" "gasm.asm-lists-transform"
                         "assembler code expected, not:~%~a" gasm))))
          asm-lists))

(defun gasm.get-code-from-file (file-name)
  (let ((file (open file-name)))
       (if file
         (let ((code (gcla.get
                       (gcla.new-object/check 'gasm (read file))
                       'code)))
              (close file)
              code)
         (gerror "gasm" "gasm.get-code-from-file"
           "file '~a' does not exist." file-name))))


(defun gasm.assem (asm-lists)
  (gasm.pass3 (gasm.pass2 (gasm.pass1 asm-lists))))


;;; pass 1
;;;;;;;;;;

(defun gasm.pass1 (asm-lists)
  (gasm.pass (gasm.init-state asm-lists) 1))

(defun gasm.init-state (asm-lists) ; creates initial state
  (gcla.new-object 'state
    'pass 1
    'asm-lists asm-lists
  ))


;;; pass 2
;;;;;;;;;;

(defun gasm.pass2 (state)
  (gasm.pass (gcla.put state 'pass 2) 2))


;;; pass 3
;;;;;;;;;;

(defun gasm.pass3 (state)
  (gasm.pass (gcla.put state 'pass 3) 3))


;;; global variables
;;;;;;;;;;;;;;;;;;;;


(defvar gasm.*default-module*)
(setq gasm.*default-module* 'user)

(defvar gasm.*default-module-size*)   ; size of module name space
(setq gasm.*default-module-size* 307) ; should be a prime:
  ; 101, 203, 307, 401, 503, 601, 701, 809, 907, 1009

(defvar gasm.*modules*)
(setq gasm.*modules* (gcla.new-object 'modules))

(defvar gasm.*instructions*)
(setq gasm.*instructions* (gcla.new-object 'asm-instructions))

(defvar gasm.*default-dynamic-mode*)
(setq gasm.*default-dynamic-mode* T)

(defvar gasm.*default-unknown-label*)
(setq gasm.*default-unknown-label* -1)


;;; all passes
;;;;;;;;;;;;;;


; remark: in the following, "gasm.st-" is a prefix for state tranformation
;         functions


(defun gasm.pass (state pass) ; general pass, used for all three passes
  (setq state (gasm.st-pass-begin state pass))
  (mapc #'(lambda (asm-list)
            (setq state (gasm.st-asm-list-begin state pass))
            (mapc #'(lambda (instruction)
                      (setq state (gasm.exec-instr instruction state pass)))
                  asm-list)
            (setq state (gasm.st-asm-list-end state pass)))
        (gcla.get state 'asm-lists))
  (gasm.st-pass-end state pass))


(defun gasm.exec-instr (instruction state pass) ; "execute" one asm instruction
  (let* ((instr (gasm.transform-instr instruction))
         (fct (gasm.instr-fct (car instr) pass)))
        (if fct
            (apply fct (cons state instr))
            state)))

(defun gasm.instr-p (symbol) ; symbol beginning with '.'
  (char= (char (string symbol) 0) #\.))

(defun gasm.transform-instr (instruction)
  (cond ((symbolp instruction)      ; global label oder nullary instruction
         (if (gasm.instr-p instruction)
             (cons instruction nil)
             (list '.global instruction)))
        ((or (numberp instruction)  ; local label
             (stringp instruction))
         (list '.local instruction))
        ((consp instruction)        ; instruction
         instruction)
        (T (gerror "gasm" "gasm.transform-instr"
                   "wrong assembler instruction format:~%~s"
                   instruction))))

(defun gasm.instr-fct (instruction-name pass)
  (let ((instr (gcla.get gasm.*instructions* instruction-name)))
       (if instr
           (gcla.get instr pass)
           (gerror "gasm" "gasm.instr-fct"
                   "unknown instruction: ~a" instruction-name))))


(defun gasm.st-pass-begin (state pass) ; at beginning of each pass
  (setq state
    (gcla.put state
      ; ...
    ))
  (when (= pass 2)
    (setq state
      (gcla.put state
        'address 0 ; relocatable, offset to be determined later
        'backpatches nil
        'current-locals nil
        'locals nil
        ; ...
      )))
  (when (= pass 3)
    (setq state
      (gcla.put state
        'address (gcla.get state 'start-address)
        'current-locals (car (gcla.get state 'locals))
        'rest-locals (cdr (gcla.get state 'locals))
        ; ...
      )))
  ; ...
  state)


(defun gasm.backpatches (bp-list offset)
  (mapc #'(lambda (adr) (gmem.put adr (+ (gmem.get adr) offset)))
        bp-list))

(defun gasm.st-pass-end (state pass) ; at end of each pass
  (setq state
    (gcla.put state
      ; ...
    ))
  (when (= pass 2)
    (setq state
      (gcla.del
        (gcla.put state
          'locals (reverse (gcla.get state 'locals))
          'start-address (gmem.alloc (gcla.get state 'address))
          ; ...
        )
        'current-locals))
    (gasm.backpatches (gcla.get state 'backpatches)
                      (gcla.get state 'start-address))
    (setq state
      (gcla.del state 'backpatches)))
  ; ...
  state)


(defun gasm.st-asm-list-begin (state pass)
  ; at beginning of each asm instruction list
  (setq state ; all passes
    (gcla.put state
      'current-module gasm.*default-module*
      'dynamic gasm.*default-dynamic-mode*
      'current-unknown-label gasm.*default-unknown-label*
      'current-label nil
      ; ...
    ))
  ; ...
  state)


(defun gasm.st-asm-list-end (state pass)
  ; at end of each asm instruction list
  (setq state
    (gcla.put state
      ; ...
    ))
  (when (= pass 2)
    (setq state
      (gcla.put state
        'locals ; implicit .proc
        (cons (reverse (gcla.get state 'current-locals))
              (gcla.get state 'locals))
        'current-locals nil
        ; ...
      )))
  (when (= pass 3)
    (setq state
      (gcla.put state
        'current-locals (car (gcla.get state 'rest-locals))
        'rest-locals (cdr (gcla.get state 'rest-locals))
        ; ...
      )))
  ; ...
  state)


;;; define instruction
;;;;;;;;;;;;;;;;;;;;;;


(defun gasm.definstr (name &rest plist)
  (setq gasm.*instructions*
    (gcla.put gasm.*instructions*
      name (gcla.new-object-from-plist 'instruction plist)))
  name)


;;; general assembler instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; .module
; -------

(defun gasm.new-module (name size imports)
  (let ((new-mod (gcla.new-object 'module
                   'size size
                   'name-space (gmht.make-ht size)
                   'imports imports)))
       (setq gasm.*modules* (gcla.add gasm.*modules* name new-mod))
       new-mod)) ; returns clob for new module!!

(defun gasm.check/create-module (name size imports)
  (unless (gcla.get gasm.*modules* name)
          (gasm.new-module name size imports)))

(defun gasm.module1 (state instr name
                     &key (size gasm.*default-module-size*)
                          (imports '(system)))
  (gasm.check/create-module name size imports)
  (gcla.put state 'current-module name))

(defun gasm.module23 (state instr name &rest dummy)
  (gcla.put state 'current-module name))

(gasm.definstr '.module
  1 #'gasm.module1
  2 #'gasm.module23
  3 #'gasm.module23)


; .import-from, .import-module
; ----------------------------


#| old primitive version

(defun gasm.import (module-name import)
  (let ((module (gcla.get gasm.*modules* module-name)))
       (setq gasm.*modules*
         (gcla.put gasm.*modules*
           module-name
           (gcla.put module
             'imports
             (cons import (gcla.get module 'imports)))))))
|#

(defun gasm.import (module-name import)
  ; import is either a module name or a list of the form
  ; (module-name label1 ...)
  (cond ((symbolp import) ; import whole module
	 (let ((module (gcla.get gasm.*modules* module-name)))
	      (unless (member import (gcla.get module 'imports))
		(setq gasm.*modules*
		      (gcla.put gasm.*modules*
			 module-name
			 (gcla.put module
			   'imports
			   (cons import (gcla.get module 'imports))))))))
  (T ; import qualified
    (let* ((module (gcla.get gasm.*modules* module-name))
	   (imports (gcla.get module 'imports))
	   (modulename (car import))
	   (new-imported-labels (remove-duplicates (cdr import)))
	   (old-imported-labels (gasm.imported-from modulename imports))
	   (new-imp-labels
	    (set-difference new-imported-labels old-imported-labels)))
	  (when new-imp-labels ; at least one new label
		(setq gasm.*modules*
		      (gcla.put gasm.*modules*
			 module-name
			 (gcla.put module
			   'imports
			   (cons (cons modulename new-imp-labels)
				 (gcla.get module 'imports))))))))))

(defun gasm.imported-from (modulename imports)
  ; returns labels imported from modulename in imports
  (mapcan #'(lambda (x)
	      (when (and (consp x) (eq modulename (car x)))
		    (copy-list (cdr x))))
	  imports))


(defun gasm.import-from (state instr &rest import)
  (gasm.import (gcla.get state 'current-module) import)
  state)

(defun gasm.import-module (state instr import)
  (gasm.import (gcla.get state 'current-module) import)
  state)

(gasm.definstr '.import-from
  1 #'gasm.import-from)

(gasm.definstr '.import-module
  1 #'gasm.import-module)


; .dynamic, .static
; -----------------

(defun gasm.dynamic (state instr)
  (gcla.put state 'dynamic T))

(defun gasm.static (state instr)
  (gcla.put state 'dynamic nil))

(gasm.definstr '.dynamic
  1 #'gasm.dynamic
  2 #'gasm.dynamic
  3 #'gasm.dynamic)

(gasm.definstr '.static
  1 #'gasm.static
  2 #'gasm.static
  3 #'gasm.static)


; .global
; -------

(defun gasm.check-dynamic-label (pos label)
  (unless (gcla.get (gmem.get (+ pos 2)) 'dynamic)
    (gerror "gasm" "gasm.check-dynamic-label" "label '~a' is static." label)))

(defun gasm.new-label (label hash-table state)
  (gmht.put hash-table
    label
    (gcla.get state 'current-unknown-label)
    (gcla.new-object 'label
      'dynamic (gcla.get state 'dynamic)
      ; ...
    )))


(defun gasm.global1 (state instr label)
  ; enter label in symbol table
  (let* ((hash-table
         (gcla.get
           (gcla.get gasm.*modules* (gcla.get state 'current-module))
           'name-space))
         (pos (gmht.get hash-table label)))
        (if pos (gasm.check-dynamic-label pos label)
                (gasm.new-label label hash-table state))
	(gcla.put state 'current-label (gmht.get hash-table label))))

(defun gasm.global2 (state instr label)
  (let ((adr (1+ (gmht.get
                   (gcla.get ; hashtable
                     (gcla.get gasm.*modules*
                       (gcla.get state 'current-module))
                     'name-space)
                   label))))
       (gmem.put adr (gcla.get state 'address))
       (gcla.put state 
	 'backpatches (cons adr (gcla.get state 'backpatches))
	 'current-label (1- adr))))

(defun gasm.global3 (state instr label)
  (gcla.put state 
    'current-label 
    (gmht.get (gcla.get
	       (gcla.get gasm.*modules* (gcla.get state 'current-module))
	       'name-space)
	      label)))


(gasm.definstr '.global
  1 #'gasm.global1
  2 #'gasm.global2
  3 #'gasm.global3)



; local labels: .local, .proc
; ---------------------------

(defun gasm.local2 (state instr label)
  (gcla.put state 'current-locals
    (cons (list label (gcla.get state 'address))
          (gcla.get state 'current-locals))))

(gasm.definstr '.local
  2 #'gasm.local2)


(defun gasm.proc2 (state instr)
  (gcla.put state
    'locals (cons (reverse (gcla.get state 'current-locals))
                  (gcla.get state 'locals))
    'current-locals nil))

(defun gasm.proc3 (state instr)
  (let ((rest-locals (gcla.get state 'rest-locals)))
       (gcla.put state
         'current-locals (car rest-locals)
         'rest-locals (cdr rest-locals))))

(gasm.definstr '.proc
  2 #'gasm.proc2
  3 #'gasm.proc3)


; .end, .dend, .destroyable, .destroyable-module
; ----------------------------------------------

(defun gasm.end3 (state instr)
  (let ((adr (+ (gcla.get state 'current-label) 2)))
       (gmem.put adr
	 (gcla.put (gmem.get adr)
	   'end (1- (gcla.get state 'address)))))
  state)
  
(gasm.definstr '.end
  3 #'gasm.end3)


(defun gasm.dend3 (state instr)
  (let ((adr (+ (gcla.get state 'current-label) 2)))
       (gmem.put adr
	 (gcla.put (gmem.get adr)
	   'end (1- (gcla.get state 'address))
	   'destroyable T)))
  state)
  
(gasm.definstr '.dend
  3 #'gasm.dend3)




;;; functions used in user defined instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; pass 2
; ------

(defun gasm.st-instr-size (state size)
  (gcla.put state 'address (+ (gcla.get state 'address) size)))


(defun gasm.std-instr2 (state &rest instr)
  (gasm.st-instr-size state 1))



; pass 3
; ------


; a) gasm.get-label-comp

(defun gasm.get-local (state label)
  (let ((adr (cadr (assoc label
                          (gcla.get state 'current-locals)
                          :test #'equal))))
       (if adr
           (+ adr (gcla.get state 'start-address))
           (gerror "gasm" "gasm.get-local"
                   "unknown local label ~a~%(valid local labels: ~a)~%"
                   label (gcla.get state 'current-locals)))))


(defun gasm.find-global (label imports state)
  (when imports ; return nil if label not found
    (let ((result (gasm.find-global/2 label (car imports) state)))
         (if result result
             (gasm.find-global label (cdr imports) state)))))

(defun gasm.find-global/2 (label import state)
  (cond ((consp import)
         (when (member label (cdr import))
               (gasm.find-global/3 label (car import) state)))
        (T (gasm.find-global/4 label import))))

(defun gasm.find-global/3 (label module state) ; label *is* in module
  (let ((mod (gcla.get gasm.*modules* module)))
       (if mod
         (let ((adr (gmht.get (gcla.get mod 'name-space) label)))
              (if adr
                (if (gcla.get (gmem.get (+ adr 2)) 'dynamic)
                  (list 'gmem.get (1+ adr))
                  (gmem.get (1+ adr)))
                (gasm.adr-of-new-dyn-label mod label state)))
         (gasm.adr-of-new-dyn-label ; new label in new module
           (gasm.new-module module gasm.*default-module-size* '(system))
           label state))))

(defun gasm.find-global/4 (label module) ; try to find label in module
  (let ((mod (gcla.get gasm.*modules* module)))
       (when mod
         (let ((adr (gmht.get (gcla.get mod 'name-space) label)))
              (when adr
                (if (gcla.get (gmem.get (+ adr 2)) 'dynamic)
                  (list 'gmem.get (1+ adr))
                  (gmem.get (1+ adr))))))))

(defun gasm.adr-of-new-dyn-label (mod label state)
  (list 'gmem.get
    (1+
      (gmht.put (gcla.get mod 'name-space)
        label
	(gcla.get state 'current-unknown-label)
        (gcla.new-object 'label
          'dynamic T
          ; ...
        )))))

(defun gasm.get-global (state label)
  (let* ((current-module (gcla.get state 'current-module))
         (cmod (gcla.get gasm.*modules* current-module))
         (global (gasm.find-global label
                   (cons
                     current-module ; begin search in current module
                     (gcla.get cmod 'imports))
		   state)))
        (if global
            global
            (gasm.adr-of-new-dyn-label cmod label state))))


(defun gasm.get-label-comp (label state)
  ; returns label address computation,
  ; i.e., an absolute address for static labels,
  ; and (gmem.get <adr>) for dynamic labels
  (if (symbolp label)
      (gasm.get-global state label)
      (gasm.get-local state label)))



(defun gasm.get-global-adr (label modules)
  (let ((label-adr
	 (eval (gasm.find-global 
		label modules 
		(gcla.new-object
		 'state  ; state must be created!!
		 'current-unknown-label gasm.*default-unknown-label*)))))
       (if label-adr label-adr
	   (gerror "gasm" "gasm.get-global-adr"
		   "label '~a' not found." label))))

(defun gasm.get-global-adr* (label modules)
  ; create label in first module if it does not exist
  (let* ((state (gcla.new-object 'state 
				 'current-unknown-label 
				 gasm.*default-unknown-label*))
	 (label-adr
	  (eval (gasm.find-global label modules state))))
       (if label-adr label-adr
           (eval (gasm.adr-of-new-dyn-label 
		  (gcla.get gasm.*modules* (car modules)) label state)))))


; b) type checker

(defvar gasm.*types* (gcla.new-object 'types))

(defun gasm.deftype (type fct)
  (setq gasm.*types*
    (gcla.put gasm.*types* type fct)))

; functions beginning with "gasm.ct-" are type checking functions
; they return ( <real-arg> . <most-special-type> )

(defun gasm.ct-nat (arg state)
  (if (and (integerp arg) (>= arg 0))
      (cons arg 'nat)
      (gerror "gasm" "gasm.ct-nat"
        "instruction expected a natural number, not '~a'." arg)))

(gasm.deftype 'nat #'gasm.ct-nat)


(defun gasm.ct-const (arg state)
  (if (not (consp arg))
      (cons (gasm.evaluable-const arg) 'const)
      (gerror "gasm" "gasm.ct-const"
        "instruction expected a constant, not '~a'." arg)))

(defun gasm.evaluable-const (const)
  (if (symbolp const) (list 'quote const)
      const))

(gasm.deftype 'const #'gasm.ct-const)


(defun gasm.ct-functor (arg state)
  (if (consp arg)
      (cons (list 'quote arg) 'functor)
      (gerror "gasm" "gasm.ct-functor"
        "instruction expected a functor, not '~a'." arg)))

(gasm.deftype 'functor #'gasm.ct-functor)


(defun gasm.ct-x (arg state) ; don't care; should be removed
  (cons arg 'x))

(gasm.deftype 'x #'gasm.ct-x)


(defun gasm.ct-function (arg state)
  (if (fboundp arg)
      (cons (list 'quote arg) 'function)
      (gerror "gasm" "gasm.ct-function"
	      "not valid as a function: ~a" arg)))

(gasm.deftype 'function #'gasm.ct-function)


(defun gasm.ct-label (arg state)
  (cond ((null arg) (cons (gcla.get state 'current-unknown-label) 'label/st))
	(T (let ((label-comp (gasm.get-label-comp arg state)))
		(if (consp label-comp)
		    (cons (cadr label-comp) 'label/dy)
		    (cons label-comp 'label/st))))))

(gasm.deftype 'label #'gasm.ct-label)


(defun gasm.gen-hashtable-pair (pair state)
  (let ((label-comp (gasm.get-label-comp (cadr pair) state)))
       (if (consp label-comp)
	   (gerror "gasm" "gasm.gen-hashtable-pair"
		   "only static labels allowed in hashtables, not ~a"
		   (cadr pair))
	   (list (car pair) label-comp))))

(defun gasm.gen-hashtable-arg (ht state)
  (list 'quote
	(mapcar #'(lambda (pair) (gasm.gen-hashtable-pair pair state))
		ht)))

(defun gasm.ct-hashtable (arg state) ; labels may only be static !!!!
  (cons (gasm.gen-hashtable-arg arg state)
        'hashtable))

(gasm.deftype 'hashtable #'gasm.ct-hashtable)


(defun gasm.check-types (arg-list type-list state)
  ; returns ( <real-args> . <most-special-types> )
  (when (/= (length arg-list) (length type-list))
        (gerror "gasm" "gasm.check-types"
          "illegal number of arguments in ~a" arg-list))
  (let ((arg-stype-list
          (mapcar #'(lambda (arg type)
                      (let ((ct-fct (gcla.get gasm.*types* type)))
                           (if type
                               (funcall ct-fct arg state)
                               (gerror "gasm" "gasm.check-types"
                                 "unknown type: ~a." type))))
                  arg-list type-list)))
       (cons (mapcar #'car arg-stype-list)
             (mapcar #'cdr arg-stype-list))))


; c) generate emulator instruction

(defun gasm.gen-emul-instr (instr-name args state)
  (let* ((instr (gcla.get gasm.*instructions* instr-name))
         (types (gcla.get instr 'types))
         (emul-fct (gcla.get instr 'emul-fct))
         (emul-fct/dy (gcla.get instr 'emul-fct/dy))
         (emul-fct/st (gcla.get instr 'emul-fct/st))
         (rargs*mstypes (gasm.check-types args types state))
         (rargs (car rargs*mstypes))
         (mstypes (cdr rargs*mstypes)))
        (cond ((member 'label/dy mstypes) ; dynamic labels
               (if (member 'label/st mstypes)
                   (gerror "gasm" "gasm.gen-emul-instr"
                    "mixing of dynamic and static labels not allowed in ~a:~%~a"
                    instr-name args)
                   (if emul-fct/dy
                       (cons emul-fct/dy rargs)
                       (gerror "gasm" "gasm.gen-emul-instr"
                         "no dynamic labels allowed in ~a:~%~a"
                         instr-name args))))
              ((member 'label/st mstypes) ; static labels
               (if (member 'label/dy mstypes)
                   (gerror "gasm" "gasm.gen-emul-instr"
                    "mixing of dynamic and static labels not allowed in ~a:~%~a"
                    instr-name args)
                   (if emul-fct/st
                       (cons emul-fct/st rargs)
                       (gerror "gasm" "gasm.gen-emul-instr"
                         "no static labels allowed in ~a:~%~a"
                         instr-name args))))
              (T (cons emul-fct rargs)))))


; d) standard instruction pass 3

(defun gasm.std-instr3 (state instr-name &rest args)
  (gmem.put (gcla.get state 'address)
    (gasm.gen-emul-instr instr-name args state))
  (gasm.st-instr-size state 1))



;;; initialization
;;;;;;;;;;;;;;;;;;


(defun gasm.init ()
  (gassem
    '( (.module system :imports () )
       (.module user :size 503) ; should be a prime
     )))


