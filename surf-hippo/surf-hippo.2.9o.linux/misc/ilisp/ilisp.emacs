;;;  -*- Mode: Emacs-Lisp -*-

;;; ilisp.emacs --

;;; This file is part of ILISP.
;;; Version: 5.10.1
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996-2000 Marco Antoniotti and Rick Campbell
;;;
;;; Send mail to 'majordomo@cons.org' to be included in the ILISP mailing
;;; list.

;;; This file shows examples of some of the things you might want to
;;; do to install or customize ILISP.  You may not want to include all
;;; of them in your .emacs.  For example, the default key binding
;;; prefix for ILISP is C-z and this file changes the default prefix
;;; to C-c. For more information on things that can be changed, see
;;; the file ilisp.el. 


;;; If ilisp lives in some non-standard directory, you must tell
;;; Emacs where to get it. This may or may not be necessary.

(setq load-path (cons (expand-file-name "~jones/emacs/ilisp/")
                      load-path))


;;; If you always want partial minibuffer completion

(require 'completer)

;;; If want TMC completion then you will have to Ftp it yourself
;;; from think.com It's become to flaky for me to deal with. -- Ivan

; (load "completion")
; (initialize-completions)


;;; If you want to redefine typeout-window keys.

; (add-hook 'ilisp-load-hook
;   '(lambda ()
;      (define-key global-map "\C-c1" 'ilisp-bury-output)
;      (define-key global-map "\C-cv" 'ilisp-scroll-output)
;      (define-key global-map "\C-cg" 'ilisp-grow-output)))


;;; Autoload based on your Lisp. You only really need the one you
;;; use. If called with a prefix, you will be prompted for a
;;; buffer and program.

(autoload 'run-ilisp   "ilisp" "Select a new inferior Lisp." t)

(autoload 'common-lisp "ilisp" "Inferior generic Common Lisp." t)

;; Franz
(autoload 'allegro     "ilisp" "Inferior Allegro Common Lisp." t)

;; Lucid
; (autoload 'lucid     "ilisp" "Inferior Lucid Common Lisp." t)

;; Harlequin
; (autoload lispworks  "ilisp"
;   "Inferior Harlequin Common Lisp (LispWorks)." t)
; (autoload harlequin  "ilisp"
;   "Inferior Harlequin Common Lisp (LispWorks)." t)
; (autoload pulcinella "ilisp"
;   "Inferior Harlequin Common Lisp (LispWorks)." t)
;; Italian "Commedia dell'Arte" twist.

;; CMUCL
(autoload 'cmulisp     "ilisp" "Inferior CMU Common Lisp." t)

;; CLISP (Bruno Haible and Michael Stoll)
(autoload 'clisp-hs   "ilisp"
  "Inferior Haible/Stoll CLISP Common Lisp." t)

;; KCL dialects
; (autoload 'kcl       "ilisp"
;   "Inferior Kyoto Common Lisp." t)
; (autoload 'akcl      "ilisp"
;   "Inferior Austin Kyoto Common Lisp." t)
; (autoload 'ibcl "ilisp"
;   "Ibuki Common Lisp." t)
; (autoload 'gcl "ilisp"
;   "Inferior GNU Common Lisp." t)
; (autoload 'ecl "ilisp"
;   "Inferior EcoLisp." t)

;; XLisp
; (autoload 'xlisp      "ilisp" "Inferior XLisp." t)
; (autoload 'xlispstat  "ilisp" "Inferior XLisp-Stat." t)

;; Scheme
; (autoload 'scheme     "ilisp" "Inferior generic Scheme." t)
; (autoload 'oaklisp    "ilisp" "Inferior Oaklisp Scheme." t)
; (autoload 'scm        "ilisp" "Inferior SCM Scheme." t)
; (autoload 'chez       "ilisp" "Inferior Chez Scheme." t)
; (autoload 'stk        "ilisp" "Inferior STk Scheme." t)
; (autoload 'snow       "ilisp" "Inferior STk Scheme without Tk." t)
; (autoload 'guile      "ilisp" "Inferior GUILE Scheme." t)


;;; Define where Lisp programs are found. (This may already have
;;; been done at your site).

(setq allegro-program "/usr/local/acl5/lisp")

; (setq lucid-program "/usr/misc/.lucid/bin/lisp")

(setq clisp-hs-program "clisp -I")

; (setq lispworks-program
;       "/somewhere/in/the/directory/tree/lispworks")

(setq cmulisp-program
      "/usr/local/lib/cmucl/bin/lisp")

;; If you are interested in maintaining CMUCL or compiling it
;; from source then set this to where the source files are.
; (setq cmulisp-local-source-directory
;       "/usr/robotics/shared/cmu-cl/17e/")

; (setq akcl-program "kcl")
; (setq gcl-program "gcl")
; (setq ecl-program "ecl")

; (setq xlisp-program "xlisp")
; (setq xlispstat-program "xlispstat")

; (setq scm-program "scm -i")
; (setq chez-program "petite")
; (setq stk-program "stk -interactive")
; (setq snow-program "snow -interactive")
; (setq guile-program "guile")


;;; This makes reading a Lisp or Scheme file load in ILISP.

(set-default 'auto-mode-alist
	     (append '(("\\.lisp$" . lisp-mode)
                       ("\\.lsp$" . lisp-mode)
                       ("\\.cl$" . lisp-mode))
                     auto-mode-alist))

(setq lisp-mode-hook '(lambda () (require 'ilisp)))

(set-default 'auto-mode-alist
             (append '(("\\.scm$" . scheme-mode)
                       ("\\.ss$" . scheme-mode)
                       ("\\.stk$" . scheme-mode)
                       ("\\.stklos$" . scheme-mode))
                     auto-mode-alist))

(setq scheme-mode-hook '(lambda () (require 'ilisp)))


;;; Configuration of Erik Naggum's HyperSpec access package.

;; If you have a local copy of the HyperSpec, set its path here.
; (setq common-lisp-hyperspec-root
;       "file:/home/joe/HyperSpec/")


;;; Sample load hook

(add-hook 'ilisp-load-hook
          '(lambda ()
             ;; Change default key prefix to C-c
             (setq ilisp-*prefix* "\C-c")

             ;; Set a keybinding for the COMMON-LISP-HYPERSPEC command
             (defkey-ilisp "" 'common-lisp-hyperspec)

             ;; Make sure that you don't keep popping up the 'inferior
             ;; Lisp' buffer window when this is already visible in
             ;; another frame. Actually this variable has more impact
             ;; than that. Watch out.
             ; (setq pop-up-frames t)

             (message "Running ilisp-load-hook")
             ;; Define LispMachine-like key bindings, too.
             ; (ilisp-lispm-bindings) Sample initialization hook.

             ;; Set the inferior Lisp directory to the directory of
             ;; the buffer that spawned it on the first prompt.
             (add-hook 'ilisp-init-hook
                       '(lambda ()
                          (default-directory-lisp ilisp-last-buffer)))
             ))


;;; end of file -- ilisp.emacs --
