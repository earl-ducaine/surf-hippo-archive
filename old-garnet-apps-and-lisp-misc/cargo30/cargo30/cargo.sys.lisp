(unless (find-package "KR")
  (load "garnet.sys"))					    ; This file should load all of your Garnet stuff

(unless (find-package "UTILS")
  (load "utils-package"))

(unless (find-package "GUI")
  (load "gui-package"))					    ; This will load the CARGO package if necessary

(load "utils")

;; Pick any one of the following
(load "gui")						    ; Garnet version
;(load "gui.no-graphics")                             ; No-graphics version for overnight runs

(load "board")
