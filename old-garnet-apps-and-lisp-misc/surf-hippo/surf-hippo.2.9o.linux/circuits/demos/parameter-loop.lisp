;; Example of a simple loop script for comparing parameter values.

;; This assumes that the loaded circuit includes a cell type named "CA1", and that the stimulation
;; and plotting parameters are already setup.

(let ((*create-new-simulation-plots* t)
      (*OVERLAY-ALL-PLOTS* nil)
      (*kill-extra-messages* t))
  (loop for r-mem in '(20000 40000 80000) do
	(loop for r-mem-soma in '(5000 10000 20000 40000 80000) do
	      (cell-type-param "CA1" 'rm r-mem)
	      (cell-type-param "CA1" 'rmsoma r-mem-soma)
	      (goferit)
	      (setq *OVERLAY-ALL-PLOTS* t))))

	