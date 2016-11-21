;This file loads the taxon system into GENERA or KCL or Lucid.

#-:our-defsystem (use-file (logdir-file :top "defsystem"))

(our-fs:logdir-define :TX (our-fs:logdir-extend1 :colab "taxon"))


(our-fs:logdir-define :front-end (our-fs:logdir-extend1 :TX "front-end"))
(our-fs:logdir-define :concrete (our-fs:logdir-extend1 :TX "concrete"))
(our-fs:logdir-define :consistency (our-fs:logdir-extend1 :TX "consistency"))
(our-fs:logdir-define :incr-real-ord (our-fs:logdir-extend1 :TX "incr-real-ord"))
(our-fs:logdir-define :print-fcts (our-fs:logdir-extend1 :TX "print-fcts"))
(our-fs:logdir-define :subsumption (our-fs:logdir-extend1 :TX "subsumption"))
(our-fs:logdir-define :representation (our-fs:logdir-extend1 :TX "representation"))
(our-fs:logdir-define :tx-docu (our-fs:logdir-extend1 :taxon "docu"))
(our-fs:logdir-define :tx-demo (our-fs:logdir-extend1 :taxon "demo"))
(our-fs:logdir-define :tx-help (our-fs:logdir-extend1 :tx-docu "help"))



;(use-file2 (logdir-file :TX "taxon-system"))
;now loaded by start-colab

(use-file2 (logdir-file :front-end "tx-signatur"))

(use-package 'TAXON)

(use-file2 (logdir-file :front-end "tax-specific"))

