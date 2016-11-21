taxon
consult micro2.tx
switch reduced-abox t
filter
switch filtered-graph t
forward
automatic
consult wp-mcad2nc.rf
consult tx-parts-emul.rf
replace-strategies  tx-additions.rf
compile-strategies
fw-compile
rfe
consult demotxfw
compile
forward
fw-emul
