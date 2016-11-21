cd "/home/ctec/rcs_colab/forward/demo"
automatic
destroy-facts
destroy-rules
destroy-magic
consult contour.rf
spy
magic-transform (contour s3 _x)
magic-query (contour s3 _x)
nospy


