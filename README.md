Parametrized TPU 
=======================

This project is a TPU generator. Both MAC bit width and systolic array size are parametrized.

# How to Run #

1. Make sure you have sbt installed. 
2. From the main directory, run `sbt test`. This will run all tests.

# Progress #
## Completed ##
- Parametrized TPU and TPU model are functional and pass tests.
- TPU has parameters for and handles convolution and sparsity.
## Future Work ##
- Integrate more of the model into testing with TPU. Model convolution and sparsity.
- Tiling to allow matrices larger than the systolic array.

