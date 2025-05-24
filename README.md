Parametrized TPU 
=======================

This project is an in-progress TPU generator. Both MAC bit width and systolic array size will be parametrized.

# How to Run #

1. Make sure you have sbt installed. 
2. From the main directory, run `sbt test`. This will run all tests.

# Progress #
## Completed ##
- Multiply-and-accumulate unit runs and is parametrizable by data width.
- A fixed size prototype of the TPU (TPU_fixed) has basic IO wiring.
- fixedTPUModel models the behavior of TPU_fixed for testing purposes
## In progress ##
- TPU_fixed needs to calculate results.
- We are working to convert TPU_fixed into a parametrized version and to build a parametrized model.
