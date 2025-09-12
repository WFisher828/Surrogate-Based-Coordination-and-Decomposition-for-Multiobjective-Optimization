This repository includes code which was used for running simulation studies and conducting a case study in the paper "A Constrained Regression Approach to Data-Driven Decomposition for Many-Objective Black-Box Optimization"
For running the code in this repository, one will need:
1. Python
2. Jupyter Notebook
3. R
4. Gurobi (free for researchers/academics)

Files:
helper_functions.py: This file contains three functions: LSE, construct_R, and decomp_orthog. These functions, when used together, allow one to perform our DECOMP method. For decomp_orthog,
we recommend that one normalize both their input variables as well as responses.

example_helper_functions_and_tutorial.ipynb: This file walks a user through using the three functions. In addition, the file discusses the modeling of strong heredity and how that is taken as
input in the decomp_orthog function.

VIPR_Presentation_Example.ipynb: This file was used to create the example included in the "Illustrative Example" section of the paper.

Consistency.ipynb: This file was used for conducting the simulation studies.

ResizeConsistencyImages.ipynb: This file was used for resizing a few of the images in the simulation study. There were a few outliers in the data that made comparing the methods in the images unclear.

SpringExample.ipynb: This is code that was used for demonstrating DECOMP, creating decomposition plots based on output from DECOMP (hard-coded), and calculating adjusted R^2 values (no intercept model)
for the case study.

function.R and main.R: These files are used for generating initial data for the case study.

xdata.csv and ydata.csv: Data for the case study.
