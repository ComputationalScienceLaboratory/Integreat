# Mathematica ODE Utils

A comprehensive set of tools for analyzing time integration methods including

- Runge-Kutta
- General Linear Methods
- Multirate Generalized Additive Runge-Kutta

## Installation

Within the cloned repository directory, run the following command.

```shell
./install.wls
```

This will copy Mathematica ODE Utils into your Mathematica applications directory.  If an old version already exists there, it will be overwritten.  Now you can load Mathematica ODE Utils within a notebook using the following command.

```mathematica
<< CSL`OdeUtils`;
```

## Todo

- Packlet
- Trees
    - NP-Trees
    - From method, generic matrix form
    - Representation?
    - MakeBoxes?
    - Functions
        - Gamma, sigma, ...
        - Simplifying assumptions
- LMM
    - Constructor
    - Catalog
    - Stability
    - Reducibility/simplify
    - Order conditions
    - Error constant
