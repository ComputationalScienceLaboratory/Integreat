# Integreat

![Integreat](Logo/logo.svg)

A comprehensive Mathematica package for deriving and analyzing time integration methods including

- Runge–Kutta (RK) methods
  - Functions for 35+ method properties
  - Support for embedded pairs and dense output
  - Order conditions generated via B-trees
- Linear multistep methods (LMMs)
  - Constructors for Adams methods, BDF, Nyström methods, and other popular families
  - Functions for 10+ method properties
- General linear methods (GLMs)
  - Functions for 15+ method properties
  - Support for casting RK methods and LMMs as GLMs
  - Order conditions for high stage order methods

# Examples

Computing properties of a Runge–Kutta method:

```mathematica
rk = RKCollocation[{0, 1/2, 1}] (* Create a 3 stage Lobatto IIIA method *)
RKA[rk] (* Access the A coefficient matrix *)
RKDenseOutput[rk] (* Collocation methods are equipped with dense output *)
RKOrder[rk] (* Returns 4. Behind the scenes, this uses B-trees. *)
RKAStableCondition[rk] (* Returns True *)
RKAbsoluteMonotonicityRadius[rk] (* Returns 0 *)
Manipulate[RKLinearStabilityPlot[rk, DenseOutput -> θ], {θ, 0, 1}] (* View the stability region of the dense output solution for different values of θ *)
```

Deriving the second order Adams–Bashforth method:

```mathematica
lmm = LMM[{0, a1, 1}, {b0, b1, 0}] (* Create an explicit, 2 step linear multistep method *)
oc = LMMOrderConditions[lmm, 2] (* Generate order conditions up to order 2 *)
sols = Solve[oc == 0] (* Find solution to order conditions *)
lmm = lmm /. First[sols] (* Substitute solution into method *)
LMMAdamsBashforth[2] (* There's also a constructor for Adams-Bashforth methods *)
```

## Installation

The easiest way to install or update to the latest release of Integreat is to evaluate

```mathematica
PacletInstall["https://github.com/ComputationalScienceLaboratory/Integreat/releases/latest/download/Integreat.paclet"]
```

within Mathematica.  For local development, it can be installed by running

```shell
./install.wls
```

within the root directory of the project. Once installed, Integreat can be loaded in Mathematica with

```mathematica
<<Integreat`;
```

If you would no longer like to use Integreat, it can be uninstalled using

```mathematica
PacletUninstall["Integreat"]
```
