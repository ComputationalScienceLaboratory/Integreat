# Integreat

![Integreat](img/logo.svg)

Integreat is a comprehensive Mathematica package for deriving and analyzing time integration methods including

- Runge–Kutta (RK) methods
  - Functions for 30+ method properties
  - Support for embedded pairs and dense output
  - Order conditions generated via B-trees
- Linear multistep methods (LMMs)
  - Constructors for Adams methods, BDF, Nyström methods, and other popular families
  - Functions for 10+ method properties
- General linear methods (GLMs)
  - Functions for 15+ method properties
  - Support for casting RK methods and LMMs as GLMs
  - Order conditions for high stage order methods
- Trees for Taylor series analysis
  - B-trees
  - N-trees for partitioned ODEs
  - Arbitrary-index DAE-trees

## Documentation

[View the online documentation](https://computationalsciencelaboratory.github.io/Integreat/html/tutorial/IntegreatPackage.html)

Documentation is also added to Mathematica's documentation center when Integrate is installed.

## Examples

Compute properties of a Runge–Kutta method:

```mathematica
<<Integreat`RK`
rk = RKCollocation[{0, 1/2, 1}] (* Create a 3 stage Lobatto IIIA method *)
RKA[rk] (* Access the A coefficient matrix *)
RKDenseOutput[rk] (* Collocation methods are equipped with dense output *)
RKOrder[rk] (* Returns 4. Behind the scenes, this uses B-trees. *)
RKAStable[rk] (* Returns True *)
RKAbsoluteMonotonicityRadius[rk] (* Returns 0 *)
RKOrderStarPlot[rk] (* Visualize the order star *)
Manipulate[RKLinearStabilityPlot[rk, DenseOutput -> θ], {θ, 0, 1}] (* View the stability region of the dense output solution for different values of θ *)
```

Derive the second order Adams–Bashforth method:

```mathematica
<<Integreat`LMM`
lmm = LMM[{0, a1, 1}, {b0, b1, 0}] (* Create an explicit, 2 step linear multistep method *)
oc = LMMOrderConditions[lmm, 2] (* Generate order conditions up to order 2 *)
sols = Solve[oc == 0] (* Find solution to order conditions *)
lmm = lmm /. First[sols] (* Substitute solution into method *)
LMMAdamsBashforth[2] (* There's also a constructor for Adams–Bashforth methods *)
```

## Installation

Installing or upgrading Integreat is as simple as running

```mathematica
PacletInstall["https://github.com/ComputationalScienceLaboratory/Integreat/releases/latest/download/Integreat.paclet"]
```

Once installed, Integreat subpackages can be loaded with

```mathematica
<<Integreat`RK`;
<<Integreat`LMM`;
<<Integreat`GLM`;
```

Integreat can be uninstalled using

```mathematica
PacletUninstall["Integreat"]
```
