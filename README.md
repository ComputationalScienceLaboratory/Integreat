# Integreat

![Integreat](Logo/logo.svg)

A comprehensive Mathematica package for analyzing time integration methods including

- Runge-Kutta methods
- Linear multistep methods
- General linear methods

## Installation

The easiest way to install or update to the latest release of Integreat is to evaluate

```mathematica
PacletInstall["https://github.com/ComputationalScienceLaboratory/Integreat/releases/latest/download/Integreat.paclet"]
```

within Mathematica.  For local development, it can be installed by running

```shell
./install.wls
```

within the root directory of the project.  If you would no longer like to use Integreat, it can be uninstalled using

```mathematica
PacletUninstall["Integreat"]
```

## Usage

Integreat can be loaded in Mathematica using

```mathematica
<<Integreat`;
```
