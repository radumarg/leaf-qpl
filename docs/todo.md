
### Features Pending Implementation

This document compiles a lists of features which are covered by the [language documentation](README.md) but not yet planned for the alpha release.

Higher priority:

- modules, resolve imports, cycle detection, shadowing
- Lean 4 code generation.
- Uncomputation support.
- `affine` & `scratch` qubits.
- Quantum contracts.
- Coherent Boolean operations on qubits
- Quantum integers and quantum arithmetic

Other:

- Rust style slices.
- Recursion.
- Quantum conditionals.
  - `qif`+`qelse` & `qmatch`.
  - `sif`+`selse` & `smatch`.
- Sum & Product data types:
  - `enum`
  - `qenum`
  - `struct` + `impl` blocks.
- Documentation comments.
- Classical subroutines.
- Prelude math functions.
