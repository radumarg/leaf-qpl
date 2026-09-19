
### Features Pending Implementation

This document compiles a lists of features which are covered by the [language documentation](README.md) but not yet planned for the alpha release.

Higher priority:

- Modules, resolve imports, cycle detection, shadowing.
- Lean 4 code generation.
- Uncomputation support.
- `affine` & `scratch` qubits.
- Quantum Boolean data (aka Qubits) and Boolean coherent operations.
- Signed/Unsigned quantum integers and quantum arithmetic.
- Signed/Unsigned quantum fixed point numbers and (quantum) operations.
- Arrays, tuples formed from the 3 above.
- Quantum contracts.
 
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
