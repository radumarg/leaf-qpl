//////////////////////////////////////////////////////////////
// Phase Kickback Example
//
// The standard extension of Leaf code files should be `.lf`
// We use `.rs` here for syntax highlighting purposes only.
//////////////////////////////////////////////////////////////


general fn phase_kickback() -> bit {
    let q0 = qalloc();
    let q1 = qalloc();

    // Prepare q0 in |+>.
    H(&q0);

    // Prepare q1 in |1>.
    X(&q1);

    // Apply controlled U1(0.785) with q0 as control and q1 as target.
    ctrl(&q0) {
        U1(0.785, &q1);
    }

    // Interference step on the control qubit.
    H(&q0);

    let b = measr(q0);

    // q1 is only used as the phase-kickback target.
    discard(q1);

    b
}

general fn main() -> bit {
    phase_kickback()
}
