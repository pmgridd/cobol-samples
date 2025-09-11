# ATTRACT.py (Migrated from COBOL)

This Python script is a migration of the original `ATTRACT.CBL` COBOL program. It demonstrates the calculation of gravitational attraction between two celestial bodies.

## How to run the script:

1.  Make sure you have Python installed.
2.  Navigate to the `python/src/main/cobol/` directory in your terminal.
3.  Run the script using the command: `python ATTRACT.py`
4.  The script will prompt you to enter the mass, velocity (vx, vy), and position (px, py) for two bodies.
5.  After entering the attributes for both bodies, you will be asked if you want to proceed with the calculations.
6.  The script will then display the distance between the bodies and the gravitational force of attraction, along with its components along the X and Y axes.

## Notes on Migration:

*   The original COBOL program used `COMP-2` for floating-point numbers, which is directly translated to Python's native float type.
*   Input and output operations are handled using `input()` and `print()` functions in Python.
*   The `goback` statements in COBOL are represented by `return` statements in Python functions or by exiting the program.
*   The `function sqrt` and `function atan` in COBOL are mapped to `math.sqrt` and `math.atan2` (for a more accurate angle calculation than `atan(dx)` as noted in the original COBOL comments) in Python. If a strict 1-to-1 conversion of the COBOL's original (and acknowledged as 'wrong') behavior is desired, replace `math.atan2(dy, dx)` with `math.atan(dx)`.
*   Error handling for numerical inputs has been added to improve robustness.
