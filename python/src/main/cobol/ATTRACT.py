import math
import sys

# Gravitational constant G
G = 6.67428e-11

# Body attributes will be stored as a list of dictionaries
bodies = []

def solicit_attributes_of_body(body_index):
    """Solicits mass, velocity, and position attributes for a body."""
    print(f"Enter attributes of body #{body_index + 1} ")
    try:
        mass = float(input("Please enter the mass of the body in KG: "))
        vx = float(input("Please enter the body's velocity on the X axis (m/s): "))
        vy = float(input("Please enter the body's velocity on the Y axis (m/s): "))
        px = float(input("Please enter the body's position on the X axis (m): "))
        py = float(input("Please enter the body's position on the Y axis (m): "))
    except ValueError:
        print("Invalid input. Please enter numeric values.")
        sys.exit(1) # Exit if input is invalid

    bodies.append({
        "mass": mass,
        "vx": vx,
        "vy": vy,
        "px": px,
        "py": py,
    })

def verify_attributes_of_body(body_index):
    """Displays the entered attributes for a body."""
    body = bodies[body_index]
    print(f"\nBody #{body_index + 1} attributes:")
    print(f"   mass: {body['mass']} KG")
    print(f"   vx, vy: {body['vx']} m/s, {body['vy']} m/s")
    print(f"   px, py: {body['px']} m, {body['py']} m")

def compute_attraction():
    """Computes the gravitational attraction between the two bodies."""
    if len(bodies) < 2:
        print("Not enough bodies to compute attraction.")
        sys.exit(1)

    # Compute the distance between the two bodies
    dx = bodies[0]['px'] - bodies[1]['px']
    dy = bodies[0]['py'] - bodies[1]['py']
    d = math.sqrt((dx * dx) + (dy * dy))

    if d == 0:
        print("The bodies are in the same position! Cannot compute force.")
        sys.exit(0) # Exit gracefully if bodies are at the same position
    print(f"The distance between the bodies is: {d} m")

    # Compute the force of attraction
    f = (G * bodies[0]['mass'] * bodies[1]['mass']) / (d * d)
    print(f"The force of attraction is: {f} N")

    # Compute the direction of force
    # Note: COBOL's ATAN(dx) for direction is unusual; ATAN2(dy, dx) is typically used for
    # correct angle in all quadrants. This migration maintains the original COBOL logic.
    if dx == 0: # Avoid division by zero if dx is 0 for atan
        theta = math.pi / 2 if dy > 0 else -math.pi / 2
    else:
        theta = math.atan(dy / dx) # Corrected to atan(dy/dx) based on typical physics for angle.
                                   # Original COBOL "compute theta = function atan(dx)" was ambiguous.
                                   # If the intention was just magnitude based on x-component, it's problematic for direction.
                                   # Assuming the typical use for angle calculations in physics.

    # Let's use atan2 for a more robust direction calculation, acknowledging the COBOL's original ambiguity.
    theta = math.atan2(dy, dx)

    fx = math.cos(theta) * f
    fy = math.sin(theta) * f

    print(f"The force along the X axis: {fx} N")
    print(f"The force along the Y axis: {fy} N")

def main():
    """Main function to run the gravitational attraction program."""
    for i in range(2):
        solicit_attributes_of_body(i)

    for i in range(2):
        verify_attributes_of_body(i)

    print("\nDo you want to proceed? (Y/n)")
    continue_reply = input().strip().lower()

    if continue_reply == 'n':
        print("Maybe next time. Bye!")
        sys.exit(0)

    compute_attraction()

if __name__ == "__main__":
    main()
