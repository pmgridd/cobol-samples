import math

# Gravitational constant
G = 6.67428e-11

# Data structures for bodies
# Each body will be a dictionary with 'mass', 'vx', 'vy', 'px', 'py'
bodies = [{}, {}]

def solicit_attributes_of_body(body_index):
    """Prompts the user to enter attributes for a body."""
    print(f"\nEnter attributes of body #{body_index + 1}")
    
    while True:
        try:
            mass = float(input(f"Please enter the mass of body #{body_index + 1} in KG: "))
            break
        except ValueError:
            print("Invalid input. Please enter a number for mass.")

    while True:
        try:
            vx = float(input(f"Please enter the body's velocity on the X axis: "))
            break
        except ValueError:
            print("Invalid input. Please enter a number for velocity X.")

    while True:
        try:
            vy = float(input(f"Please enter the body's velocity on the Y axis: "))
            break
        except ValueError:
            print("Invalid input. Please enter a number for velocity Y.")

    while True:
        try:
            px = float(input(f"Please enter the body's position on the X axis: "))
            break
        except ValueError:
            print("Invalid input. Please enter a number for position X.")

    while True:
        try:
            py = float(input(f"Please enter the body's position on the Y axis: "))
            break
        except ValueError:
            print("Invalid input. Please enter a number for position Y.")

    bodies[body_index] = {'mass': mass, 'vx': vx, 'vy': vy, 'px': px, 'py': py}

def verify_attributes_of_body(body_index):
    """Displays the attributes of a body."""
    body = bodies[body_index]
    print(f"\nBody #{body_index + 1} attributes:")
    print(f"    mass: {body['mass']}")
    print(f"    vx, vy: {body['vx']}, {body['vy']}")
    print(f"    px, py: {body['px']}, {body['py']}")

def compute_attraction():
    """Computes the gravitational attraction between the two bodies."""
    body1 = bodies[0]
    body2 = bodies[1]

    # Compute the distance between the two bodies
    dx = body1['px'] - body2['px']
    dy = body1['py'] - body2['py']
    d = math.sqrt((dx * dx) + (dy * dy))

    if d == 0:
        print("The bodies are in the same position!")
        return 
    
    print(f"The distance between the bodies is: {d}")

    # Compute the force of attraction
    f = (G * body1['mass'] * body2['mass']) / (d * d)
    print(f"The force of attraction is: {f}")

    # Compute the direction of force
    # COBOL comment from original code: "wrong, but OK for this demo." - compute theta = function atan(dx)
    # Using math.atan2(dy, dx) for a more accurate angle calculation in Python.
    # If a strict 1-to-1 conversion of the COBOL's original (and acknowledged as 'wrong')
    # behavior is desired, replace math.atan2(dy, dx) with math.atan(dx).
    theta = math.atan2(dy, dx) 

    fx = math.cos(theta) * f
    fy = math.sin(theta) * f

    print(f"The force along the X axis: {fx}")
    print(f"The force along the Y axis: {fy}")

def main():
    for i in range(2):
        solicit_attributes_of_body(i)

    for i in range(2):
        verify_attributes_of_body(i)

    print("\nDo you want to proceed? (Y/n)")
    continue_reply = input().strip().upper()

    if continue_reply == 'N':
        print("Maybe next time. Bye!")
        return

    compute_attraction()

if __name__ == "__main__":
    main()
