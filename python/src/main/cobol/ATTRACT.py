import math

# Gravitational constant G
G = 6.67428e-11

def solicit_attributes_of_body(body_number):
    print(f"Enter attributes of body #{body_number}:")
    mass = float(input(f"Please enter the mass of body #{body_number} in KG: "))
    
    print(f"Please enter the body's velocity on the X axis: ", end="")
    vx = float(input())
    print(f"Please enter the body's velocity on the Y axis: ", end="")
    vy = float(input())

    print(f"Please enter the body's position on the X axis: ", end="")
    px = float(input())
    print(f"Please enter the body's position on the Y axis: ", end="")
    py = float(input())
    
    return mass, vx, vy, px, py

def verify_attributes_of_body(body_number, mass, vx, vy, px, py):
    print(f"Body #{body_number} attributes:")
    print(f"   mass: {mass}")
    print(f"   vx, vy: {vx}, {vy}")
    print(f"   px, py: {px}, {py}")

def compute_attraction(body1_mass, body1_px, body1_py, body2_mass, body2_px, body2_py):
    dx = body1_px - body2_px
    dy = body1_py - body2_py
    
    d = math.sqrt((dx * dx) + (dy * dy))
    
    if d == 0:
        print("The bodies are in the same position!")
        return 0, 0, 0 # No force if positions are the same to avoid division by zero
    
    print(f"The distance between the bodies is: {d}")
    
    f = (G * body1_mass * body2_mass) / (d * d)
    print(f"The force of attraction is: {f}")

    # Compute direction of force
    theta = math.atan2(dy, dx) # atan2(y, x) gives the angle in radians
    
    fx = math.cos(theta) * f
    fy = math.sin(theta) * f
    
    print(f"The force along the X axis: {fx}")
    print(f"The force along the Y axis: {fy}")
    
    return f, fx, fy

def main():
    bodies_data = []
    for i in range(1, 3):
        mass, vx, vy, px, py = solicit_attributes_of_body(i)
        bodies_data.append({'mass': mass, 'vx': vx, 'vy': vy, 'px': px, 'py': py})

    for i in range(2):
        verify_attributes_of_body(i + 1, bodies_data[i]['mass'], bodies_data[i]['vx'], 
                                   bodies_data[i]['vy'], bodies_data[i]['px'], bodies_data[i]['py'])

    proceed = input("Do you want to proceed? (Y/n): ").strip().lower()
    if proceed == 'n':
        print("Maybe next time. Bye!")
        return

    compute_attraction(bodies_data[0]['mass'], bodies_data[0]['px'], bodies_data[0]['py'],
                       bodies_data[1]['mass'], bodies_data[1]['px'], bodies_data[1]['py'])

if __name__ == "__main__":
    main()
