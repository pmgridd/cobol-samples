
// This is a placeholder for the migrated COBOL program ATTRACT.CBL
// Actual migration logic would go here.

public class ATTRACT {

    // Gravitational constant G
    private static final double G = 6.67428e-11;

    private Body[] bodies;

    public ATTRACT() {
        this.bodies = new Body[2];
        // Initialize bodies with placeholder values
        bodies[0] = new Body();
        bodies[1] = new Body();
    }

    public void solicitAttributesOfBody(int bodyIndex) {
        System.out.println("Enter attributes of body #" + (bodyIndex + 1));
        // Placeholder for inputting mass, velocity, and position
        // For example:
        // bodies[bodyIndex].setMass( /* read from input */ );
        // bodies[bodyIndex].setVx( /* read from input */ );
        // bodies[bodyIndex].setVy( /* read from input */ );
        // bodies[bodyIndex].setPx( /* read from input */ );
        // bodies[bodyIndex].setPy( /* read from input */ );
    }

    public void verifyAttributesOfBody(int bodyIndex) {
        System.out.println("Body #" + (bodyIndex + 1) + " attributes:");
        // Placeholder for displaying mass, velocity, and position
        // For example:
        // System.out.println("   mass: " + bodies[bodyIndex].getMass());
        // System.out.println("   vx, vy: " + bodies[bodyIndex].getVx() + ", " + bodies[bodyIndex].getVy());
        // System.out.println("   px, py: " + bodies[bodyIndex].getPx() + ", " + bodies[bodyIndex].getPy());
    }

    public void computeAttraction() {
        double dx = bodies[0].getPx() - bodies[1].getPx();
        double dy = bodies[0].getPy() - bodies[1].getPy();

        double d = Math.sqrt( (dx * dx) + (dy * dy) );

        if (d == 0) {
            System.out.println("The bodies are in the same position!");
            return;
        }

        System.out.println("The distance between the bodies is: " + d);

        double f = (G * bodies[0].getMass() * bodies[1].getMass()) / (d * d);
        System.out.println("The force of attraction is: " + f);

        // Placeholder for calculating force along x and y axes
        // In a real scenario, this would use atan2(dy, dx)
        double theta = Math.atan(dx); // Simplified for demonstration
        double fx = Math.cos(theta) * f;
        double fy = Math.sin(theta) * f;

        System.out.println("The force along the X axis: " + fx);
        System.out.println("The force along the Y axis: " + fy);
    }

    public static void main(String[] args) {
        ATTRACT simulation = new ATTRACT();

        // Simulate input for Body 1
        simulation.bodies[0].setMass(5.972e24); // Earth mass
        simulation.bodies[0].setPx(0);
        simulation.bodies[0].setPy(0);
        simulation.bodies[0].setVx(0);
        simulation.bodies[0].setVy(0);

        // Simulate input for Body 2
        simulation.bodies[1].setMass(7.342e22); // Moon mass
        simulation.bodies[1].setPx(3.844e8); // Earth-Moon distance
        simulation.bodies[1].setPy(0);
        simulation.bodies[1].setVx(0);
        simulation.bodies[1].setVy(1022); // Moon orbital velocity

        for (int i = 0; i < 2; i++) {
            simulation.verifyAttributesOfBody(i);
        }

        System.out.println("Do you want to proceed? (Y/n)");
        // In a real application, read user input here.
        // For this demo, we'll proceed automatically.

        simulation.computeAttraction();

        System.out.println("Maybe next time. Bye!");
    }
}

class Body {
    private double mass; // in kg
    private double vx, vy; // velocities in meters per second
    private double px, py; // positions in meters

    public double getMass() { return mass; }
    public void setMass(double mass) { this.mass = mass; }
    public double getVx() { return vx; }
    public void setVx(double vx) { this.vx = vx; }
    public double getVy() { return vy; }
    public void setVy(double vy) { this.vy = vy; }
    public double getPx() { return px; }
    public void setPx(double px) { this.px = px; }
    public double getPy() { return py; }
    public void setPy(double py) { this.py = py; }
}
