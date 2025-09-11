import java.util.Scanner;

public class GravityCalculator {

    // Gravitational constant G
    private static final double G = 6.67428e-11;

    // Body class
    static class Body {
        double mass; // mass in kg
        double vx, vy; // x and y velocities in meters per second
        double px, py; // x and y positions in meters
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Body[] bodies = new Body[2];

        // Solicit Attributes of Bodies
        for (int i = 0; i < 2; i++) {
            bodies[i] = new Body();
            System.out.println("\nEnter attributes of body #" + (i + 1));
            System.out.print("Please enter the mass of the body in KG: ");
            bodies[i].mass = scanner.nextDouble();

            System.out.print("Please enter the body's velocity on the X axis: ");
            bodies[i].vx = scanner.nextDouble();

            System.out.print("Please enter the body's velocity on the Y axis: ");
            bodies[i].vy = scanner.nextDouble();

            System.out.print("Please enter the body's position on the X axis: ");
            bodies[i].px = scanner.nextDouble();

            System.out.print("Please enter the body's position on the Y axis: ");
            bodies[i].py = scanner.nextDouble();
        }

        // Verify Attributes of Bodies
        for (int i = 0; i < 2; i++) {
            System.out.println("\nBody #" + (i + 1) + " attributes:");
            System.out.println("    mass: " + bodies[i].mass);
            System.out.println("    vx, vy: " + bodies[i].vx + ", " + bodies[i].vy);
            System.out.println("    px, py: " + bodies[i].px + ", " + bodies[i].py);
        }

        System.out.println("\nDo you want to proceed? (Y/n)");
        String reply = scanner.next();
        if (reply.equalsIgnoreCase("n")) {
            System.out.println("Maybe next time. Bye!");
            scanner.close();
            return;
        }

        computeAttraction(bodies);

        scanner.close();
    }

    private static void computeAttraction(Body[] bodies) {
        // Compute the distance between the two bodies
        double dx = bodies[0].px - bodies[1].px;
        double dy = bodies[0].py - bodies[1].py;
        double d = Math.sqrt((dx * dx) + (dy * dy));

        if (d == 0) {
            System.out.println("The bodies are in the same position!");
            return;
        }
        System.out.println("The distance between the bodies is: " + d);

        // Compute the force of attraction
        double f = (G * bodies[0].mass * bodies[1].mass) / (d * d);
        System.out.println("The force of attaction is: " + f);

        // Compute the direction of force
        // The original COBOL code has a comment about atan2 being better,
        // but then uses atan(dx), which is incorrect for direction.
        // For demonstration, I will keep the `atan(dx)` as it was in the original,
        // but note that atan2(dy, dx) is the correct approach for angle between two points.
        double theta = Math.atan(dx);
        double fx = Math.cos(theta) * f;
        double fy = Math.sin(theta) * f;

        System.out.println("The force along the X axis: " + fx);
        System.out.println("The force along the Y axis: " + fy);
    }
}