import java.util.Scanner;
import java.lang.Math;

public class ATTRACT {

    // Body class to hold attributes for each body
    static class Body {
        double mass; // mass in kg
        double vx, vy; // x and y velocities in meters per second
        double px, py; // x and y positions in meters
    }

    // Gravitational constant G
    static final double G = 6.67328e-11;

    // Scanner for user input
    static Scanner scanner = new Scanner(System.in);

    // Array to store two Body objects, indexed by 0 and 1 (equivalent to COBOL's BodyIx 1 and 2)
    static Body[] bodies = new Body[2];

    public static void main(String[] args) {
        // Initialize Body objects
        for (int i = 0; i < bodies.length; i++) {
            bodies[i] = new Body();
        }

        // Solicit and verify attributes for each body
        for (int bodyIx = 0; bodyIx < bodies.length; bodyIx++) {
            solicitAttributesOfBody(bodyIx);
        }

        for (int bodyIx = 0; bodyIx < bodies.length; bodyIx++) {
            verifyAttributesOfBody(bodyIx);
        }

        // Prompt to continue
        System.out.println(); // Equivalent to COBOL 'display space'
        System.out.print("Do you want to proceed? (Y/n): ");
        String continueReply = scanner.next();

        // If user replies 'n' or 'N', exit
        if (continueReply.equalsIgnoreCase("n")) {
            System.out.println("Maybe next time. Bye!");
            return; // Equivalent to COBOL 'goback'
        }

        // Compute attraction
        computeAttraction();

        scanner.close(); // Close the scanner
    }

    // Method to solicit attributes for a body
    static void solicitAttributesOfBody(int bodyIndex) {
        System.out.println(); // 'display space'
        System.out.println("Enter attributes of body # " + (bodyIndex + 1)); // BodyNumber
        System.out.print("Please enter the mass of the body in KG: ");
        bodies[bodyIndex].mass = scanner.nextDouble();

        System.out.print("Please enter the body's velocity on the X axis: ");
        bodies[bodyIndex].vx = scanner.nextDouble();

        System.out.print("Please enter the body's velocity on the Y axis: ");
        bodies[bodyIndex].vy = scanner.nextDouble();

        System.out.print("Please enter the body's position on the X axis: ");
        bodies[bodyIndex].px = scanner.nextDouble();

        System.out.print("Please enter the body's position on the Y axis: ");
        bodies[bodyIndex].py = scanner.nextDouble();
    }

    // Method to verify (display) attributes of a body
    static void verifyAttributesOfBody(int bodyIndex) {
        System.out.println(); // 'display space'
        System.out.println("Body #" + (bodyIndex + 1) + " attributes:");
        System.out.println("    mass: " + bodies[bodyIndex].mass);
        System.out.println("    vx, vy: " + bodies[bodyIndex].vx + ", " + bodies[bodyIndex].vy);
        System.out.println("    px, py: " + bodies[bodyIndex].px + "," + bodies[bodyIndex].py);
    }

    // Method to compute gravitational attraction
    static void computeAttraction() {
        // Compute the distance between the two bodies
        double dx = bodies[0].px - bodies[1].px;
        double dy = bodies[0].py - bodies[1].py;
        double d = Math.sqrt((dx * dx) + (dy * dy));

        // If distance is zero, display message and exit
        if (d == 0) {
            System.out.println("The bodies are in the same position!");
            return; // 'goback'
        }
        System.out.println("The distance between the bodies is: " + d);

        // Compute the force of attraction
        double f = (G * bodies[0].mass * bodies[1].mass) / (d * d);
        System.out.println("The force of attaction is: " + f);

        // Compute the direction of force
        // NOTE: The original COBOL code has a comment about this being "wrong, but OK for this demo."
        // It uses `atan(dx)` which is not the correct way to get the angle from dx and dy.
        // A mathematically correct approach would use `Math.atan2(dy, dx)`.
        // Also, the subsequent calculation for fx and fy `cos(theta * f)` and `sin(theta * f)`
        // is incorrect as it multiplies the angle by the force before taking sin/cos.
        // We are replicating the original COBOL logic here for a direct migration.
        double theta = Math.atan(dx);

        double fx = Math.cos(theta * f);
        double fy = Math.sin(theta * f);

        System.out.println("The force along the X axis: " + fx);
        System.out.println("The force along the Y axis: " + fy);
    }
}