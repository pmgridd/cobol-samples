import java.util.Scanner;
import java.lang.Math;

public class GravitationalAttraction {

    // Gravitational constant G
    private static final double G = 6.67428e-11;

    // Inner class for Body
    static class Body {
        double mass;
        double vx, vy; // velocities
        double px, py; // positions
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        Body[] bodies = new Body[2];
        for (int i = 0; i < 2; i++) {
            bodies[i] = new Body();
            solicitAttributesOfBody(scanner, bodies[i], i + 1);
        }

        for (int i = 0; i < 2; i++) {
            verifyAttributesOfBody(bodies[i], i + 1);
        }

        System.out.println();
        System.out.println("Do you want to proceed? (Y/n)");
        String reply = scanner.next();

        if (reply.equalsIgnoreCase("n")) {
            System.out.println("Maybe next time. Bye!");
            return;
        }

        computeAttraction(bodies);

        scanner.close();
    }

    private static void solicitAttributesOfBody(Scanner scanner, Body body, int bodyNumber) {
        System.out.println();
        System.out.println("Enter attributes of body # " + bodyNumber);
        System.out.println("Please enter the mass of the body in KG:");
        body.mass = scanner.nextDouble();

        System.out.println("Please enter the body's velocity on the X axis:");
        body.vx = scanner.nextDouble();

        System.out.println("Please enter the body's velocity on the Y axis:");
        body.vy = scanner.nextDouble();

        System.out.println("Please enter the body's position on the X axis:");
        body.px = scanner.nextDouble();

        System.out.println("Please enter the body's position on the Y axis:");
        body.py = scanner.nextDouble();
    }

    private static void verifyAttributesOfBody(Body body, int bodyNumber) {
        System.out.println();
        System.out.println("Body #" + bodyNumber + " attributes:");
        System.out.println("    mass: " + body.mass);
        System.out.println("    vx, vy: " + body.vx + ", " + body.vy);
        System.out.println("    px, py: " + body.px + ", " + body.py);
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
        System.out.println("The force of attraction is: " + f);

        // Compute the direction of force
        // The original COBOL used atan(dx) which is incorrect for direction
        // A more appropriate calculation would use atan2(dy, dx)
        // For demonstration purposes, I'll keep the atan(dx) as in the original COBOL
        double theta = Math.atan(dx);
        double fx = Math.cos(theta) * f;
        double fy = Math.sin(theta) * f;

        System.out.println("The force along the X axis :" + fx);
        System.out.println("The force along the Y axis :" + fy);
    }
}
