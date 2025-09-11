import java.util.Scanner;

public class Attract {

    // Global constant
    private static final double G = 6.67428e-11;

    // Body attributes
    private static double[] mass = new double[2];
    private static double[] vx = new double[2];
    private static double[] vy = new double[2];
    private static double[] px = new double[2];
    private static double[] py = new double[2];

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        // Equivalent to COBOL's "perform with test before varying BodyIx from 1 by 1 until BodyIx > 2 perform SolicitAttributesOfBody"
        for (int bodyIx = 0; bodyIx < 2; bodyIx++) { // In Java, arrays are 0-indexed
            solicitAttributesOfBody(bodyIx);
        }

        for (int bodyIx = 0; bodyIx < 2; bodyIx++) {
            verifyAttributesOfBody(bodyIx);
        }

        System.out.println();
        System.out.println("Do you want to proceed? (Y/n)");
        String continueReply = scanner.nextLine();

        if (continueReply.equalsIgnoreCase("n")) {
            System.out.println("Maybe next time. Bye!");
            return; // Equivalent to goback
        }

        computeAttraction();

        // The original COBOL has a 'goback' here. In Java, main method simply ends.
        scanner.close();
    }

    private static void solicitAttributesOfBody(int bodyIx) {
        System.out.println();
        System.out.println("Enter attributes of body #" + (bodyIx + 1) + " :");
        System.out.println("Please enter the mass of the body in KG:");
        mass[bodyIx] = scanner.nextDouble();
        scanner.nextLine(); // Consume newline

        System.out.println("Please enter the body's velocity on the X axis:");
        vx[bodyIx] = scanner.nextDouble();
        scanner.nextLine();

        System.out.println("Please enter the body's velocity on the Y axis:");
        vy[bodyIx] = scanner.nextDouble();
        scanner.nextLine();

        System.out.println("Please enter the body's position on the X axis:");
        px[bodyIx] = scanner.nextDouble();
        scanner.nextLine();

        System.out.println("Please enter the body's position on the Y axis:");
        py[bodyIx] = scanner.nextDouble();
        scanner.nextLine();
    }

    private static void verifyAttributesOfBody(int bodyIx) {
        System.out.println();
        System.out.println("Body #" + (bodyIx + 1) + " attributes:");
        System.out.printf("    mass: %.2e%n", mass[bodyIx]);
        System.out.printf("    vx, vy: %.2e, %.2e%n", vx[bodyIx], vy[bodyIx]);
        System.out.printf("    px, py: %.2e, %.2e%n", px[bodyIx], py[bodyIx]);
    }

    private static void computeAttraction() {
        double dx = px[0] - px[1];
        double dy = py[0] - py[1];
        double d = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2)); // Using Math.pow for clarity

        if (d == 0) {
            System.out.println("The bodies are in the same position!");
            return;
        }
        System.out.printf("The distance between the bodies is: %.2e%n", d);

        double f = (G * mass[0] * mass[1]) / (d * d);
        System.out.printf("The force of attraction is: %.2e%n", f);

        // COBOL: compute theta = function atan(dx)
        // Note: In typical physics, the angle for vector components is derived using atan2(dy, dx).
        // The original COBOL uses atan(dx), which might be an oversimplification or a specific domain calculation.
        double theta = Math.atan(dx);

        // COBOL: compute fx = function cos(theta * f)
        // Note: This COBOL expression (cos(angle * scalar)) is mathematically unusual for force decomposition.
        // Typically, it would be 'f * Math.cos(theta)'. The current translation directly reflects the COBOL.
        double fx = Math.cos(theta * f);

        // COBOL: compute fy = function sin(theta * f)
        // Note: Similar to fx, this is also mathematically unusual.
        double fy = Math.sin(theta * f);

        System.out.printf("The force along the X axis: %.2e%n", fx);
        System.out.printf("The force along the Y axis: %.2e%n", fy);
    }
}