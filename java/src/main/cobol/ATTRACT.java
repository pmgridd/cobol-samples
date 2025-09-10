public class Attract {

    // Gravitational constant G
    private static final double G = 6.67428e-11;

    public static void main(String[] args) {
        // For demonstration, let's define two bodies with some initial values
        // In a real application, these would be user inputs or read from a file

        // Body 1
        double mass1 = 1.0; // kg
        double px1 = 0.0;   // meters
        double py1 = 0.0;   // meters

        // Body 2
        double mass2 = 1.0; // kg
        double px2 = 1.0;   // meters
        double py2 = 1.0;   // meters

        // Calculate distance components
        double dx = px1 - px2;
        double dy = py1 - py2;

        // Calculate distance between the two bodies
        double d = Math.sqrt((dx * dx) + (dy * dy));

        if (d == 0) {
            System.out.println("The bodies are in the same position!");
            return; // Exit if distance is zero to avoid division by zero
        }

        System.out.println("The distance between the bodies is: " + d + " meters");

        // Calculate the force of attraction
        double f = (G * mass1 * mass2) / (d * d);
        System.out.println("The force of attraction is: " + f + " Newtons");

        // Calculate the direction of the force (theta)
        // Using Math.atan2(dy, dx) which handles all quadrants correctly
        double theta = Math.atan2(dy, dx); // radians

        // Calculate force components along x and y axes
        double fx = Math.cos(theta) * f;
        double fy = Math.sin(theta) * f;

        System.out.println("The force along the X axis: " + fx + " Newtons");
        System.out.println("The force along the Y axis: " + fy + " Newtons");
    }
}