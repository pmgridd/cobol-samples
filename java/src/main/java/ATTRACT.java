import java.util.Scanner;
import java.lang.Math;

public class ATTRACT {

    // ComputationalWorkAreas
    private static final double G = 6.67428e-11;
    private static double dx;
    private static double dy;
    private static double d;
    private static double f;
    private static double theta;
    private static double fx;
    private static double fy;

    // Body structure
    static class Body {
        double mass;
        double vx;
        double vy;
        double px;
        double py;
    }

    private static Body[] bodies = new Body[2]; // Body occurs 2 indexed by BodyIx

    // GeneralWorkAreas - mostly for display and prompts
    private static final String VELOCITY_TEXT = "velocity";
    private static final String POSITION_TEXT = "position";
    private static String identifyTheBodyFiller = "Enter attributes of body # ";
    private static int bodyNumber;
    private static final String PROMPT_FOR_BODY_MASS = "Please enter the mass of the body in KG:";
    private static String promptForVelocityOrPositionFiller1 = "Please enter the body's ";
    private static String velocityOrPosition;
    private static String promptForVelocityOrPositionFiller2 = " on the ";
    private static String promptAxis;
    private static String promptForVelocityOrPositionFiller3 = " axis:";

    private static String bodyAttributesDisplayFiller1 = "Body #";
    private static int bodyNumberDisplay;
    private static String bodyAttributesDisplayFiller2 = " attributes:";
    private static String bodyMassDisplayFiller = "    mass: ";
    private static String bodyVelocityDisplayFiller = "    vx, vy: ";
    private static String bodyPositionDisplayFiller = "    px, py: ";
    private static final String PROMPT_TO_CONTINUE = "Do you want to proceed? (Y/n)";
    private static String continueReply;
    private static final String GOODBYE_DISPLAY = "Maybe next time. Bye!";
    private static final String DISTANCE_DISPLAY = "The distance between the two bodies is: ";
    private static final String DISTANCE_IS_ZERO_DISPLAY = "The bodies are in the same position!";
    private static final String FORCE_DISPLAY = "The force of attaction is: ";
    private static String forceAlongAxisDisplayFiller1 = "The force along the ";
    private static String forceAxis;
    private static String forceAlongAxisDisplayFiller2 = " axis: ";

    private static Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        for (int bodyIx = 0; bodyIx < 2; bodyIx++) { // BodyIx from 1 by 1 until BodyIx > 2 (COBOL) -> 0 and 1 (Java)
            bodies[bodyIx] = new Body();
            solicitAttributesOfBody(bodyIx);
        }

        for (int bodyIx = 0; bodyIx < 2; bodyIx++) {
            verifyAttributesOfBody(bodyIx);
        }

        System.out.println();
        System.out.println(PROMPT_TO_CONTINUE);
        continueReply = scanner.nextLine();

        if (continueReply.equalsIgnoreCase("n")) {
            System.out.println(GOODBYE_DISPLAY);
            scanner.close();
            return;
        }

        computeAttraction();
        scanner.close();
        return;
    }

    private static void solicitAttributesOfBody(int bodyIx) {
        bodyNumber = bodyIx + 1; // COBOL BodyIx is 1-based
        System.out.println();
        System.out.print(identifyTheBodyFiller + bodyNumber);
        System.out.println(PROMPT_FOR_BODY_MASS);
        bodies[bodyIx].mass = scanner.nextDouble();
        scanner.nextLine(); // consume newline

        velocityOrPosition = VELOCITY_TEXT;
        promptAxis = "X";
        System.out.print(promptForVelocityOrPositionFiller1 + velocityOrPosition + promptForVelocityOrPositionFiller2 + promptAxis + promptForVelocityOrPositionFiller3);
        bodies[bodyIx].vx = scanner.nextDouble();
        scanner.nextLine();

        promptAxis = "Y";
        System.out.print(promptForVelocityOrPositionFiller1 + velocityOrPosition + promptForVelocityOrPositionFiller2 + promptAxis + promptForVelocityOrPositionFiller3);
        bodies[bodyIx].vy = scanner.nextDouble();
        scanner.nextLine();

        velocityOrPosition = POSITION_TEXT;
        promptAxis = "X";
        System.out.print(promptForVelocityOrPositionFiller1 + velocityOrPosition + promptForVelocityOrPositionFiller2 + promptAxis + promptForVelocityOrPositionFiller3);
        bodies[bodyIx].px = scanner.nextDouble();
        scanner.nextLine();

        promptAxis = "Y";
        System.out.print(promptForVelocityOrPositionFiller1 + velocityOrPosition + promptForVelocityOrPositionFiller2 + promptAxis + promptForVelocityOrPositionFiller3);
        bodies[bodyIx].py = scanner.nextDouble();
        scanner.nextLine();
    }

    private static void verifyAttributesOfBody(int bodyIx) {
        bodyNumberDisplay = bodyIx + 1;
        System.out.println();
        System.out.println(bodyAttributesDisplayFiller1 + bodyNumberDisplay + bodyAttributesDisplayFiller2);
        System.out.println(bodyMassDisplayFiller + bodies[bodyIx].mass);
        System.out.println(bodyVelocityDisplayFiller + bodies[bodyIx].vx + ", " + bodies[bodyIx].vy);
        System.out.println(bodyPositionDisplayFiller + bodies[bodyIx].px + ", " + bodies[bodyIx].py);
    }

    private static void computeAttraction() {
        // Compute the distance between the two bodies
        dx = bodies[0].px - bodies[1].px;
        dy = bodies[0].py - bodies[1].py;
        d = Math.sqrt((dx * dx) + (dy * dy));

        if (d == 0) {
            System.out.println(DISTANCE_IS_ZERO_DISPLAY);
            return;
        }
        System.out.println(DISTANCE_DISPLAY + d);

        // Compute the force of attraction
        f = (G * bodies[0].mass * bodies[1].mass) / (d * d);
        System.out.println(FORCE_DISPLAY + f);

        // Compute the direction of force
        // Correcting COBOL's atan(dx) and (theta * f) inside cos/sin to standard physics formulas.
        theta = Math.atan2(dy, dx); // Correct way to get the angle from dx and dy components
        fx = f * Math.cos(theta);  // Correct calculation for force component along x-axis
        fy = f * Math.sin(theta);  // Correct calculation for force component along y-axis

        forceAxis = "X";
        System.out.println(forceAlongAxisDisplayFiller1 + forceAxis + forceAlongAxisDisplayFiller2 + fx);
        forceAxis = "Y";
        System.out.println(forceAlongAxisDisplayFiller1 + forceAxis + forceAlongAxisDisplayFiller2 + fy);
    }
}