import java.util.Scanner;

public class ATTRACT {

    // ComputationalWorkAreas
    private static final double G = 6.67428e-11;
    private double dx;
    private double dy;
    private double d;
    private double f;
    private double theta;
    private double fx;
    private double fy;

    // Body class
    private static class Body {
        double mass;
        double vx;
        double vy;
        double px;
        double py;
    }

    private Body[] bodies = new Body[2];

    // GeneralWorkAreas - PseudoConstants
    private String velocityText = "velocity";
    private String positionText = "position";

    // GeneralWorkAreas - IdentifyTheBody
    private String identifyTheBodyPrompt = "Enter attributes of body # ";
    private int bodyNumber;

    // GeneralWorkAreas - PromptForBodyMass
    private String promptForBodyMass = "Please enter the mass of the body in KG:";

    // GeneralWorkAreas - PromptForVelocityOrPosition
    private String promptBodyAttribute = "Please enter the body's ";
    private String velocityOrPosition;
    private String promptAxis;

    // GeneralWorkAreas - BodyAttributesDisplay
    private String bodyAttributesPrefix = "Body #";
    private int bodyNumberDisplay;
    private String bodyAttributesSuffix = " attributes:";

    // GeneralWorkAreas - BodyMassDisplay
    private String bodyMassLabel = "    mass: ";

    // GeneralWorkAreas - BodyVelocityDisplay
    private String bodyVelocityLabel = "    vx, vy: ";

    // GeneralWorkAreas - BodyPositionDisplay
    private String bodyPositionLabel = "    px, py: ";

    // GeneralWorkAreas - PromptToContinue
    private String promptToContinue = "Do you want to proceed? (Y/n)";
    private String continueReply;

    // GeneralWorkAreas - GoodbyeDisplay
    private String goodbyeDisplay = "Maybe next time. Bye!";

    // GeneralWorkAreas - DistanceDisplay
    private String distanceDisplay = "The distance between the bodies is: ";

    // GeneralWorkAreas - DistanceIsZeroDisplay
    private String distanceIsZeroDisplay = "The bodies are in the same position!";

    // GeneralWorkAreas - ForceDisplay
    private String forceDisplay = "The force of attraction is: ";

    // GeneralWorkAreas - ForceAlongAxisDisplay
    private String forceAlongAxisPrefix = "The force along the ";
    private String forceAxis;
    private String forceAlongAxisSuffix = " axis: ";

    private Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        ATTRACT program = new ATTRACT();
        program.run();
    }

    public ATTRACT() {
        for (int i = 0; i < bodies.length; i++) {
            bodies[i] = new Body();
        }
    }

    public void run() {

        for (int bodyIx = 0; bodyIx < 2; bodyIx++) {
            solicitAttributesOfBody(bodyIx);
        }

        for (int bodyIx = 0; bodyIx < 2; bodyIx++) {
            verifyAttributesOfBody(bodyIx);
        }

        System.out.println();
        System.out.println(promptToContinue);
        continueReply = scanner.nextLine();

        if (continueReply.equalsIgnoreCase("n")) {
            System.out.println(goodbyeDisplay);
            return;
        }

        computeAttraction();

        scanner.close();
        return;
    }

    private void solicitAttributesOfBody(int bodyIx) {
        bodyNumber = bodyIx + 1;
        System.out.println();
        System.out.println(identifyTheBodyPrompt + bodyNumber);
        System.out.println(promptForBodyMass);
        bodies[bodyIx].mass = scanner.nextDouble();
        scanner.nextLine(); // consume newline

        velocityOrPosition = velocityText;
        promptAxis = "X";
        System.out.print(promptBodyAttribute + velocityOrPosition + " on the " + promptAxis + forceAlongAxisSuffix);
        bodies[bodyIx].vx = scanner.nextDouble();
        scanner.nextLine(); // consume newline

        promptAxis = "Y";
        System.out.print(promptBodyAttribute + velocityOrPosition + " on the " + promptAxis + forceAlongAxisSuffix);
        bodies[bodyIx].vy = scanner.nextDouble();
        scanner.nextLine(); // consume newline

        velocityOrPosition = positionText;
        promptAxis = "X";
        System.out.print(promptBodyAttribute + velocityOrPosition + " on the " + promptAxis + forceAlongAxisSuffix);
        bodies[bodyIx].px = scanner.nextDouble();
        scanner.nextLine(); // consume newline

        promptAxis = "Y";
        System.out.print(promptBodyAttribute + velocityOrPosition + " on the " + promptAxis + forceAlongAxisSuffix);
        bodies[bodyIx].py = scanner.nextDouble();
        scanner.nextLine(); // consume newline
    }

    private void verifyAttributesOfBody(int bodyIx) {
        bodyNumberDisplay = bodyIx + 1;
        System.out.println();
        System.out.println(bodyAttributesPrefix + bodyNumberDisplay + bodyAttributesSuffix);
        System.out.println(bodyMassLabel + bodies[bodyIx].mass);
        System.out.println(bodyVelocityLabel + bodies[bodyIx].vx + ", " + bodies[bodyIx].vy);
        System.out.println(bodyPositionLabel + bodies[bodyIx].px + ", " + bodies[bodyIx].py);
    }

    private void computeAttraction() {
        // Compute the distance between the two bodies
        dx = bodies[0].px - bodies[1].px;
        dy = bodies[0].py - bodies[1].py;
        d = Math.sqrt((dx * dx) + (dy * dy));

        if (d == 0) {
            System.out.println(distanceIsZeroDisplay);
            return;
        }
        System.out.println(distanceDisplay + d);

        // Compute the force of attraction
        f = (G * bodies[0].mass * bodies[1].mass) / (d * d);
        System.out.println(forceDisplay + f);

        // Compute the direction of force
        // The original COBOL code has a comment about this being "wrong, but OK for this demo."
        // I will replicate the "wrong" behavior for consistency with the original.
        theta = Math.atan(dx);
        fx = Math.cos(theta * f);
        fy = Math.sin(theta * f);

        forceAxis = "X";
        System.out.println(forceAlongAxisPrefix + forceAxis + forceAlongAxisSuffix + fx);
        forceAxis = "Y";
        System.out.println(forceAlongAxisPrefix + forceAxis + forceAlongAxisSuffix + fy);
    }
}