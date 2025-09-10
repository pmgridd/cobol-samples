import java.text.DecimalFormat;

public class BRAKES {

    // GIVEN-VALUES
    private static final double m = 100.0; // Mass of brake material in KG
    private static final double sh = 800.0; // Specific heat of brake material in Joules per KG x Temp Celsius
    private static final double w = 10000.0; // Weight of the truck in KG
    private static final double d = 75.0; // Vertical displacement on the downhill run in meters
    private static final double a = 9.8; // a: 9.8 meters per second squared

    // CALCULATED-VALUES
    private static double Mgh; // Gravitational potential energy loss of the truck in descent
    private static double deltaTCelsius; // Temperature change in Celsius from the heat exchange
    private static double mc; // Mass of brake material times specific heat
    private static String deltaTCelsiusFormatted; // Temperature change formatted for display

    public static void main(String[] args) {

        // Calculate Mgh (loss of potential energy of the truck)
        // Mgh = (10,000 kg)(9.80 m/s2)(75.0 m) = 7.35 × 10^6 J.
        Mgh = w * a * d;

        // Calculate the temperature change Mgh / (m * sh)
        // where m is the mass of the brake material
        // and sh is the specific heat given in the problem setup.

        mc = m * sh;
        deltaTCelsius = Mgh / mc;

        // Format for display
        DecimalFormat df = new DecimalFormat("#,##0.00");
        deltaTCelsiusFormatted = df.format(deltaTCelsius);

        System.out.println("deltaT-Celsius: " + deltaTCelsiusFormatted);
    }
}