import java.text.DecimalFormat;

public class BrakesCalculator {

    public static void main(String[] args) {

        // GIVEN-VALUES
        double m = 100.0; // Mass of brake material in KG
        double sh = 800.0; // Specific heat of brake material in Joules per KG x Temp Celsius
        double w = 10000.0; // Weight of the truck in KG
        double d = 75.0; // Vertical displacement on the downhill run in meters
        double a = 9.8; // a: 9.8 meters per second squared

        // CALCULATED-VALUES
        double Mgh; // Gravitational potential energy loss of the truck in descent
        double deltaTCelsius; // Temperature change in Celsius from the heat exchange
        double mc; // Mass of brake material times specific heat
        String deltaTCelsiusFormatted; // Temperature change formatted for display

        // Calculate Mgh (loss of potential energy of the truck)
        // Mgh = (10,000 kg)(9.80 m/s2)(75.0 m) = 7.35 × 106 J.
        Mgh = w * a * d;

        // Calculate the temperature change Mgh / m * sh
        // where m is the mass of the brake material
        // and sh is the specific heat given in the problem setup.

        mc = m * sh;
        deltaTCelsius = Mgh / mc;

        // Format deltaT-Celsius for display
        DecimalFormat decimalFormat = new DecimalFormat("#,##0.00");
        deltaTCelsiusFormatted = decimalFormat.format(deltaTCelsius);

        // Display the result
        System.out.println("deltaT-Celsius: " + deltaTCelsiusFormatted);
    }
}