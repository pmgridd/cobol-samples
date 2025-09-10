public class IFEVAL {

    // COBOL: 01 FILLER.
    //        05 RESULT-OF-COMPARE    PIC X(10).
    private static String resultOfCompare;

    // COBOL: 05 ALPHA-1              PIC X(10).
    // COBOL: 05 ALPHA-2              PIC X(10).
    private static String alpha1;
    private static String alpha2;

    // COBOL: 05 NUMERIC-1             PIC S9(03) COMP-3.
    private static int numeric1;
    // COBOL: 05 NUMERIC-2-X.
    //        10 NUMERIC-2            PIC S9(13) COMP-3.
    // In Java, we'll use a String for NUMERIC-2-X to simulate potentially non-numeric input,
    // and an int for NUMERIC-2 when we need its numeric value.
    private static String numeric2X; // Simulates the PIC X representation
    private static long numeric2;     // Using long as COBOL S9(13) can be large

    // Helper method to check if a string is numeric (mimicking COBOL IS NUMERIC)
    private static boolean isNumeric(String str) {
        if (str == null || str.isEmpty()) {
            return false;
        }
        try {
            Long.parseLong(str.trim());
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static void main(String[] args) {
        System.out.println("*** IFEVAL COBOL to Java Migration Demo ***\n");

        // Example 1: IF statement, 2 alphanumeric items.
        System.out.println("--- Example 1: IF statement, 2 alphanumeric items ---");
        alpha1 = "cucumber"; // MOVE 'cucumber' TO ALPHA-1
        alpha2 = "radish";   // MOVE 'radish' TO ALPHA-2

        // IF ALPHA-1 IS EQUAL TO ALPHA-2
        if (alpha1.trim().equals(alpha2.trim())) {
            resultOfCompare = "equal"; // MOVE 'equal' TO RESULT-OF-COMPARE
        } else { // ELSE
            resultOfCompare = "different"; // MOVE 'different' TO RESULT-OF-COMPARE
        }
        System.out.println("Alpha-1: \"" + alpha1.trim() + "\", Alpha-2: \"" + alpha2.trim() + "\" -> Result: " + resultOfCompare);

        // Re-test with equal values
        alpha1 = "cucumber";
        alpha2 = "cucumber";
        if (alpha1.trim().equals(alpha2.trim())) {
            resultOfCompare = "equal";
        } else {
            resultOfCompare = "different";
        }
        System.out.println("Alpha-1: \"" + alpha1.trim() + "\", Alpha-2: \"" + alpha2.trim() + "\" -> Result: " + resultOfCompare);
        System.out.println();


        // Example 2: IF statement, alphanumeric field vs literal
        System.out.println("--- Example 2: IF statement, alphanumeric field vs literal ---");
        alpha1 = "foobar"; // MOVE 'foobar' TO ALPHA-1

        // IF ALPHA-1 IS EQUAL TO 'foobar'
        if (alpha1.trim().equals("foobar")) {
            resultOfCompare = "equal"; // MOVE 'equal' TO RESULT-OF-COMPARE
        } else { // ELSE
            resultOfCompare = "different"; // MOVE 'different' TO RESULT-OF-COMPARE
        }
        System.out.println("Alpha-1: \"" + alpha1.trim() + "\" vs 'foobar' -> Result: " + resultOfCompare);
        System.out.println();


        // Example 3: Verify a numeric item contains numeric data
        System.out.println("--- Example 3: Verify a numeric item contains numeric data ---");
        numeric2X = "garbage"; // MOVE 'garbage' TO NUMERIC-2-X

        // IF NUMERIC-2 IS NUMERIC
        if (isNumeric(numeric2X.trim())) {
            numeric2 = Long.parseLong(numeric2X.trim()); // ADD 1 TO NUMERIC-2
            numeric2++;
            System.out.println("Numeric-2 (after ADD 1): " + numeric2);
        } else { // ELSE
            numeric2 = 1; // MOVE 1 TO NUMERIC-2
            System.out.println("Numeric-2 (set to 1): " + numeric2);
        }
        System.out.println("Initial Numeric-2-X: \"" + numeric2X.trim() + "\" -> Result: " + numeric2);

        numeric2X = "123";
        if (isNumeric(numeric2X.trim())) {
            numeric2 = Long.parseLong(numeric2X.trim());
            numeric2++;
            System.out.println("Numeric-2 (after ADD 1): " + numeric2);
        } else {
            numeric2 = 1;
            System.out.println("Numeric-2 (set to 1): " + numeric2);
        }
        System.out.println("Initial Numeric-2-X: \"" + numeric2X.trim() + "\" -> Result: " + numeric2);
        System.out.println();


        // Example 4: Verify a numeric item is greater than zero
        System.out.println("--- Example 4: Verify a numeric item is greater than zero ---");
        numeric1 = 0; // MOVE ZERO TO NUMERIC-1
        numeric2 = 100; // MOVE 100 TO NUMERIC-2

        // IF NUMERIC-1 IS GREATER THAN ZERO
        if (numeric1 > 0) {
            // DIVIDE NUMERIC-2 BY NUMERIC-1 GIVING NUMERIC-2
            numeric2 = numeric2 / numeric1;
        } else { // ELSE
            // SUBTRACT 1 FROM NUMERIC-2 GIVING NUMERIC-2
            numeric2--;
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2 (after operation): " + numeric2);
        System.out.println();


        // Example 5: IF statement, two numeric fields
        System.out.println("--- Example 5: IF statement, two numeric fields ---");
        numeric1 = 7;  // MOVE 7 TO NUMERIC-1
        numeric2 = 36; // MOVE 36 TO NUMERIC-2

        // IF NUMERIC-1 IS GREATER THAN NUMERIC-2
        if (numeric1 > numeric2) {
            resultOfCompare = "numeric-1"; // MOVE 'numeric-1' TO RESULT-OF-COMPARE
        } else { // ELSE
            resultOfCompare = "numeric-2"; // MOVE 'numeric-2' TO RESULT-OF-COMPARE
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + " -> Result: " + resultOfCompare);

        numeric1 = 40;
        if (numeric1 > numeric2) {
            resultOfCompare = "numeric-1";
        } else {
            resultOfCompare = "numeric-2";
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + " -> Result: " + resultOfCompare);
        System.out.println();


        // Example 6: EVALUATE statement
        System.out.println("--- Example 6: EVALUATE statement (simple) ---");
        numeric1 = 8;  // MOVE 8 TO NUMERIC-1
        numeric2 = 13; // MOVE 13 TO NUMERIC-2

        // EVALUATE TRUE
        if (numeric1 > numeric2) { // WHEN NUMERIC-1 IS GREATER THAN NUMERIC-2
            resultOfCompare = "numeric-1"; // MOVE 'numeric-1' TO RESULT-OF-COMPARE
        } else if (numeric1 < numeric2) { // WHEN NUMERIC-1 < NUMERIC-2
            resultOfCompare = "numeric-2"; // MOVE 'numeric-2' TO RESULT-OF-COMPARE
        } else { // WHEN OTHER
            resultOfCompare = "equal"; // MOVE 'equal' TO RESULT-OF-COMPARE
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + " -> Result: " + resultOfCompare);

        numeric1 = 20;
        numeric2 = 10;
        if (numeric1 > numeric2) {
            resultOfCompare = "numeric-1";
        } else if (numeric1 < numeric2) {
            resultOfCompare = "numeric-2";
        } else {
            resultOfCompare = "equal";
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + " -> Result: " + resultOfCompare);
        System.out.println();


        // Example 7: EVALUATE statement, two conditions (EVALUATE TRUE ALSO TRUE)
        System.out.println("--- Example 7: EVALUATE statement, two conditions ---");
        numeric1 = 8;  // MOVE 8 TO NUMERIC-1
        numeric2 = 13; // MOVE 13 TO NUMERIC-2
        alpha1 = "THX-1138"; // MOVE 'THX-1138' TO ALPHA-1
        alpha2 = "Terminator"; // MOVE 'Terminator' TO ALPHA-2

        // EVALUATE TRUE ALSO TRUE (COBOL effectively checks each WHEN condition in order)
        if (numeric1 > numeric2 && alpha1.startsWith("THX")) { // WHEN NUMERIC-1 IS GREATER THAN NUMERIC-2 ALSO ALPHA-1(1:3) EQUAL 'THX'
            resultOfCompare = "THX and numeric-1"; // MOVE 'THX and numeric-1' TO RESULT-OF-COMPARE
        } else if (numeric1 < numeric2 && alpha1.startsWith("THX")) { // WHEN NUMERIC-1 < NUMERIC-2 ALSO ALPHA-1(1:3) EQUAL 'THX'
            resultOfCompare = "THX and numeric-2"; // MOVE 'THX and numeric-2' TO RESULT-OF-COMPARE
        } else if (numeric1 == numeric1 && alpha2.trim().equals("Terminator")) { // WHEN NUMERIC-1 = NUMERIC-1 ALSO ALPHA-2 = 'Terminator'
            // Note: NUMERIC-1 = NUMERIC-1 is always true, essentially checking only the second condition if reached.
            resultOfCompare = "Terminator and equal numbers"; // MOVE 'Terminator and equal numbers' TO RESULT-OF-COMPARE
        } else { // WHEN OTHER
            resultOfCompare = "undefined"; // MOVE 'undefined' TO RESULT-OF-COMPARE
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + ", Alpha-1: \"" + alpha1.trim() + "\", Alpha-2: \"" + alpha2.trim() + "\" -> Result: " + resultOfCompare);

        numeric1 = 15;
        numeric2 = 10;
        alpha1 = "THX-ABC";
        alpha2 = "Another";
        if (numeric1 > numeric2 && alpha1.startsWith("THX")) {
            resultOfCompare = "THX and numeric-1";
        } else if (numeric1 < numeric2 && alpha1.startsWith("THX")) {
            resultOfCompare = "THX and numeric-2";
        } else if (numeric1 == numeric1 && alpha2.trim().equals("Terminator")) {
            resultOfCompare = "Terminator and equal numbers";
        } else {
            resultOfCompare = "undefined";
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + ", Alpha-1: \"" + alpha1.trim() + "\", Alpha-2: \"" + alpha2.trim() + "\" -> Result: " + resultOfCompare);

        numeric1 = 5;
        numeric2 = 10;
        alpha1 = "ABC-DEF";
        alpha2 = "Terminator";
        if (numeric1 > numeric2 && alpha1.startsWith("THX")) {
            resultOfCompare = "THX and numeric-1";
        } else if (numeric1 < numeric2 && alpha1.startsWith("THX")) {
            resultOfCompare = "THX and numeric-2";
        } else if (numeric1 == numeric1 && alpha2.trim().equals("Terminator")) {
            resultOfCompare = "Terminator and equal numbers";
        } else {
            resultOfCompare = "undefined";
        }
        System.out.println("Numeric-1: " + numeric1 + ", Numeric-2: " + numeric2 + ", Alpha-1: \"" + alpha1.trim() + "\", Alpha-2: \"" + alpha2.trim() + "\" -> Result: " + resultOfCompare);

        System.out.println();
    }
}