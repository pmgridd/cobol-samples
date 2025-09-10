public class COND88 {

    // Working-Storage Section equivalent
    private static String theAnswer;
    private static char fillerSpace = ' '; // PIC X VALUE SPACE
    private static boolean simple88; // 88 SIMPLE-88 VALUE 'T'
    private static char fillerF = 'F'; // PIC X VALUE 'F'
    // 88 SIMPLE-88-WITH-FALSE VALUE 'T', FALSE 'F'
    private static boolean simple88WithFalse;

    private static char categoryCode; // PIC X VALUE SPACE
    // 88 CATEGORY-A VALUE 'A', '{7', '{7' - assuming '{7' are typos or non-standard, focusing on 'A'
    private static boolean categoryA() {
        return categoryCode == 'A';
    }
    // 88 CATEGORY-B VALUE 'B', '97', 'Xw' - assuming '97', 'Xw' are typos or non-standard, focusing on 'B'
    private static boolean categoryB() {
        return categoryCode == 'B';
    }

    private static int personAge; // PIC 9(03)
    private static boolean personIsAChild() {
        return personAge >= 0 && personAge <= 12;
    }
    private static boolean personIsATeen() {
        return personAge >= 13 && personAge <= 19;
    }
    private static boolean personIsYoungAdult() {
        return personAge >= 20 && personAge <= 35;
    }
    private static boolean personIsAnAdult() {
        return personAge >= 36 && personAge <= 49;
    }
    private static boolean personIsMiddleAged() {
        return personAge >= 50 && personAge <= 59;
    }
    private static boolean personIsASenior() {
        return personAge >= 60 && personAge <= 74;
    }
    private static boolean personIsElderly() {
        return personAge >= 75 && personAge <= 200;
    }

    public static void main(String[] args) {

        // Example 1: Simple 88-level.
        System.out.println("--- Example 1: Simple 88-level ---");
        simple88 = true; // SET SIMPLE-88 TO TRUE
        if (simple88) { // IF SIMPLE-88
            theAnswer = "true"; // MOVE 'true' TO THE-ANSWER
            System.out.println("SIMPLE-88 is " + theAnswer);
        }

        simple88 = false; // Simulating NOT SIMPLE-88, though COBOL SET FALSE is not explicitly shown for SIMPLE-88
        if (!simple88) { // IF NOT SIMPLE-88
            theAnswer = "false"; // MOVE 'false' TO THE-ANSWER
            System.out.println("SIMPLE-88 is " + theAnswer);
        }
        System.out.println();


        // Example 2: 88-level with FALSE clause
        System.out.println("--- Example 2: 88-level with FALSE clause ---");
        simple88WithFalse = true; // SET SIMPLE-88-WITH-FALSE TO TRUE
        if (simple88WithFalse) { // IF SIMPLE-88-WITH-FALSE
            theAnswer = "true"; // MOVE 'true' TO THE-ANSWER
            System.out.println("SIMPLE-88-WITH-FALSE is " + theAnswer);
        }

        simple88WithFalse = false; // SET SIMPLE-88-WITH-FALSE TO FALSE
        if (!simple88WithFalse) { // IF NOT SIMPLE-88-WITH-FALSE
            theAnswer = "false"; // MOVE 'false' TO THE-ANSWER
            System.out.println("SIMPLE-88-WITH-FALSE is " + theAnswer);
        }
        System.out.println();


        // Example 3: 88-level with multiple values
        System.out.println("--- Example 3: 88-level with multiple values ---");
        categoryCode = 'A'; // Simulating SET CATEGORY-A TO TRUE
        if (categoryA()) { // IF CATEGORY-A
            theAnswer = "true"; // MOVE 'true' TO THE-ANSWER
            System.out.println("CATEGORY-A is set, THE-ANSWER: " + theAnswer);
        }

        categoryCode = 'E'; // MOVE 'E' TO CATEGORY-CODE
        // EVALUATE TRUE (COBOL)
        if (categoryA()) { // WHEN CATEGORY-A
            theAnswer = "A"; // MOVE 'A' TO THE-ANSWER
        } else if (categoryB()) { // WHEN CATEGORY-B
            theAnswer = "B"; // MOVE 'B' TO THE-ANSWER
        } else { // WHEN OTHER
            theAnswer = "?"; // MOVE '?' TO THE-ANSWER
        }
        System.out.println("Category Code '" + categoryCode + "', THE-ANSWER: " + theAnswer);
        System.out.println();


        // Example 4: 88-level with a range of values
        System.out.println("--- Example 4: 88-level with a range of values ---");
        personAge = 37; // MOVE 37 TO PERSON-AGE
        // EVALUATE TRUE (COBOL)
        if (personIsAChild()) { // WHEN PERSON-IS-A-CHILD
            theAnswer = "child"; // MOVE 'child' TO THE-ANSWER
        } else if (personIsATeen()) { // WHEN PERSON-IS-A-TEEN
            theAnswer = "teen"; // MOVE 'teen' TO THE-ANSWER
        } else if (personIsYoungAdult()) { // WHEN PERSON-IS-YOUNG-ADULT
            theAnswer = "young"; // MOVE 'young' TO THE-ANSWER
        } else if (personIsAnAdult()) { // WHEN PERSON-IS-AN-ADULT
            theAnswer = "adult"; // MOVE 'adult' TO THE-ANSWER
        } else if (personIsMiddleAged()) { // WHEN PERSON-IS-MIDDLE-AGED
            theAnswer = "middle"; // MOVE 'middle' TO THE-ANSWER
        } else if (personIsASenior()) { // WHEN PERSON-IS-A-SENIOR
            theAnswer = "senior"; // MOVE 'senior' TO THE-ANSWER
        } else if (personIsElderly()) { // WHEN PERSON-IS-ELDERLY
            theAnswer = "elderly"; // MOVE 'elderly' TO THE-ANSWER
        } else { // WHEN OTHER
            theAnswer = "ageless"; // MOVE 'ageless' TO THE-ANSWER
        }
        System.out.println("Person Age " + personAge + ", THE-ANSWER: " + theAnswer);
        System.out.println();
    }
}