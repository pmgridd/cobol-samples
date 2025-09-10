import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.util.Locale;

public class DATE2 {

    // COBOL: 01 DATE-DATA.
    // Simulating these fields directly from LocalDateTime

    // COBOL: 01 DATE-YYYYMMDD-VALUE.
    // Simulating these fields directly from LocalDateTime

    // COBOL: 01 DAY-VALUE.
    // Simulating these fields directly from LocalDateTime

    // COBOL: 01 DAY-YYYYDDD-VALUE.
    // Simulating these fields directly from LocalDateTime

    // COBOL: 01 DAY-OF-WEEK-VALUE PIC 9. (1=Monday, 7=Sunday in COBOL)
    private static int dayOfWeekValue;

    // COBOL: 01 TIME-VALUE.
    // Simulating these fields directly from LocalDateTime
    private static int timeHour;
    private static int timeMinute;
    private static int timeSecond;
    private static int timeHundredths; // COBOL PIC X(02) for hundredths

    // COBOL: 01 FULL-DATE PIC X(50).
    private static String fullDate;

    // COBOL: 01 DAY-CALCULATION-FIELDS.
    private static int dayDivBy10;
    private static int dayLastDigit;

    // COBOL: 01 SHORTHAND-DATE-US-STYLE.
    private static String shorthandDateUSStyle;

    // COBOL: 01 SHORTHAND-DATE-EURO-STYLE.
    private static String shorthandDateEuroStyle;

    // COBOL: 01 MONTH-ABBR-DATA (using an array for simplicity, assuming 3-letter abbreviations)
    private static final String[] MONTH_ABBR = {
            "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
            "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
    };

    // COBOL: 01 DAY-ORDINALS-DATA (using an array for simplicity)
    private static final String[] DAY_ORDINALS = {
            "th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"
    }; // 0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th

    // COBOL: 01 DAY-NAMES-DATA.
    private static final String[] DAY_NAMES = {
            "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
    };


    public static void main(String[] args) {
        // In COBOL: ACCEPT DATE-VALUE FROM DATE
        //            ACCEPT DATE-YYYYMMDD-VALUE FROM DATE YYYYMMDD
        //            ACCEPT DAY-VALUE FROM DAY
        //            ACCEPT DAY-YYYYDDD-VALUE FROM DAY YYYYDDD
        //            ACCEPT DAY-OF-WEEK-VALUE FROM DAY-OF-WEEK
        //            ACCEPT TIME-VALUE FROM TIME

        LocalDateTime now = LocalDateTime.now();

        // Populating simulated COBOL conceptual data items
        // DAY-OF-WEEK-VALUE: 1=Monday, 7=Sunday
        dayOfWeekValue = now.getDayOfWeek().getValue();

        timeHour = now.getHour();
        timeMinute = now.getMinute();
        timeSecond = now.getSecond();
        // COBOL hundredths of a second (00-99)
        timeHundredths = now.getNano() / 10_000_000;


        // Example 1: Current date formatted verbosely
        System.out.println("--- Example 1: Current date formatted verbosely ---");
        formatVerboseDate(now);
        System.out.println("Current date formatted verbosely: " + fullDate);
        System.out.println();

        // Example 2: Shorthand date, US style MM/DD/YY
        System.out.println("--- Example 2: Shorthand date, US style MM/DD/YY ---");
        formatShorthandUSStyle(now);
        System.out.println("Shorthand date, US style MM/DD/YY: " + shorthandDateUSStyle);
        System.out.println();

        // Example 3: Shorthand date, European style DD.MM.YY
        System.out.println("--- Example 3: Shorthand date, European style DD.MM.YY ---");
        formatShorthandEuroStyle(now);
        System.out.println("Shorthand date, European style DD.MM.YY: " + shorthandDateEuroStyle);
        System.out.println();

        // Example 4: Time with precision of hundredths of a second
        System.out.println("--- Example 4: Time with precision of hundredths of a second ---");
        System.out.printf("Time with precision of hundredths of a second: %02d:%02d:%02d.%02d%n",
                timeHour, timeMinute, timeSecond, timeHundredths);
        System.out.println();
    }

    private static void formatVerboseDate(LocalDateTime dateTime) {
        // COBOL: DIVIDE DATE-DD IN DATE-YYYYMMDD-VALUE BY 10 GIVING DAY-DIV-BY-10 REMAINDER DAY-LAST-DIGIT
        // STRING 'Today is ' DELIMITED BY SIZE,
        //        DAY-NAME(DAY-OF-WEEK-VALUE) DELIMITED BY SPACE,
        //        ', the ' DELIMITED BY SIZE,
        //        DATE-DD IN DATE-YYYYMMDD-VALUE DELIMITED BY SIZE,
        //        DAY-ORDINAL(DAY-LAST-DIGIT) DELIMITED BY SIZE,
        //        ' of ' DELIMITED BY SIZE,
        //        MONTH-NAME(DATE-MM IN DATE-YYYYMMDD-VALUE) DELIMITED BY SPACE,
        //        ', ' DELIMITED BY SIZE,
        //        DATE-YYYY OF DATE-YYYYMMDD-VALUE DELIMITED BY SIZE
        // INTO FULL-DATE

        String dayName = DAY_NAMES[dateTime.getDayOfWeek().getValue() - 1]; // Adjust for 0-indexed array
        int dayOfMonth = dateTime.getDayOfMonth();
        int monthValue = dateTime.getMonthValue();
        String monthName = MONTH_ABBR[monthValue - 1]; // Reusing abbreviations for simplicity; COBOL uses full names.
        int year = dateTime.getYear();

        dayLastDigit = dayOfMonth % 10;
        String dayOrdinalSuffix;
        if (dayOfMonth >= 11 && dayOfMonth <= 13) {
            dayOrdinalSuffix = "th";
        } else {
            dayOrdinalSuffix = DAY_ORDINALS[dayLastDigit];
        }

        fullDate = String.format(Locale.US,
                "Today is %s, the %d%s of %s, %d",
                dayName, dayOfMonth, dayOrdinalSuffix, monthName, year);
    }

    private static void formatShorthandUSStyle(LocalDateTime dateTime) {
        // COBOL: MOVE DATE-MM OF DATE-VALUE TO MONTH OF SHORTHAND-DATE-US-STYLE
        //        MOVE DATE-DD OF DATE-VALUE TO DAY-OF-MONTH OF SHORTHAND-DATE-US-STYLE
        //        MOVE DATE-YY OF DATE-VALUE TO YEAR-2-DIGIT OF SHORTHAND-DATE-US-STYLE
        shorthandDateUSStyle = dateTime.format(DateTimeFormatter.ofPattern("MM/dd/yy"));
    }

    private static void formatShorthandEuroStyle(LocalDateTime dateTime) {
        // COBOL: MOVE DATE-MM OF DATE-VALUE TO MONTH OF SHORTHAND-DATE-EURO-STYLE
        //        MOVE DATE-DD OF DATE-VALUE TO DAY-OF-MONTH OF SHORTHAND-DATE-EURO-STYLE
        //        MOVE DATE-YY OF DATE-VALUE TO YEAR-2-DIGIT OF SHORTHAND-DATE-EURO-STYLE
        shorthandDateEuroStyle = dateTime.format(DateTimeFormatter.ofPattern("dd.MM.yy"));
    }
}