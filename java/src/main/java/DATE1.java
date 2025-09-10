import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.util.Locale;

public class DATE1 {

    // COBOL: 01 CURRENT-DATE-DATA.
    // In Java, we will use ZonedDateTime to get current date, time, and timezone info.
    private static ZonedDateTime currentDateTime;

    // COBOL: 01 NCSA-TIMESTAMP.
    // In Java, we'll construct this string directly.
    private static String ncsaTimestamp;

    // COBOL: 01 FULL-DATE PIC X(40).
    private static String fullDate;

    // COBOL: 01 SHORTHAND-DATE-US-STYLE.
    private static String shorthandDateUSStyle;

    // COBOL: 01 SHORTHAND-DATE-EURO-STYLE.
    private static String shorthandDateEuroStyle;

    // COBOL: 01 MONTH-ABBR-DATA (using an array for simplicity)
    private static final String[] MONTH_ABBR = {
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };

    // COBOL: 01 DAY-ORDINALS-DATA (using an array for simplicity)
    private static final String[] DAY_ORDINALS = {
            "th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"
    }; // 0th, 1st, 2nd, 3rd, 4th, 5th, 6th, 7th, 8th, 9th

    public static void main(String[] args) {
        // COBOL: MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-DATA
        currentDateTime = ZonedDateTime.now();

        // Example 1: Timestamp in NCSA common log format
        System.out.println("--- Example 1: Timestamp in NCSA common log format ---");
        formatNCSATimestamp();
        System.out.println("Timestamp (in NCSA common log format): " + ncsaTimestamp);
        System.out.println();

        // Example 2: Current date formatted verbosely
        System.out.println("--- Example 2: Current date formatted verbosely ---");
        formatVerboseDate();
        System.out.println("Current date formatted verbosely: " + fullDate);
        System.out.println();

        // Example 3: Shorthand date, US style MM/DD/YY
        System.out.println("--- Example 3: Shorthand date, US style MM/DD/YY ---");
        formatShorthandUSStyle();
        System.out.println("Shorthand date, US style MM/DD/YY: " + shorthandDateUSStyle);
        System.out.println();

        // Example 4: Shorthand date, European style DD.MM.YY
        System.out.println("--- Example 4: Shorthand date, European style DD.MM.YY ---");
        formatShorthandEuroStyle();
        System.out.println("Shorthand date, European style DD.MM.YY: " + shorthandDateEuroStyle);
        System.out.println();
    }

    private static void formatNCSATimestamp() {
        // COBOL: MOVE CORRESPONDING CURRENT-DATE TO NCSA-TIMESTAMP etc.
        // [DD/Mon/YYYY:HH:MM:SS +|-HHMM]
        int dayOfMonth = currentDateTime.getDayOfMonth();
        String monthAbbr = MONTH_ABBR[currentDateTime.getMonthValue() - 1];
        int year = currentDateTime.getYear();
        int hour = currentDateTime.getHour();
        int minute = currentDateTime.getMinute();
        int second = currentDateTime.getSecond();
        ZoneOffset offset = currentDateTime.getOffset();
        String offsetSign = offset.getTotalSeconds() < 0 ? "-" : "+";
        int offsetHours = Math.abs(offset.getTotalSeconds() / 3600);
        int offsetMinutes = Math.abs((offset.getTotalSeconds() % 3600) / 60);

        ncsaTimestamp = String.format(Locale.US,
                "[%02d/%s/%04d:%02d:%02d:%02d %s%02d%02d]",
                dayOfMonth, monthAbbr, year, hour, minute, second,
                offsetSign, offsetHours, offsetMinutes);
    }

    private static void formatVerboseDate() {
        // COBOL: DIVIDE DAY-OF-MONTH IN CURRENT-DATE BY 10 GIVING DAY-DIV-BY-10 REMAINDER DAY-LAST-DIGIT
        // COBOL: STRING MONTH-NAME(MONTH IN CURRENT-DATE) DELIMITED BY SPACE,
        //                SPACE DELIMITED BY SIZE,
        //                DAY-OF-MONTH IN CURRENT-DATE DELIMITED BY SIZE,
        //                DAY-ORDINAL(DAY-LAST-DIGIT) DELIMITED BY SIZE,
        //                ", " DELIMITED BY SIZE,
        //                FULL-YEAR OF CURRENT-DATE DELIMITED BY SIZE
        //          INTO FULL-DATE

        String monthName = currentDateTime.getMonth().getDisplayName(TextStyle.FULL, Locale.US);
        int day = currentDateTime.getDayOfMonth();
        int year = currentDateTime.getYear();

        String dayOrdinalSuffix;
        if (day >= 11 && day <= 13) { // Special case for 11th, 12th, 13th
            dayOrdinalSuffix = "th";
        } else {
            int lastDigit = day % 10;
            dayOrdinalSuffix = DAY_ORDINALS[lastDigit];
        }

        fullDate = String.format(Locale.US, "%s %d%s, %d", monthName, day, dayOrdinalSuffix, year);
    }

    private static void formatShorthandUSStyle() {
        // COBOL: MOVE CORRESPONDING CURRENT-DATE TO SHORTHAND-DATE-US-STYLE
        // MM/DD/YY
        shorthandDateUSStyle = currentDateTime.format(DateTimeFormatter.ofPattern("MM/dd/yy"));
    }

    private static void formatShorthandEuroStyle() {
        // COBOL: MOVE CORRESPONDING CURRENT-DATE TO SHORTHAND-DATE-EURO-STYLE
        // DD.MM.YY
        shorthandDateEuroStyle = currentDateTime.format(DateTimeFormatter.ofPattern("dd.MM.yy"));
    }
}