import java.nio.charset.StandardCharsets;

public class HEX2TEXT {

    // COBOL: HEXSTR PIC X(16) VALUE "0123456789ABCDEF".
    private static final char[] HEX_CHARS = "0123456789ABCDEF".toCharArray();

    /**
     * Converts any value into displayable hexadecimal characters, mimicking the COBOL HEX2TEXT program.
     * This method assumes the input String's characters can be represented as single bytes
     * using the ISO_8859_1 (Latin-1) character set, which is common for COBOL PIC X data.
     * Each input byte is converted into two hexadecimal characters.
     *
     * @param originalValue The input string to be converted to hexadecimal. Corresponds to LS-ORIGINAL-VALUE.
     * @return A string containing the hexadecimal representation of the input value. Corresponds to LS-RESULT.
     */
    public static String hexToText(String originalValue) {
        if (originalValue == null || originalValue.isEmpty()) {
            return "";
        }

        // In COBOL, `LS-ORIGINAL-VALUE(I:1) TO DECBYTE` where DECBYTE redefines DEC (COMP)
        // effectively gets the byte value of the character. We achieve this in Java
        // by converting the string to a byte array using a single-byte encoding.
        // ISO_8859_1 is a common choice for this, mapping characters 0-255 to their byte values.
        byte[] bytes = originalValue.getBytes(StandardCharsets.ISO_8859_1);

        // LS-RESULT is PIC X(240), which implies original max length 120 * 2 hex chars.
        // StringBuilder is efficient for building strings in a loop.
        StringBuilder resultBuilder = new StringBuilder(bytes.length * 2);

        // PERFORM VARYING I FROM 1 BY 1 UNTIL I > LS-ORIGINAL-LENGTH
        // In Java, loop through the byte array from index 0 to length-1.
        for (int i = 0; i < bytes.length; i++) {
            // currentByte holds the value equivalent to what would be in DECBYTE (and then DEC)
            // in COBOL after `MOVE LS-ORIGINAL-VALUE(I:1) TO DECBYTE`.
            // The `& 0xFF` converts the signed byte to an unsigned integer (0-255).
            int decValue = bytes[i] & 0xFF;

            // DIVIDE DEC BY 16 GIVING Q REMAINDER R
            // q will be the high nibble (first hex digit), r will be the low nibble (second hex digit).
            int q = decValue / 16;
            int r = decValue % 16;

            // MOVE HEXSTR(Q1:1) TO LS-RESULT(J:1)
            // MOVE HEXSTR(R1:1) TO LS-RESULT(J1:1)
            // Append the corresponding hex characters from our lookup table.
            resultBuilder.append(HEX_CHARS[q]);
            resultBuilder.append(HEX_CHARS[r]);
        }

        return resultBuilder.toString();
    }

    public static void main(String[] args) {
        // Example usage to demonstrate the conversion, similar to testing a COBOL subprogram.
        String input1 = "Hello, World!";
        System.out.println("Original: \"" + input1 + "\"");
        System.out.println("Hex:      \"" + hexToText(input1) + "\""); // Expected: 48656C6C6F2C20576F726C6421

        String input2 = "123abcABC";
        System.out.println("Original: \"" + input2 + "\"");
        System.out.println("Hex:      \"" + hexToText(input2) + "\""); // Expected: 313233616263414243

        String input3 = "Special chars: !@#$%^&*()";
        System.out.println("Original: \"" + input3 + "\"");
        System.out.println("Hex:      \"" + hexToText(input3) + "\""); // Expected: 5370656369616C2063686172733A2021402324255E262A2829

        String emptyInput = "";
        System.out.println("Original: \"" + emptyInput + "\"");
        System.out.println("Hex:      \"" + hexToText(emptyInput) + "\""); // Expected: ""

        String nullInput = null;
        System.out.println("Original: null");
        System.out.println("Hex:      \"" + hexToText(nullInput) + "\""); // Expected: ""
    }
}