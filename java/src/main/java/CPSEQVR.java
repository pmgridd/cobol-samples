import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class CPSEQVR {

    // COBOL: SELECT INFILE ASSIGN TO 'VARFILE1'
    private static final String INFILE_NAME = "VARFILE1.txt";
    // COBOL: SELECT OUTFILE ASSIGN TO 'VARFILE2'
    private static final String OUTFILE_NAME = "VARFILE2.txt";

    // COBOL: 05 RECORD-COUNT PIC S9(5) COMP-3.
    private static int recordCount = 0;

    // COBOL: 05 RECORD-FIELD OCCURS 1 TO 10 DEPENDING ON FIELD-COUNT PIC X(05).
    // The COBOL logic appends 'XXXXX' to a field. Each field is 5 chars.
    private static final String APPEND_STRING = "XXXXX";

    public static void main(String[] args) {

        // COBOL: OPEN INPUT INFILE and OPEN OUTPUT OUTFILE
        try (BufferedReader inFile = new BufferedReader(new FileReader(INFILE_NAME));
             BufferedWriter outFile = new BufferedWriter(new FileWriter(OUTFILE_NAME))) {

            String inputRecord;

            // COBOL: PERFORM UNTIL END-OF-INPUT and READ INFILE
            while ((inputRecord = inFile.readLine()) != null) {

                // COBOL: MOVE INPUT-RECORD TO RECORD-AREA
                // COBOL: ADD 1 TO FIELD-COUNT
                // COBOL: MOVE 'XXXXX' TO RECORD-FIELD(FIELD-COUNT)
                // In Java, we'll simply append the string to the line.
                String outputRecord = inputRecord + APPEND_STRING;

                // COBOL: WRITE OUTPUT-RECORD FROM RECORD-AREA
                outFile.write(outputRecord);
                outFile.newLine(); // Ensure each record is on a new line

                // COBOL: ADD 1 TO RECORD-COUNT
                recordCount++;
            }

            // COBOL: DISPLAY 'NUMBER OF RECORDS PROCESSED: ' RECORD-COUNT
            System.out.println("NUMBER OF RECORDS PROCESSED: " + recordCount);

        } catch (IOException e) {
            // COBOL: IF NOT INFILE-OK / IF NOT OUTFILE-OK ... DISPLAY 'FILE STATUS ON OPEN:'
            System.err.println("Error during file operation: " + e.getMessage());
        }
        // COBOL: GOBACK is implicit in Java when main method finishes.
    }
}