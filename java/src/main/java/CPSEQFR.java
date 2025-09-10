import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class CPSEQFR {

    // In COBOL, INFILE1 and OUTFILE1 would be defined in the SELECT clause.
    // Here, we define them as constants for the file names.
    private static final String INFILE_NAME = "INFILE1.txt";
    private static final String OUTFILE_NAME = "OUTFILE1.txt";

    private static int recordCount = 0; // Equivalent to RECORD-COUNT PIC S9(5) COMP-3

    public static void main(String[] args) {

        // The COBOL OPEN INPUT INFILE and OPEN OUTPUT OUTFILE are handled by try-with-resources
        // This ensures that the files are closed automatically.
        try (BufferedReader inFile = new BufferedReader(new FileReader(INFILE_NAME));
             BufferedWriter outFile = new BufferedWriter(new FileWriter(OUTFILE_NAME))) {

            String inputRecord; // Corresponds to INPUT-RECORD

            // PERFORMrUNTIL END-OF-INPUT is simulated by the while loop and readLine() returning null
            // READ INFILE is simulated by inFile.readLine()
            while ((inputRecord = inFile.readLine()) != null) {

                // In COBOL, the record length is fixed. We should enforce this in Java too.
                // INPUT-RECORD is 40 characters long.
                if (inputRecord.length() != 40) {
                    System.err.println("Warning: Skipping malformed record (expected 40 characters): " + inputRecord);
                    continue; // Skip to the next record like COBOL might handle an invalid record
                }

                // COBOL: 05 INPUT-FIRST-10 PIC X(10). (characters 0-9)
                // COBOL: 05 INPUT-LAST-30  PIC X(30). (characters 10-39)
                String inputFirst10 = inputRecord.substring(0, 10);
                String inputLast30 = inputRecord.substring(10, 40);

                // COBOL: 05 OUTPUT-FIRST-30 PIC X(30).
                // COBOL: 05 OUTPUT-LAST-10  PIC X(10).
                // MOVE INPUT-FIRST-10 TO OUTPUT-LAST-10
                // MOVE INPUT-LAST-30 TO OUTPUT-FIRST-30
                String outputFirst30 = inputLast30;
                String outputLast10 = inputFirst10;

                String outputRecord = outputFirst30 + outputLast10; // Construct the output record

                // WRITE OUTPUT-RECORD
                outFile.write(outputRecord);
                outFile.newLine(); // Add a newline character as files are line-oriented

                recordCount++; // ADD 1 TO RECORD-COUNT
            }

            // DISPLAY 'NUMBER OF RECORDS PROCESSED: ' RECORD-COUNT
            System.out.println("NUMBER OF RECORDS PROCESSED: " + recordCount);

        } catch (IOException e) {
            // Error handling similar to COBOL's file status checks
            System.err.println("Error during file operation: " + e.getMessage());
            // In a COBOL program, this might lead to a GOBACK or a more elaborate error routine.
        }
        // The GOBACK equivalent is implicitly handled when the main method finishes execution.
    }
}