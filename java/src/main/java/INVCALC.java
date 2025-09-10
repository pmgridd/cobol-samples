import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

public class INVCALC {

    // COBOL: 05 SALES-TAX-RATE PIC S9(5)V99 COMP-3 VALUE 0.065.
    private static final double SALES_TAX_RATE = 0.065;

    // COBOL: Working-Storage Section variables
    private static int workingIndex; // PIC S9(04) COMP
    private static double cumulativePriceBeforeTax; // PIC S9(07)V99 COMP-3
    private static double cumulativePriceWithTax;   // PIC S9(07)V99 COMP-3
    private static double cumulativeSalesTax;       // PIC S9(05)V9(03) COMP-3
    private static double lineWorkingTotal;         // PIC S9(07)V99 COMP-3
    private static double lineWorkingTax;           // PIC S9(05)V9(03) COMP-3

    // COBOL: 01 INVOICE. (simulated with direct fields and a nested class)
    private static String invDate;        // PIC X(08)
    private static String invNumber;      // PIC X(08)
    private static double invTotalAmount; // PIC S9(07)V99 COMP-3
    private static double invTotalBeforeTax; // PIC S9(07)V99 COMP-3
    private static double invTotalSalesTax; // PIC S9(05)V9(03) COMP-3
    private static boolean isReturn;      // 88 IS-RETURN VALUE 'R'.
    private static int invLineItemCount; // PIC S9(05) COMP-3

    // COBOL: 05 INV-LINE OCCURS 1 TO 100 DEPENDING ON INV-LINE-ITEM-COUNT.
    // Nested class to represent each invoice line item
    static class InvoiceLine {
        String sku;         // PIC X(10)
        double unitPrice;   // PIC S9(05)V99 COMP-3
        int quantity;       // PIC S9(05) COMP-3
        boolean isTaxable;  // 88 TAXABLE-ITEM VALUE 'T'.

        public InvoiceLine(String sku, double unitPrice, int quantity, boolean isTaxable) {
            this.sku = sku;
            this.unitPrice = unitPrice;
            this.quantity = quantity;
            this.isTaxable = isTaxable;
        }
    }

    private static List<InvoiceLine> invLines = new ArrayList<>();

    // COBOL: 01 INVOICE-FORMATTED.
    // Using DecimalFormat for formatting output
    private static DecimalFormat currencyFormatter = new DecimalFormat("$,###,##0.00");
    private static DecimalFormat salesTaxFormatter = new DecimalFormat("#,##0.000");
    private static DecimalFormat quantityFormatter = new DecimalFormat("##0"); // Z,ZZ9
    private static DecimalFormat salesTaxRateFormatter = new DecimalFormat("0.00000");

    private static String invDateFormatted; // PIC X(10)

    public static void main(String[] args) {

        // --- Example 1: Invoice total calculation ---
        System.out.println("----------------------------------------------------");
        System.out.println("Example 1: Invoice total calculation\n");

        // COBOL: INITIALIZE INVOICE REPLACING ALPHANUMERIC DATA BY SPACES NUMERIC DATA BY ZEROS
        // Manual initialization in Java
        invDate = "";
        invNumber = "";
        invTotalAmount = 0.0;
        invTotalBeforeTax = 0.0;
        invTotalSalesTax = 0.0;
        isReturn = false;
        invLineItemCount = 0;
        invLines.clear();

        cumulativePriceBeforeTax = 0.0;
        cumulativePriceWithTax = 0.0;
        cumulativeSalesTax = 0.0;
        workingIndex = 0;

        // Populate sample data for the invoice
        invDate = "20230914"; // MOVE '20230914' TO INV-DATE
        invNumber = "Sample 1"; // MOVE 'Sample 1' TO INV-NUMBER
        invLineItemCount = 3; // MOVE 3 TO INV-LINE-ITEM-COUNT

        // Add line items
        invLines.add(new InvoiceLine("PROD004411", 18.55, 2, true)); // SET TAXABLE-ITEM(1) TO TRUE
        invLines.add(new InvoiceLine("PROD004412", 6.32, 4, false)); // SET NONTAXABLE-ITEM(2) TO TRUE
        invLines.add(new InvoiceLine("PROD004413", 2.28, 8, true)); // SET TAXABLE-ITEM(3) TO TRUE

        // COBOL: PERFORM WITH TEST BEFORE VARYING WORKING-INDEX FROM 1 BY 1 UNTIL WORKING-INDEX > INV-LINE-ITEM-COUNT
        for (workingIndex = 0; workingIndex < invLineItemCount; workingIndex++) { // 0-indexed in Java
            InvoiceLine currentLine = invLines.get(workingIndex);

            // IF INV-LINE-QUANTITY(WORKING-INDEX) IS NUMERIC AND INV-LINE-UNIT-PRICE(WORKING-INDEX) IS NUMERIC
            // In Java, we rely on the types being correct from our object creation.
            lineWorkingTotal = currentLine.quantity * currentLine.unitPrice; // MULTIPLY INV-LINE-QUANTITY BY INV-LINE-UNIT-PRICE GIVING LINE-WORKING-TOTAL

            cumulativePriceBeforeTax += lineWorkingTotal; // ADD LINE-WORKING-TOTAL TO CUMULATIVE-PRICE-BEFORE-TAX

            if (currentLine.isTaxable) { // IF TAXABLE-ITEM(WORKING-INDEX)
                lineWorkingTax = lineWorkingTotal * SALES_TAX_RATE; // MULTIPLY LINE-WORKING-TOTAL BY SALES-TAX-RATE GIVING LINE-WORKING-TAX
                cumulativeSalesTax += lineWorkingTax; // ADD LINE-WORKING-TAX TO CUMULATIVE-SALES-TAX
            }
            // Note: The COBOL code `ADD LINE-WORKING-TOTAL TO LINE-WORKING-TOTAL` is unusual;
            // I will interpret the Java equivalent to mean `cumulativePriceWithTax` also accounts for line tax,
            // which is more logical for a `PRICE-WITH-TAX` field.
            cumulativePriceWithTax += lineWorkingTotal + lineWorkingTax; // This interpretation is more robust.
        }

        // COBOL: MOVE CUMULATIVE-SALES-TAX TO INV-TOTAL-SALES-TAX
        invTotalSalesTax = cumulativeSalesTax;
        // COBOL: MOVE CUMULATIVE-PRICE-BEFORE-TAX TO INV-TOTAL-BEFORE-TAX
        invTotalBeforeTax = cumulativePriceBeforeTax;
        // COBOL: MOVE CUMULATIVE-PRICE-WITH-TAX TO INV-TOTAL-AMOUNT
        invTotalAmount = cumulativePriceWithTax;

        // COBOL: PERFORM PRINT-INVOICE-DETAILS
        printInvoiceDetails();

        // GOBACK is implicit
    }

    private static void printInvoiceDetails() {
        System.out.println("----------------------------------------------------");
        System.out.println("Invoice Number:   " + invNumber);

        // COBOL: MOVE INV-DATE(1:4) TO INV-DATE-FORMATTED(1:4)
        //        MOVE '/' TO INV-DATE-FORMATTED(5:1)
        //        MOVE INV-DATE(5:2) TO INV-DATE-FORMATTED(6:2)
        //        MOVE '/' TO INV-DATE-FORMATTED(8:1)
        //        MOVE INV-DATE(7:2) TO INV-DATE-FORMATTED(9:2)
        // Assuming INV-DATE is YYYYMMDD
        if (invDate != null && invDate.length() == 8) {
            invDateFormatted = invDate.substring(0, 4) + "/" + invDate.substring(4, 6) + "/" + invDate.substring(6, 8);
        } else {
            invDateFormatted = "Invalid Date";
        }
        System.out.println("Invoice Date:     " + invDateFormatted);

        System.out.println("Total Amount:     " + currencyFormatter.format(invTotalAmount));
        System.out.println("Total Before Tax: " + currencyFormatter.format(invTotalBeforeTax));
        System.out.println("Total Sales Tax:  " + salesTaxFormatter.format(invTotalSalesTax));
        System.out.println("Sales Tax Rate:   " + salesTaxRateFormatter.format(SALES_TAX_RATE));

        if (isReturn) { // IF IS-RETURN
            System.out.println("This is a return");
        }

        System.out.println("\nLine Items:");
        System.out.println("----------------------------------------------------");
        System.out.printf("%s %-10s %10s %8s %5s %-10s%n", "Line", "SKU", "Unit Price", "Quantity", "Taxable", "Line Total");
        System.out.println("----------------------------------------------------");

        // COBOL: PERFORM WITH TEST BEFORE VARYING WORKING-INDEX FROM 1 BY 1 UNTIL WORKING-INDEX > INV-LINE-ITEM-COUNT
        for (workingIndex = 0; workingIndex < invLineItemCount; workingIndex++) {
            InvoiceLine currentLine = invLines.get(workingIndex);

            // For display purposes, recalculate line total and tax for current line
            double currentLineTotal = currentLine.quantity * currentLine.unitPrice;
            double currentLineTax = 0.0;
            if (currentLine.isTaxable) {
                currentLineTax = currentLineTotal * SALES_TAX_RATE;
            }
            double currentLineTotalWithTax = currentLineTotal + currentLineTax;

            // COBOL: DISPLAY 'Line ' LINE-NUMBER-FORMATTED etc.
            System.out.printf(Locale.US,
                    "%4s %-10s %10s %8s %5s %10s%n",
                    quantityFormatter.format(workingIndex + 1), // Line number 1-indexed
                    currentLine.sku,
                    currencyFormatter.format(currentLine.unitPrice),
                    quantityFormatter.format(currentLine.quantity),
                    currentLine.isTaxable ? "Yes" : "No",
                    currencyFormatter.format(currentLineTotalWithTax));
        }
        System.out.println("----------------------------------------------------");
    }

    // COBOL: INVALID-INVOICE-DATA procedure (not explicitly shown in problem description, but implied for error handling)
    private static void invalidInvoiceData() {
        System.err.println("Invalid invoice data");
        // In COBOL, this would typically GOBACK or perform some error recovery.
    }
}