import java.util.Scanner;

public class HELLO {

    private static final String WS_PROMPT = "Please enter a name:";
    private static final String WS_GREETING_PREFIX = "Hello, ";
    private static final String WS_EXCLAMATION_POINT = "!";

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // DISPLAY WS-PROMPT
        System.out.println(WS_PROMPT);

        // ACCEPT WS-FRIEND
        String wsFriend = scanner.nextLine();

        // INSPECT FUNCTION REVERSE(WS-FRIEND) TALLYING WS-TRAILING-SPACES FOR LEADING SPACES
        // WS-FRIEND(1:LENGTH OF WS-FRIEND - WS-TRAILING-SPACES)
        // This is equivalent to trimming trailing spaces in Java.
        String trimmedFriendName = wsFriend.trim();

        // STRING ... INTO WS-MESSAGE
        String wsMessage = WS_GREETING_PREFIX + trimmedFriendName + WS_EXCLAMATION_POINT;

        // DISPLAY WS-MESSAGE
        System.out.println(wsMessage);

        scanner.close();
        // GOBACK is implicit when main method finishes
    }
}