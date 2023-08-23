package elara;

public class Error {
    public static Throwable undefined() {
        return new RuntimeException("undefined");
    }
}