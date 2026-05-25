package Elara;

public class Error {
    public static <T> T undefined() {
        throw new ElaraException( new Elara.String("undefined"));
    }

    public static <T> T patternMatchFail() {
        throw new ElaraException( new Elara.String("pattern match failure"));
    }

    public static <T> T throwError(String message) {
        throw new ElaraException(message);
    }

    public static class ElaraException extends RuntimeException {
        public ElaraException(Elara.String message) {
            super(message.toString());
        }
    }
}