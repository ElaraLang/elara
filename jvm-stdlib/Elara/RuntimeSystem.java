package Elara;

import java.lang.String;
public class RuntimeSystem {
    private static Elara.String[] args = new Elara.String[0];

    public static Elara.String[] getArgs() {
        return args;
    }

    public static void setArgs(String[] newArgs) {
        if (newArgs != null) {
            args = new Elara.String[newArgs.length];
            for (int i = 0; i < newArgs.length; i++) {
                args[i] = new Elara.String(newArgs[i]);
            }
        } else {
            throw new IllegalArgumentException("Arguments array cannot be null");
        }
    }

    public static void init(String[] newArgs) {
        setArgs(newArgs);
    }
}
