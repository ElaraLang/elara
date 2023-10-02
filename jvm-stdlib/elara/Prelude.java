package elara;

public class Prelude {
    public static final Func<Integer, Func<Integer, Integer>> add = (a) -> (b) -> a + b;
}
