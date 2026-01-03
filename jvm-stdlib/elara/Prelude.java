package Elara;

public class Prelude {
    public static final Func<Integer, Func<Integer, Integer>> add = (a) -> (b) -> a + b;
    public static final Func<Integer, Func<Integer, Integer>> minus = (a) -> (b) -> a - b;
    public static final Func<Integer, Func<Integer, Integer>> times = (a) -> (b) -> a * b;


    public static final Func<Integer, Integer> negate = (a) -> -a;
}
