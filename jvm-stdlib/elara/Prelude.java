package elara;
public class Prelude {
    public static final Func<Int, Func<Int, Int>> add = (a) -> (b) -> a.add(b);
}