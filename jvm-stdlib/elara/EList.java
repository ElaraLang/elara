package elara;

/**
 * Elara Cons List impl
 */
public class EList<T> {
    private T head;
    private EList<T> tail;

    private EList(T head, EList<T> tail) {
        this.head = head;
        this.tail = tail;
    }


    public static <T> EList<T> empty() {
        return new EList<T>(null, null);
    }

    public static <T> EList<T> cons(T head, EList<T> tail) {
        return new EList<T>(head, tail);
    }

}
