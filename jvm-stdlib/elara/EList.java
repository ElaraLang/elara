package elara;

/**
 * Elara Cons List impl
 */
public class EList<T> {
    public final T head;
    public final EList<T> tail;

    private EList(T head, EList<T> tail) {
        this.head = head;
        this.tail = tail;
    }

    public static <T> EList<T> Empty() {
        return new EList<T>(null, null);
    }

    public static <T> EList<T> Cons(T head, EList<T> tail) {
        return new EList<T>(head, tail);
    }

    public boolean isEmpty() {
        return head == null && tail == null;
    }
}
