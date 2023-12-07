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

    @Override
    public String toString() {
        String s = "[";
        EList<T> list = this;
        while (!list.isEmpty()) {
            s += list.head.toString();
            list = list.tail;
            if (!list.isEmpty()) {
                s += ", ";
            }
        }
        s += "]";
        return s;
    }

    public static EList<Character> stringToList(String s) {
        EList<Character> list = EList.Empty();
        for (int i = s.length() - 1; i >= 0; i--) {
            list = EList.Cons((s.charAt(i)), list);
        }
        return list;
    }

    public static String listToString(EList<Character> list) {
        String s = "";
        while (!list.isEmpty()) {
            s += list.head;
            list = list.tail;
        }
        return s;
    }
}
