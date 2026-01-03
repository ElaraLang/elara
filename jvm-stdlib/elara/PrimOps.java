package Elara;

public class PrimOps {

    public static <T> T debugWithMsg(String msg, T value) {
        System.out.println("DEBUG: " + msg + " => " + value);
        return value;
    }

    public static String toString(Object obj) {
        if (obj == null) {
            return new String("null");
        } else {
            return new String(obj.toString());
        }
    }

    public static Integer compare(Object a, Object b) {
        if (a == null || b == null) {
            throw new RuntimeException("Cannot compare null objects");
        }

        if (!a.getClass().equals(b.getClass())) {
            throw new RuntimeException("Cannot compare objects of different types: "
                    + a.getClass().getName() + " vs " + b.getClass().getName());
        }
        if (a instanceof Comparable && b instanceof Comparable) {
            @SuppressWarnings("unchecked")
            Comparable<Object> compA = (Comparable<Object>) a;
            int result = compA.compareTo(b);
            if (result < 0)
                return Integer.valueOf(-1);
            if (result > 0)
                return Integer.valueOf(1);
            return Integer.valueOf(0);
        } else {
            throw new RuntimeException("Cannot compare non-comparable objects");
        }
    }

}
