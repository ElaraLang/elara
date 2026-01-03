package Elara;

public class String {
    private final Character head;
    private final String tail;


    public String(char head, String tail) {
        this.head = head;
        this.tail = tail;
    }

    public String(byte[] bytes) {
        if (bytes.length == 0) {
            this.head = null;
            this.tail = null;
        } else {
            this.head = (char) bytes[0];
            byte[] tailBytes = new byte[bytes.length - 1];
            System.arraycopy(bytes, 1, tailBytes, 0, bytes.length - 1);
            this.tail = new String(tailBytes);
        }
    }

    public String(java.lang.String s) {
        if (s.isEmpty()) {
            this.head = null;
            this.tail = null;
        } else {
            this.head = s.charAt(0);
            this.tail = new String(s.substring(1));
        }
    }

    public boolean isEmpty() {
        return head == null;
    }

    public Character head() {
        if (isEmpty()) {
            throw new RuntimeException("head called on empty string");
        }
        return head;
    }

    public String tail() {
        if (isEmpty()) {
            throw new RuntimeException("tail called on empty string");
        }
        return tail;
    }

    public String cons(Character c) {
        return new String(c, this);
    }

    public String concat(String other) {
        if (isEmpty()) {
            return other;
        } else {
            return new String(head, tail.concat(other));
        }
    }

    @Override
    public java.lang.String toString() {
        if (isEmpty()) {
            return "";
        } else {
            return head + tail.toString();
        }
    }

}
