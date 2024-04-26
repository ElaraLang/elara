package Elara;

public class Int {
    private final int val;

    public Int(int val) {
        this.val = val;
    }

    public Int add(Int other) {
        return new Int(val + other.val);
    }
}
