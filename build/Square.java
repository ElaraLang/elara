
class Square {

    public static int fact(int n ) {
        if (n == 0) {
            if (n % 2 == 0) {
                return 1;
            }
            else {
                return 2;
            }
        } else {
        return n * fact (n - 1);
        }
    }

}
