import Elara.Prim;
import elara.IO;
import elara.Prelude;

// 
// Decompiled by Procyon v0.5.36
// 

public class Test
{
    public static IO main;

    static {
        main = Prim.println(Prim.toString(fact(5)));
    }

    public static void main(String[] var0) {
        main.run();
    }

    public static Integer fact(Integer var0) {
        return Prim.eq(var0, Integer.valueOf(0)) ? 1 : Prim.times(var0, fact(Prim.minus(var0, 1)));
    }
}