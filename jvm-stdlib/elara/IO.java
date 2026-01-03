package Elara;

import java.util.function.Supplier;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class IO<T> {
    private final Supplier<T> run;

    public IO(Supplier<T> run) {
        this.run = run;
    }

    public void run() {
        this.run.get();
    }

    public static <T> IO<T> pure(T t) {
        return new IO<>(() -> t);
    }

    public <B> IO<B> bind(Func<T, IO<B>> f) {
        return new IO<>(() -> f.run(this.run.get()).run.get());
    }

    public static IO<Unit> println(Object s) {
        return new IO<>(() -> {
            System.out.println(s);
            return Unit.unit;
        });
    }

    public static IO<String> readFile(Elara.String path) {
        return new IO<>(() -> {
            try {
                return new String(Files.readAllBytes(Paths.get(path.toString())));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }


}
