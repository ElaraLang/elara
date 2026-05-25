package Elara;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class IO<T> {
    private final Func0<T> run;

    public IO(Func0<T> run) {
        this.run = run;
    }

    public void run() {
        this.run.run();
    }

    public static <T> IO<T> pure(T t) {
        return new IO<>(() -> t);
    }

    public <B> IO<B> bind(Func<T, IO<B>> f) {
        return new IO<>(() -> f.run(this.run.run()).run.run());
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
