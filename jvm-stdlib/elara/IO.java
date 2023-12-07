package elara;

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

    public <B> IO<B> bind(Func<T, IO<B>> f) {
        return new IO<>(() -> f.run(this.run.get()).run.get());
    }

    public static IO<Void> println(String s) {
        return new IO<>(() -> {
            System.out.println(s);
            return null;
        });
    }

    public static IO<EList<String>> readFile(String path) {
        return new IO<>(() -> {
            try {
                return EList.fromList(Files.readAllLines(Paths.get(path)));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        });
    }


}
