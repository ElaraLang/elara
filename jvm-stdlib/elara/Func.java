package elara;

public interface Func<In, Out> {
    Out run(In in);
}