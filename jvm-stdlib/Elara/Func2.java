package Elara;

public interface Func2<In1, In2, Out> extends Func<In1, Func<In2, Out>> {
    Out run(In1 in1, In2 in2);
    
    // Default partial application: run(a) returns a Func that takes the second arg
    @Override
    default Func<In2, Out> run(In1 in1) {
        return in2 -> this.run(in1, in2);
    }
}
