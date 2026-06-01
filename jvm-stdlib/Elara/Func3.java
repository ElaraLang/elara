package Elara;

public interface Func3<In1, In2, In3, Out> extends Func<In1, Func2<In2, In3, Out>> {
    Out run(In1 in1, In2 in2, In3 in3);
    
    // Default partial application: run(a) returns a Func2 that takes remaining args
    @Override
    default Func2<In2, In3, Out> run(In1 in1) {
        return (in2, in3) -> this.run(in1, in2, in3);
    }
}
