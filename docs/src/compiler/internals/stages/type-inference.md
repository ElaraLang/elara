# Type Inference

## Overview

The current type inference algorithm is loosely based on `OutsideIn(X)` but incredibly simplified (for now...)

It's a constraint-based Hindley-Milner style inference with unification, where we generate constraints from expressions and then solve them via unification and substitution. Kind inference is supported as a separate pass before type inference.

I will refrain writing much about this because it's very much a work in progress about expanding it to be more based on OutsideIn