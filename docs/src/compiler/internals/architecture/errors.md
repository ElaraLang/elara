# Errors


The compiler has two main kinds of error: _reportable errors_ which are normal and expected, and reported to the user as such, and _internal errors_ which are unexpected and usually indicate a bug in the compiler.

## Reportable Errors

Reportable errors are instances of the `ReportableError` type class, which provides a `report` function, along with a `errorCode` function which gives a unique code for the error type (See the [`Elara.Error.Codes`]({{repo_base/src/Elara/Error/Codes.hs}}) module for a list of error codes).

Reportable errors are slightly unusual in that they usually require two [Effects](../architecture/effects.md). We use an `Error TheErrorType` effect to handle the short circuiting, and then a `DiagnosticWriter (Doc AnsiStyle)` effect to write the error message to the diagnostics.

The main utility function for this is [`runErrorOrReport`]({{hackage_base}}//Elara-Error.html#v:runErrorOrReport), which handles running the error effect, and adding the error to the diagnostics if it is thrown. It then re-throws the error wrapped in the existential type `SomeReportableError`, to make sure the error still causes the query to short circuit.


## Internal Errors

These aren't used much, mainly by some queries when they encounter an unexpected state. They're defined in [`Elara.Error.Internal`]({{repo_base/src/Elara/Error/Internal.hs}}), and are just a normal `Exception` type thrown with `throwIO`. We don't really expect to have to catch or handle these in any special way, we just "let it crash"