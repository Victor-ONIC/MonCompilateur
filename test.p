PROGRAM addition;

VAR 
    G : UINTEGER.

FUNCTION factorial(n : UINTEGER) : UINTEGER
BEGIN
    IF n == 0 THEN 
        factorial := 1
    ELSE
        factorial := n * factorial(n - 1)
END.

BEGIN
    G := factorial(10);
    DISPLAY G
END.
