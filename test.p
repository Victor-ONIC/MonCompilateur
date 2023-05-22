PROGRAM addition;

VAR 
    G : BOOLEAN.

FUNCTION max(a, b : UINTEGER) : BOOLEAN
VAR res : UINTEGER
BEGIN
    IF a > b THEN
        res := TRUE
    ELSE IF a == b THEN
        res := FALSE
    ELSE
        res := FALSE;
    max := res
END.

BEGIN
    G := max(10, 10);
    DISPLAY G
END.
