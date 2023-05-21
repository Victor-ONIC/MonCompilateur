PROGRAM addition;

VAR 
    G : UINTEGER.

FUNCTION add(a, b : UINTEGER) : UINTEGER
BEGIN
    add := a + b
END.

BEGIN
    G := add(9999, 1);
    DISPLAY G
END.
