PROGRAM addition;

VAR 
    G : BOOLEAN.

PROCEDURE display_loop(a : UINTEGER)
BEGIN
    REPEAT
    BEGIN
        DISPLAY a;
        a := a - 1
    END
    UNTIL a > 0
END.

BEGIN
    display_loop(10)
END.
