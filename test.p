PROGRAM addition;

FUNCTION add(a, b : UINTEGER) : UINTEGER
BEGIN
    add := a + b
END;

FUNCTION sub(a, b : UINTEGER) : UINTEGER
BEGIN
    sub := a - b
END.

PROCEDURE print(param : UINTEGER)
BEGIN
    DISPLAY param
END.

BEGIN
    print(sub(9, 5) + add(9, 5))
END.
