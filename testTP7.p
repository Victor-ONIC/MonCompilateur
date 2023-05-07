(* Partie déclaration *)
VAR
    CONSTANTE, number : INTEGER;
    numberf : DOUBLE;
    isTrue : BOOLEAN;
    letter : CHAR.

(* Partie instructions *)
BEGIN
    number := 1 + (1 - 1) + (1 * 1) - (1 / 1);
    numberf := 1.5 + (9.5 - 5.5) + (3.14 * 2.0) - (1.0 / 4.0);
    isTrue := FALSE;
    letter := '!';

    CONSTANTE := 69420;

    (* IF Statement *)
    isTrue := TRUE;
    IF isTrue THEN DISPLAY CONSTANTE;

    (* WHILE Statement *)
    number := 10;
    WHILE number > 0 DO
    BEGIN
        DISPLAY CONSTANTE * number;
        number := number - 1
    END;

    (* FOR Statement *)
    FOR number := 1 TO 10 DO
    BEGIN
        IF (CONSTANTE % number == 0) THEN DISPLAY 1 ELSE DISPLAY 0
    END;

    (* BLOCK Statement *)
    number := 50;
    numberf := 3.141592653589793;
    isTrue := number > 10000;
    letter := '?';
    BEGIN
        DISPLAY CONSTANTE;
        DISPLAY number;
        DISPLAY numberf;
        DISPLAY isTrue;
        DISPLAY letter
    END;

    (* DISPLAY Statement *)
    DISPLAY (5 * 8) - (1 + 4)

END.
