PROGRAM addition;

(* Partie déclarations globales *)
VAR
    CONSTANTE, number : UINTEGER;
    numberf : DOUBLE;
    isTrue : BOOLEAN;
    letter : CHAR.

(* Partie fonctions *)
FUNCTION add(a, b : UINTEGER) : UINTEGER
BEGIN
    add := a + b
END;

FUNCTION factorial(n : UINTEGER) : UINTEGER
BEGIN
    IF n == 0 THEN
        factorial := 1
    ELSE
        factorial := n * factorial(n - 1)
END.

(* Partie procédures *)
PROCEDURE print(value : UINTEGER)
BEGIN
    DISPLAYLN value
END.

(* Partie instructions *)
BEGIN
    (* Expressions *)
    number := 1 + (1 - 1) + (1 * 1) - (1 / 1);
    numberf := 1.5 + (9.5 - 5.5) + (3.14 * 2.0) - (1.0 / 4.0);
    isTrue := FALSE;
    letter := '!';

    CONSTANTE := 69420;

    (* IF Statement *)
    isTrue := TRUE;
    IF isTrue THEN DISPLAYLN CONSTANTE;

    (* WHILE Statement *)
    number := 10;
    WHILE number > 0 DO
    BEGIN
        DISPLAYLN CONSTANTE * number;
        number := number - 1
    END;

    (* FOR Statement *)
    FOR number := 1 TO 10 DO
    BEGIN
        IF (CONSTANTE % number == 0) THEN DISPLAYLN 1 ELSE DISPLAYLN 0
    END;

    (* BLOCK Statement *)
    number := 50;
    numberf := 3.141592653589793;
    isTrue := number > 10000;
    letter := '?';
    BEGIN
        DISPLAYLN CONSTANTE;
        DISPLAYLN number;
        DISPLAYLN numberf;
        DISPLAYLN isTrue;
        DISPLAYLN letter
    END;

    (* DISPLAYLN statement *)
    DISPLAY 1;
    DISPLAY 2;
    DISPLAYLN 3;

    (* DISPLAYLN Statement *)
    DISPLAYLN (5 * 8) - (1 + 4);

    (* Appels de sous programmes *)
    DISPLAYLN add(1, 9999);
    DISPLAYLN factorial(10);
    print(factorial(5))

END.

(*
69420
694200
624780
555360
485940
416520
347100
277680
208260
138840
69420
1
1
1
1
1
1
0
0
0
1
69420
50
3.141593
FALSE
?
123
35
10000
3628800
120
*)
