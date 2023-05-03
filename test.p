(* Partie déclaration *)
VAR
    a : INTEGER;
    b : BOOLEAN.

(* Partie instructions *)
a := 1;
b := TRUE;

IF b THEN a := 9999 ELSE a := 0
;
DISPLAY a
.
