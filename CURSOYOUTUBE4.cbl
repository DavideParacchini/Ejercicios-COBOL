      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1      PIC S9(5)V99.
       77 WS-NUM2      PIC S9(5)V99.
       77 WS-RESULTADO PIC S9(10)V99.
       77 WS-OPCION    PIC A(1).
       77 WS-OPCION2   PIC A(1).
       77 WS-ELECCION  PIC 9(1).
       77 WS-RES-FORMAT PIC -Z(10).9(2).
       77 WS-RESIDUO PIC 9(4).


       PROCEDURE DIVISION.

       MAIN.

           PERFORM 00000-CALCULADORA UNTIL WS-ELECCION=2.

           STOP RUN.

       00000-CALCULADORA.

           DISPLAY "1. USAR CALCULADORA. 2. SALIR. "
           DISPLAY " ".
           ACCEPT WS-ELECCION.


           IF WS-ELECCION=1
           PERFORM 00001-PEDIR-DATOS
           PERFORM 00002-EJECUTAR-OPERACION

           ELSE IF WS-ELECCION=2
               NEXT SENTENCE
           ELSE
               DISPLAY "OPCION INVALIDA, VERIFIQUE.".



       00001-PEDIR-DATOS.

           DISPLAY "CALCULADORA".
           DISPLAY " ".
           DISPLAY "DAME EL NUMERO 1: ".
           DISPLAY " ".
           ACCEPT WS-NUM1.
           DISPLAY " ".
           DISPLAY "DAME EL NUMERO 2: ".
           DISPLAY " ".
           ACCEPT WS-NUM2.
           DISPLAY " ".
           DISPLAY "INDICA LA OPERACION A REALIZAR: S=SUMA, R=RESTA, "
      -    "M=MULTIPLICACION, D=DIVISION, E=RESIDUO, F= POTENCIA, "
      -    "G=RAIZ ".
           INITIALIZE WS-OPCION.
           ACCEPT WS-OPCION.
           MOVE FUNCTION UPPER-CASE (WS-OPCION) TO WS-OPCION2.



       00002-EJECUTAR-OPERACION.

           IF WS-OPCION2="S"
              ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULTADO
           ELSE
           IF WS-OPCION2="R"
              SUBTRACT WS-NUM1 FROM WS-NUM2 GIVING WS-RESULTADO
           ELSE
           IF WS-OPCION2="M"
              MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULTADO
           ELSE
           IF WS-OPCION2="D" OR "E"
              DIVIDE WS-NUM1 INTO WS-NUM2 GIVING WS-RESULTADO
              REMAINDER WS-RESIDUO
           ELSE
           IF WS-OPCION2="F"
              COMPUTE WS-RESULTADO= WS-NUM1 ** WS-NUM2
           ELSE
           IF WS-OPCION2="G"
               COMPUTE WS-RESULTADO= WS-NUM1 ** (1/WS-NUM2)
           ELSE
              DISPLAY "OPERACION ERRONEA, VERIFIQUE.".

           IF WS-OPCION2 = "S" OR "R" OR "M" OR "D" OR "E" OR "F" OR "G"
              MOVE WS-RESULTADO TO WS-RES-FORMAT
              DISPLAY WS-RES-FORMAT

           IF WS-OPCION2= "E"
              DISPLAY WS-RESIDUO.

           STOP "PRESIONE CUALQUIER TECLA PARA CONTINUAR...".


       END PROGRAM CALCULADORA.
