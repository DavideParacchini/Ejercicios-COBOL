      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO2-FIBONACCI.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 CONSTANTES.
           02 NUM-1 PIC 9(4)   VALUE 0.
           02 NUM-2 PIC 9(4)   VALUE 1.
           02 NUM-AUX PIC 9(4) VALUE 1.

       01 CONTADOR.
           02 FIBO PIC 9(2) VALUE 0.

       01 RESULTADO.
           02 FINAL-FIBO PIC Z(4)9.


       PROCEDURE DIVISION.

           PERFORM VARYING FIBO FROM 1 BY 1 UNTIL FIBO = 11
           ADD NUM-1 TO NUM-2 GIVING NUM-AUX
           MOVE NUM-2 TO NUM-1
           MOVE NUM-AUX TO NUM-2
           MOVE NUM-AUX TO FINAL-FIBO

           DISPLAY 'RESULTADO ' FIBO ': ' FINAL-FIBO
           END-PERFORM.



            STOP RUN.
       END PROGRAM EJERCICIO2-FIBONACCI.
