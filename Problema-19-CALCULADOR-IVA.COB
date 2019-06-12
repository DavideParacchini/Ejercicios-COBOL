      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADOR-IVA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77 NUM-1 PIC S9(5)V99.
       77 NUM-2 PIC S9(5)V99.
       77 NUM-3 PIC S9(5)V99.
       77 NUM-4 PIC S9(5)V99.
       77 NUM-5 PIC S9(5)V99.
       77 TOTAL PIC S9(10)99.
       77 IVA PIC ZZZZZ9.99.
       77 FORMAT-TOTAL PIC ZZZZZ9.99.


       PROCEDURE DIVISION.
       MAIN.

           DISPLAY "CALCULADOR IVA".

           PERFORM 100000-CARGA.

           PERFORM 200000-CALCULO.

           PERFORM 300000-RESULTADO.

           STOP RUN.


       100000-CARGA.

           DISPLAY "INGRESE PRIMER CIFRA: ".
           ACCEPT NUM-1.
           DISPLAY "INGRESE SEGUNDA CIFRA: ".
           ACCEPT NUM-2.
           DISPLAY "INGRESE TERCER CIFRA: ".
           ACCEPT NUM-3.
           DISPLAY "INGRESE CUARTA CIFRA: ".
           ACCEPT NUM-4.
           DISPLAY "INGRESE QUINTA CIFRA: ".
           ACCEPT NUM-5.




       200000-CALCULO.

           COMPUTE TOTAL = (NUM-1 + NUM-2 + NUM-3 + NUM-4 + NUM-5).
           COMPUTE IVA = TOTAL * 0.21.
           MOVE TOTAL TO FORMAT-TOTAL.



       300000-RESULTADO.

           DISPLAY "TOTAL ES: " FORMAT-TOTAL.
           DISPLAY "IVA ES: " IVA.







       END PROGRAM CALCULADOR-IVA.
