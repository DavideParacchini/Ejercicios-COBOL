      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-NOMBRE PIC A(30).

       PROCEDURE DIVISION.
           DISPLAY "BIENVENIDO AL PROGRAMA".
           DISPLAY "CAPTURA TU NOMBRE".
           ACCEPT WS-NOMBRE.
           DISPLAY "TU NOMBRE ES: " WS-NOMBRE.



            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
