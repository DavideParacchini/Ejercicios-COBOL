      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZONEANDO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 ZONEADO.
           02 NUM PIC S9(1) VALUES -3.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 00001-ZONEADO.

           STOP RUN.



       00001-ZONEADO.
           IF NUM = -3
              DISPLAY ZONEADO
              ELSE
                  NEXT SENTENCE
           END-IF.



           END PROGRAM ZONEANDO.
