      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOMINA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-DATOS-TRABA OCCURS 5 DEPENDING ON WS-FIN.
           02 WS-NOM-TRAB PIC X(50).
           02 WS-APELL-PAT PIC X(50).
           02 WS-APELL-MAT PIC X(50).
           02 WS-SALARIO PIC S9(5)V99.
           02 WS-ID-TRAB PIC 9(2).

       77 WS-FIN PIC 9(3).
       77 WS-SAL-FORMATO PIC Z(5).99.
       77 WS-CONTADOR PIC 9(2).
       77 WS-TERMINA PIC 9.
       77 WS-RECORRE pic 9(3).

       01 WS-OPCION PIC A VALUE SPACE.
           88 WS-OPC-ALTA VALUE 'A'.
           88 WS-OPC-CONS VALUE 'C'.
           88 WS-OPC-S VALUE 'S'.
           88 WS-OPC-NO VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN.

           DISPLAY "NOMINA TRABAJADORES".
           DISPLAY " ".
           MOVE 0 TO WS-TERMINA.
           PERFORM 00003-MENU UNTIL WS-TERMINA = 1.

           STOP RUN.

       00003-MENU.

           DISPLAY " "
           DISPLAY "A. ALTAS, C. CONSULTA, S. SALIR "
           DISPLAY " "

           ACCEPT WS-OPCION
           IF WS-OPC-ALTA
              PERFORM 00001-CAPTURA-TRAB
           ELSE
               IF WS-OPC-CONS
               MOVE 1 TO WS-RECORRE
               PERFORM 00002-CONSULTA-TRAB UNTIL WS-RECORRE > WS-FIN
               ELSE
                   IF WS-OPC-S
                   MOVE 1 TO WS-TERMINA
                   ELSE
           DISPLAY " "
           DISPLAY "OPCION INVALIDA VERIFIQUE..."
           DISPLAY " "
                   END-IF
                 END-IF
           END-IF.



       00001-CAPTURA-TRAB.

           DISPLAY " ".
           DISPLAY "NOMBRE EMPLEADO: ".
           ACCEPT WS-NOM-TRAB(WS-FIN).
           DISPLAY " ".
           DISPLAY "APPELLIDO PATERNO: ".
           ACCEPT WS-APELL-PAT(WS-FIN).
           DISPLAY " ".
           DISPLAY "APELLIDO MATERNO: ".
           ACCEPT WS-APELL-MAT(WS-FIN).
           DISPLAY " ".
           DISPLAY "SALARIO TRABAJADOR: ".
           ACCEPT WS-SALARIO(WS-FIN).

           ADD 10 TO WS-CONTADOR.
           MOVE WS-CONTADOR TO WS-ID-TRAB(WS-FIN).

           PERFORM 00004-OTRA-CAPTURA.

       00004-OTRA-CAPTURA.

           DISPLAY "DESEA AGREGAR OTRO TRABAJADOR? S-SI, N-NO"
           ACCEPT WS-OPCION

           IF WS-OPC-S
              ADD 1 TO WS-FIN
              PERFORM 00001-CAPTURA-TRAB
           ELSE
                  IF WS-OPC-NO
                      NEXT SENTENCE
                  ELSE
                  DISPLAY "OPCION INVALIDA VERIFIQUE..."
                  PERFORM 00004-OTRA-CAPTURA

                  END-IF
           END-IF.

       00002-CONSULTA-TRAB.

           DISPLAY " ".
           DISPLAY "ID TRABAJADOR: " WS-ID-TRAB(WS-RECORRE).
           DISPLAY " ".
           DISPLAY "NOMBRE: " WS-NOM-TRAB(WS-RECORRE).
           DISPLAY " ".
           DISPLAY "APELLIDOS : " WS-APELL-PAT(WS-RECORRE)
                                  WS-APELL-MAT(WS-RECORRE).
           MOVE WS-SALARIO(WS-RECORRE) TO WS-SAL-FORMATO.
           DISPLAY " ".
           DISPLAY "SALARIO TRABAJADOR: " WS-SAL-FORMATO.
           ADD 1 TO WS-RECORRE.


       END PROGRAM NOMINA.
