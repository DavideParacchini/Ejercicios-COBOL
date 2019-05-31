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

       01 WKS-DATOS-TRAB OCCURS 100 DEPENDING ON WKS-FIN.
           02 WKS-NOMBRE-TRAB  PIC X(50).
           02 WKS-APELL-PAT    PIC X(50).
           02 WKS-APELL-MAT    PIC X(50).
           02 WKS-SALARIO      PIC S9(5)V99.
           02 WKS-ID-TRAB      PIC 9(2).

       77 WKS-FIN              PIC 9(3).
       77 WKS-SAL-FORMATO      PIC Z(4).99.
       77 WKS-CONTADOR         PIC 9(2).
       77 WKS-TERMINA          PIC 9(1).

       01 WKS-OPCION           PIC A(1) VALUE SPACE.
           88 WKS-OPC-ALTA  VALUE 'A'.
           88 WKS-OPC-CONS  VALUE 'C'.
           88 WKS-OPC-S     VALUE 'S'.
           88 WKS-OPC-N     VALUE 'N'.



       PROCEDURE DIVISION.

       00000-MAIN.

           DISPLAY "NOMINA TRABAJADORES".
           MOVE ZERO TO WKS-TERMINA.
           MOVE 1 TO WKS-FIN.
           PERFORM 00003-MENU UNTIL WKS-TERMINA = 1.
           STOP RUN.

       00003-MENU.

           DISPLAY "A. ALTAS, C. CONSULTA, S. SALIR"
           ACCEPT WKS-OPCION
           IF WKS-OPC-ALTA
              PERFORM 00001-CAPTURA-TRAB UNTIL WKS-FIN > 5
              ELSE
              IF WKS-OPC-CONS
              MOVE 1 TO WKS-FIN
              PERFORM 00002-CONSULTA-TRAB UNTIL WKS-FIN >5
                     ELSE
                     IF WKS-OPC-S
                        MOVE 1 TO WKS-TERMINA
                        ELSE
                            DISPLAY "OPCION INVALIDA, VERIFIQUE."
                     END-IF
              END-IF
           END-IF.


       00001-CAPTURA-TRAB.

           DISPLAY "NOMBRE EMPLEADO: ".
           ACCEPT WKS-NOMBRE-TRAB(WKS-FIN).

           DISPLAY "APELLIDO PATERNO".
           ACCEPT WKS-APELL-PAT(WKS-FIN).

           DISPLAY "APELLIDO MATERNO".
           ACCEPT WKS-APELL-MAT(WKS-FIN).

           DISPLAY "SALARIO:".
           ACCEPT WKS-SALARIO(WKS-FIN).

           ADD 10 TO WKS-CONTADOR
           MOVE WKS-CONTADOR TO WKS-ID-TRAB(WKS-FIN).
           PERFORM 00004-OTRA-CAPTURA.

       00004-OTRA-CAPTURA.

           DISPLAY "DESEA AGREGAR TRABAJADOR? S-SI, N-NO"
           ACCEPT WKS-OPCION.

           IF WKS-OPC-S
           ADD 1 TO WKS-FIN
           IF WKS-FIN > 101
           PERFORM 00001-CAPTURA-TRAB
           ELSE
               DISPLAY
               "NO SE PUEDE AGREGAR TRABAJADORES, CONTACTE A SISTEMAS."
               ELSE
               IF WKS-OPC-N
                  PERFORM 00000-MAIN
                  ELSE
                      DISPLAY "OPCION INVALIDA, VERIFIQUE."
                      PERFORM 00004-OTRA-CAPTURA
               END-IF
           END-IF.

       00002-CONSULTA-TRAB.

           DISPLAY "ID TRABAJADOR: " WKS-ID-TRAB(WKS-FIN).
           DISPLAY "NOMBRE: " WKS-NOMBRE-TRAB(WKS-FIN).
           DISPLAY "APELLIDOS: " WKS-APELL-PAT(WKS-FIN)
                                 WKS-APELL-MAT(WKS-FIN).
           MOVE WKS-SALARIO(WKS-FIN) TO WKS-SAL-FORMATO.
           DISPLAY "SALARIO: " WKS-SALARIO(WKS-FIN).
           ADD 1 TO WKS-FIN.






       END PROGRAM NOMINA.
