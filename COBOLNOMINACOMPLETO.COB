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

       01 WKS-DATOS-TRAB OCCURS 100 TIMES DEPENDING ON WKS-FIN
                                    INDEXED BY WKS-INDICE.
           02 WKS-NOMBRE-TRAB  PIC X(50).
           02 WKS-APELL-PAT    PIC X(50).
           02 WKS-APELL-MAT    PIC X(50).
           02 WKS-SALARIO      PIC S9(5)V99.
           02 WKS-ID-TRAB      PIC 9(2).

       77 WKS-FIN              PIC 9(3).
       77 WKS-SAL-FORMATO      PIC ZZZZZ9.99.
       77 WKS-CONTADOR         PIC 9(2).
       77 WKS-TERMINA          PIC 9(1).
       77 WKS-RECORRE          PIC 9(3).
       77 WKS-ID-AUX           PIC 9(2).

       01 WKS-OPCION           PIC A(1) VALUE SPACE.
           88 WKS-OPC-ALTA  VALUE 'A'.
           88 WKS-OPC-CONS  VALUE 'C'.
           88 WKS-OPC-S     VALUE 'S'.
           88 WKS-OPC-N     VALUE 'N'.
           88 WKS-OPC-T     VALUE 'T'.



       PROCEDURE DIVISION.

       00000-MAIN.

           DISPLAY "NOMINA TRABAJADORES".
           MOVE ZERO TO WKS-TERMINA.
           MOVE ZERO TO WKS-FIN.
           PERFORM 00003-MENU UNTIL WKS-TERMINA = 1.
           STOP RUN.

       00003-MENU.

           DISPLAY "A. ALTAS, C. CONSULTA GENERAL, "
      -    "T. CONSULTA TRABAJADOR, S. SALIR"
           ACCEPT WKS-OPCION
           IF WKS-OPC-ALTA
              PERFORM 00001-CAPTURA-TRAB
              ELSE
              IF WKS-OPC-CONS
              MOVE 1 TO WKS-RECORRE
              PERFORM 00002-CONSULTA-GRAL UNTIL WKS-RECORRE > WKS-FIN
                     ELSE
                     IF WKS-OPC-T
                        PERFORM 00005-CONSULTA-TRAB
                     ELSE
                     IF WKS-OPC-S
                        MOVE 1 TO WKS-TERMINA
                        ELSE
                            DISPLAY "OPCION INVALIDA, VERIFIQUE."
                     END-IF
              END-IF
           END-IF.


       00001-CAPTURA-TRAB.

           ADD 1 TO WKS-FIN.
           IF WKS-FIN > 100
              DISPLAY "NO SE PUEDE AGREGAR TRABAJADORES, "
              "CONTACTE SISTEMAS"
           ELSE
           DISPLAY "NOMBRE EMPLEADO: "
           ACCEPT WKS-NOMBRE-TRAB(WKS-FIN)

           DISPLAY "APELLIDO PATERNO"
           ACCEPT WKS-APELL-PAT(WKS-FIN)

           DISPLAY "APELLIDO MATERNO"
           ACCEPT WKS-APELL-MAT(WKS-FIN)

           DISPLAY "SALARIO:"
           ACCEPT WKS-SALARIO(WKS-FIN)

           ADD 10 TO WKS-CONTADOR
           MOVE WKS-CONTADOR TO WKS-ID-TRAB(WKS-FIN)
           PERFORM 00004-OTRA-CAPTURA
           END-IF.


       00004-OTRA-CAPTURA.

           DISPLAY "DESEA AGREGAR TRABAJADOR? S-SI, N-NO"
           ACCEPT WKS-OPCION.

           IF WKS-OPC-S
           PERFORM 00001-CAPTURA-TRAB
               ELSE
               IF WKS-OPC-N
                  NEXT SENTENCE
                  ELSE
                      DISPLAY "OPCION INVALIDA, VERIFIQUE."
                      PERFORM 00004-OTRA-CAPTURA
               END-IF
           END-IF.

       00002-CONSULTA-GRAL.

           DISPLAY "ID TRABAJADOR: " WKS-ID-TRAB(WKS-RECORRE).
           DISPLAY "NOMBRE: " WKS-NOMBRE-TRAB(WKS-RECORRE).
           DISPLAY "APELLIDOS: " WKS-APELL-PAT(WKS-RECORRE)
                                 WKS-APELL-MAT(WKS-RECORRE).
           MOVE WKS-SALARIO(WKS-RECORRE) TO WKS-SAL-FORMATO.
           DISPLAY "SALARIO: " WKS-SAL-FORMATO.
           ADD 1 TO WKS-RECORRE.

       00005-CONSULTA-TRAB.

           DISPLAY "INSERTE ID DEL TRABAJADOR: "
           ACCEPT WKS-ID-AUX.
           SET WKS-INDICE TO 1
           SEARCH WKS-DATOS-TRAB
                  AT END DISPLAY "TRABAJADOR NO ENCONTRADO"
                  WHEN WKS-ID-TRAB(WKS-INDICE) = WKS-ID-AUX
                  DISPLAY "ID TRABAJADOR: " WKS-ID-TRAB(WKS-INDICE)
                  DISPLAY "NOMBRE: " WKS-NOMBRE-TRAB(WKS-INDICE)
                  DISPLAY "APELLIDOS: " WKS-APELL-PAT(WKS-INDICE)
                                 WKS-APELL-MAT(WKS-INDICE)
                  MOVE WKS-SALARIO(WKS-INDICE) TO WKS-SAL-FORMATO
                  DISPLAY "SALARIO: " WKS-SAL-FORMATO.








       END PROGRAM NOMINA.
