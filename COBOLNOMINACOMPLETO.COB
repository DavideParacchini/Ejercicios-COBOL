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
           02 WKS-DEPARTAMENTO PIC 9(2) OCCURS 3 TIMES.

       77 WKS-FIN              PIC 9(3).
       77 WKS-SAL-FORMATO      PIC ZZZZZ9.99.
       77 WKS-CONTADOR         PIC 9(2).
       77 WKS-TERMINA          PIC 9(1).
       77 WKS-RECORRE          PIC 9(3).
       77 WKS-ID-AUX           PIC 9(2).
       77 WKS-COL              PIC 9(1).
       77 WKS-ELECCION         PIC 9(1).
       77 WKS-INDICADOR        PIC 9(1).

       01 WKS-OPCION           PIC A(1) VALUE SPACE.
           88 WKS-OPC-ALTA  VALUE 'A'.
           88 WKS-OPC-CONS  VALUE 'C'.
           88 WKS-OPC-S     VALUE 'S'.
           88 WKS-OPC-N     VALUE 'N'.
           88 WKS-OPC-T     VALUE 'T'.
           88 WKS-OPC-M     VALUE 'M'.
           88 WKS-OPC-B     VALUE 'B'.

       77 WKS-AUX-NOMBRE       PIC X(50).
       77 WKS-AUX-SALARIO      PIC S9(5)V99.
       77 WKS-AUX-ID-TRAB      PIC 9(2).
       77 WKS-AUX-DEPARTAMENTO PIC 9(2).


       PROCEDURE DIVISION.

       00000-MAIN.

           DISPLAY "NOMINA TRABAJADORES".
           MOVE ZERO TO WKS-TERMINA.
           MOVE ZERO TO WKS-FIN.
           PERFORM 00003-MENU UNTIL WKS-TERMINA = 1
           STOP RUN.

       00003-MENU.

           DISPLAY "A. ALTAS, C. CONSULTA GENERAL, "
           "T. CONSULTA TRABAJADOR, M. MODIFICAR TRABAJADOR, B. BAJAS, "
           "S. SALIR"
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
                        IF WKS-OPC-M
                        PERFORM 00005-MODIFICAR-TRAB
                     ELSE
                        IF WKS-OPC-B
                        PERFORM 00015-BAJA-TRABAJADOR
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

           MOVE 1 TO WKS-COL
           PERFORM 00006-CAPTURA-DPTO UNTIL WKS-COL > 3

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

           IF WKS-ID-TRAB(WKS-RECORRE) > 0
           DISPLAY "ID TRABAJADOR: " WKS-ID-TRAB(WKS-RECORRE)
           DISPLAY "NOMBRE: " WKS-NOMBRE-TRAB(WKS-RECORRE)
           DISPLAY "APELLIDOS: " WKS-APELL-PAT(WKS-RECORRE)
                                 WKS-APELL-MAT(WKS-RECORRE)
           MOVE WKS-SALARIO(WKS-RECORRE) TO WKS-SAL-FORMATO
           DISPLAY "SALARIO: " WKS-SAL-FORMATO
           MOVE 1 TO WKS-COL
           PERFORM 00008-CONS-DPTO-GRAL UNTIL WKS-COL > 3

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
                  MOVE 1 TO WKS-COL.
                  PERFORM 00009-CONS-DPTO-TRAB UNTIL WKS-COL > 3
                  IF WKS-INDICADOR = 1
                  PERFORM 00013-MODIFICACION
                  MOVE ZEROES TO WKS-INDICADOR
                  ELSE
                  IF WKS-INDICADOR = 2
                  PERFORM 00016-BAJA
                  MOVE ZEROES TO WKS-INDICADOR.

       00006-CAPTURA-DPTO.

           IF WKS-COL > 3
              DISPLAY "SOLO SE PUEDEN INGRESAR 3 DEPARTAMENTOS."
              ELSE
              DISPLAY "INGRESE DEPARTAMENTO DEL TRABAJADOR: "
              ACCEPT WKS-DEPARTAMENTO(WKS-FIN,WKS-COL)
              PERFORM 00007-OTRO-DPTO
           END-IF.



       00007-OTRO-DPTO.

           DISPLAY "DESEA INGRESAR OTRO DEPARTAMENTO? S. SI, N. NO".
           ACCEPT WKS-OPCION.

           IF WKS-OPC-S
              ADD 1 TO WKS-COL
              PERFORM 00006-CAPTURA-DPTO
              ELSE
                  IF WKS-OPC-N
                     MOVE 4 TO WKS-COL
                     ELSE
                         DISPLAY "OPCION INVALIDA, VERIFIQUE."
                         PERFORM 00007-OTRO-DPTO
                  END-IF
           END-IF.

       00008-CONS-DPTO-GRAL.

           IF WKS-DEPARTAMENTO(WKS-RECORRE, WKS-COL) NOT = " "
           OR WKS-DEPARTAMENTO(WKS-RECORRE, WKS-COL) NOT = SPACE
              DISPLAY "DEPARTAMENTO " WKS-COL ": "
              DISPLAY WKS-DEPARTAMENTO(WKS-RECORRE, WKS-COL)
              ADD 1 TO WKS-COL
              ELSE
                  MOVE 4 TO WKS-COL
           END-IF.

       00009-CONS-DPTO-TRAB.

           IF WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL) NOT = " "
           OR WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL) NOT = SPACE
              DISPLAY "DEPARTAMENTO " WKS-COL ": "
              DISPLAY WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL)
              ADD 1 TO WKS-COL
              ELSE
                  MOVE 4 TO WKS-COL
           END-IF.

       00005-MODIFICAR-TRAB.

           MOVE 1 TO WKS-INDICADOR
           PERFORM 00005-CONSULTA-TRAB.

       00013-MODIFICACION.

           DISPLAY 'QUE DESA MODIFICAR? 1. NOMBRE 2. APELLP 3.APELLM '
                   '4. SALARIO 5.DEPARTAMENTOS 6.CANCELAR'
           ACCEPT WKS-ELECCION
           IF WKS-ELECCION = 1
              DISPLAY 'NUEVO NOMBRE:'
              ACCEPT WKS-AUX-NOMBRE
              MOVE WKS-AUX-NOMBRE TO WKS-NOMBRE-TRAB(WKS-INDICE)
           ELSE
           IF WKS-ELECCION = 2
              DISPLAY 'NUEVO APELLIDO PATERNO:'
              ACCEPT WKS-AUX-NOMBRE
              MOVE WKS-AUX-NOMBRE TO WKS-APELL-PAT(WKS-INDICE)
           ELSE
           IF WKS-ELECCION = 3
              DISPLAY 'NUEVO APELLIDO MATERNO:'
              ACCEPT WKS-AUX-NOMBRE
              MOVE WKS-AUX-NOMBRE TO WKS-APELL-MAT(WKS-INDICE)
           ELSE
           IF WKS-ELECCION = 4
              DISPLAY 'NUEVO SALARIO:'
              ACCEPT WKS-AUX-SALARIO
              MOVE WKS-AUX-SALARIO TO WKS-SALARIO(WKS-INDICE)
           ELSE
           IF WKS-ELECCION = 5
              MOVE 1 TO WKS-COL
              PERFORM 00011-MODIFICA-DPTOS UNTIL WKS-COL > 3
              ELSE
           IF WKS-ELECCION = 6
              NEXT SENTENCE
              ELSE
                  DISPLAY 'OPCION INVALIDA VERIFIQUE.'
                  PERFORM 00013-MODIFICACION.

       00011-MODIFICA-DPTOS.
              IF WKS-COL < 4
              DISPLAY "NUEVO DEPARTAMENTO " WKS-COL ": "
              ACCEPT WKS-AUX-DEPARTAMENTO
              MOVE WKS-AUX-DEPARTAMENTO TO
                   WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL)
              ADD 1 TO WKS-COL
              PERFORM 00012-MODIFICA-OTRO-DPTO
              ELSE
                  DISPLAY 'MAXIMO DEPARTAMENTOS.'.

       00012-MODIFICA-OTRO-DPTO.

              DISPLAY 'OTRO DEPARTAMENTO? S.SI N.NO'
              ACCEPT WKS-OPCION
              IF WKS-OPC-S
                 PERFORM 00011-MODIFICA-DPTOS
                 ELSE
              IF WKS-OPC-N
                 MOVE 4 TO WKS-COL
                 ELSE
                     DISPLAY 'OPCION INVALIDA VERIFIQUE.'
                     PERFORM 00012-MODIFICA-OTRO-DPTO.

       00015-BAJA-TRABAJADOR.

           MOVE 2 TO WKS-INDICADOR
           PERFORM 00005-CONSULTA-TRAB.

       00016-BAJA.

           DISPLAY 'CONFIRMA LA BAJA DEL TRABAJADOR ID: '
                   WKS-ID-TRAB(WKS-INDICE)'? S.SI N.NO'
           ACCEPT WKS-OPCION
           IF WKS-OPC-S
              MOVE LOW-VALUES TO WKS-DATOS-TRAB(WKS-INDICE)
           ELSE
           IF WKS-OPC-N
              NEXT SENTENCE
           ELSE
               DISPLAY 'ERROR VERIFIQUE'
               PERFORM 00016-BAJA.


       END PROGRAM NOMINA.
