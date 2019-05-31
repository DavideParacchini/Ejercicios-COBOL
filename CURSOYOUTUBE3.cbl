      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USODEVARIABLES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-NOMBRE PICTURE A(30).
       01 WS-REGISTROEMPLEADO.
           02 WS-NOMEMP.
               03 WS-NOMTRAB PIC A(20) VALUE "JOE".
               03 WS-APELLIDOPATERNO PIC A(20) VALUE "DOE".
               03 WS-APELLIDOMATERNO PIC A(20) VALUE "DOA".
           02 WS-EDAD PIC 9(2) VALUE 29.
           02 WS-SALARIO PIC 9(5)V99 VALUE 788.22.
       66 WS-APELLIDOSTRAB RENAMES WS-APELLIDOPATERNO
       THRU WS-APELLIDOMATERNO.

       01 WS-BANDERA PIC 9(1).
           88 WS-TRUE VALUE 1.
           88 WS-FALSE VALUE 0.
       77 GASTOS PIC 9(5)V9(2).
       77 WS-HORAS PIC 9(10).
       77 WS-DIRECCION PIC X(30) VALUE "CARLOS CALVO #241".



       PROCEDURE DIVISION.
           DISPLAY "BIENVENIDO AL 3ER PROGRAMA".
           DISPLAY "PROGRAMA DE VARIABLES".
           ACCEPT WS-NOMBRE.
           DISPLAY "HOLA " WS-NOMBRE.
           DISPLAY "REGISTRO EMPLEADO " WS-REGISTROEMPLEADO.
           DISPLAY "EDAD " WS-EDAD.
           DISPLAY "NOMBRE: " WS-NOMTRAB.
           DISPLAY "APELLIDOS: " WS-APELLIDOSTRAB.
           DISPLAY "DIRECCION: " WS-DIRECCION.
           DISPLAY "GASTOS: " GASTOS.
           DISPLAY "BANDERA: " WS-BANDERA.

           STOP RUN.
       END PROGRAM USODEVARIABLES.
