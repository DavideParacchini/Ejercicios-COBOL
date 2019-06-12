      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARCHIVOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-ARCHIVO ASSIGN TO 'DATOS.TXT'.
      *Aca se asigna el archivo a un disco o al propio programa.
       DATA DIVISION.

       FILE SECTION.
       FD F-ARCHIVO.
       01 REG-CLIENTE.
          02 RC-NOMBRE        PIC A(50).
          02 RC-APELLIDOS     PIC A(50).
          02 RC-DOMICILO      PIC X(50).
          02 RC-TELEFONO      PIC 9(10).
      *Aca se describe como esta compuesto el archivo.
       WORKING-STORAGE SECTION.

       01 WS-CLIENTE.
          02 WKS-NOMBRE-C     PIC A(50).
          02 WKS-DOMICILO     PIC X(50).
          02 WKS-TELEFONO     PIC 9(10).
          02 WKS-PASATIEMPO   PIC X(30).
          02 WKS-COLOR        PIC X(30).

       77 WKS-CONCATENADO     PIC X(150).

       PROCEDURE DIVISION.
       SECCION-PRINCIPAL SECTION.
           DISPLAY 'PROGRAMA GENERADOR DE ARCHIVO CLIENTES'

           DISPLAY 'NOMBRE COMPLETO DEL CLIENTE: '
           ACCEPT WKS-NOMBRE-C

           DISPLAY 'DIRECCION CLIENTE: '
           ACCEPT WKS-DOMICILO

           DISPLAY 'TELEFONO CLIENTE: '
           ACCEPT WKS-TELEFONO
      *Divide una variable en dos para moverla al archivo.
           UNSTRING WKS-NOMBRE-C DELIMITED BY ' '
               INTO RC-NOMBRE
                    RC-APELLIDOS

           MOVE WKS-DOMICILO TO RC-DOMICILO
           MOVE WKS-TELEFONO TO RC-TELEFONO
      *Comando para abrir el archivo y grabarlo
           OPEN OUTPUT F-ARCHIVO
           WRITE REG-CLIENTE

           DISPLAY 'PASATIEMPO FAVORITO DEL CLIENTE: '
           ACCEPT WKS-PASATIEMPO
           DISPLAY 'COLOR FAVORITO DEL CLIENTE: '
           ACCEPT WKS-COLOR
      *Une varias variables a otra
           STRING 'PASATIEMPO: ' WKS-PASATIEMPO
                  'COLOR: ' WKS-COLOR
                  DELIMITED BY SIZE
                  INTO WKS-CONCATENADO
      *Deja el archivo en blanco
           MOVE LOW-VALUES TO REG-CLIENTE
      *Escribe la variable al archivo, no hace falta abrirlo de nuevo, solo se abre una vez
           WRITE REG-CLIENTE FROM WKS-CONCATENADO

           CLOSE F-ARCHIVO

           DISPLAY 'TERMINA PROGRAMA.'

           STOP RUN.
       END PROGRAM ARCHIVOS.
