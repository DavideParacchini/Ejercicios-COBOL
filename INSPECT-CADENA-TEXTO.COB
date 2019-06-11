      ******************************************************************
      * Author: Davide, Paracchini
      * Date: 11/06/2019
      * Purpose: Ejercitació.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT-CADENA-TEXTO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77 NOMBRE   PIC A(15).
       77 CONTADOR PIC 9(02).



       PROCEDURE DIVISION.

       0000-MAIN SECTION.
       0000-PRINCIPAL.

           DISPLAY 'INGRESE NOMBRE: '
           ACCEPT NOMBRE

           INSPECT NOMBRE TALLYING CONTADOR
                   FOR CHARACTERS

           DISPLAY 'LONGITUD NOMBRE: ' CONTADOR

           MOVE ZEROES TO CONTADOR

           INSPECT NOMBRE TALLYING CONTADOR
                   FOR ALL 'A'

           DISPLAY 'CANTIDAD LETRAS A: ' CONTADOR

           MOVE ZEROES TO CONTADOR

           INSPECT NOMBRE TALLYING CONTADOR
                   FOR CHARACTERS BEFORE INITIAL ' '

           DISPLAY 'CANTIDAD LETRAS DE TU NOMBRE: ' CONTADOR

           MOVE ZEROES TO CONTADOR

           INSPECT NOMBRE TALLYING CONTADOR
                   FOR CHARACTERS AFTER INITIAL ' '

           DISPLAY 'EL RESTO DE TU NOMBRE CON ESPACIOS ES: ' CONTADOR

           MOVE ZEROES TO CONTADOR

           INSPECT NOMBRE TALLYING CONTADOR
                   FOR LEADING 'J'

           DISPLAY 'LA CADENA TIENE J: SI = 1 NO = 0' CONTADOR


           DISPLAY 'CADENA DE TEXTO 1: '
           ACCEPT NOMBRE

           INSPECT NOMBRE REPLACING CHARACTERS BY 'F'
           DISPLAY 'CADENA DE TEXTO REMPLAZADA: ' NOMBRE

           DISPLAY 'CADENA TEXTO 2: '
           ACCEPT NOMBRE

           INSPECT NOMBRE REPLACING ALL 'E' BY 'A'
           DISPLAY 'CADENA CON REEMPLAZO: ' NOMBRE

           DISPLAY 'CADENA DE TEXTO 3: '
           ACCEPT NOMBRE

           INSPECT NOMBRE REPLACING FIRST 'J' BY 'F'
           DISPLAY 'CADENA CON REEMPLAZO: ' NOMBRE

           DISPLAY 'CADENA 4: '
           ACCEPT NOMBRE

           INSPECT NOMBRE REPLACING ALL 'HOLA' BY 'BYE-'
                   AFTER INITIAL ' ' BEFORE INITIAL 'M'
           DISPLAY 'CADENA CON REEMPLAZO: ' NOMBRE

           DISPLAY 'CADENA 5: '
           ACCEPT NOMBRE

           MOVE ZEROES TO CONTADOR
           DISPLAY 'CADENA 5 ORIGINAL: ' NOMBRE
           INSPECT NOMBRE TALLYING CONTADOR
                   FOR ALL 'E' REPLACING ALL 'E' BY 'A'
                   BEFORE INITIAL 'M'
           DISPLAY 'CADENA MODIFICADA: ' NOMBRE
           DISPLAY 'CADENA 5 TIENE ESTAS E: ' CONTADOR

           DISPLAY 'CADENA 6: '
           ACCEPT NOMBRE
           DISPLAY 'CADENA 6 ORIGINAL: ' NOMBRE
           INSPECT NOMBRE CONVERTING 'AOI' TO 'EUY'
           DISPLAY 'CADENA CONVERTIDA: ' NOMBRE

           INSPECT NOMBRE CONVERTING 'ABCDEFGHIJKLMNOPQRSTUVWXYZÑ'
                          TO 'abcdefghijklmnopqrstuvwxyzñ'
           DISPLAY 'CADENA EN MINUSCULAS: ' NOMBRE





           STOP RUN.
       END PROGRAM INSPECT-CADENA-TEXTO.
