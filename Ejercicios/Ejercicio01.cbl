      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO01.
       DATA DIVISION.
           FILE SECTION.
           WORKING-STORAGE SECTION.
               01 WS-NOMBRE PIC A(25) VALUE SPACES.
               01 WS-NOMBRE-LIMPIO PIC A(15).

       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               DISPLAY "Hola, ingresa tu nombre: ".
               ACCEPT WS-NOMBRE.
               MOVE FUNCTION TRIM(WS-NOMBRE) TO WS-NOMBRE-LIMPIO.
               DISPLAY "Hola soy: " WS-NOMBRE-LIMPIO " , bienvenido a"
               "COBOL.".

            STOP RUN.
       END PROGRAM EJERCICIO01.
