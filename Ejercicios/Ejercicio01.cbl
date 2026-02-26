      ******************************************************************
      * Author:
      ******************************************************************
       IDENTIFICATION DIVISION.
      * CLAUSULA
       PROGRAM-ID. EJERCICIO01.


      * DIVISION
       DATA DIVISION.
      * SECTION
           FILE SECTION.
           WORKING-STORAGE SECTION.
      *        CLAUSULA
       77 WS-NOMBRE PIC A(25) VALUE SPACES.
       77 WS-NOMBRE-LIMPIO PIC A(15).

       PROCEDURE DIVISION.
      *    PARRAFO
           MAIN-PROCEDURE.
      *        SENTENCIA
               DISPLAY "Hola, ingresa tu nombre: ".
               ACCEPT WS-NOMBRE.
               MOVE FUNCTION TRIM(WS-NOMBRE) TO WS-NOMBRE-LIMPIO.
               DISPLAY "Hola soy: " WS-NOMBRE-LIMPIO " , bienvenido a "
               "COBOL.".
            STOP RUN.
       END PROGRAM EJERCICIO01.
