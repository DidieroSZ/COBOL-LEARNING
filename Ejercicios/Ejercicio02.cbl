      ******************************************************************
      * Author:
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. EJERCICIO02.

       DATA DIVISION.
           FILE SECTION.

           WORKING-STORAGE SECTION.
       77 WS-NUM1 PIC 99 VALUE ZERO.
       77 WS-NUM2 PIC 99 VALUE ZERO.
       77 RESULTADO PIC 9(4) VALUE ZERO.

       PROCEDURE DIVISION.

           ACCEPT-DATA.
               DISPLAY "Ingresa 2 números entre 01 y 99:".
               DISPLAY SPACE WITH NO ADVANCING.
               ACCEPT WS-NUM1.
               ACCEPT WS-NUM2.

           CALCULATE-OPERATIONS.
               DISPLAY "Número ingresados: "WS-NUM1 " y "WS-NUM2.
               COMPUTE RESULTADO = WS-NUM1 + WS-NUM2.
               DISPLAY "Suma: " RESULTADO.


      *     MAIN-PROCEDURE.
      *        DISPLAY "Hello world"
               STOP RUN.

       END PROGRAM EJERCICIO02.
