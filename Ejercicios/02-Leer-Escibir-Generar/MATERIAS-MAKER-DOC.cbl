      ******************************************************************
      * Author: DIDIER.
      * Date: 05/03/2026.
      * Purpose: MAKE A DOCUMENT WITH THE INFORMATION OF THE SUBJECTS
      *  NAMED "materias.txt".
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOC-EST-MAKER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DOC-MATERIAS ASSIGN TO "materias.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD DOC-MATERIAS.
      *    LONG DOC: 42 CHARS
           01 MAT-REG-DOC.
               05 MAT-REG-CLAVE PIC 9(02).
               05 MAT-REG-NOMBRE PIC A(20).
               05 MAT-REG-PROFE PIC A(20).

       WORKING-STORAGE SECTION.
           01 IN-DATA.
               05 WS-CLAVE PIC 9(02).
               05 WS-NOMBRE PIC A(20).
               05 WS-PROFE PIC A(20).

           01 SWITCHES.
               05 LOG-DATA-B PIC XX VALUE "SI".
                   88 SW-CONTINUAR VALUE "SI".
                   88 SW-DETENER VALUE "NO".

       PROCEDURE DIVISION.

       OPEN OUTPUT DOC-MATERIAS.
       PERFORM 100-INICIO.
       PERFORM 200-FIN.
       CLOSE DOC-MATERIAS.
       STOP RUN.

       100-INICIO.
           PERFORM 100000-DATA-LOG UNTIL SW-DETENER.
           EXIT.

       200-FIN.
           DISPLAY "*************************".
           DISPLAY "**** FIN DE PROGRAMA ****".
           DISPLAY "*************************".
           EXIT.

       100000-DATA-LOG.
           PERFORM 100100-GENERAL-DATA.
           PERFORM 100300-MOVE-DATA.
           PERFORM 100400-NEW-DATA.
           EXIT.

       100100-GENERAL-DATA.
           DISPLAY "--------------------------".
           DISPLAY "---- NUEVA MATERIA ----".
           DISPLAY "--------------------------".
           ADD 1 TO WS-CLAVE.
           DISPLAY "LOG -- NOMBRE Materia: ".
           ACCEPT WS-NOMBRE.
           MOVE FUNCTION UPPER-CASE(WS-NOMBRE) TO WS-NOMBRE.
           DISPLAY "LOG -- PROFESOR Materia: ".
           ACCEPT WS-PROFE.
           MOVE FUNCTION UPPER-CASE(WS-PROFE) TO WS-PROFE.
           EXIT.


       100300-MOVE-DATA.
           INITIALISE MAT-REG-DOC.
      *    GENERAL-DATA
           MOVE WS-CLAVE TO MAT-REG-CLAVE.
           MOVE WS-NOMBRE  TO MAT-REG-NOMBRE.
           MOVE WS-PROFE TO MAT-REG-PROFE.

      *    WRITE-DATA
           WRITE MAT-REG-DOC.
           EXIT.

       100400-NEW-DATA.
           DISPLAY "__________________________".
           DISPLAY "¿Ingresar nueva MATERIA?".
           DISPLAY "CONTINUAR ---- SI".
           DISPLAY "DETENER ---- NO".
           ACCEPT SWITCHES.
           MOVE FUNCTION UPPER-CASE(SWITCHES) TO SWITCHES.
           EXIT.

       END PROGRAM DOC-EST-MAKER.
