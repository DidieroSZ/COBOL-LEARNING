      ******************************************************************
      * Author: DIDIER.
      * Date: 04/03/2026.
      * Purpose: MAKE A DOCUMENT WITH THE INFORMATION OF STUDENTS NAMED
      *          "estudiantes.txt".
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOC-EST-MAKER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DOC-EST ASSIGN TO "estudiantes.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD DOC-EST.
      *    LONG DOC: 72 CHARS
           01 EST-REG-DOC.
               05 EST-REG-CLAVE PIC 9(08).
               05 EST-REG-NOMBRE PIC X(40).
               05 EST-REG-GRADO PIC 9(02).
               05 EST-REG-CARRERA PIC 9(02).
               05 EST-REG-MATERIA-1 PIC 9(02).
               05 EST-REG-MATERIA-2 PIC 9(02).
               05 EST-REG-MATERIA-3 PIC 9(02).
               05 EST-REG-MATERIA-4 PIC 9(02).
               05 EST-REG-MATERIA-5 PIC 9(02).
               05 EST-REG-MATERIA-6 PIC 9(02).
               05 EST-REG-MATERIA-7 PIC 9(02).
               05 EST-REG-MATERIA-8 PIC 9(02).
               05 EST-REG-MATERIA-9 PIC 9(02).
               05 EST-REG-MATERIA-10 PIC 9(02).

       WORKING-STORAGE SECTION.
      *    COPY "HeaderMaker.cbl".

           01 IN-DATA.
               05 WS-CLAVE PIC 9(08).
               05 WS-NOMBRE PIC X(40).
               05 WS-GRADO PIC X(02).
               05 WS-CARRERA PIC X(02).
               05 WS-MAT-1 PIC X(02).
               05 WS-MAT-2 PIC X(02).
               05 WS-MAT-3 PIC X(02).
               05 WS-MAT-4 PIC X(02).
               05 WS-MAT-5 PIC X(02).
               05 WS-MAT-6 PIC X(02).
               05 WS-MAT-7 PIC X(02).
               05 WS-MAT-8 PIC X(02).
               05 WS-MAT-9 PIC X(02).
               05 WS-MAT-10 PIC X(02).

           01 SWITCHES.
               05 LOG-DATA-B PIC XX VALUE "SI".
                   88 SW-CONTINUAR VALUE "SI".
                   88 SW-DETENER VALUE "NO".

       PROCEDURE DIVISION.

       OPEN OUTPUT DOC-EST.
       PERFORM 100-INICIO.
       PERFORM 200-FIN.
       CLOSE DOC-EST.
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
           PERFORM 100200-SUBJECTS-DATA.
           PERFORM 100300-MOVE-DATA.
           PERFORM 100400-NEW-DATA.
           EXIT.

       100100-GENERAL-DATA.
           DISPLAY "--------------------------".
           DISPLAY "---- NUEVO ESTUDIANTE ----".
           DISPLAY "--------------------------".
           DISPLAY "LOG -- CLAVE estudiante: ".
           ACCEPT WS-CLAVE.
           DISPLAY "LOG -- NOMBRE estudiante: ".
           ACCEPT WS-NOMBRE.
           MOVE FUNCTION UPPER-CASE(WS-NOMBRE) TO WS-NOMBRE.
           DISPLAY "LOG -- GRADO estudiante: ".
           ACCEPT WS-GRADO.
           DISPLAY "LOG -- CARRERA estudiante: ".
           ACCEPT WS-CARRERA.
           EXIT.

       100200-SUBJECTS-DATA.
           DISPLAY "LOG -- MATERIA 1: ".
           ACCEPT WS-MAT-1.
           DISPLAY "LOG -- MATERIA 2: ".
           ACCEPT WS-MAT-2.
           DISPLAY "LOG -- MATERIA 3: ".
           ACCEPT WS-MAT-3.
           DISPLAY "LOG -- MATERIA 4: ".
           ACCEPT WS-MAT-4.
           DISPLAY "LOG -- MATERIA 5: ".
           ACCEPT WS-MAT-5.
           DISPLAY "LOG -- MATERIA 6: ".
           ACCEPT WS-MAT-6.
           DISPLAY "LOG -- MATERIA 7: ".
           ACCEPT WS-MAT-7.
           DISPLAY "LOG -- MATERIA 8: ".
           ACCEPT WS-MAT-8.
           DISPLAY "LOG -- MATERIA 9: ".
           ACCEPT WS-MAT-9.
           DISPLAY "LOG -- MATERIA 10: ".
           ACCEPT WS-MAT-10.
           EXIT.

       100300-MOVE-DATA.
           INITIALISE EST-REG-DOC.
      *    GENERAL-DATA
           MOVE WS-CLAVE TO EST-REG-CLAVE.
           MOVE WS-NOMBRE  TO EST-REG-NOMBRE .
           MOVE WS-GRADO TO EST-REG-GRADO.
           MOVE WS-CARRERA TO EST-REG-CARRERA.
      *    SUBJECTS-DATA
           MOVE WS-MAT-1 TO EST-REG-MATERIA-1.
           MOVE WS-MAT-2 TO EST-REG-MATERIA-2.
           MOVE WS-MAT-3 TO EST-REG-MATERIA-3.
           MOVE WS-MAT-4 TO EST-REG-MATERIA-4.
           MOVE WS-MAT-5 TO EST-REG-MATERIA-5.
           MOVE WS-MAT-6 TO EST-REG-MATERIA-6.
           MOVE WS-MAT-7 TO EST-REG-MATERIA-7.
           MOVE WS-MAT-8 TO EST-REG-MATERIA-8.
           MOVE WS-MAT-9 TO EST-REG-MATERIA-9.
           MOVE WS-MAT-10 TO EST-REG-MATERIA-10.
      *    WRITE-DATA
           WRITE EST-REG-DOC.
           EXIT.

       100400-NEW-DATA.
           DISPLAY "__________________________".
           DISPLAY "¿Ingresar nuevo estudiante?".
           DISPLAY "CONTINUAR ---- SI".
           DISPLAY "DETENER ---- NO".
           ACCEPT SWITCHES.
           MOVE FUNCTION UPPER-CASE(SWITCHES) TO SWITCHES.
           EXIT.

       END PROGRAM DOC-EST-MAKER.
