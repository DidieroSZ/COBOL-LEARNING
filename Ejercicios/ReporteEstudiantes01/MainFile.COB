      ******************************************************************
      * Author: DIDERO
      * Purpose: MAKE A REPORT FILE NAMED "estudiantes.txt"
      * DEPENDENCIES:
      *    Headers -> HeaderMaker.cbl
      *    Main -> ArchivoApertura.cbl
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-FILE-OPEN.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT ESTUDIANTES-ARCHIVO ASSIGN TO "estudiantes.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
               FD ESTUDIANTES-ARCHIVO.
                   01 LINEA-TEMP PIC X(132).

           WORKING-STORAGE SECTION.
           01 FORMATO.
               05 LINEA-PUNTEADA PIC X(132) VALUE ALL "-  ".
               05 LINEA-TABLA PIC X(132) VALUE ALL "_".
               05 LONG-WORD PIC 9(03).
               05 POS PIC 9(03).
               05 LARGE-FORMAT PIC 9(03) VALUE 132.
               05 PHRASE-NOMBRE PIC A(40) VALUE SPACES.
               05 TEMP5 PIC X(5).
               05 TEMP7 PIC X(7).

           01 COLUMNAS.
                05 COL-GRADO    PIC X(7).
                05 COL-CARRERA  PIC X(9).
                05 COL-MAT1     PIC X(5).
                05 COL-MAT2     PIC X(5).
                05 COL-MAT3     PIC X(5).
                05 COL-MAT4     PIC X(5).
                05 COL-MAT5     PIC X(5).
                05 COL-MAT6     PIC X(5).
                05 COL-MAT7     PIC X(5).
                05 COL-MAT8     PIC X(5).
                05 COL-MAT9     PIC X(5).
                05 COL-MAT10     PIC X(5).

           01 WS-CAMPOS.
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

           01 SWITCHES PIC X(02) VALUE "SI".
               88 SW-CONTINUAR VALUE "SI".
               88 SW-DETENER VALUE "NO".

       PROCEDURE DIVISION.

       OPEN OUTPUT ESTUDIANTES-ARCHIVO.
       PERFORM 100-INICIO.
       CLOSE ESTUDIANTES-ARCHIVO.
       STOP RUN.

       100-INICIO.
           PERFORM 100100-HEADER-MAKER.
           PERFORM 100200-DATA-IN UNTIL SW-DETENER.
           EXIT.

       100101-L-PUNTEADA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-PUNTEADA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100102-L-TABLA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-TABLA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100103-L-BLANCA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE SPACES TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.

       100100-HEADER-MAKER.
           PERFORM 100101-L-PUNTEADA-MAKER.

           MOVE SPACES TO LINEA-TEMP.
          COMPUTE LONG-WORD = FUNCTION LENGTH("REPORTE DE ESTUDIANTES").
          COMPUTE POS = (LARGE-FORMAT / 2) - (LONG-WORD / 2).
           MOVE "REPORTE DE ESTUDIANTES" TO LINEA-TEMP(POS:LONG-WORD).
           WRITE LINEA-TEMP.

           PERFORM 100101-L-PUNTEADA-MAKER.

           PERFORM 100102-L-TABLA-MAKER.

           PERFORM 100103-L-BLANCA-MAKER.

           COMPUTE LONG-WORD = FUNCTION LENGTH("NOMBRE ALUMNOS").
           COMPUTE POS = (40 / 2) - (LONG-WORD / 2).
           MOVE "NOMBRE ALUMNOS" TO PHRASE-NOMBRE(POS: LONG-WORD).
           INITIALISE LINEA-TEMP.

           STRING
               " CLAVE  " DELIMITED BY SIZE
               "|"
               PHRASE-NOMBRE DELIMITED BY SIZE
               "|"
               " GRADO "  DELIMITED BY SIZE
               "|"
               " CARRERA " DELIMITED BY SIZE
               "|"
               "MAT01" DELIMITED BY SIZE
               "|"
               "MAT02" DELIMITED BY SIZE
               "|"
               "MAT03" DELIMITED BY SIZE
               "|"
               "MAT04" DELIMITED BY SIZE
               "|"
               "MAT05" DELIMITED BY SIZE
               "|"
               "MAT06" DELIMITED BY SIZE
               "|"
               "MAT07" DELIMITED BY SIZE
               "|"
               "MAT08" DELIMITED BY SIZE
               "|"
               "MAT09" DELIMITED BY SIZE
               "|"
               "MAT10" DELIMITED BY SIZE
               "|"
           INTO LINEA-TEMP.
           WRITE LINEA-TEMP.

           PERFORM 100102-L-TABLA-MAKER.

           EXIT.

       100200-DATA-IN.
           PERFORM 100201-GENERAL-DATA.
           PERFORM 100202-GRADES-DATA.
           PERFORM 100203-MOVE-DATA.
           PERFORM 100204-NEW-DATA.
           EXIT.

       100201-GENERAL-DATA.
           DISPLAY "---- NUEVO ESTUDIANTE ----".
           DISPLAY "Ingresar CLAVE estudiante: ".
           ACCEPT WS-CLAVE.
           DISPLAY "Ingresar NOMBRE estudiante: ".
           ACCEPT WS-NOMBRE.
           MOVE FUNCTION UPPER-CASE(WS-NOMBRE) TO WS-NOMBRE.
           DISPLAY "Ingresar GRADO estudiante: ".
           ACCEPT WS-GRADO.
           DISPLAY "Ingresar CARRERA estudiante: ".
           ACCEPT WS-CARRERA.
           EXIT.

       100202-GRADES-DATA.
           DISPLAY "Ingresar calificación MATERIA 1: ".
           ACCEPT WS-MAT-1.
           DISPLAY "Ingresar calificación MATERIA 2: ".
           ACCEPT WS-MAT-2.
           DISPLAY "Ingresar calificación MATERIA 3: ".
           ACCEPT WS-MAT-3.
           DISPLAY "Ingresar calificación MATERIA 4: ".
           ACCEPT WS-MAT-4.
           DISPLAY "Ingresar calificación MATERIA 5: ".
           ACCEPT WS-MAT-5.
           DISPLAY "Ingresar calificación MATERIA 6: ".
           ACCEPT WS-MAT-6.
           DISPLAY "Ingresar calificación MATERIA 7: ".
           ACCEPT WS-MAT-7.
           DISPLAY "Ingresar calificación MATERIA 8: ".
           ACCEPT WS-MAT-8.
           DISPLAY "Ingresar calificación MATERIA 9: ".
           ACCEPT WS-MAT-9.
           DISPLAY "Ingresar calificación MATERIA 10: ".
           ACCEPT WS-MAT-10.
           EXIT.

       100203-MOVE-DATA.
           INITIALIZE COL-GRADO.
           INITIALIZE POS.
           COMPUTE POS = (7 / 2).
           MOVE WS-GRADO TO COL-GRADO(POS:2).

           INITIALIZE COL-CARRERA.
           INITIALIZE POS.
           COMPUTE POS = (9 / 2).
           MOVE WS-CARRERA TO COL-CARRERA(POS:2).

           INITIALIZE COL-MAT1.
           MOVE WS-MAT-1 TO COL-MAT1(2:2).

           INITIALIZE COL-MAT2.
           MOVE WS-MAT-2 TO COL-MAT2(2:2).

           INITIALIZE COL-MAT3.
           MOVE WS-MAT-3 TO COL-MAT3(2:2).

           INITIALIZE COL-MAT4.
           MOVE WS-MAT-4 TO COL-MAT4(2:2).

           INITIALIZE COL-MAT5.
           MOVE WS-MAT-5 TO COL-MAT5(2:2).

           INITIALIZE COL-MAT6.
           MOVE WS-MAT-6 TO COL-MAT6(2:2).

           INITIALIZE COL-MAT7.
           MOVE WS-MAT-7 TO COL-MAT7(2:2).

           INITIALIZE COL-MAT8.
           MOVE WS-MAT-8 TO COL-MAT8(2:2).

           INITIALIZE COL-MAT9.
           MOVE WS-MAT-9 TO COL-MAT9(2:2).

           INITIALIZE COL-MAT10.
           MOVE WS-MAT-10 TO COL-MAT10(2:2).

           INITIALISE LINEA-TEMP.
           STRING
               WS-CLAVE DELIMITED BY SIZE
               "|"
               WS-NOMBRE DELIMITED BY SIZE
               "|"
               COL-GRADO DELIMITED BY SIZE
               "|"
               COL-CARRERA DELIMITED BY SIZE
               "|"
               COL-MAT1 DELIMITED BY SIZE
               "|"
               COL-MAT2 DELIMITED BY SIZE
               "|"
               COL-MAT3 DELIMITED BY SIZE
               "|"
               COL-MAT4 DELIMITED BY SIZE
               "|"
               COL-MAT5 DELIMITED BY SIZE
               "|"
               COL-MAT6 DELIMITED BY SIZE
               "|"
               COL-MAT7 DELIMITED BY SIZE
               "|"
               COL-MAT8 DELIMITED BY SIZE
               "|"
               COL-MAT9 DELIMITED BY SIZE
               "|"
               COL-MAT10 DELIMITED BY SIZE
               "|"
           INTO LINEA-TEMP.
           WRITE LINEA-TEMP.

       100204-NEW-DATA.
           DISPLAY "¿Ingresar nuevo estudiante?".
           DISPLAY "CONTINUAR ---- SI".
           DISPLAY "DETENER ---- NO".
           ACCEPT SWITCHES.
           MOVE FUNCTION UPPER-CASE(SWITCHES) TO SWITCHES.
           EXIT.

       END PROGRAM EJERCICIO-FILE-OPEN.
