      ******************************************************************
      * Author: DIDERO
      * Purpose: MAKE A REPORT FILE NAMED estudiantes.txt
      * DEPENDENCIES:
      *    Headers -> HeaderMaker.cbl
      *    Main -> ArchivoApertura.cbl
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-FILE-OPEN.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT ESTUDIANTES-ARCHIVO ASSIGN TO "estudiantes.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

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
            05 COL-CARRERA  PIC X(7).
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

       01 WS-STATUS PIC XX.
       01 WS-CLAVE PIC 9(08).
       01 WS-NOMBRE PIC X(40).
       01 WS-GRADO PIC X(02).
       01 WS-CARRERA PIC X(02).
       01 WS-MAT-1 PIC X(02).
       01 WS-MAT-2 PIC X(02).
       01 WS-MAT-3 PIC X(02).
       01 WS-MAT-4 PIC X(02).
       01 WS-MAT-5 PIC X(02).
       01 WS-MAT-6 PIC X(02).
       01 WS-MAT-7 PIC X(02).
       01 WS-MAT-8 PIC X(02).
       01 WS-MAT-9 PIC X(02).
       01 WS-MAT-10 PIC X(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT ESTUDIANTES-ARCHIVO.
           PERFORM HEADER-MAKER-PROCEDURE.
           PERFORM GENERAL-PROCEDURE 2 TIMES.
           CLOSE ESTUDIANTES-ARCHIVO.
           STOP RUN.

       HEADER-MAKER-PROCEDURE.
           MOVE LINEA-PUNTEADA TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           MOVE SPACES TO LINEA-TEMP.
          COMPUTE LONG-WORD = FUNCTION LENGTH("REPORTE DE ESTUDIANTES").
          COMPUTE POS = (LARGE-FORMAT / 2) - (LONG-WORD / 2).
           MOVE "REPORTE DE ESTUDIANTES" TO LINEA-TEMP(POS:LONG-WORD).
           WRITE LINEA-TEMP.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-PUNTEADA TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-TABLA TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           INITIALISE LINEA-TEMP.
           MOVE SPACES TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           COMPUTE LONG-WORD = FUNCTION LENGTH("NOMBRE ALUMNOS").
           COMPUTE POS = (40 / 2) - (LONG-WORD / 2).
           MOVE "NOMBRE ALUMNOS" TO PHRASE-NOMBRE(POS: LONG-WORD).
           INITIALISE LINEA-TEMP.
           STRING
               "CLAVE   " DELIMITED BY SIZE
               SPACE
               PHRASE-NOMBRE DELIMITED BY SIZE
               SPACE
               " GRADO "  DELIMITED BY SIZE
               SPACE
               "CARRERA" DELIMITED BY SIZE
               SPACE
               "MAT01" DELIMITED BY SIZE
               SPACE
               "MAT02" DELIMITED BY SIZE
               SPACE
               "MAT03" DELIMITED BY SIZE
               SPACE
               "MAT04" DELIMITED BY SIZE
               SPACE
               "MAT05" DELIMITED BY SIZE
               SPACE
               "MAT06" DELIMITED BY SIZE
               SPACE
               "MAT07" DELIMITED BY SIZE
               SPACE
               "MAT08" DELIMITED BY SIZE
               SPACE
               "MAT09" DELIMITED BY SIZE
               SPACE
               "MAT10" DELIMITED BY SIZE
           INTO LINEA-TEMP.
           WRITE LINEA-TEMP.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-TABLA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.

       GENERAL-PROCEDURE.
           PERFORM INGRESAR-DATOS-PROCEDURE.
           PERFORM INGRESAR-MATERIAS-PROCEDURE.
           PERFORM MOVER-DATOS-PROCEDURE.
           EXIT.

       INGRESAR-DATOS-PROCEDURE.
           DISPLAY "Ingresar CLAVE estudiante: ".
           ACCEPT WS-CLAVE.
           DISPLAY "Ingresar NOMBRE estudiante: ".
           ACCEPT WS-NOMBRE.
           DISPLAY "Ingresar GRADO estudiante: ".
           ACCEPT WS-GRADO.
           DISPLAY "Ingresar CARRERA estudiante: ".
           ACCEPT WS-CARRERA.
           EXIT.

       INGRESAR-MATERIAS-PROCEDURE.
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

       MOVER-DATOS-PROCEDURE.
           INITIALIZE COL-GRADO.
           INITIALIZE POS.
           COMPUTE POS = (7 / 2).
           MOVE WS-GRADO TO COL-GRADO(POS:2).

           INITIALIZE COL-CARRERA.
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
           INTO LINEA-TEMP.
           WRITE LINEA-TEMP.

       END PROGRAM EJERCICIO-FILE-OPEN.
