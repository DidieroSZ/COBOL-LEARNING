      ******************************************************************
      * Author: DIDERO.
      * Purpose: MAKE A REPORT FILE NAMED "repote-est.txt".
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-FILE-OPEN.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT DOC-EST ASSIGN TO "estudiantes.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS WS-STATUS-ORI.

               SELECT REP-EST ASSIGN TO "repote-est.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS WS-STATUS-DES.

       DATA DIVISION.
           FILE SECTION.
           FD DOC-EST.
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

           FD REP-EST.
               01 LINEA-TEMP PIC X(132).

           WORKING-STORAGE SECTION.
           01 WS-VARIABLES.
               05 WS-STATUS-ORI PIC XX.
               05 WS-STATUS-DES PIC XX.
               05 DATE-SYSTEM PIC 9(08).

           01 ENCABEZADOS.
               05 LINEA-FECHA.
                   10 DATA-PCB PIC X(06) VALUE "PCB999".
                   10 FILLER PIC X(48) VALUE SPACES.
                   10 DATA-BECA PIC X(15) VALUE "BECA COBOL 2026".
                   10 FILLER PIC X(49) VALUE SPACES.
                   10 DATA-PAG PIC X(10) VALUE "PAGINA NO.".
                   10 DATA-NOPAG PIC 9(04) VALUE 9999.


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

           01 SWITCHES.
               05 WS-FIN PIC XX VALUE "N".
                   88 FIN-ARCHIVO VALUE "S".

       PROCEDURE DIVISION.

       PERFORM 100-INICIO.
       CLOSE DOC-EST.
       STOP RUN.

       100-INICIO.
           PERFORM 100100-OPEN-FILES.
           PERFORM 100200-GET-DATE.
           PERFORM 100300-HEADER-MAKER.
           PERFORM 100400-READ-FIELDS UNTIL FIN-ARCHIVO.
           PERFORM 100500-CLOSE-FILES.
           EXIT.


       100100-OPEN-FILES.
           OPEN INPUT DOC-EST.
           OPEN OUTPUT REP-EST.
      *    (00 = OK)  (35 = NOT FOUND)  (10 = END FILE)
           IF WS-STATUS-ORI NOT = "00"
           DISPLAY "ERROR AL ABRIR ARCHIVO ORIGEN"
           STOP RUN
           END-IF.
           EXIT.


       100200-GET-DATE.
           ACCEPT DATE-SYSTEM FROM DATE.
           EXIT.


       100300-HEADER-MAKER.
           PERFORM 100303-L-BLANCA-MAKER.

           MOVE SPACES TO LINEA-TEMP.
          COMPUTE LONG-WORD = FUNCTION LENGTH("REPORTE DE ESTUDIANTES").
          COMPUTE POS = (LARGE-FORMAT / 2) - (LONG-WORD / 2).
           MOVE "REPORTE DE ESTUDIANTES" TO LINEA-TEMP(POS:LONG-WORD).
           WRITE LINEA-TEMP.

           PERFORM 100303-L-BLANCA-MAKER.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-FECHA TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           PERFORM 100302-L-TABLA-MAKER.
           PERFORM 100303-L-BLANCA-MAKER.

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
           PERFORM 100302-L-TABLA-MAKER.
           EXIT.

       100301-L-PUNTEADA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-PUNTEADA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100302-L-TABLA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-TABLA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100303-L-BLANCA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE SPACES TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.


       100400-READ-FIELDS.
           READ DOC-EST
             AT END
               SET FIN-ARCHIVO TO TRUE
             NOT AT END
               PERFORM 100401-MOVE-DATA
           END-READ.
           EXIT.

       100401-MOVE-DATA.
           MOVE EST-REG-DOC TO WS-CAMPOS.
           PERFORM 100402-FORMAT-DATA.
           EXIT.
       100402-FORMAT-DATA.
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
           EXIT.


       100500-CLOSE-FILES.
           IF WS-STATUS-DES NOT = "10"
               DISPLAY "******************"
               DISPLAY "* ARCHIVO CREADO *"
               DISPLAY "******************"
           END-IF.
           CLOSE DOC-EST.
           CLOSE REP-EST.
           EXIT.

       END PROGRAM EJERCICIO-FILE-OPEN.
