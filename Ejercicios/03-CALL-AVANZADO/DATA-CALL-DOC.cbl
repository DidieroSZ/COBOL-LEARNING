      ******************************************************************
      * Author: DIDERO.
      * Date: 04/03/2026.
      * Purpose: MAKE A REPORT FILE NAMED "REP-EST-MAT.txt" BASED ON
      *          INFORMTION FROM FILE "estudiantes.txt" AND CALLING
      *          FILE NAMED DATA-MATERIAS-SEARCH.cbl
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-CALL-DOC.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT DOC-EST ASSIGN TO "estudiantes.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS WS-STATUS-ORI.

               SELECT REP-EST ASSIGN TO "REP-EST-MAT.txt"
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
               05 WS-COUNTER PIC 99.
               05 DATE-SYSTEM PIC X(6).
               05 DATE-FORMAT.
                 10 DIA PIC 9(02).
                 10 FILLER PIC X VALUE "/".
                 10 MES PIC 9(02).
                 10 FILLER PIC X VALUE "/".
                 10 ANIO PIC 9(04) VALUE 2000.

           01 ENCABEZADOS.
             05 LINEA-TITLE.
               10 FILLER PIC X(55) VALUE SPACES.
              10 L-TEXT-TITULO PIC A(22) VALUE "REPORTE DE ESTUDIANTES".
               10 FILLER PIC X(38) VALUE SPACES.
               10 L-TEXT-FECHA PIC X(7) VALUE "FECHA: ".
               10 L-DATA-FECHA PIC X(10).

             05 LINEA-PAGINA.
               10 L-TEXT-PCB PIC X(06) VALUE "PCB999".
               10 FILLER PIC X(52) VALUE SPACES.
               10 L-TEXT-BECA PIC X(15) VALUE "BECA COBOL 2026".
               10 FILLER PIC X(45) VALUE SPACES.
               10 L-TEXT-PAG PIC X(10) VALUE "PAGINA NO.".
               10 L-DATA-NOPAG PIC 9(04) VALUE 9999.

             05 LINEA-COUNTER.
               10 L-TEXT-COUNTER PIC X(17) VALUE "No. Estudiantes: ".
               10 L-DATA-COUNTER PIC 99 VALUE 00.


           01 FORMATO.
               05 LINEA-TERISCO PIC X(132) VALUE ALL "*".
               05 LINEA-PUNTEADA PIC X(132) VALUE ALL "-".
               05 LINEA-TABLA PIC X(132) VALUE ALL "_".
               05 LONG-WORD PIC 9(03).
               05 POS PIC 9(03).
               05 LARGE-FORMAT PIC 9(03) VALUE 132.
               05 PHRASE-NOMBRE PIC A(40) VALUE SPACES.
               05 TEMP5 PIC X(5).
               05 TEMP7 PIC X(7).

           01 COLUMNAS.
                05 COL-GRADO    PIC X(9).
                05 COL-CARRERA  PIC X(11).
                05 COL-MATERIA  PIC X(32).
                05 COL-PROFE    PIC X(32).

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

           01 WS-CALL.
               05 WS-MATERIA PIC X(32).
               05 WS-PROFE PIC X(32).

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
           PERFORM 100300-START-VARIABLES.
           PERFORM 100400-HEADER-MAKER.
           PERFORM 100500-READ-FIELDS UNTIL FIN-ARCHIVO.
           PERFORM 100600-CLOSE-FILES.
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
           MOVE DATE-SYSTEM(5:2) TO DATE-FORMAT(1:2).
           MOVE DATE-SYSTEM(3:2) TO DATE-FORMAT(4:2).
           MOVE DATE-SYSTEM(1:2) TO DATE-FORMAT(9:2).
           DISPLAY DATE-FORMAT.
           MOVE DATE-FORMAT TO L-DATA-FECHA.
           EXIT.


       100300-START-VARIABLES.
           MOVE 00 TO WS-COUNTER.
           MOVE 0000 TO L-DATA-NOPAG.
           EXIT.


       100400-HEADER-MAKER.
           ADD 1 TO L-DATA-NOPAG.
           PERFORM 100405-L-ASTERICO-MAKER.
           PERFORM 100403-L-BLANCA-MAKER.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-TITLE TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           PERFORM 100403-L-BLANCA-MAKER.
           PERFORM 100401-L-PUNTEADA-MAKER.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-PAGINA TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           PERFORM 100402-L-TABLA-MAKER.
           PERFORM 100403-L-BLANCA-MAKER.

           COMPUTE LONG-WORD = FUNCTION LENGTH("NOMBRE ALUMNOS").
           COMPUTE POS = (40 / 2) - (LONG-WORD / 2).
           MOVE "NOMBRE ALUMNOS" TO PHRASE-NOMBRE(POS: LONG-WORD).
           INITIALISE LINEA-TEMP.

           STRING
               " CLAVE  " DELIMITED BY SIZE
               "|"
               PHRASE-NOMBRE DELIMITED BY SIZE
               "|"
               "  GRADO  "  DELIMITED BY SIZE
               "|"
               "  CARRERA  " DELIMITED BY SIZE
               "|"
               "MATERIA" DELIMITED BY SIZE
               "|"
               "PROFESOR" DELIMITED BY SIZE
               "|"
           INTO LINEA-TEMP.
           WRITE LINEA-TEMP.
           PERFORM 100402-L-TABLA-MAKER.
           EXIT.

       100401-L-PUNTEADA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-PUNTEADA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100402-L-TABLA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-TABLA TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100403-L-BLANCA-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE SPACES TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.
       100405-L-ASTERICO-MAKER.
           INITIALISE LINEA-TEMP.
           MOVE LINEA-TERISCO TO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.


       100500-READ-FIELDS.
           READ DOC-EST
             AT END
               PERFORM 100503-COUNTER-DISPLAY
               PERFORM 100405-L-ASTERICO-MAKER
               SET FIN-ARCHIVO TO TRUE
             NOT AT END
               PERFORM 100501-MOVE-DATA
           END-READ.

           EXIT.

       100501-MOVE-DATA.
           INITIALIZE WS-CAMPOS.
           MOVE EST-REG-DOC TO WS-CAMPOS.
           ADD 1 TO WS-COUNTER.
           DISPLAY WS-MAT-1.
           MOVE SPACES TO WS-MATERIA.
           MOVE SPACES TO WS-PROFE.

           CALL "DATA-MATERIAS-SEARCH" USING
           WS-MAT-1 WS-MATERIA WS-PROFE.

           PERFORM 100502-FORMAT-DATA.
           EXIT.
       100502-FORMAT-DATA.
           INITIALIZE COL-GRADO.
           INITIALIZE POS.
           COMPUTE POS = (9 / 2).
           MOVE WS-GRADO TO COL-GRADO(POS:2).

           INITIALIZE COL-CARRERA.
           INITIALIZE POS.
           COMPUTE POS = (11 / 2).
           MOVE WS-CARRERA TO COL-CARRERA(POS:2).

           INITIALIZE COL-MATERIA.
           MOVE WS-MATERIA TO COL-MATERIA.

           INITIALIZE COL-PROFE.
           MOVE WS-PROFE TO COL-PROFE.

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
               COL-MATERIA DELIMITED BY SIZE
               "|"
               COL-PROFE DELIMITED BY SIZE
               "|"
           INTO LINEA-TEMP.
           WRITE LINEA-TEMP.
           EXIT.

       100503-COUNTER-DISPLAY.
           PERFORM 100403-L-BLANCA-MAKER.

           MOVE WS-COUNTER TO L-DATA-COUNTER.

           INITIALISE LINEA-TEMP.
           MOVE LINEA-COUNTER TO LINEA-TEMP.
           WRITE LINEA-TEMP.

           EXIT.


       100600-CLOSE-FILES.
           IF WS-STATUS-DES NOT = "10"
               DISPLAY "******************"
               DISPLAY "* ARCHIVO CREADO *"
               DISPLAY "******************"
           END-IF.
           CLOSE DOC-EST.
           CLOSE REP-EST.

           EXIT.

       END PROGRAM DATA-CALL-DOC.
