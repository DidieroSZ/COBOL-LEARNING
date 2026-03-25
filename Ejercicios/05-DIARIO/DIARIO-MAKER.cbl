      ******************************************************************
      * Author: DIDERO.
      * Date: 024/03/2026.
      * Purpose: MAKE A DIARY REPORT
      *          INFORMTION FROM FILE "estudiantes.txt" AND CALLING
      *          FILE NAMED DATA-MATERIAS-SEARCH.cbl
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIARIO-MAKER.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DIARIO ASSIGN TO "diario.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS WS-FSTA-DIARIO.


       DATA DIVISION.
           FILE SECTION.
           FD DIARIO.
               01 LINEA-W-DIARIO PIC X(132).
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 DATE-SYSTEM PIC XXXX.
           05 MENU-OP PIC 9 VALUE 0.

       01 WS-FILE-STATUS.
           05 WS-FSTA-DIARIO PIC XX.

       01 ENCABEZADOS.
           05 LINEAS-FORMATO.
               10 LINEA-AST PIC X(132) VALUE ALL "*".
               10 LINEA-BLANCA PIC X(132) VALUE SPACES.

           05 TITULOS.
               10 TITULO-01.
                   15 FILLER PIC X(44) VALUES SPACES.
                   15 DIDIECO PIC A(8) VALUES "DIDIERCO".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 INDUSTRIES PIC A(10) VALUES "INDUSTRIES".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 UNI PIC A(7) VALUES "UNIFIED".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 OPERA PIC A(9) VALUES "OPERATING".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 SYS PIC A(6) VALUES "SYSTEM".
                   15 FILLER PIC X(44) VALUE SPACE.
               10 TITULO-02.
                   15 FILLER PIC X(46) VALUES SPACES.
                   15 COPYRIGHT PIC A(9) VALUES "COPYRIGHT".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 DATE-COPY PIC X(13) VALUES "1990 ─ 2080".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 DIDIECO PIC A(8) VALUES "DIDIERCO".
                   15 FILLER PIC X(1) VALUE SPACE.
                   15 INDUSTRIES PIC A(10) VALUES "INDUSTRIES".
                   15 FILLER PIC X(43) VALUE SPACE.

           05 HEADER-FORMAT.
              10 L-TEXT-TITULO PIC A(22) VALUE "REPORTE DE ESTUDIANTES".
               10 FILLER PIC X(38) VALUE SPACES.
               10 L-TEXT-FECHA PIC X(7) VALUE "FECHA: ".
               10 L-DATA-FECHA PIC X(10).

       01 SWITCHES.
               05 WS-FIN PIC XX VALUE "N".
                   88 FIN-ARCHIVO VALUE "S".
               05 WS-FOUND PIC X VALUE "N".
                   88 MATERIA-FOUND VALUE "S".

       PROCEDURE DIVISION.

       PERFORM 100000-INICIO.

       100000-INICIO.
           PERFORM 100100-OPEN-FILES.
      *    DISPLAY FUNCTION LENGTH(TITULO-02).
           PERFORM 100200-MENU-OP.
           PERFORM 100300-HEADER-MAKER.
           PERFORM 100600-CLOSE-FILES.
       EXIT.
       
       100100-OPEN-FILES.
           OPEN OUTPUT DIARIO.
       EXIT.
       
       100200-MENU-OP.
           DISPLAY "************************************************".
           DISPLAY "* DIDIERCO INDUSTRIES UNIFIED OPERATING SYSTEM *".
           DISPLAY "*  COPYRIGHT 1990 ─ 2080 DIDIERCO INDUSTRIES   *".
           DISPLAY "*                                              *". 
           DISPLAY "* ============================================ *". 
           DISPLAY "*MENU DE OPCIONES:                             *". 
           DISPLAY "*|                =============================*". 
           DISPLAY "*│                                             *". 
           DISPLAY "*└-- [ 1 ] CREAR DIARIO.                       *".
           DISPLAY "*|-- [ 2 ] OBTENER REGISTRO.                   *".
           DISPLAY "*+------+                                      *".
           DISPLAY "*|      |-- [ 1 ] LISTAR TODOS LOS REGISTROS.  *".
           DISPLAY "*|      |-- [ 2 ] LISTAR REGISTRO POR ID.      *".
           DISPLAY "*|                                             *".
           DISPLAY "*+- [ 3 ] CREAR REGISTRO.                      *".
           DISPLAY "*|                                             *".
           DISPLAY "*+-------------------------------- [ 0 ] SALIR *".
           DISPLAY "************************************************".
           ACCEPT MENU-OP.
       EXIT.
       100300-HEADER-MAKER.
           PERFORM 100201-LI-MA-AST.
           PERFORM 100202-LI-MA-TITLE.
           PERFORM 100203-LI-MA-BLANCA.
       EXIT.

       100201-LI-MA-AST.
           INITIALIZE LINEA-W-DIARIO.
           MOVE LINEA-AST TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
       EXIT.
       100202-LI-MA-TITLE.
           INITIALIZE LINEA-W-DIARIO.
           MOVE TITULO-01 TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
           INITIALIZE LINEA-W-DIARIO.
           MOVE TITULO-02 TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
       EXIT.
       100203-LI-MA-BLANCA.
           INITIALIZE LINEA-W-DIARIO.
           MOVE LINEA-BLANCA TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
       EXIT.

       100600-CLOSE-FILES.
           CLOSE DIARIO.
       EXIT.


       STOP RUN.
       END PROGRAM DIARIO-MAKER.
           

