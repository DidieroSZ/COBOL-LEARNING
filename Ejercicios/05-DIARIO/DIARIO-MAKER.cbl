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
               SELECT DIARIO ASSIGN TO "DIARY-FILE.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS WS-FSTA-DIARIO.


       DATA DIVISION.
           FILE SECTION.
           FD DIARIO.
               01 LINEA-W-DIARIO PIC X(132).
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
           05 MENU-OP PIC 9 VALUE 0.
           05 DIARY-NAME PIC X(20) VALUE SPACES.
       01 WS-DATE-V.
           05 DATE-SYSTEM PIC X(6).
           05 WS-DAY PIC X(15) VALUE "LUNES".
           05 WS-DAY-NAME PIC 99 VALUE ZEROS. 
           05 WS-MONTH PIC X(15) VALUE "ENERO". 
           05 WS-YEAR PIC 9(4) VALUE 2026. 
           05 WS-HORA PIC X(8) VALUE "00:00:00".
           05 WS-TIME PIC XX VALUE "AM".


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

           05 DISPLAY-FORAMT.
               10 L-AST PIC X(48) VALUE ALL '*'.
               10 L-BLA.
                   10 FILLER PIC X VALUE '*'.
                   10 FILLER PIC X VALUE SPACE.
                   10 L-BLA-TEXTO PIC X(44) VALUE SPACES.
                   10 FILLER PIC X VALUE SPACE.
                   10 FILLER PIC X VALUE '*'.


       01 SWITCHES.
               05 WS-FIN PIC XX VALUE "N".
                   88 FIN-ARCHIVO VALUE "S".
               05 WS-FOUND PIC X VALUE "N".
                   88 MATERIA-FOUND VALUE "S".

       PROCEDURE DIVISION.

       PERFORM 100000-INICIO.

       100000-INICIO.
      *     PERFORM 100100-OPEN-FILES.
      *    DISPLAY FUNCTION LENGTH(TITULO-02).
           PERFORM 100200-MENU-OP.
           PERFORM 100300-HAPPY-PAD.
           PERFORM 100400-HEADER-MAKER.
           PERFORM 100600-CLOSE-FILES.
       EXIT.
       
       100100-OPEN-FILES.
           OPEN OUTPUT DIARIO.
       EXIT.
       
       100200-MENU-OP.
           DISPLAY X"1B" "[1;32m" WITH NO ADVANCING.
           DISPLAY L-AST.
           DISPLAY "* DIDIERCO INDUSTRIES UNIFIED OPERATING SYSTEM *".
           DISPLAY "*  COPYRIGHT 1990 ─ 2080 DIDIERCO INDUSTRIES   *".
           DISPLAY "*                                              *". 
           DISPLAY "* ============================================ *". 
           DISPLAY "* MENU DE OPCIONES:                            *". 
           DISPLAY "*                   ========================== *". 
           DISPLAY "* +--- [ 1 ] CREAR DIARIO.                     *".
           DISPLAY "* +--+ [ 2 ] OBTENER REGISTRO.                 *".
           DISPLAY "* |  +--- [ 1 ] LISTAR TODOS LOS REGISTROS.    *".
           DISPLAY "* |  +--- [ 2 ] LISTAR REGISTRO POR ID.        *".
           DISPLAY "* |                                            *".
           DISPLAY "* +--- [ 3 ] CREAR REGISTRO.                   *".
           DISPLAY "* |                                            *".
           DISPLAY "* +------------------------------- [ 0 ] SALIR *".
           DISPLAY "*                                              *".
           DISPLAY "************************************************".
           DISPLAY "OPCION: " WITH NO ADVANCING.
           ACCEPT MENU-OP.
       EXIT.
       100300-HAPPY-PAD.
           EVALUATE MENU-OP
               WHEN 0
                   PERFORM 100600-CLOSE-FILES

               WHEN 1
                   PERFORM 100301-CREATION-DIARY-PROCCES
           
               WHEN OTHER
                  PERFORM 100600-CLOSE-FILES
           END-EVALUATE.
       EXIT.
       100301-CREATION-DIARY-PROCCES.
           PERFORM 301-100-VERIFICATION-DIARY.
      *     DISPLAY WS-FSTA-DIARIO.
           DISPLAY "INGRESA EL NOMBRE DEL DIARIO: " WITH NO ADVANCING.
           ACCEPT DIARY-NAME.
           PERFORM 301-100-DATE-GET.
       EXIT.
       301-100-VERIFICATION-DIARY.
           OPEN EXTEND DIARIO.
           IF WS-FSTA-DIARIO IS EQUAL 00 THEN
             DISPLAY X"1B" "[1;30;41m"
             DISPLAY L-AST
             DISPLAY "*              ¡ADVETENCIA!                    *"
             DISPLAY "* YA EXISTE UN DIARIO EN LOS REGISTROS         *"
             DISPLAY "* CREAR UNO NUEVO SUPONE PERDIDA DE            *"
             DISPLAY "*        DATOS DEFINITIVA.                     *"
             DISPLAY "*            ¿DESEAS CONTINUAR?                *"
             DISPLAY "* +--- [ 1 ] SI.                               *"
             DISPLAY "* +--- [ 2 ] NO.                               *"
             DISPLAY "* +------------------------------- [ 0 ] SALIR *"
             DISPLAY "OPCION: " WITH NO ADVANCING
             ACCEPT MENU-OP
           END-IF.
           IF WS-FSTA-DIARIO IS EQUAL 35 THEN
               OPEN OUTPUT DIARIO
           END-IF.
           DISPLAY WS-FSTA-DIARIO.
       EXIT.
       301-100-DATE-GET.
           ACCEPT DATE-SYSTEM FROM DATE.
       EXIT.
       100400-HEADER-MAKER.
           PERFORM 100401-LI-MA-AST.
           PERFORM 100402-LI-MA-TITLE.
           PERFORM 100403-LI-MA-BLANCA.
       EXIT.

       100401-LI-MA-AST.
           INITIALIZE LINEA-W-DIARIO.
           MOVE LINEA-AST TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
       EXIT.
       100402-LI-MA-TITLE.
           INITIALIZE LINEA-W-DIARIO.
           MOVE TITULO-01 TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
           INITIALIZE LINEA-W-DIARIO.
           MOVE TITULO-02 TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
       EXIT.
       100403-LI-MA-BLANCA.
           INITIALIZE LINEA-W-DIARIO.
           MOVE LINEA-BLANCA TO LINEA-W-DIARIO.
           WRITE LINEA-W-DIARIO.
       EXIT.

       100600-CLOSE-FILES.
           DISPLAY X"1B" "[0m".
           CLOSE DIARIO.
           STOP RUN.
       EXIT.

       END PROGRAM DIARIO-MAKER.
           