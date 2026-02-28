      ******************************************************************
      * Author: DIDERO
      * Purpose: MAKE A HEADER TO THE FILE estudiantes.txt
      * DEPENDENCIES:
      *    Headers -> HeaderMaker.cbl
      *    Main -> ArchivoApertura.cbl
      * Tectonics: cobc
      ******************************************************************
       01 FORMATO.
           05 LINEA-PUNTEADA PIC X(132) VALUE ALL "-  ".
           05 LINEA-TABLA PIC X(132) VALUE ALL "_".
           05 LONG-WORD PIC 9(03).
           05 POS PIC 9(03).
           05 LARGE-FORMAT PIC 9(03) VALUE 132.
