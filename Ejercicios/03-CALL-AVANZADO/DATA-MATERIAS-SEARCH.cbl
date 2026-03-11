******************************************************************
      * Author: DIDIER.
      * Date: 05/03/2026.
      * Purpose: SEARH DATA FROM MATERIAS ON FILE NAMED "materias.txt".
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATA-MATERIAS-SEARCH.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT MAT-DOC ASSIGN TO "materias.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD MAT-DOC.
               01 MAT-REG.
                   05 MAT-CLAVE PIC 99.
                   05 MAT-NOMBRE PIC A(20).
                   05 MAT-PROFE PIC A(20).

       WORKING-STORAGE SECTION.
           01 SWITCHES.
               05 WS-FIN PIC XX VALUE "N".
                   88 FIN-ARCHIVO VALUE "S".
       LINKAGE SECTION.
           01 LK-CLAVE PIC XX.
           01 LK-NOMBRE PIC A(20).
           01 LK-PROFE PIC A(20).

       PROCEDURE DIVISION USING LK-CLAVE LK-NOMBRE LK-PROFE.

       PERFORM MAIN-PROCEDURE.
       MAIN-PROCEDURE.
            OPEN INPUT MAT-DOC.

            INITIALIZE MAT-CLAVE.
            INITIALIZE MAT-NOMBRE.
            INITIALIZE MAT-PROFE.
            
            MOVE "N" TO WS-FIN.

            PERFORM UNTIL FIN-ARCHIVO
               READ MAT-DOC
                   AT END
                       SET FIN-ARCHIVO TO TRUE
                   NOT AT END
                       IF MAT-CLAVE = LK-CLAVE
                           MOVE MAT-NOMBRE TO LK-NOMBRE
                           MOVE MAT-PROFE TO LK-PROFE
                           SET FIN-ARCHIVO TO TRUE
                        END-IF
               END-READ
            END-PERFORM.
       CLOSE MAT-DOC.
       GOBACK.
      *END PROGRAM SEARCH-MATERIAS-DATA.
