      * Author: DIDIERO
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJERCICIO-FILE-OPEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ESTUDIANTES-ARCHIVO ASSIGN TO "estudiantes.txt"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD ESTUDIANTES-ARCHIVO.
               01 ESTUDIANTES-REGISTRO.
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
           77 WS-CLAVE PIC 9(08).
           77 WS-NOMBRE PIC X(40).
           77 WS-GRADO PIC X(02).
           77 WS-CARRERA PIC X(02).
           77 WS-MAT-1 PIC X(02).
           77 WS-MAT-2 PIC X(02).
           77 WS-MAT-3 PIC X(02).
           77 WS-MAT-4 PIC X(02).
           77 WS-MAT-5 PIC X(02).
           77 WS-MAT-6 PIC X(02).
           77 WS-MAT-7 PIC X(02).
           77 WS-MAT-8 PIC X(02).
           77 WS-MAT-9 PIC X(02).
           77 WS-MAT-10 PIC X(02).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           OPEN OUTPUT ESTUDIANTES-ARCHIVO.

           PERFORM GENERAL-PROCEDURE 3 TIMES.

           CLOSE ESTUDIANTES-ARCHIVO.

           STOP RUN.

       GENERAL-PROCEDURE.
           PERFORM INGRESAR-DATOS-PROCEDURE.
           PERFORM INGRESAR-MATERIAS-PROCEDURE.
           PERFORM MOVER-DATOS-PROCEDURE.

       INGRESAR-DATOS-PROCEDURE.
            DISPLAY "Ingresar CLAVE estudiante: "
            ACCEPT WS-CLAVE.

            DISPLAY "Ingresar NOMBRE estudiante: "
            ACCEPT WS-NOMBRE.

            DISPLAY "Ingresar GRADO estudiante: "
            ACCEPT WS-GRADO.

            DISPLAY "Ingresar CARRERA estudiante: "
            ACCEPT WS-CARRERA.


       INGRESAR-MATERIAS-PROCEDURE.
            DISPLAY "Ingresar calificación MATERIA 1: "
            ACCEPT WS-MAT-1.

            DISPLAY "Ingresar calificación MATERIA 2: "
            ACCEPT WS-MAT-2.

            DISPLAY "Ingresar calificación MATERIA 3: "
            ACCEPT WS-MAT-3.

            DISPLAY "Ingresar calificación MATERIA 4: "
            ACCEPT WS-MAT-4.

            DISPLAY "Ingresar calificación MATERIA 5: "
            ACCEPT WS-MAT-5.

            DISPLAY "Ingresar calificación MATERIA 6: "
            ACCEPT WS-MAT-6.

            DISPLAY "Ingresar calificación MATERIA 7: "
            ACCEPT WS-MAT-7.

            DISPLAY "Ingresar calificación MATERIA 8: "
            ACCEPT WS-MAT-8.

            DISPLAY "Ingresar calificación MATERIA 9: "
            ACCEPT WS-MAT-9.

            DISPLAY "Ingresar calificación MATERIA 10: "
            ACCEPT WS-MAT-10.

           MOVER-DATOS-PROCEDURE.
               INITIALISE ESTUDIANTES-REGISTRO.

               MOVE WS-CLAVE TO EST-REG-CLAVE.

               WRITE ESTUDIANTES-REGISTRO.

       END PROGRAM EJERCICIO-FILE-OPEN.
