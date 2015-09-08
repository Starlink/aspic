      SUBROUTINE ASP_RWSP

*+  If 'ASP_SWSP' has previously been called (WSL, BP <> -1) reset working-set
*   limit and base priority to values held in common block 'CASP_WSP'.

      INCLUDE 'ASPIC(CASPWSP)'

      INTEGER STATUS

      IF (WSL.NE.-1) THEN
          CALL ASP_SETWSL (WSL,STATUS)
      ENDIF
      IF (BP.NE.-1) THEN
          CALL ASP_SETPRI (BP,STATUS)
      ENDIF

      END
