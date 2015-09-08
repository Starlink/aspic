      SUBROUTINE ASP_SWSP (WS,P)

*+  Set working-set limit to 'WS' and base priority to 'P' (current
*   process. If either operation is successful, declare exit handler
*   so that in the event of breakin they can be set back again using
*   'ASP_RWSP'. If the relevant quota is too small (WSQUOTA) or the
*   relevant privilege is not possessed (ALTPRI) then no error will
*   result, but the working-set / priority will be set to the high-
*   est permissible values.
*
*   WFL Oct 1981

      INCLUDE 'ASPIC(CASPWSP)'

      INTEGER WS,P,WSSTAT,PSTAT
      EXTERNAL ASP_RWSP

*   If this is the first time through (WSL, BP = -1) then use 'ASP_GETWSL'
*   and 'ASP_GETPRI' to obtain current working-set size and base
*   priority
      IF (WSL.EQ.-1) THEN
          CALL ASP_GETWSL (WSL,WSSTAT)
      ENDIF
      IF (BP.EQ.-1) THEN
          CALL ASP_GETPRI (BP,PSTAT)
      ENDIF

*   If successful, use 'ASP_SETWSL' and/or 'ASP_SETPRI' to set the new
*   values
      IF (WSSTAT) THEN
          CALL ASP_SETWSL (WS,WSSTAT)
      ENDIF
      IF (PSTAT) THEN
          CALL ASP_SETPRI (P,PSTAT)
      ENDIF

*   If either was successful, declare exit handler.
      IF (WSSTAT.OR.PSTAT) THEN
          CALL ASP_DCLEXH (ASP_RWSP)
      ENDIF

      END
