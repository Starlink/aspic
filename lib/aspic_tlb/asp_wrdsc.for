      SUBROUTINE ASP_WRDSC (PARAM,DESCR,OUTPUT,SIZE,STATUS)

*+  ASP_WRDSC
*
*   Precisely the same as Starlink routine WRDSCR, provided solely to
*   give a uniform naming convention of routines ASP_WRDSC, ASP_WRDSI
*   and ASP_WRDSR.
*
*   Called:
*     WRDSCR: Starlink
*
*   W F Lupton RGO 23 Oct 1981
*-

      INTEGER SIZE,STATUS
      CHARACTER PARAM*(*),DESCR*(*),OUTPUT(SIZE)*(*)

      CALL WRDSCR (PARAM,DESCR,OUTPUT,SIZE,STATUS)

      END
