      SUBROUTINE ASP_RDDSC (PARAM,DESCR,MAXSIZ,OUTPUT,SIZE,STATUS)

*+  ASP_RDDSC
*
*   Precisely the same as Starlink routine RDDSCR, provided solely to
*   give a uniform naming convention of routines ASP_RDDSC, ASP_RDDSI
*   and ASP_RDDSR.
*
*   Called:
*     RDDSCR: Starlink
*
*   W F Lupton RGO 23 Oct 1981
*-

      INTEGER MAXSIZ,SIZE,STATUS
      CHARACTER PARAM*(*),DESCR*(*),OUTPUT(MAXSIZ)*(*)

      CALL RDDSCR (PARAM,DESCR,MAXSIZ,OUTPUT,SIZE,STATUS)

      END
