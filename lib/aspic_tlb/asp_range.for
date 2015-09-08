      SUBROUTINE ASP_RANGE (PARAM,INPUT,SIZE,VMIN,VMAX,STATUS)

*+  ASP_RANGE
*
*   Obtain min and max of real BDF. If they have previously been
*   calculated, the values are retrieved from frame descriptors.
*   Otherwise, they are calculated and, if possible, the frame
*   descriptors are written.
*
*   Given:
*     PARAM   C    program parameter name corresponding to BDF
*     INPUT   RA   BDF data (arbitrary dimension)
*     SIZE    I    size of INPUT
*
*   Returned:
*     VMIN    R    array minimum value
*     VMAX    R    array maximum value
*     STATUS  I    return status (Starlink)
*
*   Called:
*     ASP_STAT: ASPIC
*
*   W.F.Lupton RGO 23 Oct 1981
*-

      INTEGER SIZE,STATUS
      REAL INPUT(SIZE),VMIN,VMAX,VTOT,VMEAN,VSIG
      CHARACTER PARAM*(*)

      CALL ASP_STAT (PARAM,INPUT,SIZE,VTOT,VMEAN,VMIN,VMAX,VSIG,STATUS)

      END
