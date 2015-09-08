      subroutine args_colr(col)
*+
*   ARGS_COLR
*
*   Read all of ARGS R,G,B colour table
*
*   Returned  (argument):
*	COL	IA	colour table 3*256 (1-3: R,G,B)
*
*   Called:
*	ARGS_RVT:  ROEARGS
*
*   J.A.Cooke/UOE/17Dec81
*-
      integer col(3,256),vtt(4,256),istat,i,j

      call args_rvt(vtt,istat)

      if (istat.eq.0) then

         do i=1,256
            do j=1,3
               col(j,i)=vtt(j,i)
            enddo
         enddo

      endif

      end
