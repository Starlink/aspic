      subroutine trantbdf(ia,npix,nlines,aquire,ra,bzero,bscale)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* PURPOSE/METHOD
*
*         THIS ROUTINE AQUIRES REAL VALUES OF AN IMAGE FROM THE USER
*        ENVIROMENT WHICH ARE REWRITTEN IN FORM VALUE=INTEGER*BSCALE+BZERO
*        INVALID PIXELS ARE SET TO -32767.NEW INTEGER DATA PASSED BACK TO
*        BDFKEE AND WRITTEN INTO BDF FILE.
*
* ARGUMENTS
*          IA : OUTPUT ARRAY : CONTAINS IMAGE VALUES : INTEGER :
*
*          NPIX : INPUT : NO PIXELS IN OP IMAGE : INTEGER :
*          NLINES : INPUT : NO LINES IN OP IMAGE : INTEGER :
*          AQUIRE : RUNTIME ARRAY : USED TO STORE SINGLE LINES OF IMAGE
*                                   OBTAINED FROM USER :REAL :
*          RA : RUNTIME ARRAY : CONTAINS 2D INPUT IMAGE FROM USER :REAL :
*          BZERO/BSCALE : OUTPUT : SCALING DESCRIPTERS FOR EDRS IMAGE : REAL :
* CALLS
*          STARLINK : WRUSER,WRKEYR,RDKEYR,RDKEYC,CNPAR
* NOTES
*          RUNTIME ARRAY REFERS TO EMPTY ARRAYS ON INPUT WHERE THE SPACE HAS
*          BEEN AQUIRED AT A HIGHER LEVEL USING LIB%GET.THE ARRAY IS USED FOR
*          STORAGE DURING THE RUNNING OF THE PROGRAM
* WRITTEN BY
*          J.V.SHIRT
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*
      character*1 out*15,ans(1)
      integer*2 ia(npix,nlines)
      real ra(npix,nlines),aquire(npix),valu(1)
 
*
* AQUIRE INPUT VALUES AND PUT IN ARRAY RA.CHECK THE LINE IS CORRECT AND
* IF NOT GET CORRECT VALUES
*
      call wruser('TYPE IN VALUES AS REQUESTED AND SET INVALID',ierr)
      call wruser('PIXELS TO 999',ierr)
 
*
* ENTER MAIN LOOP
*
 
      do j=1,nlines
 
*
* INITIALIZE ARRAY
*
 
         do  k=1,npix
            aquire(k)=999
         end do
 
 
*
* OBTAIN INPUT VALUES
*
         write(out,'(''NLINES='',I5)')j
         call wruser(out,ierr)
         call rdkeyr('ARRAY',.false.,npix,aquire,aval,istat)
 
*
 
         do l=1,npix
            ra(l,j)=aquire(l)
         end do
 
 
*
* ENTER MAIN CHECKING LOOP
*
100      call wruser('IS THIS LINE CORRECT (Y/N) ?',ierr)
         ans(1)='Y'
         call rdkeyc('ANSWER',.true.,1,ans,nval,istat)
 
*
 
         if(ans(1).eq.'Y')goto 200
 
*
 
         if(ans(1).eq.'N')then
 
*
            call wruser('WHICH PIXEL POSN IS INCORRECT ?',ierr)
            npi=1
            call getpar('NPI','INTEGER',1,1.,real(npix),.true.,npi,rval
     :       ,istat)
            call wruser('WHAT IS THE CORRECT VALUE',ierr)
            valu(1)=999.0
            call rdkeyr('VALUE',.true.,1,valu,nval,istat)
            ra(npi,j)=valu(1)
 
*
* CLEAR THE PARAMETER 'ANSWER','VALUE' AND 'NPI'
*
            call cnpar('ANSWER',iostat)
            call cnpar('VALUE',iostat)
            call cnpar('NPI',iostat)
         endif
 
 
*
* CHECK THERE ARE NO MORE MISTAKES
*
         goto 100
 
 
*
* IF THERE ARE NO MISTAKES CLEAR ALL PARAMETERS AND READ IN NEXT LINE
*
200      call cnpar('ANSWER',iostat)
         call cnpar('VALUE',iostat)
         call cnpar('NPI',iostat)
         call cnpar('ARRAY',iostat)
      end do
 
 
*
* FIND MIN/MAX VALUES OF INPUT IMAGE
*
      rmax=-1.0e20
      rmin=1.0e20
 
      do  i=1,nlines
 
         do  j=1,npix
 
            if(abs(ra(j,i)-999).lt.1.0e-5)goto 600
 
            if(rmax.lt.ra(j,i))rmax=ra(j,i)
 
            if(rmin.gt.ra(j,i))rmin=ra(j,i)
600      end do
 
      end do
 
 
*
* CALCULATE BZERO AND BSCALE SUCH THAT MAX/MIN VALUES
* BECOME +/-32700
*
      bzero=(rmax+rmin)/2
      bscale=(rmax-bzero)/32700
 
*
* CHANGE INPUT VALUES TO INTEGER*2 VALUES WITH APPROPRIATE BZERO
* AND BSCALE,AND SET INVALID PIXELS TO -32767.
*
 
      do i=1,nlines
 
         do j=1,npix
 
            if(abs(ra(j,i)-999).lt.1.0e-5)then
               ia(j,i)=-32767
 
            else
               ia(j,i)=nint((ra(j,i)-bzero)/bscale)
            end if
 
         end do
 
      end do
 
 
*
 
      end
 
 
 
