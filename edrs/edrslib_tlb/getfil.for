      subroutine getfil(fname,wavel,dens,maxpts,npts,ierr)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO READ A FILTER RESPONSE FILE IN STANDARD FORMAT. SEE THE
*       DOCUMENTATION FOR FILTSPEC FOR DEFINITION OF THE FILE
*       FORMAT
*
*METHOD
*       OPEN THE FILE AND READ THE RECORDS, OMITTING COMMENT LINES
*       AND CHECKING FOR ERRORS. CONVERT FILTER TRANSMISSION TO
*       DENSITY AND RETURN THE RESULTANT TABLE OF DENSITY AGAINST
*       WAVELENGTH.
*
*ARGUMENTS
*       FNAME (IN)
*       CHARACTER*(*)
*               THE NAME OF THE FILTER RESPONSE TEXT FILE
*       WAVEL (OUT)
*       REAL(MAXPTS)
*               RETURNS THE TABLE OF WAVELENGTHS
*       DENS (OUT)
*       REAL(MAXPTS)
*               RETURNS THE TABLE OF ASSOCIATED FILTER DENSITIES
*               (-LOG10(TRANSMISSION))
*       MAXPTS (IN)
*       INTEGER
*               THE SIZE OF ARRAY TO BE FILLED...DEFINES THE MAX.
*               NUMBER OF RECORDS ALLOWED IN THE FILE
*       NPTS (OUT)
*       INTEGER
*               THE NUMBER OF FILE RECORDS READ (AND RESULTS RETURNED
*               IN WAVEL AND DENS)
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG
*                       0: SUCCESS
*                       1: CANNOT OPEN FILE
*                       2: FORMAT ERROR IN FILE
*                       3: RECORDS NOT IN WAVELENGTH ORDER
*                       4: TRANSMISSION DATA NOT ALL POSITIVE
*                       5: LESS THAN 2 DATA RECORDS IN FILE
*                       6: MORE THAN MAXPTS RECORDS IN FILE
*
*NOTES
*       USES FORTRAN UNIT 1 FOR READING DATA FILE. CHANNEL IS CLOSED
*       ON EXIT
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character fname*(*),cbuf*1
      real wavel(maxpts),dens(maxpts)
 
*
* INITIALLISE ERROR FLAG, THEN TRY TO OPEN THE DATA FILE
*
      ierr=0
      open(1,readonly,file=fname,status='OLD',iostat=ioperr)
 
*
* ABORT IF THE FILE COULD NOT BE OPENED
*
 
      if(ioperr.ne.0)then
         ierr=1
         go to 99
 
      endif
 
 
*
* READ EACH RECORD AS A CHARACTER STRING TO SEE IF IT IS A COMMENT
* RECORD
*
      npts=0
4     read(1,'(A)',end=37)cbuf
 
*
* CHECK THE MAXIMUM NUMBER OF DATA POINTS IS NOT EXCEEDED
*
 
      if(npts.ge.maxpts)then
         ierr=6
         go to 99
 
      endif
 
 
      if(cbuf.ne.'!')then
 
*
* IF NOT A COMMENT, BACKSPACE AND READ THE RECORD AS A NUMBER PAIR
*
         backspace(1)
         read(1,*,err=98)wavel(npts+1),dens(npts+1)
         go to 97
 
 
*
* ABORT IF A FREE-FORMAT READ ERROR IS FOUND
*
98       ierr=2
         go to 99
 
97       continue
 
*
* COUNT THE NUMBER OF DATA POINTS
*
         npts=npts+1
 
*
* CHECK WAVELENGTHS ARE IN STRICTLY INCREASING ORDER
*
 
         if(npts.ge.2)then
 
            if(wavel(npts-1).ge.wavel(npts))then
               ierr=3
               go to 99
 
            endif
 
         endif
 
 
*
* IF THE FILTER TRANSMISSION IS NOT POSITIVE, ABORT
* OTHERWISE CONVERT TO DENSITY
*
 
         if(dens(npts).le.0.0d0)then
            ierr=4
            go to 99
 
 
         else
            dens(npts)=-log10(dens(npts))
         endif
 
      endif
 
 
*
* LOOP TO READ ALL RECORDS IN THE DATA FILE
*
      go to 4
 
 
*
* EXIT HERE WHEN ALL RECORDS READ OK
*
37    continue
 
*
* CHECK THERE ARE NOT TOO FEW DATA POINTS
*
 
      if(npts.lt.2)then
         ierr=5
         go to 99
 
      endif
 
99    close(1)
 
      end
 
 
 
