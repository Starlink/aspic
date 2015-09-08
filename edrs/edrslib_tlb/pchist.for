      subroutine pchist(ia,npix,nlines,inval,pc,ipc,npc,ihist,minint
     : ,maxint,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE INTEGER VALUE IN AN IMAGE CORRESPONDING TO A
*       SPECIFIC FRACTION OF THE IMAGE HISTOGRAM FROM 0.0 TO 1.0
*
*METHOD
*       FORM A HISTOGRAM OF THE IMAGE INTEGERS AND SCAN UP OR DOWN TO
*       FIND THE APPROPRIATE POINT
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX*NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       PC (IN)
*       REAL(NPC)
*               ARRAY OF FRACTIONAL POSITIONS IN THE HISTOGRAM IN THE
*               RANGE 0.0 TO 1.0
*       IPC (OUT)
*       INTEGER(NPC)
*               ARRAY OF INTEGER RESULTS CORRESPONDING TO THE VALUES IN
*               THE ARRAY PC
*       NPC (IN)
*       INTEGER
*               DIMENSION OF PC, IPC
*       IHIST (WORKSPACE)
*       INTEGER(MININT:MAXINT)
*               USED TO HOLD THE IMAGE HISTOGRAM. CONTAINS HISTOGRAM
*               ON EXIT
*       MININT,MAXINT (IN)
*       INTEGER
*               MIN AND MAX INTEGER VALUES POSSIBLE IN IA
*
*
*CALLS
*       NONE
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix*nlines)
      integer ipc(npc),ihist(minint:maxint)
      real pc(npc)
 
*
* INITIALLISE HISTOGRAM
*
      ierr=0
      nn=npix*nlines
 
      do 1 i=minint,maxint
         ihist(i)=0
1     continue
 
 
*
* FORM A HISTOGRAM OF ALL THE VALID PIXELS
*
      npts=0
 
      do 2 i=1,nn
         intgr=ia(i)
 
         if(intgr.ne.inval) then
            ihist(intgr)=ihist(intgr)+1
            npts=npts+1
         endif
 
2     continue
 
 
*
* IF THERE ARE NO VALID PIXELS, EXIT WITH ERROR FLAG SET
*
 
      if(npts.eq.0) then
         ierr=1
         go to 99
 
      endif
 
 
*
* CONSIDER EACH PERCENTAGE HISTOGRAM POINT
*
 
      do 4 i=1,npc
 
*
* CALCULATE THE NUMBER OF DATA POINTS CORRESPONDING TO THIS POINT
* COUNTING UP OR DOWN DEPENDING ON WHICH SIDE OF THE MEDIAN
*
 
         if(pc(i).le.0.5) then
            limit=nint(npts*pc(i))
            istart=minint
            iend=maxint
            idirn=1
 
         else
            limit=nint(npts*(1.0-pc(i)))
            istart=maxint
            iend=minint
            idirn=-1
         endif
 
 
         if(limit.eq.0) limit=1
 
*
* COUNT THROUGH HISTOGRAM TO FIND THIS POINT
*
         n=0
 
         do 3 j=istart,iend,idirn
            n=n+ihist(j)
 
            if(n.ge.limit) go to 8
3        continue
 
8        ipc(i)=j
4     continue
 
99    return
 
      end
 
 
 
