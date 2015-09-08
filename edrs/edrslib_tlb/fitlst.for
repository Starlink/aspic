      subroutine fitlst(lista,nitema,llena,listb,nitemb,llenb,maxit
     : ,gamma,ifit,ilevel,c,ida,xa,ya,idb,xb,yb,valid,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CONTROL THE FITTING OF A LINEAR TRANSFORMATION BETWEEN
*       TWO SETS OF X,Y POSITIONS AND TO DISPLAY THE RESULTS
*
*METHOD
*       EXTRACT THE X,Y POSITIONS AND IDENTIFIERS FROM THE INPUT LISTS
*       AND MATCH THEM TOGETHER USING XYMTCH. CALL XYFITR TO PERFORM
*       THE FITTING, THEN DISPLAY A TABLE OF RESULTS.
*
*ARGUMENTS
*       LISTA (IN)
*       INTEGER(NITEMA,LLENA)
*               THE FIRST INPUT LIST OF X,Y POSITIONS
*       NITEMA,LLENA (IN)
*       INTEGER
*               THE DIMENSIONS OF LISTA
*       LISTB (IN)
*       INTEGER(NITEMB,LLENB)
*               THE SECOND INPUT LIST OF X,Y POSITIONS
*       NITEMB,LLENB (IN)
*       INTEGER
*               THE DIMENSIONS OF LISTB
*       MAXIT (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF REJECTION ITERATIONS TO BE
*               PERFORMED BY THE FITTING ROUTINE XYFITR
*       GAMMA (IN)
*       REAL
*               THE NUMBER OF STANDARD DEVIATIONS AT WHICH ABERRANT
*               POINTS ARE REJECTED IN XYFITR
*       IFIT (IN)
*       INTEGER
*               AN INTEGER DEFINING THE TYPE OF LINEAR TRANSFORMATION
*               CALCULATED BY XYFITR
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL, CONTROLLING THE PRINTING OF RESULTS
*       C (OUT)
*       REAL(6)
*               RETURNS THE 6 TRANSFORMATION COEFFICIENTS
*       IDA (WORKSPACE)
*       BYTE(20,LLENA)
*               STORAGE FOR THE IDENTIFIERS
*       XA,YA (WORKSPACE)
*       REAL(LLENA)
*               STORAGE FOR X,Y POSITIONS
*       IDB (WORKSPACE)
*       BYTE(20,LLENB)
*               STORAGE FOR IDENTIFIERS
*       XB,YB (WORKSPACE)
*       REAL(LLENB)
*               STORAGE FOR X,Y POSITIONS
*       VALID (WORKSPACE)
*       LOGICAL(*)
*               USED TO HOLD FLAGS INDICATING WHICH POINTS WERE REJECTED
*               DURING FITTING. SHOULD BE AT LEAST MIN(LLENA,LLENB)
*               ELEMENTS LONG
*       IERR (OUT)
*       INTEGER
*               ERROR STATUS RETURN... ZERO INDICATES SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               EXTLST,XYMTCH,XYFITR
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer lista(nitema,llena),listb(nitemb,llenb)
      real xa(llena),ya(llena),xb(llenb),yb(llenb),c(6)
      logical valid(*)
      byte ida(20,llena),idb(20,llenb)
      character prbuf*80,fitmsg(4)*80
 
*
* SET UP INFORMATION MESSAGES TO INDICATE THE FIT USED
*
      data fitmsg(1)/'   TRANSFORMATION IS A SHIFT OF ORIGIN ONLY'/
     : ,fitmsg(2)/'   TRANSFORMATION IS A SHIFT AND ROTATION'/,
     : fitmsg(3)/
     : '   TRANSFORMATION IS A SHIFT,ROTATION AND MAGNIFICATION'/,
     : fitmsg(4)/'   TRANSFORMATION IS A FULL 6 PARAMETER FIT'/
 
*
* EXTRACT IDENTIFIERS AND X,Y POSITIONS FROM THE INPUT LISTS
*
      ierr=0
      call extlst(lista,nitema,llena,ida,1,20)
      call extlst(lista,nitema,llena,xa,21,24)
      call extlst(lista,nitema,llena,ya,25,28)
      call extlst(listb,nitemb,llenb,idb,1,20)
      call extlst(listb,nitemb,llenb,xb,21,24)
      call extlst(listb,nitemb,llenb,yb,25,28)
 
*
* CALL XYMTCH TO MATCH THE ID'S IN EACH LIST AND PUT THE MATCHED
* POSITIONS AT THE START OF THE ID,X,Y ARRAYS
*
      call xymtch(xa,ya,ida,llena,xb,yb,idb,llenb,nmtch,istat)
 
*
* IF NO MATCHES WERE FOUND, RETURN WITH IERR=1
*
 
      if(nmtch.le.0) then
         ierr=1
         go to 99
 
      endif
 
 
*
* SET ALL MATCHED POSITIONS AS VALID, THEN CALL XYFITR TO PERFORM
* THE FITTING
*
 
      do 1 i=1,nmtch
         valid(i)=.true.
1     continue
 
      call xyfitr(xa,ya,xb,yb,valid,nmtch,maxit,gamma,ifit,c,ierr)
 
*
* PRINT NUMBER OF MATCHED POSITIONS
*
 
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         write(prbuf,10)nmtch
10       format(3x,i8,' POSITION(S) MATCHED BETWEEN INPUT LISTS')
         call lbgone(prbuf(4:))
         call wruser(prbuf,istat)
      endif
 
 
*
* IF TABLE OF RESULTS IS NEEDED, PRINT HEADINGS
*
 
      if(ilevel.ge.3) then
         call wruser(' ',istat)
         write(prbuf,11)
11       format(3x,'TRANSFORMED A POSITION',12x,'B POSITION')
         call wruser(prbuf,istat)
         write(prbuf,12)
12       format(3x,'----------------------',12x,'----------')
         call wruser(prbuf,istat)
         write(prbuf,13)
13       format(2x,2(' X COORD.      Y COORD.     '),3x,
     :   'ALIGNMENT ERROR')
         call wruser(prbuf,istat)
         write(prbuf,14)
14       format(2x,2(' --------      --------     '),3x,
     :   '---------------')
         call wruser(prbuf,istat)
      endif
 
 
*
* CALCULATE RMS ERROR AND COUNT NUMBER OF POSITIONS REJECTED
*
 
      if(ilevel.ge.2) then
         nrej=0
         sigma=0.0
 
         do 66 i=1,nmtch
 
*
* CALCULATE TRANSFORMED 'A' POSITIONS AND ERROR
*
            xd=c(1)+c(2)*xa(i)+c(3)*ya(i)
            yd=c(4)+c(5)*xa(i)+c(6)*ya(i)
            errsq=(xd-xb(i))**2+(yd-yb(i))**2
            err=sqrt(errsq)
 
            if(valid(i)) then
 
*
* INCREASE SUM OF ERRORS AND PRINT RESULT IF REQUIRED
*
               sigma=sigma+errsq
 
               if(ilevel.ge.3) write(prbuf,15)xd,yd,xb(i),yb(i),err,
     :         'OK'
 
            else
 
*
* COUNT ONE REJECTED POINT AND PRINT RESULT IF REQUIRED
*
               nrej=nrej+1
 
               if(ilevel.ge.3) write(prbuf,15)xd,yd,xb(i),yb(i),err,
     :         'REJ'
            endif
 
15          format(1x,4(1x,ss,g13.6),4x,ss,g12.4,2x,a3)
 
            if(ilevel.ge.3) call wruser(prbuf,istat)
66       continue
 
 
*
* CALCULATE RMS ERROR AND PRINT NO. OF REJECTED POINTS AND RMS ERROR
*
 
         if(nrej.lt.nmtch) then
            sigma=sqrt(sigma/(nmtch-nrej))
 
         else
            sigma=0.0
         endif
 
         call wruser(' ',istat)
         write(prbuf,16) nrej
16       format(3x,i8,' POSITION(S) REJECTED')
         call lbgone(prbuf(4:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)
         write(prbuf,17) sigma
17       format(3x,'RMS ALIGNMENT ERROR=',ss,g11.4)
         call wruser(prbuf,istat)
         call wruser(' ',istat)
 
*
* PRINT MESSAGE SHOWING TYPE OF FIT USED
*
         call wruser(fitmsg(ifit),istat)
         call wruser(' ',istat)
 
*
* FINALLY PRINT THE TRANSFORMATION COEFFICIENTS
*
         write(prbuf,18)
18       format(3x,'TRANSFORMATION COEFFICIENTS:')
         call wruser(prbuf,istat)
         call wruser(' ',istat)
 
         do 44 j=1,4,3
            write(prbuf,19)((l,c(l)),l=j,j+2)
19          format(10x,3('C(',i1,')=',ss,g13.6,2x))
            call wruser(prbuf,istat)
44       continue
 
         call wruser(' ',istat)
      endif
 
99    return
 
      end
 
 
 
