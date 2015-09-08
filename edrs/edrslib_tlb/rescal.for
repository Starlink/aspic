      subroutine rescal
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ALLOW THE USER TO CHANGE DESCRIPTOR ITEMS IN AN IMAGE SO
*       AS TO RE-SCALE THE INTEGER VALUES STORED
*
*METHOD
*       OBTAIN INPUT/OUTPUT IMAGES AND OBTAIN DESCRIPTOR ITEMS. FIND NEW
*       VALUES FROM THE ENVIRONMENT. CALL NEWSCL TO ALTER THE IMAGE
*       INTEGERS THEN UPDATE THE OUTPUT DESCRIPTOR
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT IMAGE
*       OUTPUT
*               THE OUTPUT IMAGE
*       TITLE
*               THE NEW TITLE
*       INVAL
*               THE NEW INVALID PIXEL FLAG
*       BSCALE
*               THE NEW IMAGE SCALE FACTOR
*       BZERO
*               THE NEW IMAGE ZERO LEVEL
*
*CALLS
*       THIS PACKAGE:
*               GT2DIR,GT2DIW,GTDSCR,GETPAR,NEWSCL,PTDSCR
*       STARLINK:
*               RDKEYC,CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)
 
      if(ierrin.eq.0) then
 
*
* INPUT IMAGE OBTAINED SUCCESSFULLY...OBTAIN OUTPUT IMAGE
*
         call gt2diw('OUTPUT',102,.false.,npix,nlines,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT IMAGE OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS FROM
* INPUT IMAGE
*
            title(1)=' '
            invala=-100000
            ascale=1.0
            azero=0.0
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call gtdscr('INPUT','INVAL','INTEGER',invala,rval,cval,
     :      ierr)
            call gtdscr('INPUT','BSCALE','REAL',ival,ascale,cval,ierr)
            call gtdscr('INPUT','BZERO','REAL',ival,azero,cval,ierr)
 
*
* SET DEFAULT DESCRIPTOR ITEMS FOR OUTPUT
*
            bscale=ascale
            bzero=azero
 
            if(abs(invala).le.32767) then
               invalb=invala
 
            else
               invalb=-32767
            endif
 
 
*
* OBTAIN NEW DESCRIPTOR ITEMS FROM ENVIRONMENT
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call getpar('INVAL','INTEGER',1,-32767.0,+32767.0,.true
     :       .,invalb,rval,cval,ierr)
            call getpar('BSCALE','REAL',1,-1.0e20,+1.0e20,.true.,ival
     :       ,bscale,cval,ierr)
            call getpar('BZERO','REAL',1,-1.0e20,+1.0e20,.true.,ival
     :       ,bzero,cval,ierr)
 
*
* CALL NEWSCL TO RESCALE THE IMAGE TO SUIT THE NEW DESCRIPTOR ITEMS
*
            call newscl(%val(ipin),npix,nlines,invala,ascale,azero,
     :      invalb,bscale,bzero,%val(ipout))
 
*
* COPY THE INPUT DESCRIPTOR TO THE OUTPUT AND UPDATE THOSE ITEMS WHICH
* HAVE CHANGED
*
            call cydscr('INPUT','OUTPUT',istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
         endif
 
      endif
 
 
*
* RELEASE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
