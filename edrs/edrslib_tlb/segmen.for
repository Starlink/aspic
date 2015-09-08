      subroutine segmen
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COPY POLYGONAL SEGMENTS FROM ONE IMAGE TO ANOTHER
*
*METHOD
*       OBTAIN INPUT IMAGES. TREAT THE CASE OF 'NULL' INPUT IMAGES
*       SEPARATELY. EXTRACT INPUT DESCRIPTOR ITEMS AND OBTAIN DUMMY
*       INPUT IMAGE IF NECESSARY. CALCULATE OUTPUT DESCRIPTOR ITEMS
*       ACCORDING TO INPUT IMAGES PRESENT. PROCESS EACH POLYGON IN TURN.
*       OBTAIN INPUT X,Y LIST. OBTAIN WORKSPACE AND EXTRACT X,Y
*       POSITIONS. CALL PLYSMP TO COPY THE POLYGONAL AREA.
*       FINALLY UPDATE OUTPUT DESCRIPTOR ITEMS
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       AIMAGE
*               THE IMAGE TO BE COPIED FROM
*       BIMAGE
*               THE IMAGE TO BE COPIED INTO
*       NOSPACE/ERROR/
*               ACCESSED IF WORKSPACE CANNOT BE OBTAINED
*       OUTPUT
*               THE OUTPUT IMAGE
*       POLY1,POLY2...POLY20
*               THE INPUT X,Y LISTS SPECIFYING THE POLYGONS
*       2COMPLEX/ERROR/
*               ACCESSED IF A POLYGON HAS TOO MANY SIDES TO HANDLE
*       2FEWSIDE/ERROR/
*               ACCESSED IF A POLYGON DOES NOT HAVE AT LEAST 3 SIDES
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT IMAGE
*
*CALLS
*       THIS PACKAGE:
*               GT2DIR,GT2DIW,IMGSET,GTDSCR,IMGCPY,LBGONE,GTXYLR,EXTLST,
*               PLYSMP,PTDSCR
*       STARLINK:
*               GETDYN,WRERR,FRDATA,CNPAR,RDKEYC,CYDSCR
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30,polchr*8
      integer limit(4)
 
*
* SET MAX NUMBER OF POLYGON INPUT DATA FRAMES TO BE USED
*
      parameter (maxpol=20)
 
*
* OBTAIN INPUT IMAGE 'A'
*
      call gt2dir('AIMAGE',102,.true.,npixa,nlinea,ipa,ierra)
 
*
* OBTAIN INPUT IMAGE 'B', ACCEPTING A NULL ENTRY ONLY IF 'A' WAS GIVEN
*
      call gt2dir('BIMAGE',102,(ierra.eq.0),npixb,nlineb,ipb,ierrb)
 
*
* IF AT LEAST 1 INPUT IMAGE WAS GIVEN, CONTINUE
*
 
      if((ierra.eq.0).or.(ierrb.eq.0)) then
 
*
* IF 'A' WAS NOT GIVEN, OBTAIN A DUMMY REPLACEMENT 1 PIXEL IMAGE
*
 
         if(ierra.ne.0) then
            call getdyn('DUMMYA',102,1,ipa,istata)
            npixa=1
            nlinea=1
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
            if(istata.ne.0) then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
         endif
 
 
*
* IF 'B' WAS GIVEN, OUTPUT IMAGE IS SIZE OF 'B', OTHERWISE IT IS THE
* SIZE OF 'A'
*
 
         if(ierrb.eq.0) then
            npout=npixb
            nlout=nlineb
 
         else
            npout=npixa
            nlout=nlinea
         endif
 
 
*
* OBTAIN OUTPUT IMAGE FRAME
*
         call gt2diw('OUTPUT',102,.false.,npout,nlout,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY...
*
 
            if(ierra.ne.0) then
 
*
* IF 'A' IS ABSENT, SET DEFAULT DESCRIPTOR ITEMS
* ----------------
*
               invala=-32767
               ascale=1.0
               azero=0.0
 
*
* SET DUMMY 'A' IMAGE PIXEL TO BE INVALID
*
               call imgset(%val(ipa),npixa,nlinea,-32767)
 
*
* OBTAIN DESCRIPTOR ITEMS FROM 'B' IMAGE
*
               title(1)=' '
               invalb=-32767
               bscale=1.0
               bzero=0.0
               call gtdscr('BIMAGE','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
               call gtdscr('BIMAGE','INVAL','INTEGER',invalb,rval,cval
     :          ,ierr)
               call gtdscr('BIMAGE','BSCALE','REAL',ival,bscale,cval
     :          ,ierr)
               call gtdscr('BIMAGE','BZERO','REAL',ival,bzero,cval,
     :         ierr)
 
*
* COPY CONTENTS OF IMAGE 'B' TO OUTPUT IMAGE
*
               call imgcpy(%val(ipb),npout,nlout,%val(ipout))
 
               if(abs(invalb).gt.32767) invalb=-32767
 
*
* IF IMAGE 'B' WAS NOT GIVEN, OBTAIN DESCRIPTOR ITEMS FROM 'A'
* --------------------------
*
 
            else if(ierrb.ne.0) then
               title(1)=' '
               invala=-32767
               ascale=1.0
               azero=0.0
               call gtdscr('AIMAGE','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
               call gtdscr('AIMAGE','INVAL','INTEGER',invala,rval,cval
     :          ,ierr)
               call gtdscr('AIMAGE','BSCALE','REAL',ival,ascale,cval
     :          ,ierr)
               call gtdscr('AIMAGE','BZERO','REAL',ival,azero,cval,
     :         ierr)
 
*
* OUTPUT DESCRIPTOR ITEMS DEFAULT TO THOSE OF IMAGE 'A'
*
               invalb=invala
 
               if(abs(invalb).gt.32767)invalb=-32767
               bscale=ascale
               bzero=azero
 
*
* SET THE OUTPUT IMAGE TO BE ALL INVALID
*
               call imgset(%val(ipout),npout,nlout,invalb)
 
*
* IF BOTH 'A' AND 'B' IMAGES WERE GIVEN, OBTAIN DESCRIPTOR ITEMS
* -------------------------------------
* FOR IMAGE 'A'
*
 
            else
               invala=-100000
               ascale=1.0
               azero=0.0
               call gtdscr('AIMAGE','INVAL','INTEGER',invala,rval,cval
     :          ,ierr)
               call gtdscr('AIMAGE','BSCALE','REAL',ival,ascale,cval
     :          ,ierr)
               call gtdscr('AIMAGE','BZERO','REAL',ival,azero,cval,
     :         ierr)
 
*
* OBTAIN DESCRIPTOR ITEMS FOR IMAGE 'B', TO APPLY TO OUTPUT IMAGE ALSO
*
               title(1)=' '
               invalb=-32767
               bscale=1.0
               bzero=0.0
               call gtdscr('BIMAGE','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
               call gtdscr('BIMAGE','INVAL','INTEGER',invalb,rval,cval
     :          ,ierr)
               call gtdscr('BIMAGE','BSCALE','REAL',ival,bscale,cval
     :          ,ierr)
               call gtdscr('BIMAGE','BZERO','REAL',ival,bzero,cval,
     :         ierr)
 
*
* COPY IMAGE 'B' TO OUTPUT IMAGE FRAME
*
               call imgcpy(%val(ipb),npout,nlout,%val(ipout))
 
               if(abs(invalb).gt.32767) invalb=-32767
            endif
 
 
*
* COUNT THROUGH THE NUMBER OF POLYGON INPUT DATA FRAMES AND FORM
* THE APPROPRIATE PARAMETER NAME
*
 
            do 616 npoly=1,maxpol
               write(polchr,1616)npoly
1616           format('POLY',i4)
               call lbgone(polchr(5:))
 
*
* OBTAIN INPUT FRAME, ACCEPTING A NULL RETURN ONLY IF NOT THE FIRST
* POLYGON
*
               call gtxylr(polchr,(npoly.ge.2),nitem,lstlen,ipxy,
     :         ierrxy)
 
*
* IF FRAME NOT OBTAINED, EXIT FROM LOOP, OR ABORT IF THIS IS THE FIRST
* POLYGON
*
 
               if(ierrxy.ne.0) then
 
                  if(npoly.ge.2) then
                     go to 617
 
 
                  else
                     go to 99
 
                  endif
 
               endif
 
 
*
* OBTAIN DYNAMIC MEMORY SPACE FOR X,Y POSITIONS
*
               call getdyn('X',204,lstlen,ipx,istx)
               call getdyn('Y',204,lstlen,ipy,isty)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
               if((istx.ne.0).or.(isty.ne.0)) then
                  call wrerr('NOSPACE')
                  go to 99
 
               endif
 
 
*
* EXTRACT X,Y POSITIONS FROM INPUT LIST
*
               call extlst(%val(ipxy),nitem,lstlen,%val(ipx),21,24)
               call extlst(%val(ipxy),nitem,lstlen,%val(ipy),25,28)
 
*
* CALL PLYSMP TO COPY THE POLYGONAL SEGMENT FROM IMAGE A TO IMAGE B
*
               call plysmp(%val(ipa),npixa,nlinea,invala,ascale,azero
     :          ,%val(ipx),%val(ipy),lstlen,%val(ipout),npout,nlout
     :           ,invalb,bscale,bzero,limit,ierr)
 
*
* IF IERR INDICATES THE POLYGON WAS TOO COMPLEX, GIVE MESSAGE AND ABORT
*
 
               if(ierr.eq.2) then
                  call wrerr('2COMPLEX')
                  go to 99
 
 
               else if(ierr.eq.1) then
                  call wrerr('2FEWSIDE')
                  go to 99
 
               endif
 
 
*
* FREE DYNAMIC MEMORY AND INPUT DATA FRAME BEFORE RETURNING FOR
* NEXT POLYGON
*
               call frdata('X',istat)
               call frdata('Y',istat)
               call frdata(polchr,istat)
               call cnpar(polchr,istat)
616         continue
 
617         continue
 
*
* OBTAIN TITLE FROM ENVIRONMENT
*
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* COPY THE INPUT IMAGE DESCRIPTOR TO THE OUTPUT
*
 
            if(ierrb.eq.0) then
               call cydscr('BIMAGE','OUTPUT',istat)
 
            else
               call cydscr('AIMAGE','OUTPUT',istat)
            endif
 
 
*
* UPDATE OUTPUT DESCRIPTOR ITEMS
*
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
