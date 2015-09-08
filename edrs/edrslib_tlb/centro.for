      subroutine centro
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE CENTROIDS OF STAR-LIKE IMAGES GIVEN AN INITIAL
*       ESTIMATE OF THEIR POSITIONS
*
*METHOD
*       OBTAIN INPUT IMAGE AND LIST OF POSITIONS AND EXTRACT REQUIRED
*       DESCRIPTOR ITEMS. OBTAIN WORKSPACE AND EXTRACT POSITIONS AND
*       IDENTIFIERS FROM LIST. OBTAIN REQUIRED PARAMETERS FROM ENVIRON-
*       MENT AND CALL LOCLST TO FIND CENTROIDS. OBTAIN OUTPUT DATASET
*       AND ADD OUTPUT POSITIONS TO IT. FORM OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,GTXYLR,EXTLST,GETCMD,LOCLST,GTXYLW,
*               ADDLST,PTDSCR
*       STARLINK:
*               GETDYN,WRERR,CYDSCR,RDKEYC,FRDATA
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       IMAGE
*               INPUT IMAGE
*       INPUT
*               INPUT LIST OF INITIAL POSITIONS
*       NOSPACE/ERROR/
*               CALLED IF WORKSPACE CANNOT BE OBTAINED
*       ISIZE
*               SIZE OF SEARCH SQUARE FOR FORMING CENTROID
*       ISIGN
*               INDICATES IF STAR-LIKE FEATURES ARE POSITIVE OR
*               NEGATIVE
*       MAXSHIFT
*               MAXIMUM SHIFT FROM INITIAL POSITION
*       MAXITER
*               MAXIMUM NUMBER OF CENTROIDING ITERATIONS
*       TOLERENC
*               ACCURACY TO WHICH CENTROIDS ARE REQUIRED
*       NOLIST/ERROR/
*               CALLED IF NO CENTROIDS CAN BE FOUND
*       OUTPUT
*               OUTPUT LIST OF POSITIONS
*       TITLE
*               TITLE TO REPLACE INPUT TITLE IN OUTPUT POSITION LIST
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30,idsign*8
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE FRAME
*
      call gt2dir('IMAGE',102,.false.,npix,nlines,ipimg,ierrim)
 
      if(ierrim.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY... GET INVALID FLAG FROM DESCRIPTOR
*
         inval=-100000
         call gtdscr('IMAGE','INVAL','INTEGER',inval,rval,cval,ierr)
 
*
* OBTAIN INPUT XY LIST OF INITIAL SEARCH POSITIONS
*
         call gtxylr('INPUT',.false.,nitem,lstlen,ipin,ierrxy)
 
         if(ierrxy.eq.0) then
 
*
* XY LIST OBTAINED SUCCESSFULLY... EXTRACT TITLE FROM DESCRIPTOR
*
            title(1)=' '
            call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
 
*
* OBTAIN VM WORKSPACE FOR INPUT AND OUTPUT XY LISTS AND IDENTIFIERS
*
            call getdyn('IDA',104,5*lstlen,ipida,istida)
            call getdyn('IDB',104,5*lstlen,ipidb,istidb)
            call getdyn('XA',104,lstlen,ipxa,istxa)
            call getdyn('YA',104,lstlen,ipya,istya)
            call getdyn('XB',104,lstlen,ipxb,istxb)
            call getdyn('YB',104,lstlen,ipyb,istyb)
 
*
* IF SPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
*
 
            if((istida.ne.0).or.(istidb.ne.0).or.(istxa.ne.0).or.(istya
     :       .ne.0).or.(istxb.ne.0).or.(istyb.ne.0)) then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* COPY INPUT ID,X AND Y TO WORKSPACE
*
            call extlst(%val(ipin),nitem,lstlen,%val(ipida),1,20)
            call extlst(%val(ipin),nitem,lstlen,%val(ipxa),21,24)
            call extlst(%val(ipin),nitem,lstlen,%val(ipya),25,28)
 
*
* OBTAIN SEARCH AREA SIZE (ISIZE),SIGN OF IMAGE FEATURES (ISIGN),
* MAX SHIFT FROM INITIAL POSITION (SHFTMX), MAX NO. OF CENTROIDING
* ITERATIONS (MAXIT) AND LOCATION ACCURACY (TOLL) FROM ENVIRONMENT
*
            isize=15
            call getpar('ISIZE','INTEGER',1,3.0,51.0,.true.,isize,rval
     :       ,ierr)
            isign=4
            call getcmd('ISIGN','NEGATIVE,-,POSITIVE,+.',1,isign,idsign
     :       ,lsgn,ierr)
            isign=isign-3
            shftmx=isize*1.5
            call getpar('MAXSHIFT','REAL',1,0.0,1.0e20,.true.,ival,
     :      shftmx,ierr)
            maxit=3
            call getpar('MAXITER','INTEGER',1,1.0,100.0,.true.,maxit
     :       ,rval,ierr)
            toll=0.05
            call getpar('TOLERENC','REAL',1,0.0,1.0e20,.true.,ival,toll
     :       ,ierr)
 
*
* IF IMAGE SCALE FACTOR IS NEGATIVE, THE IMAGE FEATURES WILL BE
* STORED UPSIDE DOWN IN THE INPUT ARRAY, SO INVERT THE SIGN OF
* THE IMAGE FEATURES REQUIRED
*
            scale=1.0
            call gtdscr('IMAGE','BSCALE','REAL',ival,scale,cval,ierr)
 
            if(scale.lt.0.0) isign=-(1+isign)
 
*
* CALL LOCLST TO FIND THE IMAGE CENTROIDS
*
            call loclst(%val(ipxa),%val(ipya),%val(ipida),lstlen,
     :      %val(ipimg),npix,nlines,inval,isize,isign,shftmx,maxit,toll
     :       ,ilevel,%val(ipxb),%val(ipyb),%val(ipidb),nout,ierr)
 
*
* IF THERE ARE NO OUTPUT LOCATIONS, GIVE MESSAGE AND ABORT
*
 
            if(nout.le.0) then
               call wrerr('NOLIST')
               go to 99
 
            endif
 
 
*
* OBTAIN OUTPUT DATA FRAME
*
            call gtxylw('OUTPUT',.false.,7,nout,ipout,ierrb)
 
            if(ierrb.eq.0) then
 
*
* OUTPUT FRAME OBTAINED SUCCESSFULLY... COPY OUTPUT DATA INTO LIST
*
               call addlst(%val(ipout),7,nout,%val(ipidb),1,20)
               call addlst(%val(ipout),7,nout,%val(ipxb),21,24)
               call addlst(%val(ipout),7,nout,%val(ipyb),25,28)
 
*
* ADD LIST DIMENSIONS AND TITLE TO OUTPUT DESCRIPTOR
*
               call cydscr('INPUT','OUTPUT',istat)
               call ptdscr('OUTPUT','NITEM','INTEGER',7,rval,cval,ierr)
               call ptdscr('OUTPUT','LSTLEN','INTEGER',nout,rval,cval
     :          ,ierr)
               call rdkeyc('TITLE',.true.,1,title,nval,istat)
               call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,
     :         title(1),ierr)
            endif
 
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
      return
 
      end
 
 
 
