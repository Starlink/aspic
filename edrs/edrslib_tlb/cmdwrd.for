      subroutine cmdwrd(list,commnd,ninput,ncomm,idcomm,lencom,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO COMPARE A CHARACTER PARAMETER WITH A LIST OF POSSIBLE
*       ALTERNATIVES TO CHECK ITS VALIDITY
*
*METHOD
*       EXTRACT EACH ALTERNATIVE FROM THE LIST AND MATCH IT AGAINST
*       THE PARAMETER. COUNT THE NUMBER OF CHARACTER MATCHES WHICH
*       OCCUR. DECIDE WHETHER THE PARAMETER IS UNIQUELY IDENTIFIED,
*       ABBREVIATED, AMBIGUOUSLY ABBREVIATED, MIS-TYPED BY UP TO 1
*       CHARACTER OR NOT IDENTIFIED AT ALL
*
*ARGUMENTS
*       LIST (IN)
*       CHARACTER*(*)
*               INPUT LIST OF ALTERNATIVES, SEPARATED BY COMMAS AND
*               TERMINATED WITH A FULL-STOP
*       COMMND (IN)
*       CHARACTER*(*)
*               THE PARAMETER TO BE IDENTIFIED
*       NINPUT (OUT)
*       INTEGER
*               THE NUMBER OF NON-BLANK CHARACTERS IN COMMND USED TO
*               IDENTIFY IT
*       NCOMM (OUT)
*       INTEGER
*               THE POSITION OF THE IDENTIFIED PARAMETER IN THE LIST
*               OF ALTERNATIVES
*       IDCOMM (OUT)
*       CHARACTER*(*)
*               THE FULL FORM OF THE IDENTIFIED PARAMETER
*       LENCOM (OUT)
*       INTEGER
*               THE NUMBER OF NON-BLANK CHARACTERS IN IDCOMM
*       IERR (OUT)
*       INTEGER
*               INDICATES THE SUCCESS OF THE IDENTIFICATION
*               0: UNIQUELY IDENTIFIED, OR A UNIQUE ABBREVIATION
*               1: IDENTIFIED WITH 1 MIS-TYPED CHARACTER
*               2: AMBIGUOUS ABBREVIATION
*               3: NOT IDENTIFIED
*
*CALLS
*       THIS PACKAGE:
*               LBGONE,NINDEX,NMATCH
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character list*(*),commnd*(*),idcomm*(*)
      integer start,end,endlst
 
*
* REMOVE LEADING BLANKS FROM INPUT STRING
*
      call lbgone(commnd)
 
*
* FIND THE NUMBER OF CHARACTERS TO THE FIRST BLANK OR COMMA
*
      ninput=min(nindex(commnd,' ',1),nindex(commnd,',',1))-1
 
*
* IF THERE ARE NO NON-BLANK CHARACTERS, ABORT WITH IERR=4
*
 
      if(ninput.le.0) then
         ierr=4
 
      else
 
*
* FIND THE END OF THE LIST OF ALTERNATIVE VALUES
*
         endlst=nindex(list,'.',1)-1
 
*
* INITIALLISE COUNTERS FOR PERFECT, GOOD AND POSSIBLE IDENTIFICATIONS
*
         listn=0
         nperf=0
         ngood=0
         nposs=0
 
*
* COUNT THROUGH THE ALTERNATIVES, FINDING THE START AND FINISH OF
* EACH WORD IN THE LIST
*
16       listn=listn+1
         start=nindex(list,',',listn-1)+1
         end=min(nindex(list,',',listn)-1,endlst)
         length=end-start+1
 
*
* IF A WORD HAS BEEN FOUND, MATCH IT AGAINST THE INPUT STRING
*
 
         if(start.le.endlst) then
 
            if(length.ge.1) then
               nc=nmatch(list(start:end),commnd(:ninput))
 
*
* IF IT MATCHES PERFECTLY, COUNT 1 PERFECT MATCH
*
 
               if(nc.eq.ninput.and.nc.eq.length) then
                  nperf=1
                  idcomm=list(start:end)
                  ncomm=listn
                  lencom=length
 
*
* IF IT IS AN ABBREVIATION AND NO PERFECT MATCHES HAVE BEEN
* FOUND, COUNT 1 GOOD MATCH
*
 
               else if(nc.eq.ninput.and.nc.ne.length.and.nperf.eq.0)
     :           then
                  ngood=ngood+1
                  idcomm=list(start:end)
                  ncomm=listn
                  lencom=length
 
*
* IF THERE IS 1 MIS-MATCHED CHARACTER, BUT NO BETTER MATCHES HAVE
* BEEN FOUND, COUNT 1 POSSIBLE MATCH
*
 
               else if(nc.eq.ninput-1.and.nperf.eq.0.and.ngood.eq.0)
     :           then
                  nposs=nposs+1
                  idcomm=list(start:end)
                  ncomm=listn
                  lencom=length
               endif
 
            endif
 
            go to 16
 
         endif
 
 
*
* IF 1 PERFECT MATCH WAS FOUND, INPUT IS UNIQUELY IDENTIFIED
*
 
         if(nperf.eq.1) then
            ierr=0
 
*
* IF 1 GOOD MATCH FOUND, INPUT IS A UNIQUE ABBREVIATION
*
 
         else if(ngood.gt.0) then
 
            if(ngood.eq.1) then
               ierr=0
 
*
* IF 2 OR MORE GOOD MATCHES, INPUT IS AN AMBIGUOUS ABBREVIATION
*
 
            else
               ierr=2
            endif
 
 
*
* IF 1 POSSIBLE MATCH, INPUT WAS MIS-TYPED
*
 
         else if(nposs.eq.1) then
            ierr=1
 
         else
 
*
* IF 2 OR MORE POSSIBLE MATCHES, INPUT IS NOT IDENTIFIED
*
            ierr=3
         endif
 
      endif
 
 
*
* IF IDENTIFICATION WAS UNSUCCESSFUL, RETURN WITH A BLANK
* IDENTIFICATION, OTHERWISE RETURN THE FULL FORM OF THE IDENTIFICATION
*
 
      if(ierr.gt.1) then
         ncomm=0
         idcomm=' '
         lencom=1
      endif
 
      return
 
      end
 
 
 
