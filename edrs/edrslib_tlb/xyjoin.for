      subroutine xyjoin
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PRODUCE AN XY LIST WHICH IS THE CONCATINATION OF TWO INPUT
*       XY LISTS AND TO ADD A PREFIX TO THE IDENTIFIERS OF EACH INPUT
*       LIST IN SO AS TO KEEP THE TWO LISTS DISTINCT IN THE OUTPUT FILE
*----------------------------------------------------------------------
*
*
      character cval*1,title(1)*30,apref(1)*20,bpref(1)*20
 
*
* GET THE TWO INPUT LISTS
*
      call gtxylr('LISTA',.false.,nita,lsta,ipina,ierra)
 
      if(ierra.ne.0)go to 99
      call gtxylr('LISTB',.false.,nitb,lstb,ipinb,ierrb)
 
      if(ierrb.ne.0)go to 99
 
*
* GET THE PREFIXES FOR EACH LIST
*
      apref(1)='A'
      call rdkeyc('APREFIX',.true.,1,apref,nval,istat)
      bpref(1)='B'
      call rdkeyc('BPREFIX',.true.,1,bpref,nval,istat)
 
*
* GET MAX OUTPUT LIST LENGTH
*
      lenout=lsta+lstb
 
*
* GET SPACE FOR THE ID AND COORDINATES OF EACH LIST, ALLOWING SPACE
* TO ADD THE SECOND LIST TO THE END OF THE FIRST ONE
*
      call getdyn('ID',104,5*lenout,ipid,istata)
      call getdyn('X',104,lenout,ipx,istatax)
      call getdyn('Y',104,lenout,ipy,istatay)
 
*
      call getdyn('IDB',104,5*lstb,ipidb,istatb)
      call getdyn('IXB',104,lstb,ipxb,istatbx)
      call getdyn('IYB',104,lstb,ipyb,istatby)
 
*
*
 
      if((istata.ne.0).or.(istatax.ne.0).or.(istatay.ne.0)) then
         call wrerr('NOSPACE')
         goto 99
 
      endif
 
 
      if((istatb.ne.0).or.(istatbx.ne.0).or.(istatby.ne.0)) then
         call wrerr('NOSPACE')
         goto 99
 
      endif
 
 
*
* EXTRACT THE INPUT DATA
*
      call extlst(%val(ipina),nita,lsta,%val(ipid),1,20)
      call extlst(%val(ipina),nita,lsta,%val(ipx),21,24)
      call extlst(%val(ipina),nita,lsta,%val(ipy),25,28)
 
*
      call extlst(%val(ipinb),nitb,lstb,%val(ipidb),1,20)
      call extlst(%val(ipinb),nitb,lstb,%val(ipxb),21,24)
      call extlst(%val(ipinb),nitb,lstb,%val(ipyb),25,28)
 
*
* CALL JOIN TO CONCATENATE AND ADD THE PREFIXES
*
      call join(%val(ipid),%val(ipx),%val(ipy),apref(1),%val(ipidb)
     : ,%val(ipxb),%val(ipyb),bpref(1),lsta,lstb,lenout)
 
*
* GET OUTPUT XY LIST
*
      call gtxylw('OUTPUT',.true.,7,lenout,ipout,ierr2)
 
*
* PUT DATA INTO OUTPUT
*
      call addlst(%val(ipout),7,lenout,%val(ipid),1,20)
      call addlst(%val(ipout),7,lenout,%val(ipx),21,24)
      call addlst(%val(ipout),7,lenout,%val(ipy),25,28)
 
*
* GET OUTPUT TITLE
*
      title(1)=' '
      call rdkeyc('TITLE',.true.,1,title,nval,istat)
 
*
* UPDATE OUTPUT DESCRIPTOR
*
      call ptdscr('OUTPUT','NITEM','INTEGER',7,rval,cval,ierr)
      call ptdscr('OUTPUT','LSTLEN','INTEGER',lenout,rval,cval,ierr)
      call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1),ierr)
 
*
* FREE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
