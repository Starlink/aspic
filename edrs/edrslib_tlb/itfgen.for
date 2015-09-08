      subroutine itfgen
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO GENERATE ITF TABLES FOR STANDARD LINEARITY CORRECTIONS
*
*METHOD
*       OBTAIN THE NUMBER OF TABLE ENTRIES AND AN OUTPUT DATASET.
*       DETERMINE THE TYPE OF LINEARITY CORRECTION REQUIRED AND THE
*       RANGE OF VALIDITY, THEN CALL STDLIN TO GENERATE THE TABLE.
*       INSERT REQUIRED ITEMS IN OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       NENTRY
*               NUMBER OF ENTRIES IN THE TABLE
*       OUTPUT
*               OUTPUT IMAGE DATASET FOR ITF TABLE
*       ITFTYPE
*               SPECIFIES WHICH CORRECTION IS REQUIRED
*       LOLIMIT
*               LOWER LIMIT OF TABLE VALIDITY
*       UPLIMIT
*               UPPER LIMIT OF TABLE VALIDITY
*       TITLE
*               TITLE FOR THE OUTPUT ITF TABLE
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIW,GETCMD,STDLIN,PTDSCR
*       STARLINK:
*               RDKEYC,RDKEYR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character itftyp*20,cval*1,title(1)*30
 
*
* SET NUMBER OF SUPPORTED ITF FUNCTIONS AND MAX NUMBER OF COEFFICIENTS
* SPECIFYING THE CORRECTION
*
      parameter (nfunct=6,maxord=10)
      real limitu(nfunct),limitl(nfunct),lolim,c(maxord)
      integer type(nfunct)
 
*
* SET THE TYPE OF EACH ITF CORRECTION:
*       1: POLYNOMIAL
*       2: ELECTRONOGRAPHIC EMULSION SATURATION CORRECTION
*
      data type/1,1,2,2,2,2/
 
*
* SET LIMITS FOR THE RANGE OF VALIDITY OF EACH CORRECTION FUNCTION
*
      data limitu/1.0e10,4095.0,1.0e20,6.0,6.0,6.0/
      data limitl/-1.0e10,5*0.0/
 
*
* OBTAIN REQUIRED NO. OF ENTRIES IN ITF TABLE
*
      nentry=100
      call getpar('NENTRY','INTEGER',1,2.0,10000.0,.true.,nentry,rval
     : ,ierr)
 
*
* OBTAIN OUTPUT IMAGE TO CONTAIN TABLE
*
      call gt2diw('OUTPUT',204,.false.,nentry,1,ipoint,ierrou)
 
*
* OUTPUT IMAGE SUCESSFULLY OBTAINED:
*
 
      if(ierrou.eq.0) then
 
*
* DETERMINE WHICH ITF FUNCTION IS TO BE USED
*
         nitf=1
         call getcmd('ITFTYPE','POLYNOMIAL,PDS,ELECTRONOGRAPHIC,L4,G5,'/
     :    / 'KODAK.',1,nitf,itftyp,litf,ierr)
 
*
* SET DEFAULT LOWER AND UPPER TABLE LIMITS
*
         lolim=limitl(nitf)
         uplim=limitu(nitf)
 
*
* IF CORRECTION IS A POLYNOMIAL:
* ------------------------------
*
 
         if(type(nitf).eq.1) then
 
*
* TREAT SPECIAL POLYNOMIAL CORRECTIONS FIRST
*
 
            if(itftyp.eq.'PDS') then
 
*
* SET COEFFICIENTS FOR STANDARD PDS CORRECTION
*
               c(1)=-2.097e-3
               c(2)=1.1207e-3
               c(3)=1.256e-7
               c(4)=-2.344e-11
 
               do 16 i=5,maxord
                  c(i)=0.0
16             continue
 
 
*
* NOW TREAT THE GENERAL POLYNOMIAL CORRECTION
*
 
            else
 
*
* SET DEFAULT COEFFICIENTS, THEN OBTAIN VALUES FROM THE ENVIRONMENT
*
 
               do 17 i=1,maxord
                  c(i)=0.0
17             continue
 
               c(2)=1.0
               call rdkeyr('CONST',.false.,maxord,c,nval,istat)
 
*
* SET DEFAULT TABLE LIMITS FOR THE GENERAL POLYNOMIAL CORRECTION
*
               lolim=0.0
               uplim=1.0
            endif
 
 
*
* IF THE CORRECTION IS AN ELECTRONOGRAPHIC EMULSION SATURATION CORRN.
* -------------------------------------------------------------------
*
 
         else if(type(nitf).eq.2) then
 
*
* TREAT THE SPECIAL CASES FIRST
*
 
            if(itftyp.eq.'L4') then
 
*
* SET THE L4 FILM CONSTANT
*
               c(1)=18.9
 
            else if(itftyp.eq.'G5') then
 
*
* SET THE G5 FILM CONSTANT
*
               c(1)=9.45
 
*
* SET THE KODAK FILM CONSTANT
*
 
            else if(itftyp.eq.'KODAK') then
               c(1)=11.8
 
*
* NOW TREAT THE GENERAL ELECTRONOGRAPHIC CORRECTION
*
 
            else
 
*
* SET THE DEFAULT, THEN OBTAIN A FILM CONSTANT FROM THE ENVIRONMENT
*
               c(1)=18.9
               call getpar('CONST','REAL',1,1.0e-20,1.0e20,.true.,ival
     :          ,c(1),ierr)
 
*
* SET THE DEFAULT TABLE LIMITS FOR THE GENERAL CORRECTION
*
               limitu(nitf)=0.99*c(1)
               uplim=0.5*c(1)
            endif
 
 
*
* ALLOW THE LOWER LIMIT TO BE SET NEGATIVE BY THE ENVIRONMENT
* IF REQUIRED
*
            limitl(nitf)=-0.1*c(1)
         endif
 
 
*
* NOW OBTAIN THE TABLE LIMITS FROM THE ENVIRONMENT, USING THE DEFAULTS
* AND CONSTRAINING THEM SO THAT THEY LIE WITHIN THE VALID RANGE OF THE
* CORRECTION SPECIFIED. ALSO ENSURE UPLIM.GE.LOLIM
*
         call getpar('LOLIMIT','REAL',1,limitl(nitf),limitu(nitf),.true
     :    .,ival,lolim,ierr)
         call getpar('UPLIMIT','REAL',1,lolim,limitu(nitf),.true.,ival
     :    ,uplim,ierr)
 
*
* CALL STDLIN TO FILL THE TABLE WITH THE REQUIRED FUNCTION
*
         call stdlin(type(nitf),lolim,uplim,c,maxord,%val(ipoint),
     :   nentry,ierr)
 
*
* PUT TITLE AND UPPER AND LOWER TABLE LIMITS IN OUTPUT DESCRIPTOR
*
         title(1)=itftyp(:litf)//' ITF TABLE'
         call rdkeyc('TITLE',.true.,1,title,nval,istat)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call ptdscr('OUTPUT','UPLIM','REAL',ival,uplim,cval,ierr)
         call ptdscr('OUTPUT','LOLIM','REAL',ival,lolim,cval,ierr)
      endif
 
 
*
* RELEASE DATA AREA AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
