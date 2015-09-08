      subroutine algtrp(exprs,opout,nout,img,nimg,imp,var,nvar,varp
     : ,con,ncon,errmsg,ierr)
 
*
* SET NUMBER OF RECOGNISED SYMBOLS
*
      parameter (maxsym=37)
 
*
* DIMENSION ARRAYS
*
      character exprs*(*),opout(*)*6,img(*)*2,var(*)*1,in*81,oper(-
     :3:maxsym)*7,opsymb(-3:maxsym)*7,numchr*80,test*7,arg*1,output
     : *7,errmsg*(*)
      integer symb(80),stk(0:80),tos,imp(*),varp(*),opl(-3:maxsym),opr(
     : -3:maxsym),l(-3:maxsym),prl(-3:maxsym),prr(-3:maxsym)
      real con(*)
      logical opnext,found,ischar,isnumb,issign
 
*
* DEFINE STATEMENT FUNCTIONS FOR TESTING FOR ALPHABETIC CHARACTERS
* AND NUMBERS
*
      ischar(arg)=(arg.ge.'A').and.(arg.le.'Z')
      isnumb(arg)=((arg.ge.'0').and.(arg.le.'9')).or.(arg.eq.'.')
      issign(arg)=(arg.eq.'+').or.(arg.eq.'-')
 
*
* SET UP TABLE OF OPERATOPS, SYMBOLS AND THEIR PRIORITIES
*
      data ((oper(i),opsymb(i),l(i),opl(i),opr(i),prl(i),prr(i)),i=
     : -3,10)/'       ','LDCON  ', 1, 0, 0,10,10,'       ','LDVAR  '
     : , 1, 0, 0,10,10,'       ','LDIM   ', 2, 0, 0,10,10,'=      ',
     : '=      ', 1, 1, 1, 0, 0,')      ',')      ', 1, 1, 0, 2,10,
     : '(      ','(      ', 1, 0, 1,10, 1,'-      ','-      ', 1, 1
     :  , 1, 4, 4,'+      ','+      ', 1, 1, 1, 4, 4,'**     ','**     '
     :  , 2, 1, 1, 9, 6,'*      ','*      ', 1, 1, 1, 5, 5,'/      '
     :  ,'/      ', 1, 1, 1, 5, 5,':      ',':      ', 1, 1, 1, 2, 2,
     :  '-      ','NEG    ', 1, 0, 1, 8, 7,'+      ','POS    ', 1, 0
     :   , 1, 8, 7/
      data ((oper(i),opsymb(i),l(i),opl(i),opr(i),prl(i),prr(i)),i=11
     : ,20)/'SQRT(  ','SQRT(  ', 5, 0, 1,10, 1,'EXP(   ','EXP(   ',
     :   4, 0, 1,10, 1,'LOG10( ','LOG10( ', 6, 0, 1,10, 1,'LOG(   ',
     :  'LOG(   ', 4, 0, 1,10, 1,'SIN(   ','SIN(   ', 4, 0, 1,10, 1,
     :  'COS(   ','COS(   ', 4, 0, 1,10, 1,'TAN(   ','TAN(   ', 4, 0
     :   , 1,10, 1,'ASIN(  ','ASIN(  ', 5, 0, 1,10, 1,'ACOS(  ',
     :   'ACOS(  ', 5, 0, 1,10, 1,'ATAN(  ','ATAN(  ', 5, 0, 1,10, 1/
      data ((oper(i),opsymb(i),l(i),opl(i),opr(i),prl(i),prr(i)),i=21
     : ,30)/'ATAN2( ','ATAN2( ', 6, 0, 1,10, 1,'SINH(  ','SINH(  ',
     :   5, 0, 1,10, 1,'COSH(  ','COSH(  ', 5, 0, 1,10, 1,'TANH(  ',
     :  'TANH(  ', 5, 0, 1,10, 1,'ABS(   ','ABS(   ', 4, 0, 1,10, 1,
     :  'AINT(  ','AINT(  ', 5, 0, 1,10, 1,'ANINT( ','ANINT( ', 6, 0
     :   , 1,10, 1,'MOD(   ','MOD(   ', 4, 0, 1,10, 1,'SIGN(  ',
     :   'SIGN(  ', 5, 0, 1,10, 1,'DIM(   ','DIM(   ', 4, 0, 1,10, 1/
      data ((oper(i),opsymb(i),l(i),opl(i),opr(i),prl(i),prr(i)),i=31
     : ,maxsym)/'MIN(   ','MIN(   ', 4, 0, 1,10, 1,'MAX(   ','MAX(   '
     : , 4, 0, 1,10, 1,'XX     ','XX     ', 2, 0, 0,10,10,'YY     ',
     : 'YY     ', 2, 0, 0,10,10,'CLIP(  ','CLIP(  ', 5, 0, 1,10, 1,
     : 'GAUSS( ','GAUSS( ', 6, 0, 1,10, 1,'%      ','/      ', 1, 1
     :  , 1, 5, 5/
 
*
* REMOVE IMBEDDED BLANKS AND COUNT THE NUMBER OF INPUT CHARACTERS
*
      errmsg=' '
      ncin=0
 
      do 1 i=1,len(exprs)
 
         if(exprs(i:i).ne.' '.and.ncin.lt.80) then
            ncin=ncin+1
            in(ncin:ncin)=exprs(i:i)
         endif
 
1     continue
 
 
*
* RETURN THE EXPRESSION WITH BLANKS REMOVED
*
      exprs=in(1:ncin)
 
*
* APPEND AN '=' OPERATOR TO TERMINATE THE EXPRESSION
*
      ncin=ncin+1
      in(ncin:)='='
 
*
* INITIALLISE COUNTERS
*
      ierr=0
      nimg=0
      nvar=0
      ncon=0
      i=1
      j=0
 
*
* OPNEXT INDICATES IF AN OPERATOR IS EXPECTED NEXT
* FIRST ENTITY MUST NOT LOOK LIKE AN OPERATOR
*
      opnext=.false.
 
*
* SEARCH THROUGH THE LIST OF SYMBOLS TO IDENTIFY WHICH COMES NEXT
*
67    found=.false.
 
      do 2 nsymb=0,maxsym
 
*
* SYMBOL IS ONLY VALID IF IT LOOKS LIKE AN OPERATOR OR OPERAND FROM
* THE LEFT, AS APPROPRIATE
*
 
         if(opnext.eqv.(opl(nsymb).eq.1)) then
            test=in(i:min(i+l(nsymb)-1,81))
 
            if(test.eq.oper(nsymb)) then
               found=.true.
               go to 66
 
            endif
 
         endif
 
2     continue
 
66    continue
 
*
* IF SYMBOL WAS NOT FOUND, IT CONSTITUTES AN ERROR IF AN OPERATOR
* WAS EXPECTED
*
 
      if (.not.found) then
 
         if(opnext) then
            ierr=3
            errmsg(min(i,len(errmsg)):)='?'
            go to 99
 
 
*
* IF AN OPERAND WAS EXPECTED, IT MAY BE AN IMAGE, VARIABLE OR CONST.
*
 
         else if(in(i:i).eq.'I'.and.ischar(in(i+1:i+1))) then
 
*
* IF IT IS AN IMAGE, ADD NAME TO IMAGE STACK
*
            nsymb=-1
            nimg=nimg+1
            img(nimg)=in(i:i+1)
 
         else if(ischar(in(i:i))) then
 
*
* IF IT IS A VARIABLE, ADD NAME TO VARIABLE STACK
*
            nsymb=-2
            nvar=nvar+1
            var(nvar)=in(i:i)
 
         else
 
*
* OTHERWISE IT MAY BE A CONSTANT...EXTRACT CONTIGUOUS NUMERICAL
* CHARACTERS
*
            nnum=0
            numchr=' '
 
            do 14 ll=i,ncin
 
*
* CHARACTER MAY BE PART OF A NUMERICAL CONSTANT IF IT IS 0..9 OR '.'
*
* OR IF IT IS AN 'E' FOLLOWING ONE OF THE ABOVE
*
* OR IF IT IS A SIGN FOLLOWING AN 'E'
*
 
               if(isnumb(in(ll:ll)).or.(in(ll:ll).eq.'E').or.
     :         (issign(in(ll:ll)).and.(in(ll-1:ll-1).eq.'E'))) then
                  nnum=nnum+1
                  numchr(nnum:nnum)=in(ll:ll)
 
               else
 
*
* END OF NUMBER AS SOON AS ONE OF THE ABOVE TESTS FAILS
*
                  go to 46
 
               endif
 
14          continue
 
 
*
* TRY TO READ THESE CHARACTERS AS A CONSTANT
*
46          call ctor(numchr(:nnum),const,iok)
 
*
* IF SUCCESSFUL, ADD CONSTANT TO STACK
*
 
            if (iok.eq.0.and.nnum.ne.0) then
               ncon=ncon+1
               con(ncon)=const
               nsymb=-3
               l(nsymb)=nnum
 
            else
 
*
* OTHERWISE THERE IS A BAD OPERAND ERROR
*
               ierr=2
               errmsg(min(i,len(errmsg)):)='?'
               go to 99
 
            endif
 
         endif
 
      endif
 
 
*
* PUT THE IDENTIFIED SYMBOL INTO THE OUTPUT ARRAY AND MOVE THE
* INPUT POINTER TO THE NEXT SYMBOL
*
      j=j+1
      symb(j)=nsymb
      i=i+l(nsymb)
 
*
* DECIDE WHETHER AN OPERATOR OR OPERAND FOLLOWS
*
      opnext=opr(nsymb).ne.1
 
      if(opsymb(nsymb).ne.'=') go to 67
 
*
* ARRIVE HERE WHEN THE FINAL '=' IS ENCOUNTERED
* INITIALLISE THE OPERATOR STACK FOR CONVERTING TO REVERSE POLISH
*
 
      do 101 is=0,j
         stk(is)=0
101   continue
 
 
*
* INITIALLISE POINTERS
*
      tos=0
      isymb=1
      nout=0
 
*
* IF THE TOP OF STACK AND INPUT STREAM HAVE MATCHING PARENTHESES,
* CANCEL THEM
*
 
94    if((index(oper(stk(tos)),'(').ne.0).and.(oper(symb(isymb)).eq.
     :')')) then
 
*
* IF THERE IS A FUNCTION ASSSOCIATED WITH THE OPENING PARENTHESIS
* THEN SEND IT TO THE OUTPUT STREAM
*
 
         if(oper(stk(tos)).ne.'(') then
 
*
* REMOVE THE ENCLOSED '(' FROM FUNCTIONS FIRST
*
            output=opsymb(stk(tos))
            output(index(output,'('):index(output,'('))=' '
            tos=tos-1
            isymb=isymb+1
 
         else
            tos=tos-1
            isymb=isymb+1
            go to 94
 
         endif
 
 
*
* IF THE SYMBOL ON THE TOP OF THE STACK HAS A HIGH ENOUGH PRIORITY,
* TRANSFER IT TO THE OUTPUT STREAM
*
 
      else if(prr(stk(tos)).ge.prl(symb(isymb))) then
         output=opsymb(stk(tos))
         tos=tos-1
 
*
* OTHERWISE, TRANSFER THE NEXT SYMBOL TO THE STACK
*
 
      else
         tos=tos+1
         stk(tos)=symb(isymb)
         isymb=isymb+1
 
*
* RETURN FOR NEXT TEST
*
         go to 94
 
      endif
 
 
*
* IF THERE IS SOME OUTPUT, DISREGARD IT IF IT IS
*  UNARY + OR A COMMA
*
*
* IF A BRACKET APPEARS IN THE OUTPUT, IT RESULTS FROM UNPAIRED
* PARENTHESES IN THE INPUT EXPRESSION...QUIT QITH ERROR
*
 
      if(index(output,'(').ne.0) then
         ierr=4
         errmsg='   '')'' MISSING'
         go to 99
 
      endif
 
 
      if(index(output,')').ne.0) then
         ierr=4
         errmsg='   ''('' MISSING'
         go to 99
 
      endif
 
 
      if(output.ne.'POS'.and.output.ne.':') then
         nout=nout+1
         opout(nout)=output
      endif
 
 
*
* RETURN FOR NEXT SYMBOL IF NOT THE END
*
 
      if(output.ne.'=') go to 94
 
*
* IF IMAGES OR VARIABLES ARE REFERENCED, SORT THEIR NAMES INTO
* ALPHABETICAL ORDER AND OBTAIN POINTERS TO ALLOW THEM TO BE
* ACCESSED IN THEIR ORIGINAL ORDER
*
 
      if(nimg.ge.1) then
         call stksrt(img,nimg,imp,ndiff,symb)
         nimg=ndiff
      endif
 
 
      if(nvar.ge.1) then
         call stksrt(var,nvar,varp,ndiff,symb)
         nvar=ndiff
      endif
 
99    return
 
      end
 
 
 
