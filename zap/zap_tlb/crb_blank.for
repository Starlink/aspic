	subroutine CRB_BLANK
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER IX(2),IPOUT,ISTAT
	character*1 ans
	
	print *,'Do you require information on this program?'
	read(*,111) ans
111	format(a1)
	if(ans.eq.'Y'.or.ans.eq.'y') then

	print *,' '
	print *,'BLANK'
	print *,' '
	print *,'Chris Benn  RGO  1 May 1987'
	print *,' '
	print *,'Creates an ouput frame in which every element ',
	1   'takes the same value'
	print *,' '
	print *,'Parameters:'
	print *,'   XYDIMES   x,y dimensions of output frame'
	print *,'   OUPUT     name of output frame'
	print *,'   VAL       value of elements in frame'
	print *,' '

	endif

      IX(1)=320
      IX(2)=512
      CALL RDKEYI('XYDIMES',.TRUE.,2,IX,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL WRIMAG('OUTPUT',FMT_R,IX,2,IPOUT,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRERR('HELL')
         GO TO 800
      END IF
      BACK=1.0
      CALL RDKEYR('VAL',.TRUE.,1,BACK,I,ISTAT)
      IF (ISTAT.GT.ERR_PARNUL) THEN
        CALL WRERR('HELL')
         GO TO 800
      END IF
      CALL FLAT(%VAL(IPOUT),IX(1),IX(2),BACK)
      CALL CNPAR('XYDIMES',ISTAT)
      CALL CNPAR('VAL',ISTAT)
      CALL CNPAR('OUTPUT',ISTAT)
  700 CONTINUE
  800 CONTINUE
      CALL FRDATA(' ',ISTAT)
      END
      SUBROUTINE FLAT(DATA,N,M,VALUE)
      REAL DATA(N,M)
      DO 200 J=1,M
         DO 100 I=1,N
           DATA(I,J)=VALUE
  100    CONTINUE
  200 CONTINUE
      END
