	subroutine crb_reduce_ccd
      INCLUDE 'stardir:INTERIM(FMTPAR)'
      INCLUDE 'stardir:INTERIM(ERRPAR)'

      INTEGER*4 DIM1(2),DIM2(2),DIM3(2),DIM4(2)
      CHARACTER*9 DESCR,NAME*13,ANS*1

	call welcome
      CALL RDIMAG('IMAGE',FMT_R,2,DIM1,ND1,IPT1,IST)
*
*   READ BSCALE AND BZERO DESCRIPTORS
*
      DESCR='BSCALE'
      CALL RDDSCR('IMAGE',DESCR,1,NAME,IACT,IST)
      IF(IST.EQ.ERR_NORMAL)THEN
         CALL CTOR(NAME,BSCALE,ISTATUS)
      ELSE
         BSCALE=1.0
         IST=ERR_NORMAL
      ENDIF
      DESCR='BZERO'
      CALL RDDSCR('IMAGE',DESCR,1,NAME,IACT,IST)
      IF(IST.EQ.ERR_NORMAL)THEN
         CALL CTOR(NAME,BZERO,ISTATUS)
      ELSE
         BZERO=0.0
         IST=ERR_NORMAL
      ENDIF


      CALL RDIMAG('BIAS',FMT_R,2,DIM2,ND2,IPT2,IST)
*
*   READ BSCALE AND BZERO DESCRIPTORS
*
      DESCR='BSCALE'
      CALL RDDSCR('BIAS',DESCR,1,NAME,IACT,IST)
      IF(IST.EQ.ERR_NORMAL)THEN
         CALL CTOR(NAME,BSCALEB,ISTATUS)
      ELSE
         BSCALEB=1.0
         IST=ERR_NORMAL
      ENDIF
      DESCR='BZERO'
      CALL RDDSCR('BIAS',DESCR,1,NAME,IACT,IST)
      IF(IST.EQ.ERR_NORMAL)THEN
         CALL CTOR(NAME,BZEROB,ISTATUS)
      ELSE
         BZEROB=0.0
         IST=ERR_NORMAL
      ENDIF

      IF(DIM1(1).NE.DIM2(1).OR.DIM1(2).NE.DIM2(2))THEN
        CALL WRUSER('Images not of the same dimension',ist)
        CALL WRUSER('Need overscan in both bias and image',ist)
      ENDIF



      CALL RDIMAG('FLAT',FMT_R,2,DIM4,ND4,IPT4,IST)
*
*   READ BSCALE AND BZERO DESCRIPTORS
*
      DESCR='BSCALE'
      CALL RDDSCR('FLAT',DESCR,1,NAME,IACT,IST)
      IF(IST.EQ.ERR_NORMAL)THEN
         CALL CTOR(NAME,BSCALEF,ISTATUS)
      ELSE
         BSCALEF=1.0
         IST=ERR_NORMAL
      ENDIF
      DESCR='BZERO'
      CALL RDDSCR('FLAT',DESCR,1,NAME,IACT,IST)
      IF(IST.EQ.ERR_NORMAL)THEN
         CALL CTOR(NAME,BZEROF,ISTATUS)
      ELSE
         BZEROF=0.0
         IST=ERR_NORMAL
      ENDIF


c
	ichip=0
	print *,' '
	call wruser('Select chip',ist)
	call wruser('1 AAT - will interpolate across known defects',
	1   ist)
	call wruser('2 La Palma RCA - -32k error will be corrected',
	1   ist)
	call wruser('3 La Palma GEC - -32k error will be corrected',
	1   ist)
	call wruser(' ',ist)
	call rdkeyi('CHIP',.true.,1,ichip,iels,ist)
	call wruser(' ',ist)
	if(ichip.eq.0) call exit

      ix1=dim1(1)-20
      ix2=ix1+15
      iy1=10
      iy2=dim1(2)-10

	if(ichip.eq.3) then
		ix1=2
		ix2=8
		iy1=10
		iy2=570
	endif
	print *,'Overscan columns ',ix1,' - ',ix2
	call wruser(' ',ist)
      call wruser('Will scale so that mean flat-field value in'
     1 ,ist)
      call wruser('non-overscan region is one.',ist)

C
C   Define size of output image
C
      DIM3(1)=DIM1(1)-30
	if(ichip.eq.3) dim3(1)=dim1(1)
      DIM3(2)=DIM1(2)
      call wruser('Output dimensions',ist)
      call rdkeyi('NX',.TRUE.,1,DIM3(1),NX1,IST)
      call rdkeyi('NY',.TRUE.,1,DIM3(2),NY1,IST)

      CALL WRIMAG('OUTPUT',FMT_R,DIM3,2,IPT3,ISTAT)
	print *,'OUT2 = input frame after correcting -32k error'
	print *,'(written to disc as a check)'
      CALL WRIMAG('OUT2',FMT_R,DIM3,2,IPT5,ISTAT)
      IF(ISTAT.GT.ERR_PARNUL)THEN
         CALL WRUSER('Invalid output file',IST)
         CALL EXIT
      ENDIF
      CALL aatreduce_sub(%VAL(IPT1),%VAL(IPT2),%val(ipt4),%VAL(IPT3),
	1    %val(ipt5),
     1   DIM1,DIM2,dim4,dim3,ix1,ix2,iy1,iy2,
     1   bscale,bzero,bscaleb,bzerob,bscalef,bzerof,ichip)

      CALL FRDATA(' ',ISTAT)

	call cnpar('BIAS',istat)
	CALL CNPAR('FLAT',ISTAT)
	CALL CNPAR('OUTPUT',ISTAT)
	CALL CNPAR('OUT2',ISTAT)
	CALL CNPAR('NX',ISTAT)
	CALL CNPAR('NY',ISTAT)
	CALL CNPAR('ANSWER',ISTAT)
	CALL CNPAR('CHIP',ISTAT)
      END
      subroutine aatreduce_sub(image,bias,flat,output,
	1    out2,
     1 id1,id2,idf,id3,
     1 ix1,ix2,iy1,iy2,
     1 bscale,bzero,bscaleb,bzerob,bscalef,bzerof,ichip)
      integer*4 id1(2),id2(2),id3(2),idf(2),idd(2)
      real image(id1(1),id1(2)),output(id3(1),id3(2))
	real out2(id3(1),id3(2))
      real bias(id2(1),id2(2)),flat(idf(1),idf(2)),a(400,600)
	character*80 string
      character ci*4,cj*4
c
c Subroutine to take an input image, a bias frame with
c overscan. Check the zero points in the overscan region and generate
c an output image.
c Clean defects from AAT RCA chip also.
c
	if(ichip.eq.2.or.ichip.eq.3) then
	call wruser('Now correcting for -32k error',ist)
      r64k=64.0*1024.0
      do j=1,id3(2)
         do i=1,id3(1)
            if (image(i,j).eq.0) then
               write (string,'(a,i,a,i,a)') 'Zero at (',i,',',j,')'
               call wruser (string,status)
               a(i,j)=1330.0
            else if (image(i,j).gt.0) then
               a(i,j)=real(image(i,j))
            else
               a(i,j)=real(image(i,j)) + r64k
            end if
		out2(i,j)=a(i,j)
         end do
      end do
	endif
      write(6,8888)bscale,bzero,bscaleb,bzerob,bscalef,bzerof
 8888 format(1h ,'Bscale,  bzero :',2(2x,e13.6)/
     1      1h ,'Bscaleb, bzerob:',2(2x,e13.6)/
     2      1h ,'Bscalef, bzerof:',2(2x,e13.6))
c
c First find difference in mean in overscan region
c
      bias_sum=0.0
      rimage_sum=0.0
      icount=0
      do j=iy1,iy2
         do i=ix1,ix2
           bias_sum=bias_sum+bias(i,j)*bscaleb+bzerob
           rimage_sum=rimage_sum+a(i,j)*bscale+bzero
           icount=icount+1
         enddo
      enddo
      rcount=icount
      bias_mean=bias_sum/rcount
      rimage_mean=rimage_sum/rcount
      difference=rimage_mean-bias_mean
      write(6,100)bias_mean,rimage_mean,difference,rcount
  100 format(1h ,'Bias mean, image mean, difference,rcount:'/
     1 1h ,4(2x,e13.6))
c
c Now de-bias, dark subtract and flat-field the image
c
      sum=0.0
      count=0.0
	ioverscan=0
	if(ichip.eq.3) ioverscan=9
	
      do j=1,id3(2)
         do i=1,id3(1)
		if(i.le.ioverscan) then
			output(i,j)=0
			goto 99
		endif
            count=count+1
            fvalue=bscalef*flat(i,j)+bzerof
            if(fvalue.le.0.0)then
               call itoc(i,ci,ist)
               call itoc(j,cj,ist)
        call wruser('**Warning: flat-field<=0.0 i,j='//ci//','//cj,ist)
              fvalue=1.0
            endif
            sum=sum+fvalue
            output(i,j)=(a(i,j)*bscale+bzero-
     1                   bias(i,j)*bscaleb-bzerob-difference)/fvalue
99	continue
         enddo
      enddo
      fmean=sum/count
      write(6,5555)fmean
 5555 format(1h ,'Mean value in flat-field was:',e13.6)
c


C  Clean RGOCCD image from AAT
        DO J=1,id3(2)
           DO I=1,id3(1)
            output(i,j)=output(i,j)*fmean
            if(ichip.eq.1)then
              A(I,J)=output(i,j)
            endif
           ENDDO
        ENDDO
C
	IF(ICHIP.NE.1) GOTO 80
C  remove known defects
        if(ichip.eq.0)then
          call wruser('No defect interpolation',ist)
          go to 80
        endif
        call wruser('Interpolating defects',ist)

        I=6
        DO J=242,245
        output(I,J)=0.25*(A(I-2,J)+A(I-1,J)+A(I+1,J)+A(I+2,J))
        ENDDO
        I=26
        DO J=389,397
        output(I,J)=0.25*(A(I-2,J)+A(I-1,J)+A(I+1,J)+A(I+2,J))
        ENDDO

        I=30
        DO J=164,248
        output(I,J)=0.25*(A(I-2,J)+A(I-1,J)+A(I+1,J)+A(I+2,J))
        ENDDO
      
        output(48,205)=0.25*(A(49,205)+A(47,205)+A(48,204)+A(48,206))

        I=71
        DO J=183,192
        output(I,J)=0.25*(A(I-2,J)+A(I-1,J)+A(I+1,J)+A(I+2,J))
        ENDDO

        output(112,324)=0.25*(A(111,324)+A(113,324)+A(112,323)+
     1                  A(112,325))
        output(139,69)=0.25*(A(139,68)+A(139,70)+A(140,69)+A(138,69))

        I=142
        DO J=1,512
        output(I,J)=0.375*(A(I-2,J)+A(I-1,J))
     $           +0.125*(A(I+4,J)+A(I+5,J))
        output(I+1,J)=0.2916667*(A(I-2,J)+A(I-1,J))
     $           +0.208333*(A(I+4,J)+A(I+5,J))
C        output(I+2,J)=0.208333*(A(I-2,J)+A(I-1,J))
C     $           +0.2916667*(A(I+4,J)+A(I+5,J))
C        output(I+3,J)=0.125*(A(I-2,J)+A(I-1,J))
C     $           +0.375*(A(I+4,J)+A(I+5,J))
        ENDDO

C         I=142
C         DO J=1,512
C         output(I,J)=0.40625*(A(I-2,J)+A(I-1,J))
C      $        +0.09375*(A(I+6,J)+A(I+7,J))
C
C        output(191,462)=0.25*(A(191,461)+A(191,263)+A(190,462)+A(192,462))
C
        I=207
        DO J=192,200
        output(I,J)=0.3175*(A(I-2,J)+A(I-1,J))
     $        +0.1825*(A(I+2,J)+A(I+3,J))
        output(I+1,J)=0.1825*(A(I-2,J)+A(I-1,J))
     $          +0.3175*(A(I+2,J)+A(I+3,J))
        ENDDO

        I=231
        DO J=250,460
	output(I,J)=0.3175*(A(I-2,J)+A(I-1,J))
     $	      +0.1825*(A(I+2,J)+A(I+3,J))
	output(I+1,J)=0.1825*(A(I-2,J)+A(I-1,J))
     $		+0.3175*(A(I+2,J)+A(I+3,J))
        ENDDO

        I=215
        DO J=404,450
        output(I,J)=0.3175*(A(I-2,J)+A(I-1,J))+0.1825*(A(I+2,J)+
     1              A(I+3,J))
        output(I+1,J)=0.1825*(A(I-2,J)+A(I-1,J))
     $           +0.3175*(A(I+2,J)+A(I+3,J))
        ENDDO

        I=239
        DO J=212,214
        output(I,J)=0.25*(A(I-2,J)+A(I-1,J)+A(I+1,J)+A(I+2,J))
        ENDDO

        I=263
        DO J=464,466
        output(I,J)=0.25*(A(I-2,J)+A(I-1,J)+A(I+1,J)+A(I+2,J))
        ENDDO

        I=271
        DO J=268,283
        output(I,J)=0.3175*(A(I-2,J)+A(I-1,J))+0.1825*(A(I+2,J)+
     1              A(I+3,J))
        output(I+1,J)=0.1825*(A(I-2,J)+A(I-1,J))
     $           +0.3175*(A(I+2,J)+A(I+3,J))
        ENDDO

        I=306
        DO J=162,176
        output(I,J)=0.3175*(A(I-2,J)+A(I-1,J))+0.1825*(A(I+2,J)+
     1              A(I+3,J))
        output(I+1,J)=0.1825*(A(I-2,J)+A(I-1,J))
     $           +0.3175*(A(I+2,J)+A(I+3,J))
        ENDDO
 
   80   return
        END        
	SUBROUTINE WELCOME
	CHARACTER*8 TIM
	CHARACTER*48 HOURS
	DATA HOURS /'00010203040506070809101112
	1131415161718192021222324'/
	CALL TIME(TIM)
	DO K=0,25
	LO=(K*2)+1
	LI=LO+1
	IF(TIM(1:2).EQ.HOURS(LO:LI)) IHOUR=K
	ENDDO
	PRINT *,' '
	IF(IHOUR.LE.11) PRINT *,'Good morning'
	IF(IHOUR.GE.12.AND.IHOUR.LE.16) PRINT *,'Good afternoon'
	IF(IHOUR.GE.17) PRINT *,'Good evening'
	print *,' '
	print *,'REDUCE_CCD'
	print *,' '
	print *,'Bill Sparkes 1986 (last mod. 10 Sep. 86 Chris Benn)'
	print *,' '
	print *,'Subtracts bias (after correcting for difference in'
	print *,'mean level in overscan regions) and flat-fields'
	print *,' '
	
	end
