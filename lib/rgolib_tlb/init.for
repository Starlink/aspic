      SUBROUTINE INIT(IWORD)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE INIT 
C
C
C         This routine sets up all the things needed  to  run  any  of  the 
C         greyscale routines described in LUN 14. It must be called first. 
C
C         IWORD       I*4   In    It specifies the length of  the  variable 
C                                 used  to store the output. In the case of 
C                                 the Vax it MUST have  a  value  of  8  as 
C                                 bytes are used. 
C
C
C         K F Hartley              RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                           
                                                                                
                                                                                
                                                                                
C ..LINKSTOCH IS THE COMMON BLOCK CONNECTING IPOW AND RAND...
C ..WITH SUBROUTINE STOCH
C
      LOGICAL*1 IPOW(8)
      INTEGER*4 LO
      COMMON /LINKSTOCH/IPOW,RAND(2000)
C
C ..THIS LOOP FILLS ELEMENTS OF IPOW WITH POWERS OF 2...
C
      DO 100 I=2,IWORD
        IPOW(I)=2**(IWORD-I)
  100 CONTINUE
C
C ..IPOW(1) NEEDS SPECIAL CARE...
C ..(OR NOT IN THE CASE OF THE VAX!!!!!)...
C
      IPOW(1)=128
C
C ..THIS LOOP FILLS RAND WITH RANDOM NUMBERS...
C
      LO=0
      DO 200 I=1,2000
        RAND(I)=RAN(LO)
  200 CONTINUE
C
C      THIS OPENS A DATA FILE
C
      OPEN(UNIT=8,NAME='DIT.DAT',RECORDSIZE=265/4+1,STATUS='NEW',
     1 CARRIAGECONTROL='NONE',FORM='UNFORMATTED',RECORDTYPE='FIXED')
      RETURN
      END
