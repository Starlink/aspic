      SUBROUTINE ENDIT
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE ENDIT 
C
C
C         This is used to terminate output,  and  put  the  resulting  file 
C         DIT.DAT onto the Versatec plot queue. 
C
C         It is one of the Greyscale routines described in LUN 14. 
C
C
C         K F Hartley              RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
                                                                                
                                                                                
                                                                                
      COMMON /LINKF/IO
      LOGICAL*1 IOUT(264)
      CLOSE(UNIT=90)
      CALL DO_DCL('PRINT/QUEUE=LVA0: DIT.DAT',ISTAT)
      RETURN
      END
