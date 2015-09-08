      SUBROUTINE KOPLOG (MINLGI,NPTS,OLDPRF,NEWPRF)
C+
C     KOPLOG.
C
C     Subroutine to convert an array held as intensity relative
C     to a sky of 1.0 into Log intensity above the sky.
C
C  Given;
C   MINLGI  (R)  Min value of Log I permitted in the final profile,
C                values below this will be fudged to this value.
C   NPTS    (I)  No. of points in the profiles.
C   OLDPRF  (RA) Input profile held as intensity.
C
C  Returned;
C   NEWPRF  (RA) Output profile held as Log I.
C
C  Subroutines called;
C   None.	
C
C  Structure:-
C   compute the min. intensity corresponding to the min Log I.
C   Set the previous state to above the threshold
C   Do for all points
C     copy the radius.
C     if above the threshold
C       compute Log(I-1)
C       current state = above threshold.
C     else
C       current state = below the threshold.
C     end if
C     If the (current state .ne. previouse state) then
C       If changeing from above to below the threshold
C         increment the number of troughs
C         record the staring position of the trough
C       else if changing from below to above the threshold
C         record the stopping position of the trough
C       end if
C     end if
C     archive the current state as the previous state.
C   end do
C   Do for all troughs
C     If trough starts at the begining of the profile
C       replacement value = point beyond the outer trough edge
C     else if trough stops at the end of the profile
C       replacement value = pointer interior to the inner trough edge
C     else if trough entirely contained in profile
C       replacement value = mean of 2 pts beyond either edge
C     end if
C     Do for all points in trough
C       value = replacement value.
C     end do
C   end do
C
C  A C Davenhall./ROE/                                  8/8/82.
C-
      INTEGER NPTS
      REAL OLDPRF(NPTS,2),NEWPRF(NPTS,2)
      REAL MINLGI
C
      INTEGER ZTART(50),ZTOP(50),TROUGH
      REAL VALUE,RPLACE
C
      LOGICAL CSTATE,PSTATE
C
C     CSTATE - Current state,     PSTATE - Previous state.
C                  = .TRUE. - Above threshold.
C                  = .FALSE. - Below threshold.
C
      REAL MINI
C
C
C    Compute min intensity corresponding to the minimum Log I.
C
      MINI=(1.0E1**(MINLGI))+1.0E0
C
C    Copy and convert all points.
C
      PSTATE=.TRUE.
      TROUGH=0
      DO I=1,NPTS
C
C    Radius.
C
        NEWPRF(I,1)=OLDPRF(I,1)
C
        VALUE=OLDPRF(I,2)
        IF (VALUE.GE.MINI) THEN
          NEWPRF(I,2)=ALOG10(VALUE-1.0E0)
          CSTATE=.TRUE.
        ELSE
          CSTATE=.FALSE.
        END IF
        IF (CSTATE.NE.PSTATE) THEN
          IF (PSTATE.AND..NOT.CSTATE) THEN
C
C    Changing from above to below the threshold.
C
            TROUGH=TROUGH+1
            ZTART(TROUGH)=I
          ELSE
C
C    Changing from below to above the threshold.
C
            ZTOP(TROUGH)=I-1
          END IF
        END IF
C
C    Archive the current state to the previous state.
C
        PSTATE=CSTATE
      END DO
C
C    Take care of the case where the last point in the profile 
C    was below the threshold.
C
      IF (.NOT.CSTATE) ZTOP(TROUGH)=NPTS
C
C    Remove the troughs.
C
      DO I=1,TROUGH
C
C    Find the value to fill in the troughs with.
C
C    The trough starts at the start of the profile.
C
        IF (ZTART(I).EQ.1) THEN
          RPLACE=NEWPRF(ZTOP(I)+1,2)
C
C    Trough stops at the end of the profile.
C
        ELSE IF (ZTOP(I).EQ.NPTS) THEN
          RPLACE=NEWPRF(ZTART(I)-1,2)
C
C    Trough entirely contained within the profile; good points
C    at either side.
C
        ELSE
          RPLACE=(NEWPRF(ZTOP(I)+1,2)+NEWPRF(ZTART(I)-1,2))/2.0E0
        END IF
C
C    Fill in the trough.
C
        DO K=ZTART(I),ZTOP(I)
          NEWPRF(K,2)=RPLACE
        END DO
      END DO
      END
