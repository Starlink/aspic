        SUBROUTINE INEQPR (LOGMIN,DELOGI,NLEVEL,ISTAT)
C+
C       INEQPR.
C
C         Subroutine to obtain the parameters necessary to 
C         control the equivalent profile subroutine
C         from the Starlink environment.
C
C         Given;
C         ISTAT - Input status.
C
C         Returned;
C         LOGMIN - Log I for lowest isophotal level (real).
C         DELOGI - Increment in Log I between the levels (real).
C         NLEVEL - Max possible no. of levels. (integer).
C         ISTAT - Return status = max (input status, internal status).
C
C         Subroutines called;
C         READI, READR, YESNO.
C
C         A C Davenhall. /ROE/                                11/11/81.
C         A C Davenhall. /ROE/  {Modified}                    22/7/82.
C-
        REAL LOGMIN,DELOGI
        INTEGER NLEVEL,ISTAT
        INTEGER STAT1,STAT2,STAT3,STAT4
        LOGICAL OK
        CHARACTER REPLY*1
        OK=.FALSE.
        DO WHILE (.NOT.OK)
          STAT1=0
          STAT2=0
          STAT3=0
          STAT4=0
          CALL READR ('LOGMIN',
     :     ' Enter lowest Log I in the profile;',
     :     -2.0,-10.0,10.0,LOGMIN,STAT1)
          CALL READR ('DELOGI',
     :     ' Enter increment in Log I;',
     :     0.1,0.01,10.0,DELOGI,STAT2)
          CALL READI ('NLEVEL',
     :     ' Enter max. no. of entries;',
     :     50,2,100,NLEVEL,STAT3)
          CALL YESNO (' Are these values ok?','Y',REPLY,STAT4)
          IF (REPLY.EQ.'Y') OK=.TRUE.
        END DO
        ISTAT=MAX(ISTAT,STAT1,STAT2,STAT3,STAT4)
        END
