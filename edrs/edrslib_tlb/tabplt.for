      subroutine tabplt(table,ntab,botlim,toplim,title,device,work)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PLOT A GRAPH OF AN ITF LOOK-UP TABLE USING DRPLOT ROUTINES
*
*METHOD
*       GENERATE ARRAYS TO PLOT, CONVERT TITLE TO A BYTE STRING. CALL
*       DRPLOT ROUTINES TO PLOT GRAPH ON CHOSEN GRAPHICS DEVICE.
*       WAIT FOR USER RESPONSE BEFORE FINISHING AND CLEARING SCREEN
*
*ARGUMENTS
*       TABLE (IN)
*       REAL(NTAB)
*               LOOK UP TABLE
*       NTAB (IN)
*       INTEGER
*               NUMBER OF TABLE ENTRIES
*       BOTLIM,TOPLIM (IN)
*       REAL
*               LOWER AND UPPER LIMITS OF INPUT DATA FOR LOOK UP TABLE
*       TITLE (IN)
*       CHARACTER*(*)
*               MAIN TITLE FOR PLOT (UP TO FIRST 30 CHARACTERS USED)
*       DEVICE (IN)
*       INTEGER
*               SELECT GRAPHICS DEVICE: SGS WORKSTATION IDENTIFIER
*       WORK (WORKSPACE)
*       REAL(NTAB)
*               USED TO STORE ARRAY OF X COORDINATES TO PLOT
*
*CALLS
*	THIS PACKAGE:
*		NCROPN, NCRBCK
*	NCAR:
*		AGCURV
*	SGS:
*		SGS_CLOSE
*
*NOTES
*       USES SUBROUTINE NAMES LONGER THAN 6 CHARACTERS
*
*WRITTEN BY
*       R.F.WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title*(*),text*80,device*30
      real table(ntab),work(ntab)
 
*
* GENERATE THE X COORDINATES FOR EACH TABLE ENTRY AND FIND THE MAX AND
* MIN X VALUES OF X AND Y WHICH ARE TO BE PLOTTED
*

      xintvl=(toplim-botlim)/max(1,ntab-1)
 
      xmax=-1.0e32
      xmin=1.0e32
      ymax=-1.0e32
      ymin=1.0e32
      do 1 i=1,ntab
         work(i)=botlim+(i-1)*xintvl
         xmax=max(xmax,work(i))
         xmin=min(xmin,work(i))
         ymax=max(ymax,table(i))
         ymin=min(ymin,table(i))
1     continue
 
 
*
* CALL NCAR PLOTTING ROUTINES TO PLOT GRAPH
*
      call ncropn(device,.true.,ierr)
      if(ierr.ne.0) goto 99

      call ncrbck(xmin,xmax,ymin,ymax,title,'INPUT DATA VALUE',
     :            'OUTPUT DATA VALUE')
      call agcurv(work,1,table,1,ntab,1)

      call sgs_close
 
  99  continue

      end
 
 
 
