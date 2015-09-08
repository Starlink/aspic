      subroutine bblsrt(data,nsize,nvals)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Sorts integers held in the array data into order of increasing
*       size.
*
*SOURCE
*       BBLSRT.FOR in UTILITIES.TLB
*
*METHOD
*       Uses a simple "Bubble Sort" method.
*
*ARGUMENTS
*   INPUTS:
*       data(nsize)     integer An array holding the numbers to be
*                               sorted
*       nsize           integer The size of the array data
*       nvals           integer Only the first nvals entries in data are
*                               sorted.
*   OUTPUTS:
*       data(nsize)     integer The sorted numbers
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               swapi
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 14/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer   nsize,nvals,data(nsize)
*
* DECLARE LOCAL VARIABLES
*
      integer   adjel   ! The element adjacent to the current element
      logical   done    ! True when the first nvals elements of array
                        ! data are in increasing order
      integer   elemnt  ! The current element of the array being
                        ! checked for correct order
      integer   lastel  ! Points to the last element of the array which
                        ! needs to be checked for being in order
*
* INITIALIZE LOCAL VARIABLES
*
      lastel=nvals
      done=.false.
*
* LOOP UNTIL DATA IS IN CORRECT ORDER
*
      do while(.not.done)
*
* ON EACH PASS THROUGH THE DATA THE HIGHEST NUMBER GETS 'WASHED' DOWN
* TO THE END OF THE ARRAY, THEREFORE IT IS NOT NECCESSARY TO CHECK THE
* PREVIOUSLY LAST ELEMENT BECAUSE IT IS KNOWN TO BE IN THE RIGHT ORDER
*
         lastel=lastel-1
*
* GO THROUGH ALL THE DATA CONSIDERING ADJACENT PAIRS. IF A PAIR IS IN
* THE WRONG ORDER SWAP THEM ROUND
*
         done=.true.
         do elemnt=1,lastel
            adjel=elemnt+1
            if(data(elemnt).gt.data(adjel)) then
               call swapi(data(elemnt),data(adjel))
               done=.false.
            endif
         enddo
*
* LOOP ROUND FOR NEXT PASS THROUGH DATA
*
      enddo
*
* FINISH
*
      end
