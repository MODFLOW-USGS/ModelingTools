! This file is part of the JUPITER API, documented in:
!
! Banta, E.R., Poeter, E.P., Doherty, J.E., and Hill, M.C., 2006, 
! JUPITER: Joint Universal Parameter IdenTification and Evaluation 
! of Reliability--An application programming interface (API) for 
! model analysis: U.S. Geological Survey Techniques and Methods, 
! book 6, chap. E1, 268 p.  
! (Also available at http://pubs.usgs.gov/tm/2006/tm6e1/.)
!
! For the latest updates to source code and documentation, go to:
! http://water.usgs.gov/software/JupiterApi/
!
MODULE GLOBAL_DATA
  IMPLICIT NONE
  SAVE
  PUBLIC
  !   Parameters
  CHARACTER(LEN=14),  PARAMETER :: VERSIONID = '1.7.3'
  REAL                          :: REALLOCAL
  DOUBLE PRECISION              :: DOUBLELOCAL
  INTEGER                       :: INTEGERLOCAL
  REAL,               PARAMETER :: BIGREAL = HUGE(REALLOCAL)
  DOUBLE PRECISION,   PARAMETER :: BIGDOUBLE = HUGE(DOUBLELOCAL)
  INTEGER,            PARAMETER :: BIGINTEGER = HUGE(INTEGERLOCAL)

  INTEGER,            PARAMETER :: LENDNAM = 20 ! Maximum number of characters allowed in a dependent name
  INTEGER,            PARAMETER :: MAXRECLDEFAULT = 2000 ! Default value for RECL specifier in OPEN statement for DX files
  INTEGER,            PARAMETER :: MAX_STRING_LEN = 2000 ! Maximum number of characters allowed in a string in an input block
  INTEGER,            PARAMETER :: MAXVERB = 5
  INTEGER,            PARAMETER :: MINVERB = 0
  INTEGER,            PARAMETER :: NPPREC = 500  ! Maximum number of parameters per record written to data-exchange files
  CHARACTER(LEN=100), PARAMETER :: BLANKS = &
  '                                                  &
  &                                                  '
  CHARACTER(LEN=100), PARAMETER :: HYPHENS = &
  '--------------------------------------------------&
  &--------------------------------------------------'
  !
  !   Variables
  CHARACTER(LEN=MAX_STRING_LEN) :: AMESSAGE= ' '
  CHARACTER(LEN=80)             :: ERRSUB  = ' ' ! Character string for error header
  INTEGER                       :: IVERB=3
  !   IVERB is the verbosity level--meanings are:
  !      0 - No extraneous output
  !      1 - Write warnings only
  !      2 - Write warnings and notes
  !      3 - Write warnings and notes, and echo selected input (default)
  !      4 - Write warnings and notes, and echo all input
  !      5 - Write warnings, notes, echoed input, and miscellaneous information
  !
END MODULE GLOBAL_DATA
