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
MODULE UTILITIES
  PRIVATE
  !   Selected utility subprograms are PUBLIC:
  !   Input utilities
  PUBLIC :: UTL_GETARG, UTL_GETLINE, &
            UTL_READ2D, UTL_READBLOCK, UTL_READMATRIX, UTL_DX_READ_MCMV, &
            UTL_DX_READ_WT
  !   Output utilities
  PUBLIC :: UTL_COLLBL, UTL_COLNO, UTL_DX_CLOSE, UTL_DX_GETUNIT,   &
            UTL_DX_OPEN,  &
            UTL_DX_WRITE_PCC, UTL_DX_WRITE_MCMV, &
            UTL_DX_WRITE_PA, &
            UTL_DX_WRITE_WT, UTL_EIGEN, UTL_PARSQMATRIX, UTL_TABLESECTIONS,   &
            UTL_WRITE2D, UTL_WRITEBLOCK, UTL_WRITECDMATRIX,   &
            UTL_WRITEMATRIX, UTL_WRITE_MESSAGE, UTL_WRTSIG
  !   Manipulate utilities
  PUBLIC :: UTL_ACHAR,   &
            UTL_ADDQUOTE, UTL_APPENDLIST, UTL_ARR2CDMATRIX,   &
            UTL_ARR2STRING, UTL_CALCWT, UTL_CASE, UTL_CASETRANS,   &
            UTL_CHAR2NUM, UTL_CHECK_NAMES, UTL_CHISQ, UTL_COMBINESQMATRIX,   &
            UTL_COMPRESSLINE, UTL_CONSTRUCTCDMATRIX, UTL_CONSTRUCTDMATRIX,   &
            UTL_COUNTSUBS, UTL_COVMAT, UTL_DIAGONAL, UTL_DIAGONAL_SUB,   &
            UTL_ELAPSED_TIME,   &
            UTL_ENDTIME, UTL_FILTER, UTL_FILTERLIST, UTL_FSTT, UTL_GETCOL,   &
            UTL_GETICOL, UTL_GETIROW, UTL_GET_MATRIX_SECTION, UTL_GETPOS,   &
            UTL_GETROW, UTL_GETTOKEN,   &
            UTL_GETUNIT, UTL_GETVAL, UTL_GETWORD, UTL_GETWORDLEFT,   &
            UTL_GROUPLIST, UTL_INVERT_DIAG, &
            UTL_MATMUL, UTL_MATMUL_SUB, UTL_MATMULVEC,   &
            UTL_MATMULVEC_SUB, UTL_MERGELIST,   &
            UTL_NEXTUNIT, UTL_NUM2CHAR, UTL_PREPINVERSE, UTL_REMCHAR,   &
            UTL_RWORD, UTL_SAMENAME, UTL_SHELLSORT,  &
            UTL_STRING2ARR, UTL_STUD_T, UTL_SUBSTITUTE, UTL_SUBSTITUTE_SUB,   &
            UTL_SVD, UTL_SYSTEM, UTL_SYSTEM_FUNC,  &
            UTL_TABREP, UTL_VEC2CDMATRIX, UTL_WHICH1
  ! Miscellaneous utilities
  PUBLIC :: UTL_COPYLIST, UTL_SLEEP
  !   Error-processing utilities
  PUBLIC :: UTL_STOP, UTL_SUBERROR, UTL_VERSION_CHECK
  !   Operator overloading
  PUBLIC :: ASSIGNMENT(=), UTL_ASSIGN_CDM_CDM, UTL_ASSIGN_DAR_CDM
  !
  !   PRIVATE DATA: Variables related to data exchange (DX) files
  PRIVATE :: MAXDX, IUDX, DXEXT
  !
  INTEGER :: KDXDEF = 0
  INTEGER, PARAMETER :: MAXDX = 500
  INTEGER, DIMENSION(MAXDX) :: IUDX = 0 ! Unit number for each data exchange file
  ! DXEXT is extension associated with each IUDX position
  CHARACTER(LEN=13), DIMENSION(MAXDX) :: DXEXT=' '
  !
  ! ****************************************************************************
  !   Generic interface blocks
  ! ****************************************************************************
  !
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE UTL_ASSIGN_CDM_CDM
    MODULE PROCEDURE UTL_ASSIGN_DAR_CDM
  END INTERFACE
  !
  INTERFACE UTL_ARR2CDMATRIX
    !   A generic interface for converting a 2-D array to a compressed, double-
    !   precision matrix as a CDMATRIX structure
    MODULE PROCEDURE UTL_ARR2CDMATRIX_R
    MODULE PROCEDURE UTL_ARR2CDMATRIX_D
  END INTERFACE
  !
  INTERFACE UTL_CHAR2NUM
    !   A generic interface to convert a string to either an INTEGER,
    !   a REAL number, or a DOUBLE PRECISION number, or a LOGICAL.
    !   Arguments are as follows:-
    !     IFAIL:   indicates failure if returned as non-zero
    !     STRING:  a character string containing a number
    !     NUM:     an integer (for a2i), real (for a2r), double precision (for
    !              a2d), or logical (for a2l) number extracted from the string.
    MODULE PROCEDURE UTL_CHAR2NUM_A2I
    MODULE PROCEDURE UTL_CHAR2NUM_A2L
    MODULE PROCEDURE UTL_CHAR2NUM_A2R
    MODULE PROCEDURE UTL_CHAR2NUM_A2D
  END INTERFACE
  !
  INTERFACE UTL_DIAGONAL
    !   A generic interface for extracting diagonal elements from a
    !   compressed matrix structure, as a function
    MODULE PROCEDURE UTL_DIAGONAL_CDM_D
  END INTERFACE
  !
  INTERFACE UTL_DIAGONAL_SUB
    !   A generic interface for extracting diagonal elements from a
    !   compressed matrix structure, as a subroutine
    MODULE PROCEDURE UTL_DIAGONAL_SUB_CDM_D
  END INTERFACE
  !
  INTERFACE UTL_FILTERLIST
    !   A generic interface for traversing a nest of linked lists and storing
    !   values associated with a given keyword into an array of type REAL,
    !   DOUBLE PRECISION, INTEGER, CHARACTER, or LOGICAL
    MODULE PROCEDURE UTL_FILTERLIST_R   ! REAL array
    MODULE PROCEDURE UTL_FILTERLIST_D   ! DOUBLE PRECISION array
    MODULE PROCEDURE UTL_FILTERLIST_I   ! INTEGER array
    MODULE PROCEDURE UTL_FILTERLIST_C   ! CHARACTER array
    MODULE PROCEDURE UTL_FILTERLIST_L   ! LOGICAL array
  END INTERFACE
  !
  INTERFACE UTL_FILTER
    !   A generic interface for traversing a linked list and storing
    !   a value associated with a given keyword into a scalar of type REAL,
    !   DOUBLE PRECISION, INTEGER, CHARACTER, or LOGICAL
    MODULE PROCEDURE UTL_FILTER_D  ! DOUBLE PRECISION scalar
    MODULE PROCEDURE UTL_FILTER_R  ! REAL scalar
    MODULE PROCEDURE UTL_FILTER_I  ! INTEGER scalar
    MODULE PROCEDURE UTL_FILTER_C  ! CHARACTER scalar
    MODULE PROCEDURE UTL_FILTER_L  ! LOGICAL scalar
  END INTERFACE
  !
  INTERFACE UTL_GETCOL
    !   A generic interface for retrieving one row of a compressed matrix
    MODULE PROCEDURE UTL_GETCOL_CDM_D
    MODULE PROCEDURE UTL_GETCOL_CDM_SUB_D
  END INTERFACE
  !
  INTERFACE UTL_GETICOL
    !   A generic interface for retrieving the column index for a specified
    !   location in a compressed matrix
    MODULE PROCEDURE UTL_GETICOL_CDM
  END INTERFACE
  !
  INTERFACE UTL_GETIROW
    !   A generic interface for retrieving the row index for a specified
    !   location in a compressed matrix
    MODULE PROCEDURE UTL_GETIROW_CDM
  END INTERFACE
  !
  INTERFACE UTL_GET_MATRIX_SECTION
    !   A generic interface for retrieving upper left corner of a compressed matrix
    MODULE PROCEDURE UTL_GET_MATRIX_SECTION_CDM
  END INTERFACE
  !
  INTERFACE UTL_GETPOS
    !   A generic interface for retrieving the position in the IPOS component of
    !   a compressed matrix for a specified row and column location
    MODULE PROCEDURE UTL_GETPOS_CDM
  END INTERFACE
  !
  INTERFACE UTL_GETROW
    !   A generic interface for retrieving one row of a compressed matrix
    MODULE PROCEDURE UTL_GETROW_CDM_D
    MODULE PROCEDURE UTL_GETROW_CDM_SUB_D
  END INTERFACE
  !
  INTERFACE UTL_GETVAL
    !   A generic interface for retrieving a value from a specified location
    !   in a compressed matrix
    MODULE PROCEDURE UTL_GETVAL_CDM
  END INTERFACE
  !
  INTERFACE UTL_INVERT_DIAG
    !   A generic interface for inverting a diagonal CDMATRIX
    !   Argument list:(IFAIL,A,AS,DTLA)
    MODULE PROCEDURE UTL_INVERT_DIAG_CDM
    ! and so on
  END INTERFACE
  !
  INTERFACE UTL_MATMUL
    !   A generic interface for multiplying an ordinary 2-D array by an array
    !   stored in a compressed matrix
    !   Argument list: (NRA,NCA,OA,CB) Result: OC
    MODULE PROCEDURE UTL_MATMUL_ODM_CDM_D   ! OA is DP; CB is CDMATRIX; OC is DP
    MODULE PROCEDURE UTL_MATMUL_CDM_ODM_D   ! OA is CDMATRIX; CB is DP; OC is DP
    ! and so on
  END INTERFACE
  !
  INTERFACE UTL_MATMUL_SUB
    !   A generic interface for multiplying an ordinary 2-D array by an array
    !   stored in a compressed matrix
    !   Argument list: (NRA,NCA,OA,CB) Result: OC
    MODULE PROCEDURE UTL_MATMUL_SUB_ODM_CDM_D   ! OA is DP; CB is CDMATRIX; OC is DP
    MODULE PROCEDURE UTL_MATMUL_SUB_CDM_ODM_D   ! OA is CDMATRIX; CB is DP; OC is DP
    ! and so on
  END INTERFACE
  !
  INTERFACE UTL_MATMULVEC
    !   A generic interface for performing matrix multiplication on a compressed
    !   matrix and an ordinary column vector, as a function
    MODULE PROCEDURE UTL_MATMULVEC_CDM_ODV_D
  END INTERFACE
  !
  INTERFACE UTL_MATMULVEC_SUB
    !   A generic interface for performing matrix multiplication on a compressed
    !   matrix and an ordinary column vector, as a subroutine
    MODULE PROCEDURE UTL_MATMULVEC_SUB_CDM_ODV_D
  END INTERFACE
  !
  INTERFACE UTL_NUM2CHAR
    !   A generic interface to convert a number to a character.
    !   Arguments are as follows:-
    !     VALUE:   the number to be expressed in character form
    !     STRING:  the number expressed in character form
    !     NCHAR:   the maximum number of characters in which to express number
    MODULE PROCEDURE UTL_NUM2CHAR_I2A
    MODULE PROCEDURE UTL_NUM2CHAR_R2A
    MODULE PROCEDURE UTL_NUM2CHAR_D2A
  END INTERFACE
  !
  INTERFACE UTL_READMATRIX
    !   A generic interface for reading a matrix from a file coded as either
    !   a standard matrix or a compressed matrix into either a CDMATRIX or
    !   DMATRIX structure.  Currently, only the CDMATRIX
    !   type is supported.
    MODULE PROCEDURE UTL_READMATRIX_CDM
    !MODULE PROCEDURE UTL_READMATRIX_DM
  END INTERFACE
  !
  INTERFACE UTL_READ_CMPMATRIX
    !   A generic interface for reading a data from a file coded in the
    !   COMPRESSEDMATRIX format into either a CDMATRIX or DMATRIX
    !   structure.  Currently, only the CDMATRIX type is supported.
    MODULE PROCEDURE UTL_READ_CMPMATRIX_CDM
    !MODULE PROCEDURE UTL_READ_CMPMATRIX_DM
  END INTERFACE
  !
  INTERFACE UTL_READ_STDMATRIX
    !   A generic interface for reading a data from a file coded in the
    !   standard MATRIX format into either a CDMATRIX or DMATRIX
    !   structure.  Currently, only the CDMATRIX type is supported.
    MODULE PROCEDURE UTL_READ_STDMATRIX_CDM
    !MODULE PROCEDURE UTL_READ_STDMATRIX_DM
  END INTERFACE
  !
  INTERFACE UTL_READ2D
    !   A generic interface for reading a 2-D array
    MODULE PROCEDURE UTL_READ2D_D
    MODULE PROCEDURE UTL_READ2D_R
    MODULE PROCEDURE UTL_READ2D_I
  END INTERFACE
  !
  INTERFACE UTL_RWORD
    !   A generic interface for extracting a word or quoted phrase from a string
    MODULE PROCEDURE UTL_RWORD_8D !  Optionally assign an INTEGER(KIND=8) or DOUBLE PRECISION argument
    MODULE PROCEDURE UTL_RWORD_D !  Optionally assign a DOUBLE PRECISION argument
    MODULE PROCEDURE UTL_RWORD_R !  Optionally assign a REAL argument
  END INTERFACE
  !
  INTERFACE UTL_VEC2CDMATRIX
    !   A generic interface for converting a vector to a compressed, double-
    !   precision matrix as a CDMATRIX structure
    MODULE PROCEDURE UTL_VEC2CDMATRIX_R
    MODULE PROCEDURE UTL_VEC2CDMATRIX_D
  END INTERFACE
  !
  INTERFACE UTL_WRITE2D
  !   A generic interface for writing a 2-D array
    MODULE PROCEDURE UTL_WRITE2D_D
    MODULE PROCEDURE UTL_WRITE2D_R
    MODULE PROCEDURE UTL_WRITE2D_I
  END INTERFACE
  !
  INTERFACE UTL_WRITEMATRIX
  !   A generic interface for writing a 2-D matrix with row and column names
    MODULE PROCEDURE UTL_WRITEMATRIX_DOUBLE
    MODULE PROCEDURE UTL_WRITEMATRIX_REAL
  END INTERFACE
  !
CONTAINS
  !
  !-----------------------------------------------------------------------------
  FUNCTION UTL_ACHAR(I) RESULT(A)
    !   Return 1-byte character represented by ASCII code I.  This is needed
    !   because gfortran version of ACHAR function does not correctly handle
    !   a hard-coded reference like "ACHAR(2)"
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER, INTENT(IN) :: I
    CHARACTER(LEN=1)    :: A
    !
    A = ACHAR(I)
    RETURN
  END FUNCTION UTL_ACHAR
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ADDNODE(IOUT,KEYWORD,TAIL,VALUE)
    !   Add a node of type LNODE to TAIL of list of type LLIST.  Assign
    !   KEYWORD and VALUE components in newly allocated node.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(IN)    :: IOUT
    CHARACTER(LEN=*), INTENT(IN)    :: KEYWORD
    TYPE (LLIST),     INTENT(INOUT) :: TAIL
    CHARACTER(LEN=*), INTENT(IN)    :: VALUE
    !
    !   Local variables
    INTEGER :: ISTAT, LENSTR
    !
    !   Format statements
    40  FORMAT(/,1X,'Error allocating memory, STATUS = ',I5,' -- (UTL_ADDNODE)')
    !
    !   Store keyword and value
    IF (.NOT. ASSOCIATED(TAIL%LHEAD)) THEN
      ALLOCATE(TAIL%LHEAD,STAT=ISTAT)
        IF (ISTAT > 0) THEN
          WRITE(IOUT,40) ISTAT
          CALL UTL_STOP(' ')
        ENDIF
      TAIL%LTAIL => TAIL%LHEAD
      NULLIFY(TAIL%LTAIL%NEXTNODE)
      NULLIFY(TAIL%LTAIL%STRING)
    ELSE
      ALLOCATE(TAIL%LTAIL%NEXTNODE,STAT=ISTAT)
        IF (ISTAT > 0) THEN
          WRITE(IOUT,40) ISTAT
          CALL UTL_STOP(' ')
        ENDIF
      TAIL%LTAIL => TAIL%LTAIL%NEXTNODE
      NULLIFY(TAIL%LTAIL%NEXTNODE)
      NULLIFY(TAIL%LTAIL%STRING)
    ENDIF
    TAIL%LTAIL%KEYWORD = TRIM(KEYWORD)
    TAIL%LTAIL%VALUE = TRIM(VALUE)
    LENSTR = LEN_TRIM(VALUE)
    ALLOCATE(TAIL%LTAIL%STRING(LENSTR))
    TAIL%LTAIL%NCHAR = LENSTR
    CALL UTL_STRING2ARR(LENSTR,VALUE,TAIL%LTAIL%STRING)
    !
    RETURN
  END SUBROUTINE UTL_ADDNODE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ADDQUOTE(TEXTIN,TEXTOUT)

    !   Subroutine UTL_ADDQUOTE adds quotes to a string of text if it has a
    !   space in it.

    ! -- Arguments are as follows:-
    !        TEXTIN:       the input text
    !        TEXTOUT:      the text with quotes added

    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER (LEN=*), INTENT(IN)   :: TEXTIN
    CHARACTER (LEN=*), INTENT(OUT)  :: TEXTOUT
    !
    !   Local variable
    INTEGER NBB

    IF(INDEX(TRIM(TEXTIN),' ').EQ.0)THEN
      TEXTOUT=TEXTIN
    ELSE
      TEXTOUT(1:1)='"'
      TEXTOUT(2:)=TRIM(TEXTIN)
      NBB=LEN_TRIM(TEXTOUT)+1
      TEXTOUT(NBB:NBB)='"'
    END IF

    RETURN
  END SUBROUTINE UTL_ADDQUOTE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_APPENDLIST(LIST1,LIST2)
    !   Append a list (LIST2) to another list (LIST1) by assigning the NEXTLIST
    !   element of the last LLIST structure in LIST1 to point to the head of
    !   LIST2.
    USE GLOBAL_DATA, ONLY: AMESSAGE
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER :: LIST1
    TYPE (LLIST), POINTER :: LIST2
    !
    !   Local variables
    INTEGER :: ISTAT
    TYPE (LLIST), POINTER :: LLPTR
    !
    !   Format statements
    40  FORMAT(/,1X,'Error allocating memory, STATUS = ',I5,' -- (UTL_APPENDLIST)')
    !
    NULLIFY(LLPTR)
    IF (.NOT. ASSOCIATED(LIST1)) THEN  ! no LLIST structures in linked list LIST1
      ALLOCATE(LIST1,STAT=ISTAT)       !   allocate new structure
      IF (ISTAT > 0) THEN
        WRITE(AMESSAGE,40) ISTAT
        CALL UTL_STOP(' ')
      ENDIF
      LIST1%GROUP = ' '
      NULLIFY(LIST1%LHEAD)        !   nullify pointer to head of list
      NULLIFY(LIST1%LTAIL)        !   nullify pointer to tail of list
      LIST1%NEXTLIST => LIST2     ! Assign NEXTLIST pointer to point to LIST2
    ELSE                        ! list already contains >= 1 entry
      !   Iterate through list to find last LLIST structure in list
      LLPTR => LIST1
      FINDLAST: DO
        IF (.NOT. ASSOCIATED(LLPTR%NEXTLIST)) EXIT FINDLAST
        LLPTR => LLPTR%NEXTLIST
      ENDDO FINDLAST
      LLPTR%NEXTLIST => LIST2     ! Assign NEXTLIST pointer to point to LIST2
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_APPENDLIST
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ARR2CDMATRIX_R(NR,NC,ARR,CDM,IDIMOPT,ANAME)
    !   Convert a 2-D REAL array to a double-precision matrix of type CDMATRIX.
    !   Only nonzero entries in ARR are stored in CDM.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                    INTENT(IN)    :: NR
    INTEGER,                    INTENT(IN)    :: NC
    REAL, DIMENSION(NR,NC),     INTENT(IN)    :: ARR
    TYPE (CDMATRIX),            INTENT(INOUT) :: CDM
    INTEGER,          OPTIONAL, INTENT(IN)    :: IDIMOPT
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: ANAME
    !
    !   Local variables
    INTEGER :: IDIM, K, NNZ
    INTEGER(KIND=8) :: IC, IR
    REAL :: ZERO = 0.0
    DOUBLE PRECISION :: DZERO = 0.0D0
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: NR or NC < 1 in UTL_ARR2CDMATRIX_R')
    !
    IF (NR<1 .OR. NC<1) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    IF (PRESENT(ANAME)) THEN
      CDM%ARRAYNAME = ANAME
    ELSE
      CDM%ARRAYNAME = ' '
    ENDIF
    !
    !   If arrays in CDM are allocated, deallocate them
    IF (ASSOCIATED(CDM%DVAL)) DEALLOCATE(CDM%DVAL)
    IF (ASSOCIATED(CDM%IPOS)) DEALLOCATE(CDM%IPOS)
    IF (ASSOCIATED(CDM%ICOL)) DEALLOCATE(CDM%ICOL)
    !
    !   Determine number of nonzero entries in ARR
    NNZ = 0
    DO IC=1,NC
      DO IR=1,NR
        IF (ARR(IR,IC).NE.ZERO) NNZ = NNZ+1
      ENDDO
    ENDDO
    IDIM = NNZ
    IF (PRESENT(IDIMOPT) .AND. IDIMOPT>NNZ) IDIM = IDIMOPT
    IF (IDIM<1) IDIM = 1
    !
    !   Construct CDM structure:
    !   Assign dimensions of CDM component arrays
    CDM%NR = NR
    CDM%NC = NC
    CDM%NNZ = NNZ
    CDM%IDIM = IDIM
    !   Allocate and populate CDM component arrays
    ALLOCATE(CDM%DVAL(IDIM),CDM%IPOS(IDIM),CDM%ICOL(NC))
    CDM%ICOL = 0
    IF (NNZ==0) THEN
      !   Special case: ARR contains no nonzero entries.  Initialize
      !   arrays appropriately
      CDM%DVAL(1) = DZERO
      CDM%IPOS(1) = 1
    ELSE
      !   Populate DVAL with values from ARR; assign IPOS and ICOL accordingly
      K = 0
      EACHCOL: DO IC=1,NC
        EACHROW: DO IR=1,NR
          IF (ARR(IR,IC).NE.ZERO) THEN
            K = K+1
            CDM%DVAL(K) = DBLE(ARR(IR,IC))
            CDM%IPOS(K) = NR*(IC-1)+IR
            IF (CDM%ICOL(IC)==0) CDM%ICOL(IC) = K
            IF (K==NNZ) EXIT EACHCOL
          ENDIF
        ENDDO EACHROW
      ENDDO EACHCOL
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_ARR2CDMATRIX_R
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ARR2CDMATRIX_D(NR,NC,ARR,CDM,IDIMOPT,ANAME)
    !   Convert a 2-D DOUBLE PRECISION array to a double-precision matrix of
    !   type CDMATRIX.  Only nonzero entries in ARR are stored in CDM.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                            INTENT(IN)    :: NR
    INTEGER,                            INTENT(IN)    :: NC
    DOUBLE PRECISION, DIMENSION(NR,NC), INTENT(IN)    :: ARR
    TYPE (CDMATRIX),                    INTENT(INOUT) :: CDM
    INTEGER,                  OPTIONAL, INTENT(IN)    :: IDIMOPT
    CHARACTER(LEN=*),         OPTIONAL, INTENT(IN)    :: ANAME
    !
    !   Local variables
    INTEGER :: IDIM, K, NNZ
    INTEGER(KIND=8) :: IC, IR
    DOUBLE PRECISION :: DZERO = 0.0D0
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: NR or NC < 1 in UTL_ARR2CDMATRIX_D')
    !
    IF (NR<1 .OR. NC<1) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    IF (PRESENT(ANAME)) THEN
      CDM%ARRAYNAME = ANAME
    ELSE
      CDM%ARRAYNAME = ' '
    ENDIF
    !
    !   If arrays in CDM are allocated, deallocate them
    IF (ASSOCIATED(CDM%DVAL)) DEALLOCATE(CDM%DVAL)
    IF (ASSOCIATED(CDM%IPOS)) DEALLOCATE(CDM%IPOS)
    IF (ASSOCIATED(CDM%ICOL)) DEALLOCATE(CDM%ICOL)
    !
    !   Determine number of nonzero entries in ARR
    NNZ = 0
    DO IC=1,NC
      DO IR=1,NR
        IF (ARR(IR,IC).NE.DZERO) NNZ = NNZ+1
      ENDDO
    ENDDO
    IDIM = NNZ
    IF (PRESENT(IDIMOPT) .AND. IDIMOPT>NNZ) IDIM = IDIMOPT
    IF (IDIM<1) IDIM = 1
    !
    !   Construct CDM structure:
    !   Assign dimensions of CDM component arrays
    CDM%NR = NR
    CDM%NC = NC
    CDM%NNZ = NNZ
    CDM%IDIM = IDIM
    !   Allocate and populate CDM component arrays
    ALLOCATE(CDM%DVAL(IDIM),CDM%IPOS(IDIM),CDM%ICOL(NC))
    CDM%ICOL = 0
    IF (NNZ==0) THEN
      !   Special case: ARR contains no nonzero entries.  Initialize
      !   arrays appropriately
      CDM%DVAL(1) = DZERO
      CDM%IPOS(1) = 1
    ELSE
      !   Populate DVAL with values from ARR; assign IPOS and ICOL accordingly
      K = 0
      EACHCOL: DO IC=1,NC
        EACHROW: DO IR=1,NR
          IF (ARR(IR,IC).NE.DZERO) THEN
            K = K+1
            CDM%DVAL(K) = ARR(IR,IC)
            CDM%IPOS(K) = NR*(IC-1)+IR
            IF (CDM%ICOL(IC)==0) CDM%ICOL(IC) = K
            IF (K==NNZ) EXIT EACHCOL
          ENDIF
        ENDDO EACHROW
      ENDDO EACHCOL
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_ARR2CDMATRIX_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ARR2STRING(IDIM,CHARARR,STRING)
    !   Convert an array of characters to a character string.  If the length
    !   of the non-blank string stored in CHARARR exceeds the length of STRING,
    !   only the characters that can be accommodated are stored in STRING.
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)  :: IDIM  ! Dimension of CHARARR
    CHARACTER(LEN=1), DIMENSION(IDIM), INTENT(IN)  :: CHARARR
    CHARACTER(LEN=*),                  INTENT(OUT) :: STRING
    !
    !   Local variables
    INTEGER :: I, LENSTR, LNB, N
    !
    STRING = ' '
    LENSTR = LEN(STRING)
    !   Find last non-blank character in CHARARR
    DO LNB=IDIM,1,-1
      IF (CHARARR(LNB).NE.' ') EXIT
    ENDDO
    !
    N = MIN(LENSTR,LNB)
    DO I=1,N
      STRING(I:I) = CHARARR(I)
    ENDDO
    RETURN
  END SUBROUTINE UTL_ARR2STRING
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ASSIGN_CDM_CDM(CDMOUT,CDMIN)
    !   Overload "=" assignment operator to assign one CDMATRIX structure equal
    !   to another CDMATRIX structure
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (CDMATRIX), INTENT(INOUT) :: CDMOUT
    TYPE (CDMATRIX), INTENT(IN)    :: CDMIN
    !
    !   If any array pointer components are associated, deallocate the arrays.
    !   This prevents memory leak, which could be a problem if "=" operator is
    !   not overloaded for operation CDMATRIXa=CDMATRIXb.
    IF (ASSOCIATED(CDMOUT%DVAL)) DEALLOCATE(CDMOUT%DVAL)
    IF (ASSOCIATED(CDMOUT%IPOS)) DEALLOCATE(CDMOUT%IPOS)
    IF (ASSOCIATED(CDMOUT%ICOL)) DEALLOCATE(CDMOUT%ICOL)
    !
    !   Allocate arrays
    ALLOCATE (CDMOUT%DVAL(CDMIN%IDIM), CDMOUT%IPOS(CDMIN%IDIM),   &
              CDMOUT%ICOL(CDMIN%NC))
    !
    !   Assign all CDMOUT components equal to corresponding CDMIN components
    CDMOUT%ARRAYNAME = CDMIN%ARRAYNAME
    CDMOUT%IDIM      = CDMIN%IDIM
    CDMOUT%NNZ       = CDMIN%NNZ
    CDMOUT%NR        = CDMIN%NR
    CDMOUT%NC        = CDMIN%NC
    CDMOUT%DVAL      = CDMIN%DVAL
    CDMOUT%IPOS      = CDMIN%IPOS
    CDMOUT%ICOL      = CDMIN%ICOL
    RETURN
  END SUBROUTINE UTL_ASSIGN_CDM_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ASSIGN_DAR_CDM(DAROUT,CDMIN)
    !   Overload "=" assignment operator to assign a 2-D array equal
    !   to a CDMATRIX structure
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (CDMATRIX),                                INTENT(IN)  :: CDMIN
    DOUBLE PRECISION, DIMENSION(CDMIN%NR,CDMIN%NC), INTENT(OUT) :: DAROUT
    !
    !   Local variables
    INTEGER :: I, J, N
    !
    DAROUT = 0.0D0
    DO N=1,CDMIN%NNZ
      I = UTL_GETIROW(CDMIN,N)
      J = UTL_GETICOL(CDMIN,N)
      DAROUT(I,J) = CDMIN%DVAL(N)
    ENDDO
    !
    RETURN
  END SUBROUTINE UTL_ASSIGN_DAR_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CALCWT(IOUT,NAME,STATFLAG,STATISTIC,VALUE,WTMULT,IERR,   &
                            WEIGHT,VAR,CONVERT_STAT)
    !   Calculate WEIGHT and VARiance associated with an observed or reference
    !   VALUE, using a STATISTIC.  STATFLAG must be one of: 'CV', 'SD', 'VAR',
    !   'WT', or 'SQRWT'
    USE GLOBAL_DATA, ONLY: BIGDOUBLE, LENDNAM
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                INTENT(IN)    :: IOUT
    CHARACTER(LEN=LENDNAM), INTENT(IN)    :: NAME
    CHARACTER(LEN=6),       INTENT(IN)    :: STATFLAG
    DOUBLE PRECISION,       INTENT(IN)    :: STATISTIC
    DOUBLE PRECISION,       INTENT(IN)    :: VALUE
    DOUBLE PRECISION,       INTENT(IN)    :: WTMULT
    INTEGER,                INTENT(INOUT) :: IERR
    DOUBLE PRECISION,       INTENT(OUT)   :: WEIGHT
    DOUBLE PRECISION,       INTENT(OUT)   :: VAR
    LOGICAL, OPTIONAL,      INTENT(IN)    :: CONVERT_STAT
    !
    !   Local variables
    DOUBLE PRECISION :: SD
    DOUBLE PRECISION :: SQCONVERT
    !
    !   Format statements
    100 FORMAT(1X,'*** Error: STATFLAG cannot be CV for dependent "',A,'"', &
                  ' because observed value is zero')
    150 FORMAT(1X,'*** ERROR: Variance = 0.0 for "',A,'"')
    200 FORMAT(1X,'*** ERROR: No STATFLAG entered for "',A,'"')
    250 FORMAT(1X,'*** ERROR: Invalid STATFLAG "',A,'" for "',A,'"')
    300 FORMAT(1X,'*** ERROR: Weight not assigned for "',A,'"')
    !
    SQCONVERT = LOG(10.D0) * LOG(10.D0)
    !
    IF (STATISTIC .NE. BIGDOUBLE) THEN
      !   STATISTIC has been read, so interpret it according to STATFLAG
      IF (UTL_SAMENAME(STATFLAG,'CV')) THEN
        !   STATISTIC is a coefficient of variation
        SD = STATISTIC*VALUE
        IF (SD .NE. 0.0D0) THEN
          IF(PRESENT(CONVERT_STAT)) THEN
            IF(CONVERT_STAT) THEN
              VAR = (LOG((SD/VALUE)*(SD/VALUE)+1.D0))/SQCONVERT
            ELSE
              VAR = SD * SD
            ENDIF
          ELSE
            VAR = SD*SD
          ENDIF
          WEIGHT = 1.0D0/VAR
        ELSE
          IERR = IERR+1
          WRITE(IOUT,100) TRIM(NAME)
        ENDIF
      ELSEIF (UTL_SAMENAME(STATFLAG,'SD')) THEN
        !   STATISTIC is a standard deviation
        SD = STATISTIC
        IF (SD .NE. 0.0D0) THEN
          IF(PRESENT(CONVERT_STAT)) THEN
            IF(CONVERT_STAT) THEN
              VAR = (LOG((SD/VALUE)*(SD/VALUE)+1.D0))/SQCONVERT
            ELSE
              VAR = SD * SD
            ENDIF
          ELSE
            VAR = SD*SD
          ENDIF
          WEIGHT = 1.0D0/VAR
        ELSE
          IERR = IERR+1
          WRITE(IOUT,150) TRIM(NAME)
        ENDIF
      ELSEIF (UTL_SAMENAME(STATFLAG,'VAR')) THEN
        !   STATISTIC is a variance
        VAR = STATISTIC
        IF (VAR .NE. 0.0D0) THEN
          IF(PRESENT(CONVERT_STAT)) THEN
            IF(CONVERT_STAT) THEN
              SD = SQRT(VAR)
              VAR = (LOG((SD/VALUE)*(SD/VALUE)+1.D0))/SQCONVERT
            ENDIF
          ENDIF
          WEIGHT = 1.0D0/VAR
        ELSE
          IERR = IERR+1
          WRITE(IOUT,150) TRIM(NAME)
        ENDIF
      ELSEIF (UTL_SAMENAME(STATFLAG,'WT')) THEN
        !   STATISTIC is a weight (1/variance)
        WEIGHT = STATISTIC
      ELSEIF (UTL_SAMENAME(STATFLAG,'SQRWT')) THEN
        !   STATISTIC is square root of weight (1/s.d.)
        WEIGHT = STATISTIC*STATISTIC
      ELSEIF (STATFLAG .EQ.' ') THEN
        IERR = IERR+1
        WRITE(IOUT,200) TRIM(NAME)
      ELSE
        IERR = IERR+1
        WRITE(IOUT,250) TRIM(STATFLAG),TRIM(NAME)
      ENDIF
    ELSE
      !   STATISTIC has not been read
      IERR = IERR+1
      WEIGHT = BIGDOUBLE
      VAR = BIGDOUBLE
      WRITE(IOUT,300) TRIM(NAME)
      RETURN
    ENDIF
    WEIGHT = WEIGHT*WTMULT
    VAR = 1.0D0/WEIGHT
    !
    RETURN
  END SUBROUTINE UTL_CALCWT
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CASE(WORDIN,WORDOUT,ICASE)
    !   CONVERT A CHARACTER STRING TO ALL UPPER (ICASE > 0) OR ALL
    !   LOWER (ICASE < 0) CASE.  IF ICASE = 0, NO CASE CONVERSION IS DONE.
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),  INTENT(IN)  :: WORDIN
    CHARACTER(LEN=*),  INTENT(OUT) :: WORDOUT
    INTEGER, OPTIONAL, INTENT(IN)  :: ICASE   ! Default = 1
    !
    !   Local variables
    INTEGER :: ICAS, IDIFF, K, LENGNB, LENGOUT
    !
    ICAS = 1
    IF (PRESENT(ICASE)) ICAS = ICASE
    !   DETERMINE LENGTH OF STRING VARIABLES
    LENGOUT = LEN(WORDOUT)

    !   DETERMINE IF WORDOUT IS LONG ENOUGH TO CONTAIN NON-BLANK LENGTH
    !   OF WORDIN
    LENGNB = LEN_TRIM(WORDIN)
    IF (LENGNB.GT.LENGOUT) CALL UTL_STOP('STRING-LENGTH ERROR IN UTL_CASE')
    !
    WORDOUT = WORDIN
    IDIFF=ICHAR('a')-ICHAR('A')
    IF (ICAS.GT.0) THEN
    !     CONVERT STRING TO UPPER CASE
      DO 10 K=1,LENGNB
        IF(WORDIN(K:K).GE.'a' .AND. WORDIN(K:K).LE.'z')   &
            WORDOUT(K:K)=CHAR(ICHAR(WORDIN(K:K))-IDIFF)
10    CONTINUE
    !
    ELSEIF (ICAS.LT.0) THEN
    !     CONVERT STRING TO LOWER CASE
      DO 20 K=1,LENGNB
        IF(WORDIN(K:K).GE.'A' .AND. WORDIN(K:K).LE.'Z')   &
            WORDOUT(K:K)=CHAR(ICHAR(WORDIN(K:K))+IDIFF)
20    CONTINUE
    !
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_CASE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CASETRANS(STRING,HI_OR_LO)
    !
    ! -- Subroutine UTL_CASETRANS converts a string to upper or lower case.
    !
    ! -- Arguments are as follows:-
    !      string:      contains the string whose case must be changed
    !      hi_or_lo:  must be either 'lo' or 'hi' to indicate
    !                 change of case direction.
    !
    ! -- Revision history:-
    !       June-November, 1995: version 1.
    !
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER (LEN=*), INTENT(INOUT)        :: STRING
    CHARACTER (LEN=*), INTENT(IN)           :: HI_OR_LO
    !
    !   Local variables
    CHARACTER                               :: ALO, AHI
    INTEGER                                 :: INC,I
    !
    IF(HI_OR_LO.EQ.'lo') THEN
      ALO='A'; AHI='Z'; INC=IACHAR('a')-IACHAR('A')
    ELSE IF(HI_OR_LO.eq.'hi') THEN
      ALO='a'; AHI='z'; INC=IACHAR('A')-IACHAR('a')
    ELSE
      CALL UTL_SUBERROR('UTL_CASETRANS')
    ENDIF
    !
    DO I=1,LEN_TRIM(STRING)
      IF((STRING(I:I).GE.ALO).AND.(STRING(I:I).LE.AHI)) &
      STRING(I:I)=ACHAR(IACHAR(STRING(I:I))+INC)
    END DO
    !
    RETURN
    !
  END SUBROUTINE UTL_CASETRANS
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   Subroutines comprising the generic subroutine UTL_CHAR2NUM
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------

  ! -- The subroutines comprising char2num convert a string to either an
  !    integer, a real number, or a double precision number.

  ! -- Arguments are as follows:-
  !      ifail:   indicates failure if returned as non-zero
  !      string:  a character string containing a number
  !      num:     an integer (for a2i), real (for a2r), double precision (for
  !               a2d), or logical (for a2l) number extracted from the string.

  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CHAR2NUM_a2d(ifail,string,num)
    IMPLICIT NONE
    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    double precision, intent(out)   :: num
    character (len=10)              :: afmt

    if(string.eq.' ') go to 10
    ! Disallow 'D' and 'E' as numbers, even though they are interpretable
    ! as zero -- ERB 1/7/08
    IF (UTL_SAMENAME(STRING,'D') .OR. UTL_SAMENAME(STRING,'E')) GOTO 10
    ifail=0
    afmt='(f    .0)'
    WRITE(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

    10 ifail=1
    return

  end SUBROUTINE UTL_CHAR2NUM_a2d
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CHAR2NUM_a2i(ifail,string,num)
    IMPLICIT NONE
    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    integer, intent(out)            :: num
    character (len=10)              :: afmt

    if(string.eq.' ') go to 10
    ifail=0
    afmt='(i    )'
    WRITE(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

    10 ifail=1
    return

  end SUBROUTINE UTL_CHAR2NUM_a2i
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CHAR2NUM_a2l(ifail,string,num)
    IMPLICIT NONE
    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    logical, intent(out)            :: num
    character (len=10)              :: afmt

    if(string.eq.' ') go to 10
    ifail=0
    afmt='(l    )'
    WRITE(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

    10 ifail=1
    return

  end SUBROUTINE UTL_CHAR2NUM_a2l
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CHAR2NUM_a2r(ifail,string,num)
    IMPLICIT NONE
    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    real, intent(out)               :: num
    character (len=10)              :: afmt

    if(string.eq.' ') go to 10
    ! Disallow 'D' and 'E' as numbers, even though they are interpretable
    ! as zero -- ERB 1/7/08
    IF (UTL_SAMENAME(STRING,'D') .OR. UTL_SAMENAME(STRING,'E')) GOTO 10
    ifail=0
    afmt='(f    .0)'
    WRITE(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

    10 ifail=1
    return

  end SUBROUTINE UTL_CHAR2NUM_a2r
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   End of subroutines comprising the generic subroutine CHAR2NUM
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CHECK_NAMES(IOUT,NUMNAMES,NAMEARRAY)
    !   Left-justify the name in each element of NAMEARRAY, then check each
    !   name for conformance with the JUPITER naming convention.
    !   The convention is that:
    !   (1) The first character must be a letter; and
    !   (2) All characters after the first letter must be a letter, digit,
    !       or member of the set: "_", ".", ":", "&", "#", "@"
    !       (underscore, dot, colon, ampersand, number sign, at symbol).
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: NUMNAMES
    CHARACTER(LEN=*), DIMENSION(NUMNAMES), INTENT(INOUT) :: NAMEARRAY
    !
    !   Local variables
    INTEGER :: I, ICONV, IERR, ILOC, L, LENNAME
    CHARACTER(LEN=68) :: LEGAL
    DATA LEGAL   &
    !            1         2         3         4         5         6         7
    !   1234567890123456789012345678901234567890123456789012345678901234567890
      /'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.:&#@'/
    !
    !   Format statements
    !
    100 FORMAT(1X,'ERROR: Name "',A,   &
        '" does not comply with naming convention (UTL_CHECK_NAMES)')
    !
    IERR = 0
    !ICONV = 63  ! Fortran naming convention
    ICONV = 68  ! Jupiter naming convention
    !
    IF (NUMNAMES>0) THEN
      NAMES: DO I=1,NUMNAMES
        IF (NAMEARRAY(I).NE.' ') THEN
          NAMEARRAY(I) = ADJUSTL(NAMEARRAY(I))
          LENNAME = LEN_TRIM(NAMEARRAY(I))
          ILOC = INDEX(LEGAL(1:52),NAMEARRAY(I)(1:1))
          IF (ILOC==0) THEN
            IERR = IERR+1
            WRITE(IOUT,100)TRIM(NAMEARRAY(I))
            CYCLE NAMES
          ENDIF
          IF (LENNAME>1) THEN
            DO L=2,LENNAME
            ILOC = INDEX(LEGAL(1:ICONV),NAMEARRAY(I)(L:L))
              IF (ILOC==0) THEN
                IERR = IERR+1
                WRITE(IOUT,100)TRIM(NAMEARRAY(I))
                CYCLE NAMES
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO NAMES
    ENDIF
    !
    IF (IERR>0) CALL UTL_STOP(' ')
    !
    RETURN
    !
  END SUBROUTINE UTL_CHECK_NAMES
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CHISQ(IDOF,CHISQL,CHISQU)
    ! DETERMINE CHI SQUARED FOR N DEGREES OF FREEDOM
    ! FOR A TWO-SIDED SIGNIFICANCE LEVEL OF 0.05
    !
    !   Argument-list variables
    INTEGER,          INTENT(IN)  :: IDOF
    DOUBLE PRECISION, INTENT(OUT) :: CHISQL
    DOUBLE PRECISION, INTENT(OUT) :: CHISQU
    !
    !   Local variables
    DOUBLE PRECISION, DIMENSION(39) :: TABLE975
    DOUBLE PRECISION, DIMENSION(39) :: TABLE025
    INTEGER, DIMENSION(39) :: ITABLE
    INTEGER :: I
    !
    !     ------------------------------------------------------------------
    DATA (ITABLE(I),I=1,39)/1, 2, 3, 4, 5, &
           6,  7,  8,  9, 10, 11, 12,    &
          13, 14, 15, 16, 17, 18, 19,    &
          20, 21, 22, 23, 24, 25, 26,    &
          27, 28, 29, 30, 40, 50, 60,    &
          70, 80, 90, 100, 120, 240/
    DATA (TABLE975(I),I=1,39)/ &
                    0.000982D0, 0.05064D0, 0.2158D0, 0.4844D0, 0.8312D0,   &
          1.237D0, 1.690D0, 2.180D0, 2.700D0, 3.247D0, 3.816D0, 4.404D0,   &
          5.009D0, 5.629D0, 6.262D0, 6.908D0, 7.564D0, 8.231D0, 8.907D0,   &
          9.591D0, 10.28D0, 10.98D0, 11.69D0, 12.40D0, 13.12D0, 13.84D0,   &
          14.57D0, 15.31D0, 16.06D0, 16.79D0, 24.43D0, 32.36D0, 40.48D0,   &
          48.76D0, 57.15D0, 65.65D0, 74.22D0, 91.57D0, 198.98D0/
    DATA (TABLE025(I),I=1,39)/5.024D0, 7.378D0, 9.348D0, 11.14D0, 12.83D0,   &
          14.45D0, 16.01D0, 17.53D0, 19.02D0, 20.48D0, 21.92D0, 23.34D0,   &
          24.74D0, 26.12D0, 27.49D0, 28.85D0, 30.19D0, 31.53D0, 32.85D0,   &
          34.17D0, 35.48D0, 36.78D0, 38.08D0, 39.36D0, 40.65D0, 41.92D0,   &
          43.19D0, 44.46D0, 45.72D0, 46.98D0, 59.34D0, 71.42D0, 83.30D0,   &
          95.02D0, 106.63D0, 118.14D0, 129.56D0, 152.21D0, 284.80D0/
    !     ------------------------------------------------------------------
    !
    IF (IDOF .LE. 30) THEN
      CHISQU=TABLE975(IDOF)
      CHISQL=TABLE025(IDOF)
      RETURN
    ENDIF
    DO I=31,39
      IF(IDOF .LE. ITABLE(I)) THEN
        CHISQU = TABLE975(I-1)+(TABLE975(I)-TABLE975(I-1))*                &
             DBLE(IDOF-ITABLE(I-1))/DBLE(ITABLE(I)-ITABLE(I-1))
        CHISQL = TABLE025(I-1)+(TABLE025(I)-TABLE025(I-1))*                &
             DBLE(IDOF-ITABLE(I-1))/DBLE(ITABLE(I)-ITABLE(I-1))
        RETURN
      ENDIF
    ENDDO
    CHISQU=TABLE975(39)
    CHISQL=TABLE025(39)
    RETURN
  END SUBROUTINE UTL_CHISQ
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_COLLBL(NCOL,NSPACE,NCPL,NDIG,IOUT,COLNAM)
    !   ******************************************************************
    !   LABEL THE COLUMNS OF MATRIX PRINTOUT WITH NAMES IN COLNAM
    !      NCOL IS THE NUMBER OF COLUMNS IN THE MATRIX
    !      NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
    !      NCPL IS NUMBER OF COLUMNS PER LINE
    !      NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
    !      IOUT IS OUTPUT UNIT
    !
    !      NLBL1 IS THE START COLUMN (NUMBER)
    !      NLBL2 IS THE STOP COLUMN (NUMBER)
    !   ******************************************************************
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN) :: NCOL
    INTEGER,                           INTENT(IN) :: NSPACE
    INTEGER,                           INTENT(IN) :: NCPL
    INTEGER,                           INTENT(IN) :: NDIG
    INTEGER,                           INTENT(IN) :: IOUT
    CHARACTER(LEN=*), DIMENSION(NCOL), INTENT(IN) :: COLNAM
    !
    !   Local variables
    INTEGER :: I, IND, J, J1, J2, LCN, LENPN, MINCOLSP, N, NBF, NBF2,   &
               NLBL, NLBL1, NLBL2, NTOT, NWRAP
    CHARACTER(LEN=1) :: DOT, SPACE
    CHARACTER(LEN=MAX_STRING_LEN) :: BF
    DATA DOT,SPACE/'.',' '/
    !
    !   Format statements
    !
  1 FORMAT(1X)
 31 FORMAT(1X,A)
 51 FORMAT(1X,200A1)
    !
    NLBL1 = 1
    NLBL2 = NCOL
    !
    !   Calculate # of columns to be printed (NLBL), width
    !   of a line (NTOT), number of lines (NWRAP).
    WRITE(IOUT,1)
    NLBL = NLBL2-NLBL1+1
    N = NLBL
    IF(NLBL.GT.NCPL) N = NCPL
    LCN = LEN(COLNAM)
    LENPN = MAX(LCN,NDIG)
    MINCOLSP = LENPN+1
    NTOT = NSPACE+N*MINCOLSP
    IND = (NDIG-LCN)/2
    NWRAP = (NLBL-1)/NCPL+1
    J1 = NLBL1-NCPL
    J2 = NLBL1-1
    !
    !   Build and print each line
    DO N=1,NWRAP
      !
      !   Clear the buffer (BF).
      BF = SPACE
      NBF = NSPACE+1+IND-MINCOLSP
      !
      !   Determine first (J1) and last (J2) column # for this line.
      J1=J1+NCPL
      J2=J2+NCPL
      IF(J2.GT.NLBL2) J2=NLBL2
      !   Load the column labels into the buffer.
      DO J=J1,J2
        NBF=NBF+MINCOLSP
        NBF2 = NBF+LENPN-1
        BF(NBF:NBF2) = COLNAM(J)
      ENDDO
      !
      !   Print the contents of the buffer (i.e. print the line).
      WRITE(IOUT,31) BF(1:NBF2)
      !
    ENDDO
    !
    !   Print a line of dots (for esthetic purposes only).
    WRITE(IOUT,51) (DOT,I=1,NTOT)
    !
    RETURN
  END SUBROUTINE UTL_COLLBL
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_COLNO(NLBL1,NLBL2,NSPACE,NCPL,NDIG,IOUT,NDOTS)
    !     ******************************************************************
    !     OUTPUT COLUMN NUMBERS ABOVE A MATRIX PRINTOUT
    !        NLBL1 IS THE START COLUMN LABEL (NUMBER)
    !        NLBL2 IS THE STOP COLUMN LABEL (NUMBER)
    !        NSPACE IS NUMBER OF BLANK SPACES TO LEAVE AT START OF LINE
    !        NCPL IS NUMBER OF COLUMN NUMBERS PER LINE
    !        NDIG IS NUMBER OF CHARACTERS IN EACH COLUMN FIELD
    !        IOUT IS OUTPUT UNIT NUMBER
    !        NDOTS IS THE NUMBER OF DOTS TO ADD AFTER THE LAST COLUMN NUMBER
    !     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN) :: NLBL1
    INTEGER, INTENT(IN) :: NLBL2
    INTEGER, INTENT(IN) :: NSPACE
    INTEGER, INTENT(IN) :: NCPL
    INTEGER, INTENT(IN) :: NDIG
    INTEGER, INTENT(IN) :: IOUT
    INTEGER, INTENT(IN), OPTIONAL :: NDOTS
    !
    !   Local variables
    INTEGER :: I, I1, I2, I3, J, J1, J2, N, NBF, NDIGP1, NDOT, NLBL, NTOT,   &
               NWRAP
    CHARACTER(LEN=1) :: DOT,SPACE
    CHARACTER(LEN=1), DIMENSION(130) :: BF
    CHARACTER(LEN=1), DIMENSION(10) :: DG
    DATA DG(1),DG(2),DG(3),DG(4),DG(5),DG(6),DG(7),DG(8),DG(9),DG(10)/   &
         '0','1','2','3','4','5','6','7','8','9'/
    DATA DOT,SPACE/'.',' '/
    !
    ! Format statements
    10 FORMAT(1X)
    30 FORMAT(1X,130A1)
    !
    NDIGP1 = NDIG+1
    NDOT = 0
    IF (PRESENT(NDOTS)) NDOT = NDOTS
    !C1------CALCULATE # OF COLUMNS TO BE PRINTED (NLBL), WIDTH
    !C1------OF A LINE (NTOT), NUMBER OF LINES (NWRAP).
    WRITE(IOUT,10)
    NLBL=NLBL2-NLBL1+1
    N=NLBL
    IF(NLBL.GT.NCPL) N=NCPL
    NTOT=NSPACE+N*NDIGP1
    IF(NTOT.LE.130) THEN
      NWRAP=(NLBL-1)/NCPL + 1
      J1=NLBL1-NCPL
      J2=NLBL1-1
      !
      !C2------BUILD AND PRINT EACH LINE
      DO N=1,NWRAP
      !
      !C3------CLEAR THE BUFFER (BF).
        DO I=1,130
          BF(I)=SPACE
        ENDDO
        NBF=NSPACE
        !
        !C4------DETERMINE FIRST (J1) AND LAST (J2) COLUMN # FOR THIS LINE.
        J1=J1+NCPL
        J2=J2+NCPL
        IF(J2.GT.NLBL2) J2=NLBL2
        !C5------LOAD THE COLUMN #'S INTO THE BUFFER.
        COLNUM: DO J=J1,J2
          NBF=NBF+NDIGP1
          I2=J/10
          I1=J-I2*10+1
          BF(NBF)=DG(I1)
          IF(I2.EQ.0) CYCLE COLNUM
          I3=I2/10
          I2=I2-I3*10+1
          BF(NBF-1)=DG(I2)
          IF(I3.EQ.0) CYCLE COLNUM
          BF(NBF-2)=DG(I3+1)
        ENDDO COLNUM
        !
        !C6------PRINT THE CONTENTS OF THE BUFFER (I.E. PRINT THE LINE).
        WRITE(IOUT,30) (BF(I),I=1,NBF)
        !
      ENDDO
      !
      !C7------PRINT A LINE OF DOTS (FOR ESTHETIC PURPOSES ONLY).
    ENDIF
    NTOT = NTOT+NDOT
    IF(NTOT.GT.130) NTOT=130
    WRITE(IOUT,30) (DOT,I=1,NTOT)
    !
    !C8------RETURN
    RETURN
  END SUBROUTINE UTL_COLNO
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_COMBINESQMATRIX(A,B,C,CNAME)
    !   Use two square matrices (A, B) to generate a third square matrix as
    !   a CDMATRIX structure (C) containing the elements of A in the
    !   upper left section and the elements of B in the lower right section.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    TYPE (CDMATRIX),            INTENT(IN)    :: A
    TYPE (CDMATRIX),            INTENT(IN)    :: B
    TYPE (CDMATRIX),            INTENT(INOUT) :: C
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: CNAME
    !
    !   Local variables
    INTEGER :: I, ICL, NCA, NCB, NRA, NRB, NNZA, NNZB, NNZC
    INTEGER(KIND=8) :: IC, IR, NRCC
    !
    !   Format statements
    100 FORMAT(1X,'Programmer error: Matrix(ces) input to',   &
        ' UTL_COMBINESQMATRIX is(are) not square')
    !
    !   Find matrix dimensions, and check that input matrices are square
    NRA = A%NR
    NCA = A%NC
    NRB = B%NR
    NCB = B%NC
    IF (NRA.NE.NCA .OR. NRB.NE.NCB) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    NNZA = A%NNZ
    NNZB = B%NNZ
    NNZC = NNZA+NNZB
    NRCC = NRA+NRB
    !
    !   Construct and initialize combined matrix
    IF (ASSOCIATED(C%DVAL)) DEALLOCATE(C%DVAL)
    IF (ASSOCIATED(C%IPOS)) DEALLOCATE(C%IPOS)
    IF (ASSOCIATED(C%ICOL)) DEALLOCATE(C%ICOL)
    ALLOCATE(C%DVAL(NNZC), C%IPOS(NNZC), C%ICOL(NRCC))
    IF (PRESENT(CNAME)) THEN
      C%ARRAYNAME = CNAME
    ELSE
      C%ARRAYNAME = ' '
    ENDIF
    C%IDIM = NNZC
    C%NNZ = NNZC
    C%NR = NRCC
    C%NC = NRCC
    C%DVAL = 0.0D0
    C%IPOS = 0
    C%ICOL = 0
    !
    !   Populate upper left part of C with elements from A
    DO I=1,NCA
      C%ICOL(I) = A%ICOL(I)
    ENDDO
    DO I=1,NNZA
      C%DVAL(I) = A%DVAL(I)
      IC = UTL_GETICOL(A,I)
      IR = UTL_GETIROW(A,I)
      C%IPOS(I) = NRCC*(IC-1)+IR
    ENDDO
    !
    !   Populate lower right part of C with elements from B
    DO I=1,NCB
      C%ICOL(NCA+I) = NNZA+B%ICOL(I)
    ENDDO
    DO I=1,NNZB
      ICL = NNZA+I
      C%DVAL(ICL) = B%DVAL(I)
      IC = UTL_GETICOL(B,I)+NCA
      IR = UTL_GETIROW(B,I)+NRA
      C%IPOS(ICL) = NRCC*(IC-1)+IR
    ENDDO
    !
    RETURN
  END SUBROUTINE UTL_COMBINESQMATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_COMPRESSLINE(LINE)
    ! -- SUBROUTINE UTL_COMPRESSLINE COMPRESSES AN INSTRUCTION LINE BY REMOVING
    ! -- EXCESS BLANK CHARACTERS
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER (LEN=*), INTENT(INOUT) :: LINE
    !
    !   Local variables
    INTEGER NBLC,J
    !
    IF(LINE.EQ.' ') RETURN
    10 NBLC=LEN_TRIM(LINE)
    J=INDEX(LINE(1:NBLC),'  ')
    IF(J.NE.0) THEN
      LINE(j+1:)=ADJUSTL(LINE(j+1:))
      GO TO 10
    END IF
    !
    RETURN
  END SUBROUTINE UTL_COMPRESSLINE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CONSTRUCTCDMATRIX(IDIM,NR,NC,CDM,ANAME)
    !   Construct CDM as a CDMATRIX structure.  Any associated pointer (array)
    !   components of CDM are deallocated.  Allocate DVAL and IPOS component
    !   arrays to IDIM; allocate ICOL array; and initialize all arrays to zero.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                    INTENT(IN)    :: IDIM   ! Dimension of DVAL and IPOS components of CDM
    INTEGER,                    INTENT(IN)    :: NR     ! Number of rows
    INTEGER,                    INTENT(IN)    :: NC     ! Number of columns
    TYPE (CDMATRIX),            INTENT(INOUT) :: CDM    ! Structure to be constructed
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: ANAME  ! Name to be assigned to CDM
    !   NNZ is Number of nonzero entries, and minimum dimension of DVAL and
    !   IPOS components of CDM
    !
    !   Local variables
    INTEGER :: ISTAT, KERR
    !
    !   Format statements
    100 FORMAT(1X,I1,1X,'Error(s) encountered while allocating memory',   &
        ' (UTL_CONSTRUCTCDMATRIX)')
    !
    ISTAT = 0
    KERR = 0
    IF (PRESENT(ANAME)) THEN
      CDM%ARRAYNAME = ANAME
    ELSE
      CDM%ARRAYNAME = ' '
    ENDIF
    !
    CDM%IDIM = IDIM
    CDM%NNZ = 0
    CDM%NR = NR
    CDM%NC = NC
    IF (ASSOCIATED(CDM%DVAL)) THEN
      DEALLOCATE(CDM%DVAL)
    ENDIF
    IF (ASSOCIATED(CDM%IPOS)) THEN
      DEALLOCATE(CDM%IPOS)
    ENDIF
    IF (ASSOCIATED(CDM%ICOL)) THEN
      DEALLOCATE(CDM%ICOL)
    ENDIF
    !
    ALLOCATE (CDM%DVAL(IDIM),STAT=ISTAT)
    IF (ISTAT.NE.0) KERR = KERR+1
    ALLOCATE (CDM%IPOS(IDIM),STAT=ISTAT)
    IF (ISTAT.NE.0) KERR = KERR+1
    ALLOCATE (CDM%ICOL(NC),STAT=ISTAT)
    IF (ISTAT.NE.0) KERR = KERR+1
    IF (KERR>0) THEN
      WRITE(*,100)KERR
      CALL UTL_STOP(' ')
    ENDIF
    CDM%DVAL = 0.0D0
    CDM%IPOS = 0
    CDM%ICOL = 0
    !
    RETURN
  END SUBROUTINE UTL_CONSTRUCTCDMATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_CONSTRUCTDMATRIX(NR,NC,DM,ANAME)
    !   Construct a DMATRIX-type structure, allocating the double-precision
    !   array component as (NR,NC).  Initialize the array to 0.0D0.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                    INTENT(IN)    :: NR  ! Number of rows
    INTEGER,                    INTENT(IN)    :: NC  ! Number of columns
    TYPE (DMATRIX),             INTENT(INOUT) :: DM
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: ANAME
    !
    !   Local variables
    INTEGER        :: IFAIL
    !
    IF (PRESENT(ANAME)) THEN
      DM%ARRAYNAME = ANAME
    ELSE
      DM%ARRAYNAME = ' '
    ENDIF
    DM%NR = NR
    DM%NC = NC
    IF (ASSOCIATED(DM%DVAL)) DEALLOCATE(DM%DVAL)
    ALLOCATE(DM%DVAL(NR,NC),STAT=IFAIL)
    IF (IFAIL .NE. 0) THEN
      AMESSAGE = 'Subroutine UTL_CONSTRUCTDMATRIX:  Unable to allocate'//   &
                 ' memory for DMATRIX structure'
      CALL UTL_STOP(' ')
      RETURN
    ENDIF
    DM%DVAL = 0.0D0
    RETURN
  END SUBROUTINE UTL_CONSTRUCTDMATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_COPYLIST(HEAD,COPY)
    ! Make a copy of data contained in a linked list
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN
    USE DATATYPES
    IMPLICIT NONE
    INTEGER, PARAMETER :: IDIM = MAX_STRING_LEN
    !   Argument-list and result variables
    TYPE (LLIST), POINTER :: HEAD
    TYPE (LLIST), POINTER :: COPY
    !   Local variables
    INTEGER :: ISTAT
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    TYPE (LLIST), POINTER :: TAIL
    INTEGER               :: I, KL, KL2
    INTEGER               :: IOUT = 0
    INTEGER :: J, LENSTR
    CHARACTER(LEN=1), DIMENSION(IDIM) :: CHARARR
    CHARACTER(LEN=IDIM)               :: STRING
    !
    CALL TYP_DEALLOC(COPY)
    !
    NULLIFY(PTR,PIPTR)
    !   Find the number of lists and compare to NDIM
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    !
    !   Traverse the lists and store CHARACTER values associated with keyword
    PTR => HEAD
    I = 0
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      CALL UTL_NEWLIST(PTR%GROUP,COPY,IOUT,TAIL)
      KL2 = KL2+1
      I = I+1
      IF (I>KL) THEN
        EXIT OUTER
      ENDIF
      PIPTR => PTR%LHEAD
      !   Traverse the entries for this list
      INNER: DO
        IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
        LENSTR = SIZE(PIPTR%STRING)
        DO J=1,IDIM
          IF (J <= LENSTR) THEN
            CHARARR(J) = PIPTR%STRING(J)
          ELSE
            CHARARR(J) = ' '
          ENDIF
        ENDDO
        CALL UTL_ARR2STRING(IDIM,CHARARR,STRING)
        CALL UTL_ADDNODE(IOUT,PIPTR%KEYWORD,TAIL,STRING)  
        PIPTR => PIPTR%NEXTNODE     ! Get pointer to next entry
      ENDDO INNER
      PTR => PTR%NEXTLIST           ! Get pointer to next list
    ENDDO OUTER
    RETURN
  END SUBROUTINE UTL_COPYLIST
  !-----------------------------------------------------------------------------
  FUNCTION UTL_COUNTSUBS(STR,SUBSTR) RESULT(K)
    !   Return count of number of occurrences of substring SUBSTR in string STR
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*), INTENT(IN) :: STR
    CHARACTER(LEN=*), INTENT(IN) :: SUBSTR
    INTEGER                      :: K
    !
    !   Local variables
    INTEGER :: I, LAST, LSTR, LSUB
    !
    K = 0
    LSTR = LEN(STR)
    LSUB = LEN(SUBSTR)
    IF (LSUB>LSTR) GOTO 900
    !
    LAST = LSTR-LSUB+1
    DO I=1,LAST
      IF (STR(I:I+LSUB-1) == SUBSTR) K = K+1
    ENDDO
    !
    900 CONTINUE
    RETURN
  END FUNCTION UTL_COUNTSUBS
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_COVMAT(IOUT,NCOVMAT,NMEM,NGPS,COVMATARR,COVMATNAM,   &
                        GROUP,GROUPNAM,MEMNAM,VARIANCE,WTCORR,CDCOVMAT)
    !   Build a variance-covariance matrix from a set of group variance-
    !   covariance matrices and an array of variances
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB, LENDNAM
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN)    :: IOUT
    INTEGER,                                 INTENT(IN)    :: NCOVMAT
    INTEGER,                                 INTENT(IN)    :: NMEM
    INTEGER,                                 INTENT(IN)    :: NGPS
    TYPE (CDMATRIX), DIMENSION(NCOVMAT),     INTENT(IN)    :: COVMATARR ! Array of all group variance-covariance matrices
    CHARACTER(LEN=12), DIMENSION(NGPS),      INTENT(IN)    :: COVMATNAM ! (Blank if group is not correlated)
    CHARACTER(LEN=12), DIMENSION(NMEM),      INTENT(IN)    :: GROUP
    CHARACTER(LEN=12), DIMENSION(NGPS),      INTENT(IN)    :: GROUPNAM
    CHARACTER(LEN=LENDNAM), DIMENSION(NMEM), INTENT(IN)    :: MEMNAM
    DOUBLE PRECISION, DIMENSION(NMEM),       INTENT(IN)    :: VARIANCE  ! Variance (uncorrelated)
    LOGICAL, DIMENSION(NMEM),                INTENT(IN)    :: WTCORR
    TYPE (CDMATRIX),                         INTENT(INOUT) :: CDCOVMAT
    !
    !   Local variables
    INTEGER :: I, ICOR1, ICOR2, IUNCOR1, IUNCOR2  ! Member subscripts
    INTEGER :: J, JGP   ! Group subscripts
    INTEGER :: M        ! Covariance-matrix subscript
    INTEGER :: IDIM, IERR
    CHARACTER(LEN=12) :: CVM1, GPNAM, NEXTGROUP, THISGROUP
    TYPE (CDMATRIX) :: CDMTEMP1, CDMTEMP2
    LOGICAL, DIMENSION(NCOVMAT) :: CVMUSED
    !
    !   Format statements
    200 FORMAT(1X,'WARNING: No match found for group "',A,'" (UTL_COVMAT)')
    250 FORMAT(1X,'ERROR: No match found for CovMatrix "',A,   &
        '" (UTL_COVMAT)')
    300 FORMAT(/,1X,'ERROR: Number of contiguous members listed',   &
        ' for group "',A,'"',/,3X,'is ',I6,',',   &
        ' which is inconsistent with dimension (',I6,' by ',I6,')',/,   &
        3X,'of covariance matrix "',A,'" (UTL_COVMAT)')
    350 FORMAT(/,1X,'WARNING: Covariance matrix "',A,'" is being used',   &
        ' more than once',/,3X,'in populating a variance-covariance',   &
        ' matrix for all members.',/,3X,'The last member in',   &
        ' the group causing the problem is',/,3X,'"',A,'" (UTL_COVMAT)')
    !
    CALL TYP_NULL(CDCOVMAT)
    CALL TYP_NULL(CDMTEMP1)
    CALL TYP_NULL(CDMTEMP2)
    ICOR1 = 1
    ICOR2 = 0
    IUNCOR1 = 1
    IUNCOR2 = 0
    THISGROUP = ' '
    NEXTGROUP = ' '
    IERR = 0
    CVMUSED = .FALSE.
    !
    MEMLOOP: DO I=1,NMEM
      CALL UTL_CASE(GROUP(I),THISGROUP,1)
      IF (I<NMEM) THEN
        CALL UTL_CASE(GROUP(I+1),NEXTGROUP,1)
        IF (NEXTGROUP==THISGROUP) CYCLE MEMLOOP
        IF (.NOT. WTCORR(I) .AND. .NOT. WTCORR(I+1)) CYCLE MEMLOOP
      ENDIF
      !   If control gets to here, add variances in diagonal positions or add
      !   a variance-covariance matrix for the current group to the global
      !   variance-covariance matrix
      IF (WTCORR(I)) THEN
        ICOR2 = I
        IDIM = ICOR2-ICOR1+1
        !   Group just ended is correlated -- Find group number
        JGP = 0
        DO J=1,NGPS
          CALL UTL_CASE(GROUPNAM(J),GPNAM,1)
          IF (GPNAM==THISGROUP) THEN
            CALL UTL_CASE(COVMATNAM(J),CVM1,1)
             JGP = J
             GOTO 50
          ENDIF
        ENDDO
        IF (IVERB>0) WRITE(IOUT,200) TRIM(THISGROUP)
        IERR = IERR + 1
        50 CONTINUE
        !   Find matching CovMatrix values and populate first block
        !   of CDCOVMAT
        CVMLOOP: DO M=1,NCOVMAT
          IF (COVMATARR(M)%ARRAYNAME==COVMATNAM(JGP)) THEN
            IF (CVMUSED(M)) THEN
              WRITE(IOUT,350) TRIM(COVMATARR(M)%ARRAYNAME),TRIM(MEMNAM(I))
            ENDIF
            IF (COVMATARR(M)%NC .NE. IDIM) THEN
              WRITE(IOUT,300)TRIM(THISGROUP),IDIM,COVMATARR(M)%NR,   &
                             COVMATARR(M)%NC,COVMATNAM(JGP)
              CALL UTL_STOP(' ')
            ENDIF
            !   Check if this is the first block.
            IF (.NOT. ASSOCIATED(CDCOVMAT%DVAL)) THEN
              CDCOVMAT = COVMATARR(M)
            ELSE
              CDMTEMP1 = CDCOVMAT
              CALL UTL_COMBINESQMATRIX(CDMTEMP1,COVMATARR(M),CDCOVMAT)
            ENDIF
            CVMUSED(M) = .TRUE.
            GOTO 100
          ENDIF
        ENDDO CVMLOOP
        WRITE(IOUT,250) COVMATNAM(JGP)
        CALL UTL_STOP(' ')
        100 CONTINUE
      ELSE   ! Group just ended is not correlated
        IUNCOR2 = I
        IDIM = IUNCOR2-IUNCOR1+1
        IF (.NOT. ASSOCIATED(CDCOVMAT%DVAL)) THEN
          CALL UTL_VEC2CDMATRIX(IDIM,VARIANCE(IUNCOR1:IUNCOR2),CDCOVMAT,1)
        ELSE
          CDMTEMP1 = CDCOVMAT
          CALL UTL_VEC2CDMATRIX(IDIM,VARIANCE(IUNCOR1:IUNCOR2),CDMTEMP2,1)
          CALL UTL_COMBINESQMATRIX(CDMTEMP1,CDMTEMP2,CDCOVMAT)
        ENDIF
      ENDIF
      !
      !   Assign pointer to beginning of next block
      IF (I<NMEM) THEN
        IF (WTCORR(I+1)) THEN
          ICOR1 = I+1
        ELSE
          IUNCOR1 = I+1
        ENDIF
      ENDIF
      !
    ENDDO MEMLOOP
    !
    RETURN
  END SUBROUTINE UTL_COVMAT
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DIAGONAL_CDM_D(CDM) RESULT(ARR)
    !   Extract diagonal elements from a CDMATRIX structure that contains
    !   a square matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and variables
    TYPE (CDMATRIX), INTENT(IN) :: CDM
    DOUBLE PRECISION, DIMENSION(CDM%NR) ::  ARR
    !
    !   Local variables
    INTEGER :: I
    !
    !   Format statements
    100 FORMAT(1X,'Error: CDM argument of UTL_DIAGONAL_CDM_D does not',   &
                  ' contain a square matrix')
    110 FORMAT(1X,'Error: CDM argument of UTL_DIAGONAL_CDM_D contains', &
                  ' a matrix of invalid dimensions: ',I7,' by ',I7)
    !
    IF (CDM%NR .NE. CDM%NC) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ELSEIF (CDM%NR<1) THEN
      WRITE(*,110)CDM%NR,CDM%NC
      CALL UTL_STOP(' ')
    ENDIF
    !
    DO I=1,CDM%NR
      ARR(I) = UTL_GETVAL(CDM,I,I)
    ENDDO
    RETURN
  END FUNCTION UTL_DIAGONAL_CDM_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DIAGONAL_SUB_CDM_D(CDM,ARR)
    !   Extract diagonal elements from a CDMATRIX structure that contains
    !   a square matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (CDMATRIX),                     INTENT(IN)  :: CDM
    DOUBLE PRECISION, DIMENSION(CDM%NR), INTENT(OUT) :: ARR
    !
    !   Local variables
    INTEGER :: I
    !
    !   Format statements
    100 FORMAT(1X,'Error: CDM argument of UTL_DIAGONAL_SUB_CDM_D does not',   &
                  ' contain a square matrix')
    110 FORMAT(1X,'Error: CDM argument of UTL_DIAGONAL_SUB_CDM_D contains', &
                  ' a matrix of invalid dimensions: ',I7,' by ',I7)
    !
    IF (CDM%NR .NE. CDM%NC) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ELSEIF (CDM%NR<1) THEN
      WRITE(*,110)CDM%NR,CDM%NC
      CALL UTL_STOP(' ')
    ENDIF
    !
    DO I=1,CDM%NR
      ARR(I) = UTL_GETVAL(CDM,I,I)
    ENDDO
    RETURN
  END SUBROUTINE UTL_DIAGONAL_SUB_CDM_D
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_CLOSE(EXT,FILESTAT) RESULT (IUDATEX)
    !   Close a data exchange file, given the extension, and return 0
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*),           INTENT(IN) :: EXT
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: FILESTAT
    INTEGER                                :: IUDATEX
    !
    !   Local variables
    INTEGER :: KDX
    !
    !   Format statements
    100 FORMAT(/,   &
        1X,75('*'),/,   &
        5X,' WARNING: "',A,   &
        '" has not been defined as an extension for a data-exchange file',/,   &
        5X,' (UTL_DX_CLOSE)',/,   &
        1X,75('*'),/)
    !
    DO KDX=1,KDXDEF
      IF (UTL_SAMENAME(EXT,DXEXT(KDX))) THEN
        IF (PRESENT(FILESTAT)) THEN
          IUDATEX = UTL_DX_CLOSE_INDEX(KDX,FILESTAT)
        ELSE
          IUDATEX = UTL_DX_CLOSE_INDEX(KDX)
        ENDIF
        GOTO 200
      ENDIF
    ENDDO
    WRITE(*,100)EXT
    200 CONTINUE
    RETURN
  END FUNCTION UTL_DX_CLOSE
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_CLOSE_INDEX(KDX,FILESTAT) RESULT (IUDATEX)
    !   Close a data exchange file, given the index in IUDX, and return 0
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                    INTENT(IN) :: KDX  !  Index in IUDX array associated with file to be closed
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: FILESTAT
    INTEGER                                :: IUDATEX
    !
    !   Local variables
    CHARACTER(LEN=6) :: FSTAT
    LOGICAL :: LOP
    !
    FSTAT = 'KEEP'
    IF (PRESENT(FILESTAT)) THEN
      IF (UTL_SAMENAME(FILESTAT,'DELETE')) FSTAT = 'DELETE'
    ENDIF
    !
    IF (KDX>0 .AND. KDX.LE.KDXDEF) THEN
      IUDATEX = IUDX(KDX)
      IF (IUDATEX>0) THEN
        ! Check to see if file is open.  If so, close it
        INQUIRE(UNIT=IUDATEX,OPENED=LOP)
        IF (LOP) CLOSE(IUDATEX,STATUS=FSTAT)
      ENDIF
    ENDIF
    IUDX(KDX) = 0
    IUDATEX = 0
    RETURN
  END FUNCTION UTL_DX_CLOSE_INDEX
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_GETUNIT(EXT) RESULT(IUDATEX)
    !   Return unit number associated with a specified data-exchange file
    !   extension
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*), INTENT(IN) :: EXT
    INTEGER :: IUDATEX
    !
    !   Local variables
    INTEGER :: KDX
    !
    !   Format statements
    100 FORMAT(/,   &
        1X,75('*'),/,   &
        5X,' WARNING: "',A,   &
        '" has not been defined as an extension for a data-exchange file',/,   &
        5X,' (UTL_DX_GETUNIT)',/,   &
        1X,75('*'),/)
    !
    IUDATEX = -1
    DO KDX=1,KDXDEF
      IF (UTL_SAMENAME(EXT,DXEXT(KDX))) THEN
        IUDATEX = IUDX(KDX)
        GOTO 200
      ENDIF
    ENDDO
    WRITE(*,100)EXT
    200 CONTINUE
    RETURN
  END FUNCTION UTL_DX_GETUNIT
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_GETUNIT_INDEX(KDX) RESULT(IUDATEX)
    !   Return unit number associated with a specified data-exchange file,
    !   given the index in IUDX of the file type.
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER, INTENT(IN) :: KDX
    INTEGER :: IUDATEX
    !
    IUDATEX = -1
    IF (KDX>0 .AND. KDX.LE.KDXDEF) IUDATEX = IUDX(KDX)
    !
    RETURN
  END FUNCTION UTL_DX_GETUNIT_INDEX
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_OPEN(FBASE,EXT,FILESTAT,MAXRECL) RESULT (IUDATEX)
    !   Open a data exchange file and return the unit number for it.  If an
    !   error is encountered in opening the file, -2 is returned.
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAXRECLDEFAULT
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*),           INTENT(IN) :: FBASE ! Filename base
    CHARACTER(LEN=*),           INTENT(IN) :: EXT
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: FILESTAT
    INTEGER, OPTIONAL,          INTENT(IN) :: MAXRECL
    INTEGER                                :: IUDATEX
    !
    !   Local variables
    INTEGER :: I, KDX, MAXRCL
    INTEGER :: MAXRECLBIG = 300000 ! 3E5 supports NPPREC up to 11,537 (parameters per record)
    CHARACTER(LEN=7) :: FSTAT
    CHARACTER(LEN=13) :: EXTLOCAL
    !
    IUDATEX = -1
    IF (EXT==' ') RETURN
    IF (PRESENT(FILESTAT)) THEN
      FSTAT = FILESTAT
    ELSE
      FSTAT = 'UNKNOWN'
    ENDIF
    !
    !   Ensure that lowercase is used for extension
    CALL UTL_CASE(EXT,EXTLOCAL,-1)
    !
    !   Determine KDX, which is or will be position in DXEXT of extension
    !   EXTLOCAL.
    IF (KDXDEF>0) THEN
      KDX = 0
      FINDEXT: DO I=1,KDXDEF
        IF (EXTLOCAL==DXEXT(I)) THEN
          KDX = I
          EXIT FINDEXT
        ENDIF
      ENDDO FINDEXT
      IF (KDX==0) THEN
        KDX = KDXDEF+1
      ENDIF
    ELSE
      KDX = 1
    ENDIF
    !
    !   If EXTLOCAL is not already stored in DXEXT, increment KDXDEF and store
    !   the extension.
    IF (KDX>KDXDEF) THEN
      IF (KDX>MAXDX) THEN
        AMESSAGE = 'Error: Number of DX files exceeds MAXDX.  Increase MAXDX'
        CALL UTL_STOP()
      ELSE
        KDXDEF = KDX
        DXEXT(KDX) = EXTLOCAL
      ENDIF
    ENDIF
    !
    !   Assign maximum record length (RECL) for OPEN statement
    IF (PRESENT(MAXRECL)) THEN
      MAXRCL = MAXRECL
    ELSE
      !   Get RECL specified to be used for files with record length
      !   dependent on NPPREC.  For other files, UTL_DX_RECL returns -1
      MAXRCL = UTL_DX_RECL(EXTLOCAL)
      IF (MAXRCL>-1) THEN
        IF (UTL_SAMENAME('OLD',FSTAT)) THEN
          MAXRCL = MAXRECLBIG
        ENDIF
      ELSE
        MAXRCL = MAXRECLDEFAULT
      ENDIF
    ENDIF
    IUDATEX = UTL_DX_OPEN_INDEX(FBASE,KDX,MAXRCL,FSTAT)
    !
    RETURN
  END FUNCTION UTL_DX_OPEN
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_OPEN_INDEX(FBASE,KDX,MAXRECL,FILESTAT) RESULT (IUDATEX)
    !   Open a data exchange file and return the unit number for it.  If an
    !   error is encountered, A negative number is returned:  -1 is returned if
    !   KDX is less than 1 or greater than MAXDX; -2 is returned if an error is
    !   encountered in opening the file.
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*),           INTENT(IN) :: FBASE ! Filename base
    INTEGER,                    INTENT(IN) :: KDX   ! Index in IDX array associated with file type to be opened
    INTEGER,                    INTENT(IN) :: MAXRECL ! Maximum record length for file
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: FILESTAT
    INTEGER                                :: IUDATEX
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    CHARACTER(LEN=7) :: FSTAT
    INTEGER :: ISTAT
    LOGICAL :: LOP
    !
    FSTAT = 'UNKNOWN'
    IF (PRESENT(FILESTAT)) THEN
      IF (UTL_SAMENAME(FILESTAT,'REPLACE')) THEN
        FSTAT = 'REPLACE'
      ELSEIF (UTL_SAMENAME(FILESTAT,'SCRATCH')) THEN
        FSTAT = 'SCRATCH'
      ELSEIF (UTL_SAMENAME(FILESTAT,'OLD')) THEN
        FSTAT = 'OLD'
      ELSEIF (UTL_SAMENAME(FILESTAT,'NEW')) THEN
        FSTAT = 'NEW'
      ENDIF
    ENDIF
    !
    IUDATEX = -1
    IF (KDX>0 .AND. KDX .LE. KDXDEF) THEN
      IUDATEX = IUDX(KDX)
      IF (IUDATEX>0) THEN
        ! Check to see if file is already open.  If so, close it
        INQUIRE(UNIT=IUDATEX,OPENED=LOP)
        IF (LOP) CLOSE(IUDATEX)
      ENDIF
      !   Build the file name
      FNAME = TRIM(ADJUSTL(FBASE))//'.'//DXEXT(KDX)
      !   Find an unused unit number
      IUDATEX = UTL_NEXTUNIT()
      !   Open the file
      OPEN(UNIT=IUDATEX,FILE=FNAME,RECL=MAXRECL,STATUS=FSTAT,IOSTAT=ISTAT)
      IF (ISTAT.NE.0) IUDATEX = -2
    ENDIF
    !
    IF (IUDATEX>0) THEN
      IUDX(KDX) = IUDATEX
    ELSE
      IUDX(KDX) = 0
    ENDIF
    !
    RETURN
  END FUNCTION UTL_DX_OPEN_INDEX
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_READ_MCMV(EXT,NPE,OUTNAM,PARNAM,CMAT)
    !   Read _mc or _mv DX File: Parameter correlation (_mc) or variance-
    !   covariance (_mv) matrix
    USE GLOBAL_DATA, ONLY: AMESSAGE, BIGINTEGER
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),                     INTENT(IN)  :: EXT
    INTEGER,                              INTENT(IN)  :: NPE
    CHARACTER(LEN=*),                     INTENT(IN)  :: OUTNAM
    CHARACTER(LEN=12), DIMENSION(NPE),    INTENT(OUT) :: PARNAM
    DOUBLE PRECISION, DIMENSION(NPE,NPE), INTENT(OUT) :: CMAT
    !
    !   Local variables
    INTEGER :: I, IP1, IP2, IUM, J, KB, KC, KP, KQ, MRECL, MREM, NPREC1
    CHARACTER(LEN=1) :: CH
    CHARACTER(LEN=13) :: EXTLOCAL
    !
    CALL UTL_CASE(EXT,EXTLOCAL,-1)
    IF (EXTLOCAL=='_mc' .OR. EXTLOCAL=='_mc_presvd' .OR. EXTLOCAL=='_mv' .OR. &
        EXTLOCAL=='_mvp' .OR. EXTLOCAL=='_mv_presvd') THEN
      IUM = UTL_DX_OPEN(OUTNAM,EXTLOCAL,'OLD')
    ELSE
      AMESSAGE = 'Programmer error: Invalid extension "'//TRIM(EXT)//   &
                 '" sent to UTL_DX_READ_MCMV as argument EXT'
      CALL UTL_STOP()
    ENDIF
    !
    !   Read header one character at a time to determine number of parameters
    !   per record
    MRECL = BIGINTEGER
    KQ = 0
    DO KC=1,MRECL
      READ(IUM,'(A1)',ADVANCE='NO',EOR=50) CH
      IF (CH=='"') KQ = KQ+1
    ENDDO
    50 CONTINUE
    NPREC1 = KQ/2
    MREM = MOD(KQ,2)
    IF (MREM .NE. 0) THEN
      AMESSAGE = 'Programming error: UTL_DX_READ_MCMV'//   &
                 ' finds odd number of quotes'
      CALL UTL_STOP()
    ENDIF
    REWIND(IUM)
    !
    ! READ Header(s) and matrix values
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPREC1+1
      KB = KB+1
      IP2 = KB*NPREC1
      IF (IP2>NPE) IP2 = NPE
      READ (IUM,*,END=100,ERR=100)(PARNAM(I),I=IP1,IP2)
      DO I=1,NPE
        READ (IUM,*,END=100,ERR=100)(CMAT(I,J),J=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    !
    !   Close data-exchange file
    IUM = UTL_DX_CLOSE(EXTLOCAL)
    !
    RETURN
    100 CONTINUE
    IUM = UTL_DX_CLOSE(EXTLOCAL)
    AMESSAGE = 'Error encountered in UTL_DX_READ_MCMV: Unexpected content in'//   &
               EXTLOCAL//' file, check version/re-create '//EXTLOCAL
    CALL UTL_STOP()
    RETURN
  END SUBROUTINE UTL_DX_READ_MCMV
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_READ_WT(IOUT,OUTNAM,EXT,WTMAT,WTMATSQR)
    !   Read _wtdep data exchange file: Weight Matrix Deps and
    !   (optionally) SqRoot of WTMAT Deps
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                  INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),         INTENT(IN)    :: OUTNAM
    CHARACTER(LEN=*),         INTENT(IN)    :: EXT
    TYPE(CDMATRIX),           INTENT(INOUT) :: WTMAT
    TYPE(CDMATRIX), OPTIONAL, INTENT(INOUT) :: WTMATSQR
    !
    !   Local variables
    CHARACTER(LEN=13) :: EXTLOCAL
    INTEGER :: IUWT, KMAT
    !
    !   Format statements
    50 FORMAT(I5)
    100 FORMAT(/,1X,'*** WARNING: Unable to open file: ',A,A,/)
    !
    IF (UTL_SAMENAME(EXT,'_wt')) THEN
      EXTLOCAL = '_wt'
    ELSEIF (UTL_SAMENAME(EXT,'_wtpri')) THEN
      EXTLOCAL = '_wtpri'
    ELSEIF (UTL_SAMENAME(EXT,'_wtprip')) THEN
      EXTLOCAL = '_wtprip'
    ELSEIF (UTL_SAMENAME(EXT,'_wt_presvd')) THEN
      EXTLOCAL = '_wt_presvd'
    ELSEIF (UTL_SAMENAME(EXT,'_wtpri_presvd')) THEN
      EXTLOCAL = '_wtpri_presvd'
    ELSE
      AMESSAGE = 'ERROR in UTL_DX_READ_WT: EXT argument not supported: "'//   &
          TRIM(EXT)//'"'
      CALL UTL_STOP()
    ENDIF
    !
    !   Open data exchange file
    IUWT = UTL_DX_OPEN(OUTNAM,EXTLOCAL,'OLD')
    IF (IUWT<1) THEN
      WRITE(IOUT,100)TRIM(OUTNAM),TRIM(EXTLOCAL)
      RETURN
    ENDIF
    !   Determine number of matrices (either 1 or 2) to be read
    READ(IUWT,50)KMAT
    !   Read data
    CALL UTL_READMATRIX(IUWT,IOUT,WTMAT)
    IF(KMAT==2 .AND. PRESENT(WTMATSQR)) THEN
      CALL UTL_READMATRIX(IUWT,IOUT,WTMATSQR)
    ENDIF
    !
    !   Close data exchange file and return
    IUWT = UTL_DX_CLOSE(EXTLOCAL)
    RETURN
  END SUBROUTINE UTL_DX_READ_WT
  !-----------------------------------------------------------------------------
  FUNCTION UTL_DX_RECL(EXT) RESULT (MRECL)
    !   Return appropriate RECL value (maximum record length) for opening a
    !   data-exchange file for which RECL is dependent on NPPREC (the number
    !   parameters per record).  If EXT is ' ', 0 is returned.  If EXT is
    !   one of the file types for which RECL depends on NPPREC, the maximum
    !   record length for opening the file is returned.  For any other value
    !   of EXT, -1 is returned.
    !
    USE GLOBAL_DATA, ONLY: NPPREC
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*), INTENT(IN) :: EXT
    INTEGER                      :: MRECL
    !
    !   Local variables
    CHARACTER(LEN=13) :: EXTLOCAL
    !
    CALL UTL_CASE(EXT,EXTLOCAL,-1)
    IF (EXTLOCAL=='') THEN
      MRECL = 0
    ELSEIF (EXTLOCAL=='_mc') THEN
      MRECL = NPPREC*26+11
    ELSEIF (EXTLOCAL=='_mv') THEN
      MRECL = NPPREC*26+11
    ELSEIF (EXTLOCAL=='_mvp') THEN
      MRECL = NPPREC*26+11
    ELSEIF (EXTLOCAL=='_s1') THEN
      MRECL = NPPREC*26+52
    ELSEIF (EXTLOCAL=='_sd') THEN
      MRECL = NPPREC*26+52
    ELSEIF (EXTLOCAL=='_su') THEN
      MRECL = NPPREC*26+52
    ELSEIF (EXTLOCAL=='_supri') THEN
      MRECL = NPPREC*26+52
    ELSEIF (EXTLOCAL=='_suprip') THEN
      MRECL = NPPREC*26+52
    ELSEIF (EXTLOCAL=='_eig') THEN
      MRECL = NPPREC*26+110
    ELSE
      !   Value of -1 indicates default maximum record length (MAXRECLDEFAULT
      !   of the GLOBAL_DATA module) should be used.
      MRECL = -1
    ENDIF
    RETURN
  END FUNCTION UTL_DX_RECL
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_WRITE_EIG(IUEIG,NPE,NPT,IPTR,PARNAM,EIGVAL,EIGVEC)
    !   WRITE _EIG DX FILE  EigenValues and EigenVectors
    USE GLOBAL_DATA, ONLY: AMESSAGE, BLANKS, NPPREC
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                 INTENT(IN) :: IUEIG  ! Unit number opened for DX file
    INTEGER,                                 INTENT(IN) :: NPE    ! Number of active parameters
    INTEGER,                                 INTENT(IN) :: NPT    ! Number of parameters, total
    INTEGER,                DIMENSION(NPE),  INTENT(IN) :: IPTR   ! Pointers to adjustable parameters
    CHARACTER(LEN=12),      DIMENSION(NPT),  INTENT(IN) :: PARNAM ! Parameter names
    DOUBLE PRECISION,   DIMENSION(NPE),      INTENT(IN) :: EIGVAL ! Array of eigenvalues
    DOUBLE PRECISION,   DIMENSION(NPE,NPE),  INTENT(IN) :: EIGVEC ! Array of eigenvectors
    !
    !   Local variables
    INTEGER :: IP, IP1, IP2, J, KB, KP, N
    CHARACTER(LEN=150) :: FMT_STRING1, FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    !
    ! Formats
    2 FORMAT(1X,'"Variance-Covariance Matrix of the Parameters scaled by ', &
                'parameter values: Eigenvalues and Eigenvectors"')
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING1 = '(1X,''"EIGENVALUES FOR EACH VECTOR"     '// &
                  '"PARAMETER FOR EACH VECTOR ELEMENT "'',5X,'// &
                  TRIM(STR_NPPREC)//'(1X,''"EIGENVECTOR'',1X,I11,''"'',:))'
    FMT_STRING2 = '(1X,G25.16,9X,A,27X,'//TRIM(STR_NPPREC)//'(1X,G25.16))'
    !
    !   Write eigenvalues & vectors, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    WRITE (IUEIG,2)
    DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      WRITE (IUEIG,FMT_STRING1) (J,J=1,NPE)
      DO N = 1, NPE
        WRITE (IUEIG,FMT_STRING2) EIGVAL(N),PARNAM(IPTR(N)), &
                                        (EIGVEC(N,IP),IP=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO
    RETURN
  END SUBROUTINE UTL_DX_WRITE_EIG
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_WRITE_MCMV(EXT,NPE,NPS,IPTR,OUTNAM,PARNAM,CMAT)
    !   WRITE _MC DX FILE Correlation Matrix
    USE GLOBAL_DATA, ONLY: AMESSAGE, MAX_STRING_LEN, NPPREC
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),                     INTENT(IN) :: EXT
    INTEGER,                              INTENT(IN) :: NPE
    INTEGER,                              INTENT(IN) :: NPS
    INTEGER,          DIMENSION(NPE),     INTENT(IN) :: IPTR
    CHARACTER(LEN=*),                     INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12),DIMENSION(NPS),     INTENT(IN) :: PARNAM
    DOUBLE PRECISION, DIMENSION(NPE,NPE), INTENT(IN) :: CMAT
    !
    !   Local variables
    INTEGER :: I, IP, IP1, IP2, KB, KP, IUM
    CHARACTER(LEN=100) :: FMT_STRING1, FMT_STRING2
    CHARACTER(LEN=10)  :: STR_NPPREC
    CHARACTER(LEN=4)   :: EXTLOCAL
    !
    !   Open Data exchange File
    CALL UTL_CASE(EXT,EXTLOCAL,-1)
    IF (EXTLOCAL=='_mc' .OR. EXTLOCAL=='_mv' .OR. EXTLOCAL=='_mvp') THEN
      IUM = UTL_DX_OPEN(OUTNAM,EXTLOCAL,'REPLACE')
    ELSE
      AMESSAGE = 'Programmer error: Invalid extension "'//TRIM(EXT)//   &
                 '" sent to UTL_DX_WRITE_MCMV as argument EXT'
      CALL UTL_STOP()
    ENDIF
    !
    !   Construct format strings
    WRITE(STR_NPPREC,'(I10)') NPPREC
    STR_NPPREC = ADJUSTL(STR_NPPREC)
    FMT_STRING1 = '('//TRIM(STR_NPPREC)//'(1X,''"'',A12,''"'',:))'
    FMT_STRING2 = '('//TRIM(STR_NPPREC)//'(1X,1PG25.16))'
    !
    !   Write header and matrix values, in blocks of NPPREC parameters
    KB = 0
    KP = 0
    BLOK: DO WHILE (KP<NPE)
      IP1 = KB*NPPREC+1
      KB = KB+1
      IP2 = KB*NPPREC
      IF (IP2>NPE) IP2 = NPE
      !   Write Header
      WRITE(IUM,FMT_STRING1) (PARNAM(IPTR(I)),I=IP1,IP2)
      !   Write  matrix values
      DO IP = 1, NPE
        WRITE(IUM,FMT_STRING2) (CMAT(I,IP),I=IP1,IP2)
      ENDDO
      KP = IP2
    ENDDO BLOK
    !   Close Data exchange File
    IUM = UTL_DX_CLOSE(EXTLOCAL)
    RETURN
  END SUBROUTINE UTL_DX_WRITE_MCMV
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_WRITE_PA(ITERP,MAXITER,NPS,OUTNAM,PARNAM, &
                             PAREST,PVAL)
    !   Write _pa and _pasub DX files:
    !     _pa -- Parameter estimates listed by parameter, then by iteration
    !     _pasub -- Parameter estimates listed by iteration, then by parameter,
    !             formatted for subsitution into a PARAMETER_VALUES input block
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                    INTENT(IN) :: ITERP
    INTEGER,                                    INTENT(IN) :: MAXITER
    INTEGER,                                    INTENT(IN) :: NPS
    CHARACTER(LEN=*),                           INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12), DIMENSION(NPS),          INTENT(IN) :: PARNAM
    DOUBLE PRECISION, DIMENSION(MAXITER,NPS),   INTENT(IN) :: PAREST
    DOUBLE PRECISION, DIMENSION(NPS), OPTIONAL, INTENT(IN) :: PVAL
    !
    !   Local variables
    INTEGER :: I, IUPA, IUPASUB, J
    !
    !   Format statements
    100 FORMAT(1X,'PARAMETER: ',A,/,1X,'"ITERATION"  "ESTIMATE"')
    101 FORMAT(1X,I5,6X,1PE15.7)
    200 FORMAT(1X,'ITERATION: ',I5,/,1X,'"PARAMETER"  "ESTIMATE"')
    201 FORMAT(1X,A,6X,1PE15.7)
    !
    !   Write parameter estimates to _pa file
    IUPA = UTL_DX_OPEN(OUTNAM,'_pa','REPLACE')
    DO I = 1,NPS
      WRITE(IUPA,100)PARNAM(I)
      DO J=1,ITERP
        WRITE(IUPA,101)J,PAREST(J,I)
      ENDDO
      IF (PRESENT(PVAL)) WRITE (IUPA,101) ITERP+1,PVAL(I)
    ENDDO
    IUPA = UTL_DX_CLOSE('_pa')
    !
    !   Write parameter estimates to _pasub file
    IUPASUB = UTL_DX_OPEN(OUTNAM,'_pasub','REPLACE')
    DO I = 1,ITERP
      WRITE(IUPASUB,200)I
      DO J=1,NPS
        WRITE(IUPASUB,201)PARNAM(J),PAREST(I,J)
      ENDDO
    ENDDO
    IUPASUB = UTL_DX_CLOSE('_pasub')
    !
    RETURN
  END SUBROUTINE UTL_DX_WRITE_PA
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_WRITE_PCC(NPE,NPS,BUFF,DATAEXCHANGE,IOUT,IPTR,OUTNAM,PARNAM)
    !   WRITE _pcc Data Exchange file: Correlation Matrix
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                INTENT(IN) :: NPE
    INTEGER,                                INTENT(IN) :: NPS
    DOUBLE PRECISION,   DIMENSION(NPE,NPE), INTENT(IN) :: BUFF
    LOGICAL,                                INTENT(IN) :: DATAEXCHANGE
    INTEGER,                                INTENT(IN) :: IOUT
    INTEGER,            DIMENSION(NPE),     INTENT(IN) :: IPTR
    CHARACTER(LEN=*),                       INTENT(IN) :: OUTNAM
    CHARACTER(LEN=12),  DIMENSION(NPS),     INTENT(IN) :: PARNAM
    !
    !   Local variables
    DOUBLE PRECISION :: TMP
    INTEGER :: I, I1, IP, IIPP, IUPCC
    INTEGER :: I95, I90, I85
    !
    !   Format statements
    810 FORMAT (' "PARAMETER"   "PARAMETER"    "CORRELATION"')
    815 FORMAT (/,                                                           &
        ' THE CORRELATION OF THE FOLLOWING PARAMETER PAIRS >= .95'   &
        ,/,5X,'PARAMETER   PARAMETER   CORRELATION')
    819 FORMAT (5X,A12,2X,A12,2X,F12.6)
    820 FORMAT (5X,A12,2X,A12,2X,F8.2)
    821 FORMAT (/,7X,' NONE ')
    822 FORMAT (7X,' NONE ',7X,' NONE ',7X,'0.0')
    825 FORMAT (/,' THE CORRELATION OF THE FOLLOWING PARAMETER PAIRS IS ',   &
        'BETWEEN .90 AND .95',/,5X,                                  &
        'PARAMETER   PARAMETER   CORRELATION')
    830 FORMAT (/,' THE CORRELATION OF THE FOLLOWING PARAMETER PAIRS IS ',   &
        'BETWEEN .85 AND .90',/,5X,                                          &
        'PARAMETER   PARAMETER   CORRELATION')
    940 FORMAT (/,                                                           &
        ' CORRELATIONS GREATER THAN 0.95 COULD INDICATE THAT THERE MAY',     &
        ' NOT BE ENOUGH',/,                                                     &
        ' INFORMATION IN THE OBSERVATIONS AND PRIOR USED IN THE',            &
        ' REGRESSION TO ESTIMATE',/,' PARAMETER VALUES INDIVIDUALLY.',/,     &
        ' TO CHECK THIS,',                                                   &
        ' START THE REGRESSION FROM SETS OF INITIAL PARAMETER VALUES',/,     &
        ' THAT DIFFER BY MORE THAT TWO STANDARD DEVIATIONS FROM THE',        &
        ' ESTIMATED ',/,                                                     &
        ' VALUES.  IF THE RESULTING ESTIMATES ARE WELL WITHIN ONE STANDARD', &
        ' DEVIATION',/,                                                      &
        ' OF THE PREVIOUSLY ESTIMATED VALUE, THE ESTIMATES ARE PROBABLY',/,  &
        ' DETERMINED INDEPENDENTLY WITH THE OBSERVATIONS AND PRIOR USED IN',/,&
        ' THE REGRESSION.  OTHERWISE, YOU MAY ONLY BE ESTIMATING THE RATIO',/,&
        ' OR SUM OF THE HIGHLY CORRELATED PARAMETERS.')
    ! Initialize
    I95 = 0
    I90 = 0
    I85 = 0
    IF(DATAEXCHANGE) THEN
      ! Open Data exchange File
      IUPCC = UTL_DX_OPEN(OUTNAM,'_pcc','REPLACE')
      ! Write Header
      WRITE(IUPCC,810)
    ENDIF
    !--------CHECK FOR HIGHLY CORRELATED PARAMETER PAIRS
    WRITE (IOUT,815)
    DO IP = 1, NPE
      IIPP = IPTR(IP)
      I1 = IP + 1
      DO I = I1, NPE
        IF (ABS(BUFF(IP,I)).GE. 0.95) THEN
          WRITE (IOUT,820) PARNAM(IIPP),PARNAM(IPTR(I)),BUFF(IP,I)
          IF(DATAEXCHANGE) &
             WRITE (IUPCC,819) PARNAM(IIPP),PARNAM(IPTR(I)),BUFF(IP,I)
          I95 = 1
        ENDIF
      ENDDO
    ENDDO
    IF (I95 .EQ. 0) WRITE(IOUT,821)
    WRITE (IOUT,825)
    DO IP = 1, NPE
      IIPP = IPTR(IP)
      I1 = IP + 1
      DO I = I1, NPE
        TMP = ABS(BUFF(IP,I))
        IF (TMP.GE. 0.90 .AND. TMP.LT. 0.95) THEN
          WRITE (IOUT,820) PARNAM(IIPP),PARNAM(IPTR(I)),BUFF(IP,I)
          IF(DATAEXCHANGE) &
             WRITE (IUPCC,819) PARNAM(IIPP),PARNAM(IPTR(I)),BUFF(IP,I)
          I90 = 1
        ENDIF
      ENDDO
    ENDDO
    IF (I90 .EQ. 0) WRITE(IOUT,821)
    WRITE (IOUT,830)
    DO IP = 1, NPE
      IIPP = IPTR(IP)
      I1 = IP + 1
      DO I = I1, NPE
        TMP = ABS(BUFF(IP,I))
        IF (TMP.GE. 0.85 .AND. TMP.LT. 0.90) THEN
          WRITE (IOUT,820) PARNAM(IIPP),PARNAM(IPTR(I)),BUFF(IP,I)
          IF(DATAEXCHANGE) &
             WRITE (IUPCC,819) PARNAM(IIPP),PARNAM(IPTR(I)),BUFF(IP,I)
          I85 = 1
        ENDIF
      ENDDO
    ENDDO
    IF (I85 .EQ. 0) THEN
      WRITE(IOUT,821)
      IF(DATAEXCHANGE .AND. I95 .EQ. 0 .AND. I90 .EQ. 0) WRITE(IUPCC,822)
    ENDIF
  IF (I95.EQ.1) THEN
    WRITE (IOUT,940)
  ENDIF
  ! Close Data exchange File
  IF(DATAEXCHANGE) IUPCC = UTL_DX_CLOSE('_pcc')
  RETURN
  END SUBROUTINE UTL_DX_WRITE_PCC
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_DX_WRITE_WT(OUTNAM,EXT,WTFULL,WTFULLSQR)
    !   Write _wt data exchange file: Weight Matrix and
    !   (optionally) SqRoot of WTFULL
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),         INTENT(IN) :: OUTNAM
    CHARACTER(LEN=*),         INTENT(IN) :: EXT
    TYPE(CDMATRIX),           INTENT(IN) :: WTFULL
    TYPE(CDMATRIX), OPTIONAL, INTENT(IN) :: WTFULLSQR
    !
    !   Local variables
    INTEGER :: IUWT, KMAT
    CHARACTER(LEN=7) :: EXTLOCAL
    !
    !   Format statements
    50 FORMAT(1X,I1)
    !
    IF (UTL_SAMENAME(EXT,'_wt')) THEN
      EXTLOCAL = '_wt'
    ELSEIF (UTL_SAMENAME(EXT,'_wtpri')) THEN
      EXTLOCAL = '_wtpri'
    ELSEIF (UTL_SAMENAME(EXT,'_wtprip')) THEN
      EXTLOCAL = '_wtprip'
    ELSE
      AMESSAGE = 'ERROR in UTL_DX_WRITE_WT: EXT argument not supported: "'//   &
          TRIM(EXT)//'"'
      CALL UTL_STOP()
    ENDIF
    KMAT = 1
    IF(PRESENT(WTFULLSQR)) KMAT = 2
    ! Open data exchange file
    IUWT = UTL_DX_OPEN(OUTNAM,EXTLOCAL,'REPLACE')
    !   Write number of matrices to be written
    WRITE(IUWT,50) KMAT
    ! Write  data
    CALL UTL_WRITECDMATRIX(WTFULL,0,IUWT,.TRUE.)
    IF(KMAT == 2) CALL UTL_WRITECDMATRIX(WTFULLSQR,0,IUWT,.TRUE.)
    ! Close data exchange file
    IUWT = UTL_DX_CLOSE(EXTLOCAL)
    RETURN
  END SUBROUTINE UTL_DX_WRITE_WT
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_EIGEN(IOUT,IPRC,NPE,NPS,IPTR,ITERP,PARNAM,PVALINIT,PVAL,C, &
                       DATAEXCHANGE,OUTNAM,PRINTIOUTIN)
    !   CALCULATE EIGENVALUES AND EIGENVECTORS OF COVARIANCE MATRIX ON THE
    !   PARAMETERS.
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN)    :: IOUT
    INTEGER,                               INTENT(IN)    :: IPRC
    INTEGER,                               INTENT(IN)    :: NPE
    INTEGER,                               INTENT(IN)    :: NPS
    INTEGER,           DIMENSION(NPE),     INTENT(IN)    :: IPTR
    INTEGER,                               INTENT(IN)    :: ITERP
    CHARACTER(LEN=12), DIMENSION(NPS),     INTENT(IN)    :: PARNAM
    DOUBLE PRECISION,  DIMENSION(NPS),     INTENT(IN)    :: PVALINIT
    DOUBLE PRECISION,  DIMENSION(NPS),     INTENT(IN)    :: PVAL
    DOUBLE PRECISION,  DIMENSION(NPE,NPE), INTENT(INOUT) :: C
    LOGICAL, OPTIONAL, INTENT(IN)                        :: DATAEXCHANGE
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)               :: OUTNAM
    LOGICAL, OPTIONAL, INTENT(IN)                        :: PRINTIOUTIN
    !
    !   Local variables
    !DOUBLE PRECISION EIGL(NPE)
    !DOUBLE PRECISION EIGV(NPE,NPE)
    !DOUBLE PRECISION EIGW(NPE)
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)   :: EIGL, EIGW
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: EIGV
    DOUBLE PRECISION TEMP
    INTEGER I, IIP, IIPP, IP, IUEIG, J
    LOGICAL PRINTIOUT
    CHARACTER(LEN=47) :: ANAME
    DATA ANAME /'VARIANCE-COVARIANCE MATRIX SCALED W/ PARAMETERS'/
    !
    !   Format statements
    500 FORMAT (A12,8D13.4,/,10(12X,8D13.4))
    505 FORMAT (/,12X,8D13.4,/,10(12X,8D13.4))
    510 FORMAT (/,' EIGENVALUES')
    515 FORMAT (/,' EIGENVECTORS',/)
    520 FORMAT (/,10X,A47,/,10X,47('-'))
    !
    PRINTIOUT = .TRUE.
    IF (PRESENT(PRINTIOUTIN)) THEN
      PRINTIOUT = PRINTIOUTIN
    ENDIF
    !---------SCALE VARIANCE-COVARIANCE MATRIX WITH PARAMETER VALUES
    ALLOCATE(EIGL(NPE),EIGV(NPE,NPE),EIGW(NPE))
    EIGV = 0.D0
    EIGL = 0.D0
    EIGW = 0.D0
    IF (ITERP.GT.1) THEN
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)/(PVAL(IIPP)*PVAL(IPTR(IIP)))
        ENDDO
      ENDDO
    ELSE
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)/(PVALINIT(IIPP)*PVALINIT(IPTR(IIP)))
        ENDDO
      ENDDO
    ENDIF
    IF(PRINTIOUT) THEN
      WRITE (IOUT,520) ANAME
      CALL UTL_PARSQMATRIX(IOUT,IPRC,NPE,NPS,C,IPTR,PARNAM)
    ENDIF
    !-------CALCULATE EIGENVALUES AND EIGENVECTORS
    DO IP = 1, NPE
      DO I = 1, NPE
        EIGV(I,IP) = C(I,IP)
      ENDDO
    ENDDO
    CALL UTL_EIGEN1(NPE,NPE,EIGL,EIGV,EIGW)
    CALL UTL_EIGEN2(NPE,NPE,IOUT,EIGL,EIGV,EIGW)
    CALL UTL_EIGEN3(NPE,NPE,EIGL,EIGV)
    !-------SCALE EIGENVECTORS TO FORM UNIT VECTORS
    DO I = 1, NPE
      TEMP = 0.D0
      DO J = 1, NPE
        TEMP = TEMP + EIGV(J,I)**2
      ENDDO
      TEMP = TEMP**.5
      DO J = 1, NPE
        EIGV(J,I) = EIGV(J,I)/TEMP
      ENDDO
    ENDDO
    IF(PRINTIOUT) THEN
    !-------PRINT EIGENVALUES
  !mch 1/2009 eigenvectors should be numbered, not assoicated with parameters. Second write statement changed
      WRITE (IOUT,510)
      WRITE (IOUT,'(/,6X,8I13,/,10(6X,8I13))') (J,J=1,NPE)
      WRITE (IOUT,505) (EIGL(J),J=1,NPE)
      !-------PRINT EIGENVECTORS
      WRITE (IOUT,515)
      DO J = 1, NPE
        WRITE (IOUT,500) PARNAM(IPTR(J)), (EIGV(J,I),I=1,NPE)
      ENDDO
    ENDIF
    IF (PRESENT(DATAEXCHANGE) .AND. PRESENT(OUTNAM)) THEN
      IF (DATAEXCHANGE) THEN
        !SAVE EIGENVALUES AND EIGENVECTORS TO _EIG
        ! Open data exchange file
        IUEIG = UTL_DX_OPEN(OUTNAM,'_eig','REPLACE')
        CALL UTL_DX_WRITE_EIG(IUEIG,NPE,NPS,IPTR,PARNAM,EIGL,EIGV)
      ENDIF
    ENDIF
    !-------EIGENVECTOR TEST (FROM PRESS AND OTHERS, 1989)
    !WRITE (IOUT,'(/1X,A)') 'EIGENVECTOR TEST'
    !DO J = 1, NPE
    !  DO L = 1, NPE
    !    EIGW(L) = 0.0
    !    DO K = 1, NPE
    !      IF (K.GT.L) THEN
    !        KK = L
    !        LL = K
    !      ELSE
    !        KK = K
    !        LL = L
    !      ENDIF
    !      EIGW(L) = EIGW(L) + C(LL,KK)*EIGV(K,J)
    !    ENDDO
    !  ENDDO
    !  WRITE (IOUT,'(/1X,A,I3)') 'VECTOR NUMBER', J
    !  WRITE (IOUT,'(/1X,T7,A,T18,A,T31,A)') 'VECTOR', 'MTRX*VEC ',       &
    !                                        'RATIO'
    !  DO L = 1, NPE
    !    RATIO = EIGW(L)/EIGV(L,J)
    !    WRITE (IOUT,'(1X,3G12.5)') EIGV(L,J), EIGW(L), RATIO
    !  ENDDO
    !ENDDO
    !---------UNSCALE VARIANCE-COVARIANCE MATRIX
    IF (ITERP.GT.1) THEN
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)*(PVAL(IIPP)*PVAL(IPTR(IIP)))
        ENDDO
      ENDDO
    ELSE
      DO IP = 1, NPE
        IIPP = IPTR(IP)
        DO IIP = 1, NPE
          C(IP,IIP) = C(IP,IIP)*(PVALINIT(IIPP)*PVALINIT(IPTR(IIP)))
        ENDDO
      ENDDO
    ENDIF
    DEALLOCATE(EIGL,EIGV,EIGW)
    RETURN
  END SUBROUTINE UTL_EIGEN
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_EIGEN1(N,NP,EIGL,EIGV,EIGW)
    !     VERSION 20031110 EPP
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, SUBROUTINE TRED2,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION
    !     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,                            INTENT(IN)    :: NP
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGL
    DOUBLE PRECISION, DIMENSION(NP,NP), INTENT(INOUT) :: EIGV
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGW
    !
    !   Local variables
    DOUBLE PRECISION :: F, G, H, HH, SCALE
    INTEGER :: I, J, K, L
    !
    !     ------------------------------------------------------------------
    IF (N.GT.1) THEN
      DO I = N, 2, -1
        L = I - 1
        H = 0.D0
        SCALE = 0.D0
        IF (L.GT.1) THEN
          DO K = 1, L
            SCALE = SCALE + DABS(EIGV(I,K))
          ENDDO
          IF (SCALE.EQ.0.0) THEN
            EIGW(I) = EIGV(I,L)
          ELSE
            DO K = 1, L
              EIGV(I,K) = EIGV(I,K)/SCALE
              H = H + EIGV(I,K)**2
            ENDDO
            F = EIGV(I,L)
            G = -SIGN(DSQRT(H),F)
            EIGW(I) = SCALE*G
            H = H - F*G
            EIGV(I,L) = F - G
            F = 0.D0
            DO J = 1, L
              EIGV(J,I) = EIGV(I,J)/H
              G = 0.D0
              DO K = 1, J
                G = G + EIGV(J,K)*EIGV(I,K)
              ENDDO
              IF (L.GT.J) THEN
                DO K = J+1, L
                  G = G + EIGV(K,J)*EIGV(I,K)
                ENDDO
              ENDIF
              EIGW(J) = G/H
              F = F + EIGW(J)*EIGV(I,J)
          ENDDO
          HH = F/(H+H)
          DO J = 1, L
            F = EIGV(I,J)
            G = EIGW(J) - HH*F
            EIGW(J) = G
            DO K = 1, J
              EIGV(J,K) = EIGV(J,K) - F*EIGW(K) - G*EIGV(I,K)
            ENDDO
          ENDDO
        ENDIF
        ELSE
          EIGW(I) = EIGV(I,L)
        ENDIF
        EIGL(I) = H
      ENDDO
    ENDIF
    EIGL(1) = 0.D0
    EIGW(1) = 0.D0
    DO I = 1, N
      L = I - 1
      IF (EIGL(I).NE.0.0) THEN
        DO J = 1, L
          G = 0.D0
          DO K = 1, L
            G = G + EIGV(I,K)*EIGV(K,J)
          ENDDO
          DO K = 1, L
            EIGV(K,J) = EIGV(K,J) - G*EIGV(K,I)
          ENDDO
        ENDDO
      ENDIF
      EIGL(I) = EIGV(I,I)
      EIGV(I,I) = 1.D0
      IF (L.GE.1) THEN
        DO J = 1, L
          EIGV(I,J) = 0.D0
          EIGV(J,I) = 0.D0
        ENDDO
      ENDIF
    ENDDO
  RETURN
  END SUBROUTINE UTL_EIGEN1
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_EIGEN2(N,NP,IOUT,EIGL,EIGV,EIGW)
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, TQLI,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION
    !     ******************************************************************
    !        SPECIFICATIONS:
    !     ------------------------------------------------------------------
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,                            INTENT(IN)    :: NP
    INTEGER,                            INTENT(IN)    :: IOUT
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGL
    DOUBLE PRECISION, DIMENSION(NP,NP), INTENT(INOUT) :: EIGV
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGW
    !
    !   Local variables
    DOUBLE PRECISION :: B, CC, DDD, F, G, P, R, S
    INTEGER :: I, ITER, K, L, M
    !
    !   Format statement
    45  FORMAT(/,' WARNING:  TOO MANY ITERATIONS IN SPES1BAS6TQ',/)
    !
    IF (N.GT.1) THEN
      DO I = 2, N
        EIGW(I-1) = EIGW(I)
      ENDDO
      EIGW(N) = 0.D0
      DO L = 1, N
        ITER = 0
        20 DO M = L, N-1
          DDD = DABS(EIGL(M)) + DABS(EIGL(M+1))
          IF (DABS(EIGW(M))+DDD.EQ.DDD) GOTO 40
        ENDDO
        M = N
        40 IF (M.NE.L) THEN
          IF (ITER.EQ.30) THEN
            WRITE (IOUT,45)
          ENDIF
          ITER = ITER + 1
          G = (EIGL(L+1)-EIGL(L))/(2.D0*EIGW(L))
          R = DSQRT(G**2+1.D0)
          G = EIGL(M) - EIGL(L) + EIGW(L)/(G+SIGN(R,G))
          S = 1.D0
          CC = 1.D0
          P = 0.D0
          DO I = M-1, L, -1
            F = S*EIGW(I)
            B = CC*EIGW(I)
            IF (DABS(F).GE.DABS(G)) THEN
              CC = G/F
              R = DSQRT(CC**2+1.D0)
              EIGW(I+1) = F*R
              S = 1.D0/R
              CC = CC*S
            ELSE
              S = F/G
              R = DSQRT(S**2+1.D0)
              EIGW(I+1) = G*R
              CC = 1.D0/R
              S = S*CC
            ENDIF
            G = EIGL(I+1) - P
            R = (EIGL(I)-G)*S + 2.D0*CC*B
            P = S*R
            EIGL(I+1) = G + P
            G = CC*R - B
            DO K = 1, N
              F = EIGV(K,I+1)
              EIGV(K,I+1) = S*EIGV(K,I) + CC*F
              EIGV(K,I) = CC*EIGV(K,I) - S*F
            ENDDO
          ENDDO
          EIGL(L) = EIGL(L) - P
          EIGW(L) = G
          EIGW(M) = 0.D0
          GOTO 20
        ENDIF
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE UTL_EIGEN2
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_EIGEN3(N,NP,EIGL,EIGV)
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, EIGSRT,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION AND TO ORDER THE EIGENVALUES FROM
    !     SMALLEST TO LARGEST
    !     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,                            INTENT(IN)    :: NP
    DOUBLE PRECISION, DIMENSION(NP),    INTENT(INOUT) :: EIGL
    DOUBLE PRECISION, DIMENSION(NP,NP), INTENT(INOUT) :: EIGV
    !
    !   Local variables
    DOUBLE PRECISION P
    INTEGER I, J, K
    !
    !     ------------------------------------------------------------------
    DO I = 1, N-1
      K = I
      P = EIGL(I)
      DO J = I+1, N
        IF (EIGL(J).LE.P) THEN
          K = J
          P = EIGL(J)
        ENDIF
      ENDDO
      IF (K.NE.I) THEN
        EIGL(K) = EIGL(I)
        EIGL(I) = P
        DO J = 1, N
          P = EIGV(J,I)
          EIGV(J,I) = EIGV(J,K)
          EIGV(J,K) = P
        ENDDO
      ENDIF
    ENDDO
    RETURN
  END SUBROUTINE UTL_EIGEN3
  !-----------------------------------------------------------------------------
  DOUBLE PRECISION FUNCTION UTL_ELAPSED_TIME(IBDT) RESULT (ELTIME)
    !   Calculate elapsed time, in seconds
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, DIMENSION(8), INTENT(IN) :: IBDT
    !
    !   Local variables
    INTEGER :: IBD, IED, LEAP, M, MB, MC, ME, NDAYS, NM,   &
               NSPD
    DOUBLE PRECISION :: ELSEC
    CHARACTER(LEN=10) :: CHDATE, CHTIME, CHZONE
    INTEGER, DIMENSION(8) :: IEDT
    INTEGER, DIMENSION(12) :: IDPM
    DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
    DATA NSPD/86400/  ! Seconds per day
    !
    !     Get current date and time and assign to IEDT
    CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IEDT)
    !
    !     Calculate elapsed time in days and seconds
    NDAYS=0
    LEAP=0
    IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
    IBD = IBDT(3)            ! BEGIN DAY
    IED = IEDT(3)            ! END DAY
    !     FIND DAYS
    IF (IBDT(2).NE.IEDT(2)) THEN
    !       MONTHS DIFFER
      MB = IBDT(2)             ! BEGIN MONTH
      ME = IEDT(2)             ! END MONTH
      NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
      IF (MB.GT.ME) NM = NM+12
      MC=MB-1
      DO 10 M=1,NM
        MC=MC+1                ! MC IS CURRENT MONTH
        IF (MC.EQ.13) MC = 1
        IF (MC.EQ.MB) THEN
          NDAYS = NDAYS+IDPM(MC)-IBD
          IF (MC.EQ.2) NDAYS = NDAYS + LEAP
        ELSEIF (MC.EQ.ME) THEN
          NDAYS = NDAYS+IED
        ELSE
          NDAYS = NDAYS+IDPM(MC)
          IF (MC.EQ.2) NDAYS = NDAYS + LEAP
        ENDIF
 10   CONTINUE
    ELSEIF (IBD.LT.IED) THEN
      !       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
      NDAYS = IED-IBD
    ENDIF
    ELSEC=NDAYS*NSPD
    !
    !     ADD OR SUBTRACT SECONDS
    ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0D0
    ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0D0
    ELSEC = ELSEC+(IEDT(7)-IBDT(7))*1.0D0
    ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001D0
    !
    ELTIME = ELSEC
    !
    RETURN
  END FUNCTION UTL_ELAPSED_TIME
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_ENDTIME(IBDT,IOUT)
    !   Get end time and calculate elapsed time
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, DIMENSION(8), INTENT(IN) :: IBDT
    INTEGER,               INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I, IBD, IED, LEAP, M, MB, MC, ME, MSECS, NDAYS, NHOURS, NM,   &
               NMINS, NRSECS, NSECS, NSPD
    DOUBLE PRECISION :: ELSEC, RSECS
    CHARACTER(LEN=10) :: CHDATE, CHTIME, CHZONE
    INTEGER, DIMENSION(8) :: IEDT
    INTEGER, DIMENSION(12) :: IDPM
    DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
    DATA NSPD/86400/  ! Seconds per day
    !
    !   Format statements
    1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',   &
        I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
    1010 FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,  &
        ' Minutes, ',I2,' Seconds',/)
    1020 FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,   &
        ' Minutes, ',I2,' Seconds',/)
    1030 FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',    &
        I2,'.',I3.3,' Seconds',/)
    1040 FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
    !
    !     Get current date and time, assign to IEDT, and write to screen
    CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IEDT)
    WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
    !
    !     Calculate elapsed time in days and seconds
    NDAYS=0
    LEAP=0
    IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
    IBD = IBDT(3)            ! BEGIN DAY
    IED = IEDT(3)            ! END DAY
    !     FIND DAYS
    IF (IBDT(2).NE.IEDT(2)) THEN
    !       MONTHS DIFFER
      MB = IBDT(2)             ! BEGIN MONTH
      ME = IEDT(2)             ! END MONTH
      NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
      IF (MB.GT.ME) NM = NM+12
      MC=MB-1
      DO 10 M=1,NM
        MC=MC+1                ! MC IS CURRENT MONTH
        IF (MC.EQ.13) MC = 1
        IF (MC.EQ.MB) THEN
          NDAYS = NDAYS+IDPM(MC)-IBD
          IF (MC.EQ.2) NDAYS = NDAYS + LEAP
        ELSEIF (MC.EQ.ME) THEN
          NDAYS = NDAYS+IED
        ELSE
          NDAYS = NDAYS+IDPM(MC)
          IF (MC.EQ.2) NDAYS = NDAYS + LEAP
        ENDIF
 10   CONTINUE
    ELSEIF (IBD.LT.IED) THEN
      !       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
      NDAYS = IED-IBD
    ENDIF
    ELSEC=NDAYS*NSPD
    !
    !     ADD OR SUBTRACT SECONDS
    ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
    ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
    ELSEC = ELSEC+(IEDT(7)-IBDT(7))
    ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
    !
    !     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
    NDAYS = ELSEC/NSPD
    RSECS = MOD(ELSEC,86400.0D0)
    NHOURS = RSECS/3600.0D0
    RSECS = MOD(RSECS,3600.0D0)
    NMINS = RSECS/60.0D0
    RSECS = MOD(RSECS,60.0D0)
    NSECS = RSECS
    RSECS = MOD(RSECS,1.0D0)
    MSECS = NINT(RSECS*1000.0D0)
    NRSECS = NSECS
    IF (RSECS.GE.0.5) NRSECS=NRSECS+1
    !
    !     Write elapsed time to screen
    IF (NDAYS.GT.0) THEN
      WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
    ELSEIF (NHOURS.GT.0) THEN
      WRITE(*,1020) NHOURS,NMINS,NRSECS
    ELSEIF (NMINS.GT.0) THEN
      WRITE(*,1030) NMINS,NSECS,MSECS
    ELSE
      WRITE(*,1040) NSECS,MSECS
    ENDIF
    !
    !     Write times to IOUT file if requested
    IF (IOUT .GT. 0) THEN
      WRITE(IOUT,'(1X)')
      WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
      IF (NDAYS.GT.0) THEN
        WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
      ELSEIF (NHOURS.GT.0) THEN
        WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
      ELSEIF (NMINS.GT.0) THEN
        WRITE(IOUT,1030) NMINS,NSECS,MSECS
      ELSE
        WRITE(IOUT,1040) NSECS,MSECS
      ENDIF
    ENDIF
    RETURN
  END SUBROUTINE UTL_ENDTIME
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   Subroutines comprising the generic subroutine UTL_FILTER
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTER_C(IERR,HEAD,IOUT,KEYWORD,CHARV,LIST)
    !   Traverse a linked list and a store value associated with a
    !   given keyword into an CHARACTER scalar variable (CHARV)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,               INTENT(INOUT) :: IERR
    TYPE (LLIST),          POINTER       :: HEAD
    INTEGER,               INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),      INTENT(IN)    :: KEYWORD
    CHARACTER(LEN=*),      INTENT(INOUT) :: CHARV
    INTEGER, OPTIONAL,     INTENT(IN)    :: LIST
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY
    !
    !   Format statements
    200 FORMAT(/,1X,'*** PROGRAMMER ERROR: LIST exceeds number of lists ',/,   &
        1X,'-- STOP EXECUTION (UTL_FILTER_C)')
    !
    NULLIFY(PTR,PIPTR)
    IERR = 0
    LISTLOCAL = 1
    IF (PRESENT(LIST)) THEN
      IF (LIST>1) LISTLOCAL = LIST
    ENDIF
    !   Find the number of lists and compare to LISTLOCAL
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    IF (LISTLOCAL>KL) THEN
      WRITE(*,200)
      IERR = IERR+1
      GOTO 900
    ENDIF
    !
    !   Traverse the list and store CHARACTER value associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2==LISTLOCAL) THEN
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            CALL UTL_ARR2STRING(PIPTR%NCHAR,PIPTR%STRING,CHARV)
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
        EXIT OUTER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    900 CONTINUE
    IF (IERR>0) CALL UTL_STOP(' ')
    RETURN
  END SUBROUTINE UTL_FILTER_C
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTER_D(IERR,HEAD,IOUT,KEYWORD,RS,LIST)
    !   Traverse a linked list and a store value associated with a
    !   given keyword into a DOUBLE PRECISION scalar variable (RS)
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,               INTENT(INOUT) :: IERR
    TYPE (LLIST),          POINTER       :: HEAD
    INTEGER,               INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),      INTENT(IN)    :: KEYWORD
    DOUBLE PRECISION,      INTENT(INOUT) :: RS
    INTEGER, OPTIONAL,     INTENT(IN)    :: LIST
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: IOST, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY, KWLOC, VLLOC
    !
    !   Format statements
    200 FORMAT(/,1X,'*** PROGRAMMER ERROR: LIST exceeds number of lists ',/,   &
        1X,'-- STOP EXECUTION (UTL_FILTER_D)')
    240 FORMAT(/,1X,   &
        '*** WARNING: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a DOUBLE PRECISION',   &
        ' -- (UTL_FILTER_D)')
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a DOUBLE PRECISION',   &
        ' -- (UTL_FILTER_D)')
    !
    NULLIFY(PTR,PIPTR)
    IERR = 0
    LISTLOCAL = 1
    IF (PRESENT(LIST)) THEN
      IF (LIST>1) LISTLOCAL = LIST
    ENDIF
    !   Find the number of lists and compare to LISTLOCAL
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    IF (LISTLOCAL>KL) THEN
      WRITE(IOUT,200)
      IERR = 1
      GOTO 900
    ENDIF
    !
    !   Traverse the list and store DOUBLE PRECISION value associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2==LISTLOCAL) THEN
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            READ(PIPTR%VALUE,*,IOSTAT=IOST) RS
            IF (IOST .NE. 0) THEN
              IF (IVERB>0) WRITE(IOUT,240) TRIM(PIPTR%KEYWORD),TRIM(PIPTR%VALUE)
              IERR = 2
              KWLOC = PIPTR%KEYWORD
              VLLOC = PIPTR%VALUE
            ELSE
              IERR = 0
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
        EXIT OUTER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    900 CONTINUE
    IF (IERR>0) THEN
      WRITE(IOUT,250) TRIM(KWLOC),TRIM(VLLOC)
      CALL UTL_STOP(' ')
    ENDIF
    RETURN
  END SUBROUTINE UTL_FILTER_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTER_I(IERR,HEAD,IOUT,KEYWORD,INTS,LIST)
    !   Traverse a linked list and a store value associated with a
    !   given keyword into an INTEGER scalar variable (INTS)
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,               INTENT(INOUT) :: IERR
    TYPE (LLIST),          POINTER       :: HEAD
    INTEGER,               INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),      INTENT(IN)    :: KEYWORD
    INTEGER,               INTENT(INOUT) :: INTS
    INTEGER, OPTIONAL,     INTENT(IN)    :: LIST
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: IOST, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY, KWLOC, VLLOC
    !
    !   Format statements
    200 FORMAT(/,1X,'*** PROGRAMMER ERROR: LIST exceeds number of lists ',/,   &
        1X,'-- STOP EXECUTION (UTL_FILTER_I)')
    240 FORMAT(/,1X,   &
        '*** WARNING: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as an INTEGER',   &
        ' -- (UTL_FILTER_I)')
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as an INTEGER',   &
        ' -- (UTL_FILTER_I)')
    !
    NULLIFY(PTR,PIPTR)
    IERR = 0
    LISTLOCAL = 1
    IF (PRESENT(LIST)) THEN
      IF (LIST>1) LISTLOCAL = LIST
    ENDIF
    !   Find the number of lists and compare to LISTLOCAL
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    IF (LISTLOCAL>KL) THEN
      WRITE(*,200)
      IERR = 1
      GOTO 900
    ENDIF
    !
    !   Traverse the list and store INTEGER value associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2==LISTLOCAL) THEN
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            READ(PIPTR%VALUE,*,IOSTAT=IOST) INTS
            IF (IOST .NE. 0) THEN
              IF (IVERB>0) WRITE(IOUT,240) TRIM(PIPTR%KEYWORD),TRIM(PIPTR%VALUE)
              IERR = 2
              KWLOC = PIPTR%KEYWORD
              VLLOC = PIPTR%VALUE
            ELSE
              IERR = 0
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
        EXIT OUTER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    900 CONTINUE
    IF (IERR>0) THEN
      IF (IERR==2) WRITE(IOUT,250) TRIM(KWLOC),TRIM(VLLOC)
      CALL UTL_STOP(' ')
    ENDIF
    RETURN
  END SUBROUTINE UTL_FILTER_I
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTER_L(IERR,HEAD,IOUT,KEYWORD,LV,LIST)
    !   Traverse a linked list and a store value associated with a
    !   given keyword into a LOGICAL scalar variable (LV)
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,               INTENT(INOUT) :: IERR
    TYPE (LLIST),          POINTER       :: HEAD
    INTEGER,               INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),      INTENT(IN)    :: KEYWORD
    LOGICAL,               INTENT(INOUT) :: LV
    INTEGER, OPTIONAL,     INTENT(IN)    :: LIST
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY
    CHARACTER(LEN=40)     :: LSTRING, UPWORD, KWLOC, VLLOC
    !
    !   Format statements
    200 FORMAT(/,1X,'*** PROGRAMMER ERROR: LIST exceeds number of lists ',/,   &
        1X,'-- STOP EXECUTION (UTL_FILTER_L)')
    240 FORMAT(/,1X,   &
        '*** WARNING: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a LOGICAL',   &
        ' -- (UTL_FILTER_L)',/)
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a LOGICAL',   &
        ' -- (UTL_FILTER_L)',/)
    !
    NULLIFY(PTR,PIPTR)
    IERR = 0
    LISTLOCAL = 1
    IF (PRESENT(LIST)) THEN
      IF (LIST>1) LISTLOCAL = LIST
    ENDIF
    !   Find the number of lists and compare to LISTLOCAL
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    IF (LISTLOCAL>KL) THEN
      WRITE(IOUT,200)
      IERR = 1
      GOTO 900
    ENDIF
    !
    !   Traverse the list and store LOGICAL value associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2==LISTLOCAL) THEN
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            CALL UTL_ARR2STRING(PIPTR%NCHAR,PIPTR%STRING,LSTRING)
            CALL UTL_CASE(LSTRING,UPWORD,1)
            IF (UPWORD=='TRUE' .OR. UPWORD == 'T' .OR. UPWORD=='YES'   &
                .OR. UPWORD == 'Y') THEN
              LV = .TRUE.
              IERR = 0
            ELSEIF (UPWORD=='FALSE' .OR. UPWORD == 'F' .OR. UPWORD=='NO'   &
                .OR. UPWORD == 'N') THEN
              LV = .FALSE.
              IERR = 0
            ELSE
              IF (IVERB>0) WRITE(IOUT,240)TRIM(KEYWORD),TRIM(LSTRING)
              IERR = 2
              KWLOC = PIPTR%KEYWORD
              VLLOC = PIPTR%VALUE
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
        EXIT OUTER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    900 CONTINUE
    IF (IERR>0) THEN
      IF (IERR==2) WRITE(IOUT,250)TRIM(KWLOC),TRIM(VLLOC)
      CALL UTL_STOP(' ')
    ENDIF
    RETURN
  END SUBROUTINE UTL_FILTER_L
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTER_R(IERR,HEAD,IOUT,KEYWORD,RS,LIST)
    !   Traverse a linked list and a store value associated with a
    !   given keyword into a REAL scalar variable (RS)
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,               INTENT(INOUT) :: IERR
    TYPE (LLIST),          POINTER       :: HEAD
    INTEGER,               INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),      INTENT(IN)    :: KEYWORD
    REAL,                  INTENT(INOUT) :: RS
    INTEGER, OPTIONAL,     INTENT(IN)    :: LIST
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: IOST, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY, KWLOC, VLLOC
    !
    !   Format statements
    200 FORMAT(/,1X,'*** PROGRAMMER ERROR: LIST exceeds number of lists ',/,   &
        1X,'-- STOP EXECUTION (UTL_FILTER_R)')
    240 FORMAT(/,1X,   &
        '*** WARNING: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a REAL',   &
        ' -- (UTL_FILTER_R)')
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a REAL',   &
        ' -- (UTL_FILTER_R)')
    !
    NULLIFY(PTR,PIPTR)
    IERR = 0
    LISTLOCAL = 1
    IF (PRESENT(LIST)) THEN
      IF (LIST>1) LISTLOCAL = LIST
    ENDIF
    !   Find the number of lists and compare to LISTLOCAL
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    IF (LISTLOCAL>KL) THEN
      WRITE(IOUT,200)
      IERR = 1
      GOTO 900
    ENDIF
    !
    !   Traverse the list and store REAL value associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2==LISTLOCAL) THEN
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            READ(PIPTR%VALUE,*,IOSTAT=IOST) RS
            IF (IOST .NE. 0) THEN
              IF (IVERB>0) WRITE(IOUT,240) TRIM(PIPTR%KEYWORD),TRIM(PIPTR%VALUE)
              IERR = 2
              KWLOC = PIPTR%KEYWORD
              VLLOC = PIPTR%VALUE
            ELSE
              IERR = 0
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
        EXIT OUTER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    900 CONTINUE
    IF (IERR>0) THEN
      IF (IERR==2) WRITE(IOUT,250) TRIM(KWLOC),TRIM(VLLOC)
      CALL UTL_STOP(' ')
    ENDIF
    RETURN
  END SUBROUTINE UTL_FILTER_R
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   End of subroutines comprising the generic subroutine UTL_FILTER
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   Subroutines comprising the generic subroutine UTL_FILTERLIST
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTERLIST_C(HEAD,IOUT,KEYWORD,NDIM,IERR,CA,MORE,IARRSTART,   &
                              LISTSTART)
    !   Traverse a nest of linked lists and store values associated with a
    !   given keyword into a CHARACTER array (CA)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER                            :: HEAD
    INTEGER,                           INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),                  INTENT(IN)    :: KEYWORD
    INTEGER,                           INTENT(IN)    :: NDIM
    INTEGER,                           INTENT(INOUT) :: IERR
    CHARACTER(LEN=*), DIMENSION(NDIM), INTENT(INOUT) :: CA
    INTEGER,                           INTENT(OUT)   :: MORE
    INTEGER, OPTIONAL,                 INTENT(IN)    :: IARRSTART
    INTEGER, OPTIONAL,                 INTENT(IN)    :: LISTSTART
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: I, IARRLOCAL, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY
    !
    !   Format statements
!    200 FORMAT(/,1X,'*** PROGRAMMER ERROR: Number of lists (',I6,   &
!        ') exceeds array dimension (',I6,')',/,1X,   &
!        '-- STOP EXECUTION (UTL_FILTERLIST_C)')
    !
    NULLIFY(PTR,PIPTR)
    MORE = 0
    IARRLOCAL = 1
    IF (PRESENT(IARRSTART)) THEN
      IF (IARRSTART>1) IARRLOCAL = IARRSTART
    ENDIF
    LISTLOCAL = 1
    IF (PRESENT(LISTSTART)) THEN
      IF (LISTSTART>1) LISTLOCAL = LISTSTART
    ENDIF
    !   Find the number of lists and compare to NDIM
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    !
    !   Traverse the lists and store CHARACTER values associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    I = IARRLOCAL-1
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2.GE.LISTLOCAL) THEN
        I = I+1
        IF (I>NDIM) THEN
          MORE = KL-KL2
          EXIT OUTER
        ENDIF
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            CALL UTL_ARR2STRING(PIPTR%NCHAR,PIPTR%STRING,CA(I))
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    RETURN
  END SUBROUTINE UTL_FILTERLIST_C
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTERLIST_D(HEAD,IOUT,KEYWORD,NDIM,IERR,DA,MORE,IARRSTART,   &
                              LISTSTART)
    !   Traverse a nest of linked lists and store values associated with a
    !   given keyword into a DOUBLE PRECISION array (DA)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER                            :: HEAD
    INTEGER,                           INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),                  INTENT(IN)    :: KEYWORD
    INTEGER,                           INTENT(IN)    :: NDIM
    INTEGER,                           INTENT(INOUT) :: IERR
    DOUBLE PRECISION, DIMENSION(NDIM), INTENT(INOUT) :: DA
    INTEGER,                           INTENT(OUT)   :: MORE
    INTEGER, OPTIONAL,                 INTENT(IN)    :: IARRSTART
    INTEGER, OPTIONAL,                 INTENT(IN)    :: LISTSTART
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: I, IARRLOCAL, IOST, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY
    !
    !   Format statements
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a DOUBLE PRECISION',   &
        ' -- STOP EXECUTION (UTL_FILTERLIST_D)')
    !
    NULLIFY(PTR,PIPTR)
    MORE = 0
    IARRLOCAL = 1
    IF (PRESENT(IARRSTART)) THEN
      IF (IARRSTART>1) IARRLOCAL = IARRSTART
    ENDIF
    LISTLOCAL = 1
    IF (PRESENT(LISTSTART)) THEN
      IF (LISTSTART>1) LISTLOCAL = LISTSTART
    ENDIF
    !   Find the number of lists and compare to NDIM
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    !
    !   Traverse the lists and store DOUBLE PRECISION values associated with
    !   keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    I = IARRLOCAL-1
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2.GE.LISTLOCAL) THEN
        I = I+1
        IF (I>NDIM) THEN
          MORE = KL-KL2
          EXIT OUTER
        ENDIF
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            READ(PIPTR%VALUE,*,IOSTAT=IOST) DA(I)
            IF (IOST .NE. 0) THEN
              WRITE(IOUT,250) TRIM(PIPTR%KEYWORD),TRIM(PIPTR%VALUE)
              IERR = IERR+1
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    RETURN
  END SUBROUTINE UTL_FILTERLIST_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTERLIST_I(HEAD,IOUT,KEYWORD,NDIM,IERR,IA,MORE,IARRSTART,   &
                              LISTSTART)
    !   Traverse a nest of linked lists and store values associated with a
    !   given keyword into an INTEGER array (IA)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER                   :: HEAD
    INTEGER,                  INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),         INTENT(IN)    :: KEYWORD
    INTEGER,                  INTENT(IN)    :: NDIM
    INTEGER,                  INTENT(INOUT) :: IERR
    INTEGER, DIMENSION(NDIM), INTENT(INOUT) :: IA
    INTEGER,                  INTENT(OUT)   :: MORE
    INTEGER, OPTIONAL,        INTENT(IN)    :: IARRSTART
    INTEGER, OPTIONAL,        INTENT(IN)    :: LISTSTART
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: I, IARRLOCAL, IOST, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY
    !
    !   Format statements
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as an INTEGER',   &
        ' -- STOP EXECUTION (UTL_FILTERLIST_I)')
    !
    NULLIFY(PTR,PIPTR)
    MORE = 0
    IARRLOCAL = 1
    IF (PRESENT(IARRSTART)) THEN
      IF (IARRSTART>1) IARRLOCAL = IARRSTART
    ENDIF
    LISTLOCAL = 1
    IF (PRESENT(LISTSTART)) THEN
      IF (LISTSTART>1) LISTLOCAL = LISTSTART
    ENDIF
    !   Find the number of lists and compare to NDIM
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    !
    !   Traverse the lists and store INTEGER values associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    I = IARRLOCAL-1
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2.GE.LISTLOCAL) THEN
        I = I+1
        IF (I>NDIM) THEN
          MORE = KL-KL2
          EXIT OUTER
        ENDIF
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            READ(PIPTR%VALUE,*,IOSTAT=IOST) IA(I)
            IF (IOST .NE. 0) THEN
              WRITE(IOUT,250) TRIM(PIPTR%KEYWORD),TRIM(PIPTR%VALUE)
              IERR = IERR+1
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    RETURN
  END SUBROUTINE UTL_FILTERLIST_I
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTERLIST_L(HEAD,IOUT,KEYWORD,NDIM,IERR,LA,MORE,IARRSTART,   &
                              LISTSTART)
    !   Traverse a nest of linked lists and store values associated with a
    !   given keyword into a LOGICAL array (LA)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER                   :: HEAD
    INTEGER,                  INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),         INTENT(IN)    :: KEYWORD
    INTEGER,                  INTENT(IN)    :: NDIM
    INTEGER,                  INTENT(INOUT) :: IERR
    LOGICAL, DIMENSION(NDIM), INTENT(INOUT) :: LA
    INTEGER,                  INTENT(OUT)   :: MORE
    INTEGER, OPTIONAL,        INTENT(IN)    :: IARRSTART
    INTEGER, OPTIONAL,        INTENT(IN)    :: LISTSTART
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: I, IARRLOCAL, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPWORD, UPKEY
    !
    !   Format statements
    !
    NULLIFY(PTR,PIPTR)
    MORE = 0
    IARRLOCAL = 1
    IF (PRESENT(IARRSTART)) THEN
      IF (IARRSTART>1) IARRLOCAL = IARRSTART
    ENDIF
    LISTLOCAL = 1
    IF (PRESENT(LISTSTART)) THEN
      IF (LISTSTART>1) LISTLOCAL = LISTSTART
    ENDIF
    !   Find the number of lists and compare to NDIM
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    !
    !   Traverse the lists and store LOGICAL values associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    I = IARRLOCAL-1
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2.GE.LISTLOCAL) THEN
        I = I+1
        IF (I>NDIM) THEN
          MORE = KL-KL2
          EXIT OUTER
        ENDIF
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            CALL UTL_CASE(PIPTR%VALUE,UPWORD,1)
            IF (UPWORD == 'TRUE' .OR. UPWORD == 'T' .OR. UPWORD == 'YES'   &
                .OR. UPWORD == 'Y') THEN
              LA(I) = .TRUE.
            ELSEIF (UPWORD == 'FALSE' .OR. UPWORD == 'F' .OR. UPWORD == 'NO'   &
                .OR. UPWORD == 'N') THEN
              LA(I) = .FALSE.
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    RETURN
  END SUBROUTINE UTL_FILTERLIST_L
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FILTERLIST_R(HEAD,IOUT,KEYWORD,NDIM,IERR,RA,MORE,IARRSTART,   &
                              LISTSTART)
    !   Traverse a nest of linked lists and store values associated with a
    !   given keyword into a REAL array (RA)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER                :: HEAD
    INTEGER,               INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),      INTENT(IN)    :: KEYWORD
    INTEGER,               INTENT(IN)    :: NDIM
    INTEGER,               INTENT(INOUT) :: IERR
    REAL, DIMENSION(NDIM), INTENT(INOUT) :: RA
    INTEGER,               INTENT(OUT)   :: MORE
    INTEGER, OPTIONAL,     INTENT(IN)    :: IARRSTART
    INTEGER, OPTIONAL,     INTENT(IN)    :: LISTSTART
    !
    !   Local variables
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    INTEGER               :: I, IARRLOCAL, IOST, KL, KL2, LISTLOCAL
    CHARACTER(LEN=40)     :: UPKEY
    !
    !   Format statements
    250 FORMAT(/,1X,   &
        '*** ERROR: For keyword "',A,'" the value "',A,'"',/,1X,   &
        ' can''t be interpreted as a REAL',   &
        ' -- STOP EXECUTION (UTL_FILTERLIST_R)')
    !
    NULLIFY(PTR,PIPTR)
    MORE = 0
    IARRLOCAL = 1
    IF (PRESENT(IARRSTART)) THEN
      IF (IARRSTART>1) IARRLOCAL = IARRSTART
    ENDIF
    LISTLOCAL = 1
    IF (PRESENT(LISTSTART)) THEN
      IF (LISTSTART>1) LISTLOCAL = LISTSTART
    ENDIF
    !   Find the number of lists and compare to NDIM
    PTR => HEAD
    KL = 0
    COUNTLISTS: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT COUNTLISTS ! Pointer valid?
      KL = KL+1
      PTR => PTR%NEXTLIST                        ! Get pointer to next list
    ENDDO COUNTLISTS
    !
    !   Traverse the lists and store REAL values associated with keyword
    CALL UTL_CASE(KEYWORD,UPKEY,1)
    PTR => HEAD
    I = IARRLOCAL-1
    KL2 = 0
    OUTER: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTER  ! Pointer valid?
      KL2 = KL2+1
      IF (KL2.GE.LISTLOCAL) THEN
        I = I+1
        IF (I>NDIM) THEN
          MORE = KL-KL2
          EXIT OUTER
        ENDIF
        PIPTR => PTR%LHEAD
        !   Traverse the entries for this list
        INNER: DO
          IF (.NOT. ASSOCIATED(PIPTR)) EXIT INNER
          IF (PIPTR%KEYWORD == UPKEY) THEN
            READ(PIPTR%VALUE,*,IOSTAT=IOST) RA(I)
            IF (IOST .NE. 0) THEN
              WRITE(IOUT,250) TRIM(PIPTR%KEYWORD),TRIM(PIPTR%VALUE)
              IERR = IERR+1
            ENDIF
          ENDIF
          PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
        ENDDO INNER
      ENDIF
      PTR => PTR%NEXTLIST                     ! Get pointer to next list
    ENDDO OUTER
    RETURN
  END SUBROUTINE UTL_FILTERLIST_R
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   End of subroutines comprising the generic subroutine UTL_FILTERLIST
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_FSTT(NP,IDOF,TST)
    !   DETERMINE THE VALUE OF THE F STATISTIC NEEDED TO CALCULATE
    !   BEALE'S MEASURE OF LINEARITY AND SCHEFFE CONFIDENCE INTERVALS
    !   -- MODIFIED FROM UCODE VERSION -- ERB 9/23/99
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(IN)  :: NP
    INTEGER,          INTENT(IN)  :: IDOF
    DOUBLE PRECISION, INTENT(OUT) :: TST
    !
    !   Local variables
    DOUBLE PRECISION, DIMENSION(19,34) :: F
    DOUBLE PRECISION :: TST1, TST2
    INTEGER :: I, J
    INTEGER, DIMENSION(19) :: ITABLE1
    INTEGER, DIMENSION(34) :: ITABLE2
    !
    !  ------------------------------------------------------------------
    !
    !  NP is indicator for table1
    DATA (ITABLE1(I),I=1,19)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12,  &
          15, 20, 24, 30, 40, 60, 120, 32000/
    !  IDOF is indicator for table2
    DATA (ITABLE2(I),I=1,34)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,  &
          13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,  &
          28, 29, 30, 40, 60, 120, 32000/
    !
    !  F TABLE IS SET UP AS (NP,IDOF)
    !
    DATA (F(1,I),I=1,34)   &
         /161.45D0, 18.513D0, 10.128D0, 7.7086D0, 6.6079D0, 5.9874D0,   &
          5.5914D0, 5.3177D0, 5.1174D0, 4.9646D0, 4.8443D0, 4.7472D0,   &
          4.6672D0, 4.6001D0, 4.5431D0, 4.4940D0, 4.4513D0, 4.4139D0,   &
          4.3808D0, 4.3513D0, 4.3248D0, 4.3009D0, 4.2793D0, 4.2597D0,   &
          4.2417D0, 4.2252D0, 4.2100D0, 4.1960D0, 4.1830D0, 4.1709D0,   &
          4.0848D0, 4.0012D0, 3.9201D0, 3.8415D0/
    !
    DATA (F(2,I),I=1,34)   &
         /199.50D0, 19.000D0, 9.5521D0, 6.9443D0, 5.7861D0, 5.1433D0,   &
          4.7374D0, 4.4590D0, 4.2565D0, 4.1028D0, 3.9823D0, 3.8853D0,   &
          3.8056D0, 3.7389D0, 3.6823D0, 3.6337D0, 3.5915D0, 3.5546D0,   &
          3.5219D0, 3.4928D0, 3.4668D0, 3.4434D0, 3.4221D0, 3.4028D0,   &
          3.3852D0, 3.3690D0, 3.3541D0, 3.3404D0, 3.3277D0, 3.3158D0,   &
          3.2317D0, 3.1504D0, 3.0718D0, 2.9957D0/
    !
    DATA (F(3,I),I=1,34)   &
         /215.71D0, 19.164D0, 9.2766D0, 6.5914D0, 5.4095D0, 4.7571D0,   &
          4.3468D0, 4.0662D0, 3.8626D0, 3.7083D0, 3.5874D0, 3.4903D0,   &
          3.4105D0, 3.3439D0, 3.2874D0, 3.2389D0, 3.1968D0, 3.1599D0,   &
          3.1274D0, 3.0984D0, 3.0725D0, 3.0491D0, 3.0280D0, 3.0088D0,   &
          2.9912D0, 2.9751D0, 2.9604D0, 2.9467D0, 2.9340D0, 2.9223D0,   &
          2.8387D0, 2.7581D0, 2.6802D0, 2.6049D0/
    !
    DATA (F(4,I),I=1,34)   &
         /224.58D0, 19.247D0, 9.1172D0, 6.3883D0, 5.1922D0, 4.5337D0,   &
          4.1203D0, 3.8378D0, 3.6331D0, 3.4780D0, 3.3567D0, 3.2592D0,   &
          3.1791D0, 3.1122D0, 3.0556D0, 3.0069D0, 2.9647D0, 2.9277D0,   &
          2.8951D0, 2.8661D0, 2.8401D0, 2.8167D0, 2.7955D0, 2.7763D0,   &
          2.7587D0, 2.7426D0, 2.7278D0, 2.7141D0, 2.7014D0, 2.6896D0,   &
          2.6060D0, 2.5252D0, 2.4472D0, 2.3719D0/
    !
    DATA (F(5,I),I=1,34)   &
         /230.16D0, 19.296D0, 9.0135D0, 6.2560D0, 5.0503D0, 4.3874D0,   &
          3.9715D0, 3.6875D0, 3.4817D0, 3.3258D0, 3.2039D0, 3.1059D0,   &
          3.0254D0, 2.9582D0, 2.9013D0, 2.8524D0, 2.8100D0, 2.7729D0,   &
          2.7401D0, 2.7109D0, 2.6848D0, 2.6613D0, 2.6400D0, 2.6207D0,   &
          2.6030D0, 2.5868D0, 2.5719D0, 2.5581D0, 2.5454D0, 2.5336D0,   &
          2.4495D0, 2.3683D0, 2.2900D0, 2.2141D0/
    !
    DATA (F(6,I),I=1,34)   &
         /233.99D0, 19.330D0, 8.9406D0, 6.1631D0, 4.9503D0, 4.2839D0,   &
          3.8660D0, 3.5806D0, 3.3738D0, 3.2172D0, 3.0946D0, 2.9961D0,   &
          2.9153D0, 2.8477D0, 2.7905D0, 2.7413D0, 2.6987D0, 2.6613D0,   &
          2.6283D0, 2.5990D0, 2.5757D0, 2.5491D0, 2.5277D0, 2.5082D0,   &
          2.4904D0, 2.4741D0, 2.4591D0, 2.4453D0, 2.4324D0, 2.4205D0,   &
          2.3359D0, 2.2540D0, 2.1750D0, 2.0986D0/
    !
    DATA (F(7,I),I=1,34)   &
         /236.77D0, 19.353D0, 8.8868D0, 6.0942D0, 4.8759D0, 4.2066D0,   &
          3.7870D0, 3.5005D0, 3.2927D0, 3.1355D0, 3.0123D0, 2.9134D0,   &
          2.8321D0, 2.7642D0, 2.7066D0, 2.6572D0, 2.6143D0, 2.5767D0,   &
          2.5435D0, 2.5140D0, 2.4876D0, 2.4638D0, 2.4422D0, 2.4226D0,   &
          2.4047D0, 2.3883D0, 2.3732D0, 2.3593D0, 2.3463D0, 2.3343D0,   &
          2.2490D0, 2.1665D0, 2.0867D0, 2.0096D0/
    !
    DATA (F(8,I),I=1,34)   &
         /238.88D0, 19.371D0, 8.8452D0, 6.0410D0, 4.8183D0, 4.1468D0,   &
          3.7257D0, 3.4381D0, 3.2296D0, 3.0717D0, 2.9480D0, 2.8486D0,   &
          2.7669D0, 2.6987D0, 2.6408D0, 2.5911D0, 2.5480D0, 2.5102D0,   &
          2.4768D0, 2.4471D0, 2.4205D0, 2.3965D0, 2.3748D0, 2.3551D0,   &
          2.3371D0, 2.3205D0, 2.3053D0, 2.2913D0, 2.2782D0, 2.2662D0,   &
          2.1802D0, 2.0970D0, 2.0164D0, 1.9384D0/
    !
    DATA (F(9,I),I=1,34)   &
         /240.54D0, 19.385D0, 8.8123D0, 5.9988D0, 4.7725D0, 4.0990D0,   &
          3.6767D0, 3.3881D0, 3.1789D0, 3.0204D0, 2.8962D0, 2.7964D0,   &
          2.7144D0, 2.6458D0, 2.5876D0, 2.5377D0, 2.4943D0, 2.4563D0,   &
          2.4227D0, 2.3928D0, 2.3661D0, 2.3419D0, 2.3201D0, 2.3002D0,   &
          2.2821D0, 2.2655D0, 2.2501D0, 2.2360D0, 2.2229D0, 2.2107D0,   &
          2.1240D0, 2.0401D0, 1.9588D0, 1.8799D0/
    !
    DATA (F(10,I),I=1,34)   &
         /241.88D0, 19.396D0, 8.7855D0, 5.9644D0, 4.7351D0, 4.0600D0,   &
          3.6365D0, 3.3472D0, 3.1373D0, 2.9782D0, 2.8536D0, 2.7534D0,   &
          2.6710D0, 2.6021D0, 2.5437D0, 2.4935D0, 2.4499D0, 2.4117D0,   &
          2.3779D0, 2.3479D0, 2.3210D0, 2.2967D0, 2.2747D0, 2.2547D0,   &
          2.2365D0, 2.2197D0, 2.2043D0, 2.1900D0, 2.1768D0, 2.1646D0,   &
          2.0772D0, 1.9926D0, 1.9105D0, 1.8307D0/
    !
    DATA (F(11,I),I=1,34)   &
         /243.91D0, 19.413D0, 8.7446D0, 5.9117D0, 4.6777D0, 3.9999D0,   &
          3.5747D0, 3.2840D0, 3.0729D0, 2.9130D0, 2.7876D0, 2.6866D0,   &
          2.6037D0, 2.5342D0, 2.4753D0, 2.4247D0, 2.3807D0, 2.3421D0,   &
          2.3080D0, 2.2776D0, 2.2504D0, 2.2258D0, 2.2036D0, 2.1834D0,   &
          2.1649D0, 2.1479D0, 2.1323D0, 2.1179D0, 2.1045D0, 2.0921D0,   &
          2.0035D0, 1.9174D0, 1.8337D0, 1.7522D0/
    !
    DATA (F(12,I),I=1,34)   &
         /245.95D0, 19.429D0, 8.7029D0, 5.8578D0, 4.6188D0, 3.9381D0,   &
          3.5108D0, 3.2184D0, 3.0061D0, 2.8450D0, 2.7186D0, 2.6169D0,   &
          2.5331D0, 2.4630D0, 2.4035D0, 2.3522D0, 2.3077D0, 2.2686D0,   &
          2.2341D0, 2.2033D0, 2.1757D0, 2.1508D0, 2.1282D0, 2.1077D0,   &
          2.0889D0, 2.0716D0, 2.0558D0, 2.0411D0, 2.0275D0, 2.0148D0,   &
          1.9245D0, 1.8364D0, 1.7505D0, 1.6664D0/
    !
    DATA (F(13,I),I=1,34)   &
         /248.01D0, 19.446D0, 8.6602D0, 5.8025D0, 4.5581D0, 3.8742D0,   &
          3.4445D0, 3.1503D0, 2.9365D0, 2.7740D0, 2.6464D0, 2.5436D0,   &
          2.4589D0, 2.3879D0, 2.3275D0, 2.2756D0, 2.2304D0, 2.1906D0,   &
          2.1555D0, 2.1242D0, 2.0960D0, 2.0707D0, 2.0476D0, 2.0267D0,   &
          2.0075D0, 1.9898D0, 1.9736D0, 1.9586D0, 1.9446D0, 1.9317D0,   &
          1.8389D0, 1.7480D0, 1.6587D0, 1.5705D0/
    !
    DATA (F(14,I),I=1,34)   &
         /249.05D0, 19.454D0, 8.6385D0, 5.7744D0, 4.5272D0, 3.8415D0,   &
          3.4105D0, 3.1152D0, 2.9005D0, 2.7372D0, 2.6090D0, 2.5055D0,   &
          2.4202D0, 2.3487D0, 2.2878D0, 2.2354D0, 2.1898D0, 2.1497D0,   &
          2.1141D0, 2.0825D0, 2.0540D0, 2.0283D0, 2.0050D0, 1.9838D0,   &
          1.9643D0, 1.9464D0, 1.9299D0, 1.9147D0, 1.9005D0, 1.8874D0,   &
          1.7929D0, 1.7001D0, 1.6084D0, 1.5173D0/
    !
    DATA (F(15,I),I=1,34)   &
         /250.09D0, 19.462D0, 8.6166D0, 5.7459D0, 4.4957D0, 3.8082D0,   &
          3.3758D0, 3.0794D0, 2.8637D0, 2.6996D0, 2.5705D0, 2.4663D0,   &
          2.3803D0, 2.3082D0, 2.2468D0, 2.1938D0, 2.1477D0, 2.1071D0,   &
          2.0712D0, 2.0391D0, 2.0102D0, 1.9842D0, 1.9605D0, 1.9390D0,   &
          1.9192D0, 1.9010D0, 1.8842D0, 1.8687D0, 1.8543D0, 1.8409D0,   &
          1.7444D0, 1.6491D0, 1.5543D0, 1.4591D0/
    !
    DATA (F(16,I),I=1,34)   &
         /251.14D0, 19.471D0, 8.5944D0, 5.7170D0, 4.4638D0, 3.7743D0,   &
          3.3404D0, 3.0428D0, 2.8259D0, 2.6609D0, 2.5309D0, 2.4259D0,   &
          2.3392D0, 2.2664D0, 2.2043D0, 2.1507D0, 2.1040D0, 2.0629D0,   &
          2.0264D0, 1.9938D0, 1.9645D0, 1.9380D0, 1.9139D0, 1.8920D0,   &
          1.8718D0, 1.8533D0, 1.8361D0, 1.8203D0, 1.8055D0, 1.7918D0,   &
          1.6928D0, 1.5943D0, 1.4952D0, 1.3940D0/
    !
    DATA (F(17,I),I=1,34)   &
         /252.20D0, 19.479D0, 8.5720D0, 5.6878D0, 4.4314D0, 3.7398D0,   &
          3.3043D0, 3.0053D0, 2.7872D0, 2.6211D0, 2.4901D0, 2.3842D0,   &
          2.2966D0, 2.2230D0, 2.1601D0, 2.1058D0, 2.0584D0, 2.0166D0,   &
          1.9796D0, 1.9464D0, 1.9165D0, 1.8895D0, 1.8649D0, 1.8424D0,   &
          1.8217D0, 1.8027D0, 1.7851D0, 1.7689D0, 1.7537D0, 1.7396D0,   &
          1.6373D0, 1.5343D0, 1.4290D0, 1.3180D0/
    !
    DATA (F(18,I),I=1,34)   &
         /253.25D0, 19.487D0, 8.5494D0, 5.6581D0, 4.3984D0, 3.7047D0,   &
          3.2674D0, 2.9669D0, 2.7475D0, 2.5801D0, 2.4480D0, 2.3410D0,   &
          2.2524D0, 2.1778D0, 2.1141D0, 2.0589D0, 2.0107D0, 1.9681D0,   &
          1.9302D0, 1.8963D0, 1.8657D0, 1.8380D0, 1.8128D0, 1.7897D0,   &
          1.7684D0, 1.7488D0, 1.7307D0, 1.7138D0, 1.6981D0, 1.6835D0,   &
          1.5766D0, 1.4673D0, 1.3519D0, 1.2214D0/
    !
    DATA (F(19,I),I=1,34)   &
         /254.32D0, 19.496D0, 8.5265D0, 5.6281D0, 4.3650D0, 3.6688D0,   &
          3.2298D0, 2.9276D0, 2.7067D0, 2.5379D0, 2.4045D0, 2.2962D0,   &
          2.2064D0, 2.1307D0, 2.0658D0, 2.0096D0, 1.9604D0, 1.9168D0,   &
          1.8780D0, 1.8432D0, 1.8117D0, 1.7831D0, 1.7570D0, 1.7331D0,   &
          1.7110D0, 1.6906D0, 1.6717D0, 1.6541D0, 1.6377D0, 1.6223D0,   &
          1.5089D0, 1.3893D0, 1.2539D0, 1.0000D0/
    !
    !  ------------------------------------------------------------------
    !
    IF (NP .LE. 10.AND.IDOF .LE. 30) THEN
    !    ENTRIES ARE EXACT FOR FIRST AND SECOND SUBSCRIPTS
      TST=F(NP,IDOF)
      RETURN
    ENDIF
    !
    IF (NP .GT. 10.AND.IDOF .LE. 30) THEN
    !    INTERPOLATE BETWEEN ENTRIES FOR FIRST SUBSCRIPT;
    !    ENTRIES FOR SECOND SUBSCRIPT ARE EXACT
      DO I=11,19
        IF(NP .LE. ITABLE1(I)) THEN
          TST = ((F(I,IDOF)-F(I-1,IDOF))*DBLE(NP-ITABLE1(I-1)) &
                /DBLE(ITABLE1(I)-ITABLE1(I-1)))+F(I-1,IDOF)
          RETURN
        ENDIF
      ENDDO
      TST = F(19,IDOF)
      RETURN
    ENDIF
    !
    IF (NP .LE. 10.AND.IDOF .GT. 30) THEN
    !    INTERPOLATE BETWEEN ENTRIES FOR SECOND SUBSCRIPT;
    !    ENTS FOR FIRST SUBSCRIPT ARE EXACT
    DO J=31,34
      IF(IDOF .LE. ITABLE2(J)) THEN
        TST = ((F(NP,J)-F(NP,J-1))*DBLE(IDOF-ITABLE2(J-1))   &
              /DBLE(ITABLE2(J)-ITABLE2(J-1)))+F(NP,J-1)
        RETURN
      ENDIF
    ENDDO
    TST = F(NP,34)
    RETURN
    ENDIF
    !
    IF (NP .LE. ITABLE1(19)) THEN
      IF (IDOF .LE. ITABLE2(34)) THEN
      ! INTERPOLATE BETWEEN ENTRIES FOR FIRST AND SECOND SUBSCRIPTS
        DO I=11,19
          IF(NP .LE. ITABLE1(I)) THEN
            DO J=31,34
              IF(IDOF .LE. ITABLE2(J)) THEN
                TST1 = ((F(I,J)-F(I-1,J))*DBLE(NP-ITABLE1(I-1))  &
                       /DBLE(ITABLE1(I)-ITABLE1(I-1)))+F(I-1,J)
                TST2 = ((F(I,J-1)-F(I-1,J-1))*DBLE(NP-ITABLE1(I-1))  &
                       /DBLE(ITABLE1(I)-ITABLE1(I-1)))+F(I-1,J-1)
                TST = ((TST1-TST2)*DBLE(IDOF-ITABLE2(J-1))   &
                       /DBLE(ITABLE2(J)-ITABLE2(J-1)))+TST2
                RETURN
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ELSE
      !      NP IS WITHIN RANGE OF VALUES IN ITABLE1, BUT IDOF EXCEEDS
      !      LARGEST VALUE IN ITABLE2
        DO I=11,19
          IF (NP .LE. ITABLE1(I)) THEN
            TST = ((F(I,34)-F(I-1,34))*DBLE(NP-ITABLE1(I-1))   &
                  /DBLE(ITABLE1(I)-ITABLE1(I-1)))+F(I-1,34)
            RETURN
          ENDIF
        ENDDO
      ENDIF
    ENDIF
    !
    TST=F(19,34)
    !
    RETURN
  END SUBROUTINE UTL_FSTT
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETARG(NARG) RESULT (ARG)
    !   Return an argument listed on command line
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER, INTENT(IN), OPTIONAL :: NARG
    CHARACTER(LEN=MAX_STRING_LEN) :: ARG
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: COMLIN
    CHARACTER(LEN=6) :: SUBNAME
    INTEGER :: I, IARG, ICOL, ISTART, ISTOP, N
    DOUBLE PRECISION :: R
    !
    !   Format statements
    100 FORMAT(1X,'Warning: Argument NARG of UTL_GETARG is less',   &
                  ' than 1, using default value 1')
    !
    ARG = ' '
    COMLIN = ' '
    IARG = 1
    IF (PRESENT(NARG)) THEN
      IF (NARG>0) THEN
        IARG = NARG
      ELSE
        IF (IVERB>0) WRITE(*,100)
      ENDIF
    ENDIF
    ! ***
    ! *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
    ! *** allow a program to retrieve command-line arguments.  To enable
    ! *** reading of the name of an input file from the command line,
    ! *** either GETARG or GETCL must be called, but not both.  As
    ! *** distributed, the call to GETCL is uncommented.  For compilers
    ! *** that support GETARG but not GETCL, comment out the line containing
    ! *** the call to GETCL and uncomment the line containing the call to
    ! *** GETARG.
    ! ***
    CALL GETARG(IARG,COMLIN); SUBNAME = 'GETARG' ! Supported by: IVF, ...
    !CALL GETCL(COMLIN); SUBNAME = 'GETCL'       ! Supported by: LF95, ...
    IF (SUBNAME=='GETARG') THEN
      ARG = COMLIN
    ELSEIF (SUBNAME=='GETCL') THEN
      ICOL = 1
      ARGLOOP: DO I=1,IARG
        CALL UTL_RWORD(-1,0,.TRUE.,ICOL,COMLIN,ISTART,ISTOP,N,R)
        IF (COMLIN(ISTART:ISTOP)==' ') EXIT ARGLOOP
        IF (I==IARG) ARG = COMLIN(ISTART:ISTOP)
      ENDDO ARGLOOP
    ENDIF
    RETURN
  END FUNCTION UTL_GETARG
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETCOL_CDM_D(CDM,IC,COL)
    !   Populate an array with values from one column of a compressed matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list veriables
    TYPE (CDMATRIX),                     INTENT(IN)  :: CDM
    INTEGER,                             INTENT(IN)  :: IC
    DOUBLE PRECISION, DIMENSION(CDM%NR), INTENT(OUT) :: COL
    !
    !   Local variables
    INTEGER :: I, ICOL1, IR, NNZ, NR
    INTEGER(KIND=8) :: IPOSI, NEIC, NEPIC
    !
    NR = CDM%NR
    ICOL1 = CDM%ICOL(IC)
    NEPIC = (IC-1)*NR  ! # of elements preceding column IC in true matrix CDM
    NEIC = IC*NR
    NNZ = CDM%NNZ
    !
    COL = 0.0D0
    IF (ICOL1>0) THEN
      DO I=ICOL1,NNZ
        IPOSI = CDM%IPOS(I)
        IF (IPOSI>NEIC) RETURN
        IR = IPOSI-NEPIC
        COL(IR) = CDM%DVAL(I)
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE UTL_GETCOL_CDM_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETCOL_CDM_SUB_D(CDM,IC,COL,IR1,IR2)
    !   Populate an array with values from one column of a compressed matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list veriables
    TYPE (CDMATRIX),                     INTENT(IN)    :: CDM
    INTEGER,                             INTENT(IN)    :: IC
    DOUBLE PRECISION, DIMENSION(CDM%NR), INTENT(INOUT) :: COL
    INTEGER,                             INTENT(IN)    :: IR1
    INTEGER,                             INTENT(IN)    :: IR2
    !
    !   Local variables
    INTEGER :: I, ICOL1, IR, NNZ, NR
    INTEGER(KIND=8) :: IPOSI, NEIC, NEPIC
    !
    NR = CDM%NR
    ICOL1 = CDM%ICOL(IC)
    NEPIC = (IC-1)*NR  ! # of elements preceding column IC in true matrix CDM
    NEIC = NEPIC+IR2
    NNZ = CDM%NNZ
    !
    COL(IR1:IR2) = 0.0D0
    IF (ICOL1>0) THEN
      DO I=ICOL1,NNZ
        IPOSI = CDM%IPOS(I)
        IF (IPOSI>NEIC) RETURN
        IR = IPOSI-NEPIC
        IF (IR .GE. IR1) THEN
          COL(IR) = CDM%DVAL(I)
        ENDIF
      ENDDO
    ENDIF
    RETURN
  END SUBROUTINE UTL_GETCOL_CDM_SUB_D
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETICOL_CDM(CDM,LOC) RESULT (ICOL)
    !   Return column index associated with element LOC in CDMATRIX
    !   structure CDM
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    TYPE (CDMATRIX), INTENT(IN) :: CDM
    INTEGER,         INTENT(IN) :: LOC
    INTEGER :: ICOL
    !
    !   Local variables
    INTEGER :: NR
    INTEGER(KIND=8) :: IPOS
    !
    ICOL = 0
    IF (LOC .GE. 1 .AND. LOC .LE. CDM%NNZ) THEN
      IPOS = CDM%IPOS(LOC)
      NR = CDM%NR
      ICOL = INT((DBLE(IPOS)-0.1)/DBLE(NR))+1
    ENDIF
    RETURN
  END FUNCTION UTL_GETICOL_CDM
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETIROW_CDM(CDM,LOC) RESULT (IROW)
    !   Return row index associated with element LOC in CDMATRIX structure CDM
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    TYPE (CDMATRIX), INTENT(IN) :: CDM
    INTEGER,         INTENT(IN) :: LOC
    INTEGER :: IROW
    !
    !   Local variables
    INTEGER(KIND=8) :: IPOS, NR
    !
    IPOS = CDM%IPOS(LOC)
    NR = CDM%NR
    IROW = MOD((IPOS-1),NR)+1
    RETURN
  END FUNCTION UTL_GETIROW_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETKEYLINE(BLOCKLABEL,ENDKEYS,INUNIT,IOUT,KEYEQUAL,LINE)
    !   Find line where KEYITEM=value is the first entry
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=40),  INTENT(IN)    :: BLOCKLABEL
    LOGICAL,            INTENT(OUT)   :: ENDKEYS
    INTEGER,            INTENT(IN)    :: INUNIT
    INTEGER,            INTENT(IN)    :: IOUT
    CHARACTER(LEN=41),  INTENT(INOUT) :: KEYEQUAL
    CHARACTER(LEN=*),   INTENT(OUT)   :: LINE
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: WORD1, WORD2, UPWORD1, UPWORD2
    INTEGER :: IERR, IFIND, ISTART, ISTOP, LENKEY, LLOC, N
    DOUBLE PRECISION :: R
    !
    LENKEY = LEN_TRIM(KEYEQUAL)
    IERR = 0
    !
    !   Get non-comment lines from file until keyitem is found in
    !   the first entry on a new line
    ENDKEYS = .FALSE.
    NEWNAME: DO
      LINE = UTL_GETLINE(INUNIT,IOUT)
      IF (LINE == ' ') THEN
        ENDKEYS = .TRUE.
        EXIT NEWNAME
      ENDIF
      LLOC = 1
      !   UTL_PREPLINE condenses equalities not enclosed in parentheses.  The
      !   first call allows keyword = value to be recognized by UTL_GETTOKEN as
      !   a single token.  The second call ensures that "keyword = value" will
      !   be interpreted correctly as keyword=value in the following code.
      CALL UTL_PREPLINE(LINE)
      CALL UTL_GETTOKEN(IERR,INUNIT,IOUT,0,LLOC,LINE,ISTART,ISTOP,WORD1)
      CALL UTL_PREPLINE(WORD1)
      CALL UTL_CASE(WORD1,UPWORD1,1)

      !   Check for end of information block
      IF (UPWORD1 == 'END') THEN
        CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
        WORD2 = LINE(ISTART:ISTOP)
        CALL UTL_CASE(WORD2,UPWORD2,1)
        IF (UPWORD2 == BLOCKLABEL) THEN
          ENDKEYS = .TRUE.
          EXIT NEWNAME
        ELSE
          CYCLE NEWNAME
        ENDIF
      ENDIF

      CALL UTL_PREPLINE(UPWORD1)
      !   Check for "*" (wildcard) entry, indicating block contains one list,
      !   with no keyitem
      IF (KEYEQUAL(1:LENKEY) == '*=' .AND. INDEX(UPWORD1,'=') > 1) THEN
        KEYEQUAL='unlikelycharacterstring'
        EXIT NEWNAME
      ENDIF

      !   Check for new keyitem
      IFIND = INDEX(UPWORD1,KEYEQUAL(1:LENKEY))
      IF (IFIND == 1) THEN
        EXIT NEWNAME
      ELSE
        CYCLE NEWNAME
      ENDIF
    ENDDO NEWNAME
    RETURN
  END SUBROUTINE UTL_GETKEYLINE
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETLINE(INUNIT,ICOM) RESULT(LINE)
    !   Return the next non-blank line that is not a comment line, indicated
    !   by a '#' character in column 1.  Argument ICOM is optional; if ICOM
    !   is present and greater than 0, print any comment lines to unit ICOM.
    !   If end of file is reached without finding a non-blank, non-comment line,
    !   a blank line is returned.  If IPREP is present and greater than 0,
    !   eliminate blanks and tab characters that precede or follow "=" signs.
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list and Result variables
    INTEGER, INTENT(IN)           :: INUNIT
    INTEGER, INTENT(IN), OPTIONAL :: ICOM
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    !
    !   Local variables
    INTEGER :: IFOUT, ISTAT
    CHARACTER(LEN=MAX_STRING_LEN) :: TEMP
    !
    !   Check for ICOM argument and set IFOUT accordingly
    IFOUT = 0
    IF(PRESENT(ICOM) .AND. IVERB>3) THEN
      IF (ICOM .GT. 0) IFOUT = ICOM
    ENDIF
    !
    !   Read from unit INUNIT until a non-comment, non-blank line is found
    LINE=' '
    LINEREAD: DO
      READ(INUNIT,'(A)',IOSTAT=ISTAT) TEMP
      IF (ISTAT == 0) THEN
        IF (TEMP(1:1) == '#' .OR. TEMP == ' ') THEN
          IF (IFOUT > 0 .AND. TEMP /= ' ')  &
              WRITE(IFOUT,'(A)') TRIM(TEMP)
          CYCLE
        ELSE
          LINE = TEMP
          EXIT LINEREAD
        ENDIF
      ELSE
        EXIT LINEREAD
      ENDIF
    ENDDO LINEREAD
    !
    RETURN
  END FUNCTION UTL_GETLINE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GET_MATRIX_SECTION_CDM(IERR,CDM,IC1,IC2,IR1,IR2,CDMNEW,ANAME)
    !   Populate a CDM with values from a section of a compressed matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list veriables
    INTEGER,                    INTENT(OUT) :: IERR  ! error flag
    TYPE (CDMATRIX),            INTENT(IN)  :: CDM     ! existing CDM
    INTEGER,                    INTENT(IN)  :: IC1     ! col of upper left corner to extract
    INTEGER,                    INTENT(IN)  :: IC2     ! col of lower right corner to extract
    INTEGER,                    INTENT(IN)  :: IR1     ! row of upper left corner to extract
    INTEGER,                    INTENT(IN)  :: IR2     ! row of lower right corner to extract
    TYPE (CDMATRIX),            INTENT(OUT) :: CDMNEW   ! new CDM of UL
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)  :: ANAME   ! new name
    !
    !   Local variables
    INTEGER :: I, IC, IDIM, J, K, L, NNZ
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: COL
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: UTL_GET_MATRIX_SECTION_CDM')
    101 FORMAT(1X,'       IR1>IR2 so upper left row is below lower right')
    102 FORMAT(1X,'       IC1>IC2 so upper left col is below lower right')
    110 FORMAT(1X,'       IR1, IR2, IC1 or IC2 < 1')
    111 FORMAT(1X,'       IR1 or IR2 > #ROWS in Existing Matrix')
    112 FORMAT(1X,'       IC1 or IC2 > #COLS in Existing Matrix')
    !
    IERR = 0
    IF (IR1 > IR2) THEN
      WRITE(*,100)
      WRITE(*,101)
      IERR=1
      RETURN
    ENDIF
    IF (IC1 > IC2) THEN
      WRITE(*,100)
      WRITE(*,102)
      IERR=1
      RETURN
    ENDIF
    IF (IR1<1 .OR. IR1<1 .OR. IC1<1 .OR. IC2<1) THEN
      WRITE(*,100)
      WRITE(*,110)
      IERR=1
      RETURN
    ENDIF
    IF (IR1>(CDM%NR) .OR. IR2>(CDM%NR)) THEN
      WRITE(*,100)
      WRITE(*,111)
      IERR=1
      RETURN
    ENDIF
    IF (IC1>(CDM%NC) .OR. IC2>(CDM%NC)) THEN
      WRITE(*,100)
      WRITE(*,112)
      IERR=1
      RETURN
    ENDIF
    !
    !ALLOCATE
    ALLOCATE(COL(CDM%NR))
    !
    !   Determine number of nonzero entries in UL
    NNZ = 0
    DO I=IC1,IC2
      CALL UTL_GETCOL(CDM,I,COL,IR1,IR2)
      DO J=IR1,IR2
        IF(COL(J) .NE. 0.D0)NNZ=NNZ+1
      ENDDO
    ENDDO
    IDIM = NNZ
    IF (IDIM<1) IDIM = 1
    !
    !   Construct CDM structure:
    IF (PRESENT(ANAME)) THEN
      CALL UTL_CONSTRUCTCDMATRIX(IDIM,IR2-IR1+1,IC2-IC1+1,CDMNEW,ANAME)
    ELSE
      CALL UTL_CONSTRUCTCDMATRIX(IDIM,IR2-IR1+1,IC2-IC1+1,CDMNEW)
    ENDIF
    CDMNEW%NNZ = NNZ
    !
    ! Populate CDM New Matrix
    IF (NNZ==0) THEN
      !   Special case: CDM contains no nonzero entries.  Initialize
      !   arrays appropriately
      CDMNEW%DVAL(1) = 0.D0
      CDMNEW%IPOS(1) = 1
    ELSE
      !   Populate DVAL with values from ARR; assign IPOS and ICOL accordingly
      K = 0
      L = 0
      EACHCOL: DO I=IC1,IC2
        IC = I-IC1+1
        CALL UTL_GETCOL(CDM,I,COL,IR1,IR2)
        EACHROW: DO J=IR1,IR2
          L=L+1
          IF(COL(J) .NE. 0.D0) THEN
            K=K+1
            CDMNEW%DVAL(K) = DBLE(COL(J))
            CDMNEW%IPOS(K) = L
            IF(CDMNEW%ICOL(IC) == 0)CDMNEW%ICOL(IC) = K
            IF (K==NNZ) EXIT EACHCOL
          ENDIF
        ENDDO EACHROW
      ENDDO EACHCOL
    ENDIF
    ! DEALLOCATE
    DEALLOCATE(COL)
    !
    RETURN
  END SUBROUTINE UTL_GET_MATRIX_SECTION_CDM
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETPOS_CDM(CDM,IR,IC) RESULT (POS)
    !   Returns position in DVAL and IPOS arrays corresponding to element at
    !   row IR, column IC in CDMATRIX structure CDM.  If (IR,IC) is not stored,
    !   0 is returned.  If (IR,IC) is out of range, -1 is returned.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    TYPE (CDMATRIX), INTENT(IN) :: CDM
    INTEGER,         INTENT(IN) :: IR
    INTEGER,         INTENT(IN) :: IC
    INTEGER                     :: POS
    !
    !   Local variables
    INTEGER :: I, ICOL
    INTEGER(KIND=8) :: IPOSI, IPOSX, ITEMP, LASTPOS
    !
    !   Format statements
    100 FORMAT(1X,'WARNING: Probable programmer error.',   &
                  '  (IR,IC) out of range in UTL_GETPOS_CDM')
    !
    POS = -1
    IF (IR<1 .OR. IR>CDM%NR .OR. IC<1 .OR. IC>CDM%NC) THEN
      IF (IVERB>0) WRITE(*,100)
      RETURN
    ENDIF
    !
    POS = 0
    ICOL = CDM%ICOL(IC)
    IF (ICOL .NE. 0) THEN
      ITEMP = (INT(IC,8)-INT(1,8))*INT(CDM%NC,8)
      IPOSX = ITEMP + INT(IR,8)
      LASTPOS = INT(IC,8)*INT(CDM%NR)
      DOI: DO I=ICOL,CDM%NNZ
        IPOSI = CDM%IPOS(I)
        IF (IPOSI==IPOSX) THEN
          POS = I
          EXIT DOI
        ENDIF
        IF (IPOSI>LASTPOS) EXIT DOI
      ENDDO DOI
    ENDIF
    RETURN
  END FUNCTION UTL_GETPOS_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETROW_CDM_D(CDM,IR,ROW)
    !   Populate an array with values from one row of a compressed matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list veriables
    TYPE (CDMATRIX),                     INTENT(IN)  :: CDM
    INTEGER,                             INTENT(IN)  :: IR
    DOUBLE PRECISION, DIMENSION(CDM%NC), INTENT(OUT) :: ROW
    !
    !   Local variables
    INTEGER :: I, IC, ICOL1, NNZ, NR
    INTEGER(KIND=8) :: IPOS, IPOSI
    !
    NNZ = CDM%NNZ
    NR = CDM%NR
    ROW = 0.0D0
    !
    IPOS = IR
    DO IC=1,CDM%NC
      ICOL1 = CDM%ICOL(IC)
      IF (ICOL1>0) THEN
        ONECOL: DO I=ICOL1,NNZ
          IPOSI = CDM%IPOS(I)
          IF (IPOSI==IPOS) THEN
            ROW(IC) = CDM%DVAL(I)
            EXIT ONECOL
          ELSEIF (IPOSI>IPOS) THEN
            EXIT ONECOL
          ENDIF
        ENDDO ONECOL
      ENDIF
      IPOS = IPOS+NR
    ENDDO
    RETURN
  END SUBROUTINE UTL_GETROW_CDM_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETROW_CDM_SUB_D(CDM,IR,ROW,IC1,IC2)
    !   Populate an array with values from one row of a compressed matrix
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list veriables
    TYPE (CDMATRIX),                     INTENT(IN)    :: CDM
    INTEGER,                             INTENT(IN)    :: IR
    DOUBLE PRECISION, DIMENSION(CDM%NC), INTENT(INOUT) :: ROW
    INTEGER,                             INTENT(IN)    :: IC1
    INTEGER,                             INTENT(IN)    :: IC2
    !
    !   Local variables
    INTEGER :: I, IC, ICOL1, NNZ, NR
    INTEGER(KIND=8) :: IPOS, IPOSI
    !
    NNZ = CDM%NNZ
    NR = CDM%NR
    ROW(IC1:IC2) = 0.0D0
    !
    IPOS = NR*(IC1-1)+IR
    DO IC=IC1,IC2
      ICOL1 = CDM%ICOL(IC)
      IF (ICOL1>0) THEN
        ONECOL: DO I=ICOL1,NNZ
          IPOSI = CDM%IPOS(I)
          IF (IPOSI==IPOS) THEN
            ROW(IC) = CDM%DVAL(I)
            EXIT ONECOL
          ELSEIF (IPOSI>IPOS) THEN
            EXIT ONECOL
          ENDIF
        ENDDO ONECOL
      ENDIF
      IPOS = IPOS+NR
    ENDDO
    RETURN
  END SUBROUTINE UTL_GETROW_CDM_SUB_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETTOKEN(KERR,INUNIT,IOUT,NCODE,ICOL,LINE,ISTART,ISTOP,TOKEN)
    !   Return a TOKEN starting at position ICOL in LINE.  TOKEN may include
    !   embedded spaces if enclosed in matching single or double quotes.  Any
    !   (matching) quotes surrounding TOKEN are removed.  Unmatched single or
    !   double quotes may not be embedded in TOKEN.
    USE GLOBAL_DATA, ONLY: BLANKS, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(INOUT) :: KERR
    INTEGER,          INTENT(IN)    :: INUNIT
    INTEGER,          INTENT(IN)    :: IOUT
    INTEGER,          INTENT(IN)    :: NCODE
    INTEGER,          INTENT(INOUT) :: ICOL
    CHARACTER(LEN=*), INTENT(INOUT) :: LINE
    INTEGER,          INTENT(OUT)   :: ISTART
    INTEGER,          INTENT(OUT)   :: ISTOP
    CHARACTER(LEN=*), INTENT(OUT)   :: TOKEN
    !
    !   Local variables
    CHARACTER(LEN=1) :: CH1, CH2
    INTEGER :: ISTART1, KDQ, KSQ, LENWORD, MKDQ, MKSQ, N
    DOUBLE PRECISION :: R
    !
    !   Format statements
    30  FORMAT(1X,'*** ERROR: Unmatched quotes in line:',/,1X,A)
    !
    TOKEN = ' '
    CALL UTL_RWORD(IOUT,NCODE,.FALSE.,ICOL,LINE,ISTART,ISTOP,N,R,INUNIT)
    TOKEN = LINE(ISTART:ISTOP)
    ISTART1 = ISTART
    !
    !   If TOKEN contains an unmatched single or double quote, concatenate
    !   additional words as needed to match quotes
    500 CONTINUE
    KSQ = UTL_COUNTSUBS(TOKEN,CHAR(39))  ! Count single quotes
    MKSQ = MOD(KSQ,2)
    KDQ = UTL_COUNTSUBS(TOKEN,'"')       ! Count double quotes
    MKDQ = MOD(KDQ,2)
    IF (MKSQ>0 .OR. MKDQ>0) THEN
      CALL UTL_RWORD(IOUT,NCODE,.FALSE.,ICOL,LINE,ISTART,ISTOP,N,R,INUNIT)
      IF (LINE(ISTART:ISTOP).NE.' ') THEN
        TOKEN = LINE(ISTART1:ISTOP)
        GOTO 500
      ELSE
        WRITE(IOUT,30) TRIM(LINE)
        KERR = KERR+1
      ENDIF
    ENDIF
    !   Remove outermost matching single or double quotes
    LENWORD = LEN_TRIM(TOKEN)
    IF (LENWORD>1) THEN
      CH1 = TOKEN(1:1)
      CH2 = TOKEN(LENWORD:LENWORD)
      IF (CH1==CHAR(39) .OR. CH1=='"') THEN
        IF(CH1==CH2) THEN
          TOKEN = TOKEN(2:LENWORD-1)
        ENDIF
      ENDIF
    ENDIF
    !
    ISTART = ISTART1
    RETURN
  END SUBROUTINE UTL_GETTOKEN
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETUNIT(IFIRST,MAXUNIT) RESULT (KUNIT)
    !   Find first unused file unit number between IFIRST and MAXUNIT
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER, INTENT(IN) :: IFIRST
    INTEGER, INTENT(IN) :: MAXUNIT
    INTEGER             :: KUNIT
    !
    !   Local variables
    INTEGER I, IOST
    LOGICAL LOP
    !
    !   Loop through range provided to find first unused unit number
    DO I=IFIRST,MAXUNIT
      INQUIRE(UNIT=I,IOSTAT=IOST,OPENED=LOP,ERR=5)
      IF (IOST==0) THEN
        IF (.NOT. LOP) THEN
          KUNIT = I
          RETURN
        ENDIF
      ENDIF
 5    CONTINUE
    ENDDO
    !
    !   If there are no unused unit numbers in range provided, return
    !   a value indicating an error
    KUNIT = -1
    !
    RETURN
  END FUNCTION UTL_GETUNIT
  !-----------------------------------------------------------------------------
  FUNCTION UTL_GETVAL_CDM(CDM,IR,IC) RESULT(V)
    !   Return value in CDM at row IR, column IC
    USE DATATYPES; USE GLOBAL_DATA
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    TYPE (CDMATRIX), INTENT(IN) :: CDM
    INTEGER,    INTENT(IN) :: IR
    INTEGER,    INTENT(IN) :: IC
    DOUBLE PRECISION :: V
    !
    !   Local variables
    INTEGER I, ICOL1
    INTEGER(KIND=8) :: IPOS, IPOSI
    !
    V = 0.0D0
    ICOL1 = CDM%ICOL(IC)
    IF (ICOL1>0) THEN
      IPOS = INT(CDM%NR,8)*(INT(IC,8)-1)+INT(IR,8)
      ONECOL: DO I=ICOL1,CDM%NNZ
        IPOSI = CDM%IPOS(I)
        IF (IPOSI==IPOS) THEN
          V = CDM%DVAL(I)
          RETURN
        ELSEIF (IPOSI>IPOS) THEN
          RETURN
        ENDIF
      ENDDO ONECOL
    ENDIF
    !
    RETURN
  END FUNCTION UTL_GETVAL_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_INVERT_DIAG_CDM(IFAIL,A,AS,DTLA)
    !     CALCULATE THE INVERSE AND THE SQUARE-ROOT OF THE INVERSE OF A
    !     DIAGONAL MATRIX STORED IN A CDMATRIX STRUCTURE
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(INOUT) :: IFAIL
    TYPE (CDMATRIX),  INTENT(INOUT) :: A     ! Input matrix on invocation;
                                             !    inverse on return
    TYPE (CDMATRIX),  INTENT(INOUT) :: AS    ! Square-root of inverse
    DOUBLE PRECISION, INTENT(OUT)   :: DTLA  ! Log-determinant of the matrix
    !   Local variabless
    INTEGER           :: I, IC, IR, NRC
    INTEGER           :: IP ! Position in A%DVAL of diagonal element
    CHARACTER(LEN=12) :: ASNAME
    DOUBLE PRECISION  :: VAL
    !
    ! Format statements
    100 FORMAT(/,1X,   &
        'Programmer ERROR: "A" matrix not square in UTL_INVERT_DIAG_CDM')
    120 FORMAT(/,1X,   &
        'Programmer ERROR: "A" matrix not diagonal in UTL_INVERT_DIAG_CDM')
    !
    IFAIL = 0
    NRC = A%NR
    !
    !   Check for errors
    IF (NRC .NE. A%NC) THEN
      WRITE(*,100)
      CALL UTL_STOP()
    ENDIF
    !
    !   Check for non-zero off-diagonal elements in matrix A
    IF (A%NNZ>NRC) THEN
      DO I=1,A%NNZ
        IR = UTL_GETIROW(A,I)
        IC = UTL_GETICOL(A,I)
        IF (IR .NE. IC) THEN
          VAL = A%DVAL(I)
          IF (VAL .NE. 0.0D0) THEN
            !  Off-diagonal element is nonzero
            WRITE(*,120)
            CALL UTL_STOP()
          ENDIF
        ENDIF
      ENDDO
    ENDIF
    !
    !   Initialize CDMATRIX structure AS
    ASNAME = TRIM(A%ARRAYNAME)//'SQRT'
    CALL UTL_CONSTRUCTCDMATRIX(NRC,NRC,NRC,AS,ASNAME)
    !
    DTLA = 0.D0
    !   Replace diagonal elements of A with their inverse
    DO I=1,NRC
      VAL = UTL_GETVAL(A,I,I)
      IF (VAL <= 0.D0) THEN
        IFAIL = 1
        GOTO 900
      ENDIF
      !   Sum for log determinant of the matirx
      DTLA = DTLA + LOG(VAL)
      !   Change term to inverse in A
      IP = UTL_GETPOS(A,I,I)
      A%DVAL(I) = 1.D0/VAL
      !   Assign sqrt of inverse to AS, and assign values to ICOL and IPOS
      AS%DVAL(IP) = SQRT(A%DVAL(I))
      AS%ICOL(IP) = IP
      AS%IPOS(IP) = INT(NRC,8)*(INT(IP,8)-1)+INT(IP,8)
    ENDDO
    AS%NNZ = NRC
    !
    900 CONTINUE
    RETURN
  END SUBROUTINE UTL_INVERT_DIAG_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETWORD(J1,LINE,NBLC,IFAIL,NUM1,NUM2)
    ! -- SUBROUTINE UTL_GETWORD GETS THE NEXT SPACE-DELIMITED WORD IN LINE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,           INTENT(IN)  :: J1
    CHARACTER (LEN=*), INTENT(IN)  :: LINE
    INTEGER,           INTENT(IN)  :: NBLC
    INTEGER,           INTENT(OUT) :: IFAIL
    INTEGER,           INTENT(OUT) :: NUM1
    INTEGER,           INTENT(OUT) :: NUM2
    !
    !   Local variables
    INTEGER :: I
    !
    IFAIL=0
    DO 20 I=J1+1,NBLC
      IF(LINE(I:I).NE.' ') GO TO 50
    20 CONTINUE
    IFAIL=1
    RETURN
    50 NUM1=I
    I=INDEX(LINE(NUM1:NBLC),' ')
    IF(I.EQ.0) THEN
      NUM2=NBLC
    ELSE
      NUM2=NUM1+I-2
    END IF
    !
    RETURN
  END SUBROUTINE UTL_GETWORD
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GETWORDLEFT(LINE,IFAIL,J1,J2,TPAR)
    ! -- SUBROUTINE UTL_GETWORDLEFT EXTRACTS A PARAMETER NAME FROM A STRING
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER (LEN=*), INTENT(IN) :: LINE
    INTEGER, INTENT(OUT)          :: IFAIL   ! reports error condition
    INTEGER, INTENT(IN)           :: J1      ! beginning of word
    INTEGER, INTENT(IN)           :: J2      ! end of word
    CHARACTER (LEN=*), INTENT(OUT):: TPAR    ! the extracted parameter
    !
    !   Local variables
    INTEGER             :: I, J, MAXLEN
    !
    IFAIL=0
    TPAR=' '
    MAXLEN=LEN(TPAR)
    IF(J2-J1.LE.1) THEN
      IFAIL=1
      RETURN
    END IF
    DO 10 I=J1+1,J2-1
      IF(LINE(I:I).EQ.' ') CYCLE
      GO TO 30
    10 CONTINUE
    IFAIL=2
    RETURN
    30 J=MIN(MAXLEN,J2-I)
    TPAR(1:J)=LINE(I:I+J-1)
    RETURN
  END SUBROUTINE UTL_GETWORDLEFT
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_GROUPLIST(DEFGROUP,GRPHEAD,IOUT,LSTHEAD,NGROUPS,NLISTS)
    !   Insert nodes contained in Group-information list GRPHEAD immediately
    !   after head of individual lists included in LSTHEAD, for lists where
    !   GROUPNAME in GRPHEAD list matches GROUPNAME in LSTHEAD, or insert
    !   default if GROUPNAME is not specified in LSTHEAD
    !
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=12), INTENT(IN) :: DEFGROUP
    TYPE (LLIST),      POINTER    :: GRPHEAD
    INTEGER,           INTENT(IN) :: IOUT
    TYPE (LLIST),      POINTER    :: LSTHEAD
    INTEGER,           INTENT(IN) :: NGROUPS
    INTEGER,           INTENT(IN) :: NLISTS
    !
    !   Local variables
    TYPE (LLIST), POINTER :: GPTR, LPTR
    TYPE (LNODE), POINTER :: GLNPTR, LNPTR, NEWLNPTR, PTR1, PTR2
    CHARACTER(LEN=12) :: GPNAME, LGROUP, NAM1, NAM2, UPGROUP
    INTEGER :: I, IERR, ISTAT, J, K
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: GPNAM
    !
    !   Format statements
    100 FORMAT(1X,'*** ERROR: Group name "',A,'" repeated')
    105 FORMAT(1X,'*** ERROR: No lists to process')
    110 FORMAT(/,1X,I5,' Error(s) -- STOP EXECUTION (UTL_GROUPLIST)')
    125 FORMAT(1X,'*** ERROR: "',A,'" group not defined')
    130 FORMAT(1X,'*** ERROR: Unable to allocate memory (UTL_GROUPLIST)')
    !
    NULLIFY(GPTR,LPTR,GLNPTR,LNPTR,NEWLNPTR,PTR1,PTR2)
    IF (ALLOCATED(GPNAM)) DEALLOCATE(GPNAM)
    ALLOCATE (GPNAM(NGROUPS))
    !
    !   Traverse through individual lists to identify GROUPNAME for each list
    LPTR => LSTHEAD
    LISTS: DO
      IF (.NOT. ASSOCIATED(LPTR)) EXIT LISTS   ! Pointer valid?
      LNPTR => LPTR%LHEAD
      !   Assign GROUP as default
      LPTR%GROUP = DEFGROUP
      !   Traverse this list
      THISLIST: DO
        IF (.NOT. ASSOCIATED(LNPTR)) EXIT THISLIST
        !   If GROUPNAME is found, assign value to GROUP for list
        IF (LNPTR%KEYWORD == 'GROUPNAME') THEN
          CALL UTL_CASE(LNPTR%VALUE(1:12),UPGROUP,1)
          LPTR%GROUP = UPGROUP
          EXIT THISLIST    ! Use first GROUPNAME found, ignore others
        ENDIF
        LNPTR => LNPTR%NEXTNODE                ! Get pointer to next node
      ENDDO THISLIST
      LPTR => LPTR%NEXTLIST                    ! Get pointer to next list
    ENDDO LISTS
    !
    !   Traverse through group lists and populate GROUP for each group list
    K = 0
    GPTR => GRPHEAD
    GLISTS: DO
      IF (.NOT. ASSOCIATED(GPTR)) EXIT GLISTS   ! Pointer valid?
      K = K+1
      LNPTR => GPTR%LHEAD
      !   Assign GROUP as default
      GPTR%GROUP = DEFGROUP
      !   Traverse this list
      THISGLIST: DO
        IF (.NOT. ASSOCIATED(LNPTR)) EXIT THISGLIST
        !   If GROUPNAME is found, assign value to GROUP for list
        IF (LNPTR%KEYWORD == 'GROUPNAME') THEN
          CALL UTL_CASE(LNPTR%VALUE(1:12),UPGROUP,1)
          GPTR%GROUP = UPGROUP
          GPNAM(K) = UPGROUP
          EXIT THISGLIST    ! Use first GROUPNAME found, ignore others
        ENDIF
        LNPTR => LNPTR%NEXTNODE                ! Get pointer to next node
      ENDDO THISGLIST
      GPTR => GPTR%NEXTLIST                    ! Get pointer to next list
    ENDDO GLISTS
    !
    !   Check that no group name is repeated
    IERR = 0
    IF (K > 1) THEN
      DO I=1,K-1
        NAM1 = GPNAM(I)
        DO J=I+1,K
          NAM2 = GPNAM(J)
          IF (NAM1 == NAM2) THEN
            WRITE(IOUT,100) TRIM(NAM1)
            IERR = IERR+1
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    !
    !   Insert appropriate group information into each individual list
    IF (NLISTS > 0) THEN
      LPTR => LSTHEAD
      LISTS2: DO
        IF (.NOT. ASSOCIATED(LPTR)) EXIT LISTS2     ! Pointer valid?
        LGROUP = LPTR%GROUP               ! Group name for this individual list
        LNPTR => LPTR%LHEAD
        GPTR => GRPHEAD
        GROUPS: DO
          IF (.NOT. ASSOCIATED(GPTR)) THEN
            IF (UTL_SAMENAME(LGROUP,DEFGROUP)) THEN
              CONTINUE
            ELSE
              WRITE(IOUT,125) TRIM(LGROUP)
              IERR = IERR+1
            ENDIF
            EXIT GROUPS   ! Pointer valid?
          ENDIF
          GPNAME = GPTR%GROUP             ! Group name for this group
          IF (UTL_SAMENAME(GPNAME,LGROUP)) THEN
            !   Insert group information
            PTR1 => LNPTR
            PTR2 => LNPTR%NEXTNODE
            GLNPTR => GPTR%LHEAD
            !
            !   Each iteration of the INSERT loop inserts one LNODE from group
            !   list into individual list
            INSERT: DO
              IF (.NOT. ASSOCIATED(GLNPTR)) EXIT INSERT
              ALLOCATE (NEWLNPTR,STAT=ISTAT)
              IF (ISTAT.NE.0) THEN
                WRITE(IOUT,130)
                CALL UTL_STOP(' ')
              ENDIF
              PTR1%NEXTNODE => NEWLNPTR
              NEWLNPTR%KEYWORD = GLNPTR%KEYWORD
              NEWLNPTR%VALUE = GLNPTR%VALUE
              NEWLNPTR%NCHAR = GLNPTR%NCHAR
              ALLOCATE(NEWLNPTR%STRING(NEWLNPTR%NCHAR))
              NEWLNPTR%STRING = GLNPTR%STRING
              NEWLNPTR%NEXTNODE => PTR2
              GLNPTR => GLNPTR%NEXTNODE
              PTR1 => NEWLNPTR
            ENDDO INSERT
            EXIT GROUPS
          ENDIF
          GPTR => GPTR%NEXTLIST           ! Get pointer to next group
        ENDDO GROUPS
        LPTR => LPTR%NEXTLIST             ! Get pointer to next individual list
      ENDDO LISTS2
    ELSE
      IERR = IERR+1
      WRITE(IOUT,105)
    ENDIF
    !
    IF (IERR > 0) THEN
      WRITE(IOUT,110) IERR
      CALL UTL_STOP(' ')
    ENDIF
    !
    DEALLOCATE(GPNAM)
    RETURN
  END SUBROUTINE UTL_GROUPLIST
  !-----------------------------------------------------------------------------
  FUNCTION UTL_MATMUL_ODM_CDM_D(NRA,NCA,OA,CB) RESULT (OC)
    !   Perform matrix multiplication involving a matrix stored as a CDMATRIX
    !   structure:  [Ordinary]x[Compressed]
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                              INTENT(IN) :: NRA
    INTEGER,                              INTENT(IN) :: NCA
    DOUBLE PRECISION, DIMENSION(NRA,NCA), INTENT(IN) :: OA
    TYPE (CDMATRIX),                      INTENT(IN) :: CB
    DOUBLE PRECISION, DIMENSION(NRA,CB%NC)           :: OC
    !
    !   Local variables
    INTEGER :: IA, IB, JB, N, NNZ
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: NCA does not match row dimension of multiplier',  &
        ' matrix (UTL_MATMUL_ODM_CDM_D)')
    !
    IF (NCA.NE.CB%NR) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    OC = 0.0D0
    NNZ = CB%NNZ
    !
    DO N=1,NNZ
      IB = UTL_GETIROW(CB,N)
      JB = UTL_GETICOL(CB,N)
      DO IA=1,NRA
        OC(IA,JB) = OC(IA,JB) + OA(IA,IB)*CB%DVAL(N)
      ENDDO
    ENDDO
    !
    RETURN
  END FUNCTION UTL_MATMUL_ODM_CDM_D
  !-----------------------------------------------------------------------------
  FUNCTION UTL_MATMUL_CDM_ODM_D(NRB,NCB,CA,OB) RESULT (OC)
    !   Perform matrix multiplication involving a matrix stored as a CDMATRIX
    !   structure:  [Compressed]x[Ordinary]
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                              INTENT(IN) :: NRB
    INTEGER,                              INTENT(IN) :: NCB
    TYPE (CDMATRIX),                      INTENT(IN) :: CA
    DOUBLE PRECISION, DIMENSION(NRB,NCB), INTENT(IN) :: OB
    DOUBLE PRECISION, DIMENSION(CA%NR,NCB) :: OC
    !
    !   Local variables
    INTEGER :: IA, JA, JB, N, NNZ
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: Column dimension of compressed matrix does not',   &
        ' match NRB (UTL_MATMUL_CDM_ODM_D)')
    !
    IF (CA%NC.NE.NRB) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    OC = 0.0D0
    NNZ = CA%NNZ
    !
    DO N=1,NNZ
      IA = UTL_GETIROW(CA,N)
      JA = UTL_GETICOL(CA,N)
      DO JB=1,NCB
        OC(IA,JB) = OC(IA,JB) + CA%DVAL(N)*OB(JA,JB)
      ENDDO
    ENDDO
    !
    RETURN
  END FUNCTION UTL_MATMUL_CDM_ODM_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_MATMUL_SUB_ODM_CDM_D(NRA,NCA,OA,CB,OC)
    !   Perform matrix multiplication involving a matrix stored as a CDMATRIX
    !   structure:  [Ordinary]x[Compressed]
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                                INTENT(IN)  :: NRA
    INTEGER,                                INTENT(IN)  :: NCA
    DOUBLE PRECISION, DIMENSION(NRA,NCA),   INTENT(IN)  :: OA
    TYPE (CDMATRIX),                        INTENT(IN)  :: CB
    DOUBLE PRECISION, DIMENSION(NRA,CB%NC), INTENT(OUT) :: OC
    !
    !   Local variables
    INTEGER :: IA, IB, JB, N, NNZ
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: NCA does not match row dimension of multiplier',  &
        ' matrix (UTL_MATMUL_SUB_ODM_CDM_D)')
    !
    IF (NCA.NE.CB%NR) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    OC = 0.0D0
    NNZ = CB%NNZ
    !
    DO N=1,NNZ
      IB = UTL_GETIROW(CB,N)
      JB = UTL_GETICOL(CB,N)
      DO IA=1,NRA
        OC(IA,JB) = OC(IA,JB) + OA(IA,IB)*CB%DVAL(N)
      ENDDO
    ENDDO
    !
    RETURN
  END SUBROUTINE UTL_MATMUL_SUB_ODM_CDM_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_MATMUL_SUB_CDM_ODM_D(NRB,NCB,CA,OB,OC)
    !   Perform matrix multiplication involving a matrix stored as a CDMATRIX
    !   structure:  [Compressed]x[Ordinary]
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                                INTENT(IN)  :: NRB
    INTEGER,                                INTENT(IN)  :: NCB
    TYPE (CDMATRIX),                        INTENT(IN)  :: CA
    DOUBLE PRECISION, DIMENSION(NRB,NCB),   INTENT(IN)  :: OB
    DOUBLE PRECISION, DIMENSION(CA%NR,NCB), INTENT(OUT) :: OC
    !
    !   Local variables
    INTEGER :: IA, JA, JB, N, NNZ
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: Column dimension of compressed matrix does not',   &
        ' match NRB (UTL_MATMUL_SUB_CDM_ODM_D)')
    !
    IF (CA%NC.NE.NRB) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    OC = 0.0D0
    NNZ = CA%NNZ
    !
    DO N=1,NNZ
      IA = UTL_GETIROW(CA,N)
      JA = UTL_GETICOL(CA,N)
      DO JB=1,NCB
        OC(IA,JB) = OC(IA,JB) + CA%DVAL(N)*OB(JA,JB)
      ENDDO
    ENDDO
    !
    RETURN
  END SUBROUTINE UTL_MATMUL_SUB_CDM_ODM_D
  !-----------------------------------------------------------------------------
  FUNCTION UTL_MATMULVEC_CDM_ODV_D(NRB,CA,OB) RESULT (OC)
    !   Perform matrix multiplication: Matrix stored as a CDMATRIX structure
    !   times a vector assumed to represent a (NRB,1) matrix:
    !   [Compressed matrix]x[Ordinary vector]
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                          INTENT(IN) :: NRB
    TYPE (CDMATRIX),                  INTENT(IN) :: CA
    DOUBLE PRECISION, DIMENSION(NRB), INTENT(IN) :: OB
    DOUBLE PRECISION, DIMENSION(CA%NR) :: OC
    !
    !   Local variables
    INTEGER :: IA, JA, N, NNZ
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: Column dimension of compressed matrix does not',   &
        ' match NRB (UTL_MATMULVEC_CDM_ODV_D)')
    !
    IF (CA%NC.NE.NRB) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    OC = 0.0D0
    NNZ = CA%NNZ
    !
    DO N=1,NNZ
      IA = UTL_GETIROW(CA,N)
      JA = UTL_GETICOL(CA,N)
      OC(IA) = OC(IA) + CA%DVAL(N)*OB(JA)
    ENDDO
    !
    RETURN
  END FUNCTION UTL_MATMULVEC_CDM_ODV_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_MATMULVEC_SUB_CDM_ODV_D(NRB,CA,OB,OC)
    !   Perform matrix multiplication: Matrix stored as a CDMATRIX structure
    !   times a vector assumed to represent a (NRB,1) matrix:
    !   [Compressed matrix]x[Ordinary vector]
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN)  :: NRB
    TYPE (CDMATRIX),                    INTENT(IN)  :: CA
    DOUBLE PRECISION, DIMENSION(NRB),   INTENT(IN)  :: OB
    DOUBLE PRECISION, DIMENSION(CA%NR), INTENT(OUT) :: OC
    !
    !   Local variables
    INTEGER :: IA, JA, N, NNZ
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: Column dimension of compressed matrix does not',   &
        ' match NRB (UTL_MATMULVEC_SUB_CDM_ODV_D)')
    !
    IF (CA%NC.NE.NRB) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    OC = 0.0D0
    NNZ = CA%NNZ
    !
    DO N=1,NNZ
      IA = UTL_GETIROW(CA,N)
      JA = UTL_GETICOL(CA,N)
      OC(IA) = OC(IA) + CA%DVAL(N)*OB(JA)
    ENDDO
    !
    RETURN
  END SUBROUTINE UTL_MATMULVEC_SUB_CDM_ODV_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_MERGELIST(BLOCKLABEL1,BLOCKLABEL2,HEAD1,HEAD2,IOUT)
    !   Merge two nested linked lists by adding lists included in HEAD2 to
    !   the tails of corresponding lists included in HEAD1.  A list in HEAD2
    !   corresponds to a list in HEAD1 if the contents of the VALUE components
    !   of the first node (i.e. the KEYITEM) in each list match each other.
    !   The comparison is case-insensitive.   If no match is found, the case
    !   is flagged as an error.
    !
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*), INTENT(IN) :: BLOCKLABEL1
    CHARACTER(LEN=*), INTENT(IN) :: BLOCKLABEL2
    TYPE (LLIST), POINTER        :: HEAD1
    TYPE (LLIST), POINTER        :: HEAD2
    INTEGER,          INTENT(IN) :: IOUT
    !
    !   Local variables
    TYPE (LLIST), POINTER :: LPTR1, LPTR2
    TYPE (LNODE), POINTER :: LNPTR1, LNPTR2
    INTEGER :: IERR
    CHARACTER(LEN=MAX_STRING_LEN) :: UPVAL1, UPVAL2, VAL1, VAL2
    !
    !   Format statements
    110 FORMAT(/,1X,I5,' Error(s) -- STOP EXECUTION (UTL_MERGELIST)')
    200 FORMAT(1X,'Error: Match for item "',A,'" in block "',A,'"',/,1X,   &
        ' not found in block "',A,'"')
    !
    NULLIFY(LPTR1,LPTR2,LNPTR1,LNPTR2)
    IERR = 0
    !   Loop through lists in HEAD2
    LPTR2 => HEAD2
    LISTS2: DO
      IF (.NOT. ASSOCIATED(LPTR2)) EXIT LISTS2     ! Pointer valid?
      LNPTR2 => LPTR2%LHEAD
      CALL UTL_ARR2STRING(LNPTR2%NCHAR,LNPTR2%STRING,VAL2)
      CALL UTL_CASE(VAL2,UPVAL2,1)
      !   Loop through lists in HEAD1 to finding matching VALUE component
      LPTR1 => HEAD1
      LISTS1: DO
        IF (.NOT. ASSOCIATED(LPTR1)) THEN   ! Pointer valid?
          WRITE(IOUT,200) TRIM(VAL2),TRIM(BLOCKLABEL2),TRIM(BLOCKLABEL1)
          IERR = IERR+1
          EXIT LISTS1
        ENDIF
        LNPTR1 => LPTR1%LHEAD
        CALL UTL_ARR2STRING(LNPTR1%NCHAR,LNPTR1%STRING,VAL1)
        CALL UTL_CASE(VAL1,UPVAL1,1)
        IF (UPVAL1 .EQ. UPVAL2) THEN
          !   Found match, so add nodes from list in HEAD2 to tail of list
          !   in HEAD1.  To do this, make NEXTNODE component of node pointed
          !   to by LTAIL point to second node in list in HEAD2 list, and make
          !   LTAIL point to last node of list that is coming from HEAD2 list.
          !
          LPTR1%LTAIL%NEXTNODE => LNPTR2
          LPTR1%LTAIL => LPTR2%LTAIL
          EXIT LISTS1
        ENDIF
        LPTR1 => LPTR1%NEXTLIST           ! Get pointer to next list in HEAD1
      ENDDO LISTS1
      LPTR2 => LPTR2%NEXTLIST             ! Get pointer to next list in HEAD2
    ENDDO LISTS2
    !
    IF (IERR > 0) THEN
      WRITE(IOUT,110) IERR
      CALL UTL_STOP(' ')
    ENDIF
    !
  END SUBROUTINE UTL_MERGELIST
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_NEWLIST(GROUP,HEAD,IOUT,TAIL)
    !   Add list of type LLIST to upper-level linked list
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*), INTENT(IN) :: GROUP
    TYPE (LLIST), POINTER        :: HEAD
    INTEGER,          INTENT(IN) :: IOUT
    TYPE (LLIST), POINTER        :: TAIL
    !
    !   Local variables
    INTEGER :: ISTAT
    !
    !   Format statements
    40  FORMAT(/,1X,'Error allocating memory, STATUS = ',I5,' -- (UTL_NEWLIST)')
    !
    IF (.NOT. ASSOCIATED(HEAD)) THEN  ! no values in linked list
      ALLOCATE(HEAD,STAT=ISTAT)       !   allocate new entry
      IF (ISTAT > 0) THEN
        WRITE(IOUT,40) ISTAT
        CALL UTL_STOP(' ')
      ENDIF
      TAIL => HEAD                    !   tail points to new entry
      NULLIFY(TAIL%NEXTLIST)          !   nullify NEXTLIST in new entry
      NULLIFY(TAIL%LHEAD)             !   nullify pointer to head of list
      NULLIFY(TAIL%LTAIL)             !   nullify pointer to tail of list
    ELSE                              ! list already contains >= 1 entry
      ALLOCATE(TAIL%NEXTLIST,STAT=ISTAT)  !   allocate new entry
      IF (ISTAT > 0) THEN
        WRITE(IOUT,40) ISTAT
        CALL UTL_STOP(' ')
      ENDIF
      TAIL => TAIL%NEXTLIST           !   tail points to new entry
      NULLIFY(TAIL%NEXTLIST)          !   nullify NEXTPAR in new entry
      NULLIFY(TAIL%LHEAD)             !   nullify pointer to head of list
      NULLIFY(TAIL%LTAIL)             !   nullify pointer to tail of list
    ENDIF
    TAIL%GROUP = GROUP
    !
    RETURN
  END SUBROUTINE UTL_NEWLIST
  !-----------------------------------------------------------------------------
  FUNCTION UTL_NEXTUNIT() RESULT(NEXTUNIT)
    ! -- Function UTL_NEXTUNIT determines the lowest unit number available for
    ! -- opening.
    !
    ! -- Revision history:-
    !       June-November, 1995: version 1.
    IMPLICIT NONE
    !
    !   Result variable
    INTEGER :: NEXTUNIT
    !
    !   Local variables
    LOGICAL :: LOPEN
    !
    DO NEXTUNIT=10,1000
      INQUIRE(UNIT=NEXTUNIT,OPENED=LOPEN)
      IF(.NOT.LOPEN) RETURN
    END DO
    WRITE(*,10)
    10 FORMAT(' *** No more unit numbers to open files ***')
    CALL UTL_STOP(' ')
    !
  END FUNCTION UTL_NEXTUNIT
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   Subroutines comprising the generic subroutine UTL_NUM2CHAR
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------

  ! -- Subroutine num2char writes the character equivalent of a number.

  ! -- Arguments are as follows:-
  !       value:   the number to be expressed in character form
  !       string:  the number expressed in character form
  !       nchar:   the maximum number of characters in which to express number

  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_NUM2CHAR_d2a(value,string,nchar)
    IMPLICIT NONE
    double precision, intent(in)    :: value
    character (len=*), intent(out)  :: string
    integer, intent(in), optional   :: nchar
    integer                         :: llen, ifail
    double precision                :: value_check
    character (len=32)              :: word

    string=' '
    llen=min(29,len(string))
    if(present(nchar)) llen=min(llen,nchar)
    call UTL_WRTSIG(ifail,value,word,llen,1,value_check,0)
    if(ifail.lt.0) then
      call UTL_SUBERROR('UTL_D2A')
    else if(ifail.gt.0) then
      string(1:llen)=repeat('#',llen)
    else
      string=adjustl(word)
    end if
    return

  end SUBROUTINE UTL_NUM2CHAR_d2a
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_NUM2CHAR_i2a(value,string,nchar)
    IMPLICIT NONE
    integer, intent(in)             :: value
    character (len=*), intent(out)  :: string
    integer, intent(in), optional   :: nchar
    character (len=12)              :: afmt
    integer                         :: llen

    string=' '
    afmt='(i    )'
    llen=min(30,len(string))
    if(present(nchar)) llen=min(llen,nchar)
    WRITE(afmt(3:6),'(i4)') llen
    WRITE(string(1:llen),afmt,err=100) value
    string=adjustl(string)
    if(string(1:1).eq.'*') go to 100
    return

    100 string(1:llen)=repeat('#',llen)
    return

  end SUBROUTINE UTL_NUM2CHAR_i2a
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_NUM2CHAR_r2a(value,string,nchar)
    IMPLICIT NONE
    real,intent(in)                 :: value
    character (len=*), intent(out)  :: string
    integer, intent(in), optional   :: nchar
    integer                         :: llen,ifail
    double precision                :: dvalue,dvalue_check
    character (len=32)              :: word

    string=' '
    llen=min(29,len(string))
    if(present(nchar)) llen=min(llen,nchar)
    dvalue=value
    call UTL_WRTSIG(ifail,dvalue,word,llen,0,dvalue_check,0)
    if(ifail.lt.0) then
      call UTL_SUBERROR('UTL_R2A')
    else if(ifail.gt.0) then
      string(1:llen)=repeat('#',llen)
    else
      string=adjustl(word)
    end if
    return

  end SUBROUTINE UTL_NUM2CHAR_r2a
  !-----------------------------------------------------------------------------
  !
  !*****************************************************************************
  !   End of subroutines comprising the generic subroutine UTL_NUM2CHAR
  !*****************************************************************************
  !
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_PARSQMATRIX(IOUT,IPRC,NPE,NPS,BUFF,IPTR,PARNAM)
    !   VERSION 20050302 ERB
    !   Print one NPE*NPE correlation or variance-covariance matrix
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                               INTENT(IN) :: IOUT
    INTEGER,                               INTENT(IN) :: IPRC
    INTEGER,                               INTENT(IN) :: NPE
    INTEGER,                               INTENT(IN) :: NPS
    DOUBLE PRECISION,  DIMENSION(NPE,NPE), INTENT(IN) :: BUFF
    INTEGER,           DIMENSION(NPE),     INTENT(IN) :: IPTR
    CHARACTER(LEN=12), DIMENSION(NPS),     INTENT(IN) :: PARNAM
    !
    !   Local variables
    INTEGER :: I
    !CHARACTER(LEN=12) :: PN(NPE)
    CHARACTER(LEN=12), ALLOCATABLE, DIMENSION(:) :: PN
    !
    ALLOCATE(PN(NPE))
    DO I=1,NPE
      PN(I) = PARNAM(IPTR(I))
    ENDDO
    !
    CALL UTL_WRITEMATRIX(NPE,NPE,BUFF,PN,PN,IPRC,IOUT)
    !
    DEALLOCATE(PN)
    RETURN
  END SUBROUTINE UTL_PARSQMATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_PREPINVERSE(A,AI,AINAME)
    !   Find number of nonzero entries required for the inverse of
    !   CDMATRIX A.  Assume all diagonal entries in inverse matrix will be
    !   nonzero.  Populate AI where A has nonzero values.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (CDMATRIX),            INTENT(IN)    :: A
    TYPE (CDMATRIX),            INTENT(INOUT) :: AI
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: AINAME
    !
    !   Local variables
    INTEGER :: I, IBOTROW, ICOL1, IROW1, IROW2, ITOPROW,   &
               J, JL, KC, KE, M, MP, NC, NNZ, NNZI
    INTEGER(KIND=8) :: IP, IPOS1, IPOS2, JP, NR
    LOGICAL :: INSQ
    !
    !   Format statements
    50 FORMAT(   &
        ' Programmer error: UTL_PREPINVERSE expecting square matrix as',/,   &
        ' input, but NR not equal to NC for input CDMATRIX structure')
    !
    NR = A%NR
    NC = A%NC
    IF (NR.NE.NC) THEN
      WRITE(*,50)
      CALL UTL_STOP(' ')
    ENDIF
    NNZ = A%NNZ
    NNZI = 0
    !   Initially assume column (and column in matrix inverse) does not
    !   include off-diagonal terms
    INSQ = .FALSE.
    ITOPROW = 1  ! Top row of box
    IBOTROW = 1  ! Bottom row of box
    BOX: DO J=1,NC  ! Execute once for each diagonal element; J is
                    ! row and column number
      !
      !   Find row index of first and last nonzero entry in this column
      IF (J==1) THEN
        ICOL1 = 1
        IPOS1 = 1
        IROW1 = 1
      ELSE
        ICOL1 = A%ICOL(J)            ! Position in A%IPOS of first nonzero entry in column J
        IPOS1 = A%IPOS(ICOL1)        ! Position in matrix of first nonzero entry in column J
        IROW1 = MOD((IPOS1-1),NR)+1  ! Row index of first nonzero entry in column J
      ENDIF
      !
      IF (ICOL1==NNZ) THEN
        !   IPOS1 contains the last nonzero entry in matrix
        IROW2 = IROW1
      ELSE
        !   Initially assume last nonzero entry in matrix is in column J
        IPOS2 = A%IPOS(NNZ)
        !   Find the next column with a nonzero entry and work from there
        IF (J<NC) THEN
          DOJL: DO JL=J+1,NC
            IF (A%ICOL(JL)>0) THEN
              IF (A%ICOL(JL)>1) THEN
                IPOS2 = A%IPOS(A%ICOL(JL)-1)
              ELSE
                IPOS2 = 1
              ENDIF
              EXIT DOJL
            ENDIF
          ENDDO DOJL
        ENDIF
        IROW2 = MOD((IPOS2-1),NR)+1  ! Row index of last nonzero entry in column J
      ENDIF
      !
      IF (IROW2.GT.J) THEN
        !   Last nonzero entry in column J is below diagonal
        IF (.NOT. INSQ) THEN
          !   Set top row of box as current row/column
          ITOPROW = J
        ENDIF
        INSQ = .TRUE.
        !   Keep track of bottommost row in box
        IF (IROW2>IBOTROW) IBOTROW = IROW2
      ELSE
        !   Last nonzero entry in column J is on diagonal
        IF (J.EQ.IBOTROW) THEN
          ! Row J is bottom of box
          INSQ = .FALSE.
        ENDIF
      ENDIF
      !
      !   If INSQ is false, (J,J) is the lower right corner of a box: Increment
      !   NNZI and reset ITOPROW and IBOTROW
      IF (.NOT. INSQ) THEN
        NNZI = NNZI+(IBOTROW-ITOPROW+1)**2
        ITOPROW = J+1  ! Top row of next box
        IBOTROW = J+1  ! Bottom row of next box
        CYCLE BOX
      ENDIF
      !
    ENDDO BOX
    !
    !   Construct AI to hold inverse of A
    CALL UTL_CONSTRUCTCDMATRIX(NNZI,NC,NC,AI,AINAME)
    AI%NNZ = NNZI
    !
    !   Populate IPOS and ICOL as needed
    !
    !   Initially assume column (and column in matrix inverse) does not
    !   include off-diagonal terms
    INSQ = .FALSE.
    ITOPROW = 1  ! Top row of box
    IBOTROW = 1  ! Bottom row of box
    KE = 1       ! Element counter
    KC = 1       ! Column counter
    BOX2: DO J=1,NC  ! Execute once for each diagonal element; J is
                     ! row and column number
      !
      !   Find row index of first and last nonzero entry in this column
      IF (J==1) THEN
        ICOL1 = 1
        IPOS1 = 1
        IROW1 = 1
      ELSE
        ICOL1 = A%ICOL(J)            ! Position in A%IPOS of first nonzero entry in column J
        IPOS1 = A%IPOS(ICOL1)        ! Position in matrix of first nonzero entry in column J
        IROW1 = MOD((IPOS1-1),NR)+1  ! Row index of first nonzero entry in column J
      ENDIF
      !
      IF (ICOL1==NNZ) THEN
        !   IPOS1 contains the last nonzero entry in matrix
        IROW2 = IROW1
      ELSE
        !   Initially assume last nonzero entry in matrix is in column J
        IPOS2 = A%IPOS(NNZ)
        !   Find the next column with a nonzero entry and work from there
        IF (J<NC) THEN
          DOJL2: DO JL=J+1,NC
            IF (A%ICOL(JL)>0) THEN
              IF (A%ICOL(JL)>1) THEN
                IPOS2 = A%IPOS(A%ICOL(JL)-1)
              ELSE
                IPOS2 = 1
              ENDIF
              EXIT DOJL2
            ENDIF
          ENDDO DOJL2
        ENDIF
        IROW2 = MOD((IPOS2-1),NR)+1  ! Row index of last nonzero entry in column J
      ENDIF
      !
      IF (IROW2.GT.J) THEN
        !   Last nonzero entry in column J is below diagonal
        IF (.NOT. INSQ) THEN
          !   Set top row of box as current row/column
          ITOPROW = J
        ENDIF
        INSQ = .TRUE.
        !   Keep track of bottommost row in box
        IF (IROW2>IBOTROW) IBOTROW = IROW2
      ELSE
        !   Last nonzero entry in column J is on diagonal
        IF (J.EQ.IBOTROW) THEN
          ! Row J is bottom of box
          INSQ = .FALSE.
        ENDIF
      ENDIF
      !
      !   If INSQ is false, (J,J) is the lower right corner of a box: Populate
      !   IPOS and ICOL and reset ITOPROW and IBOTROW
      IF (.NOT. INSQ) THEN
        DO JP=ITOPROW,IBOTROW  ! Iterate through columns in box
          AI%ICOL(KC) = KE
          KC = KC+1
          DO IP=ITOPROW,IBOTROW   ! Iterate through rows in box
            AI%IPOS(KE) = (JP-1)*NR+IP
            KE = KE+1
          ENDDO
        ENDDO
        ITOPROW = J+1  ! Top row of next box
        IBOTROW = J+1  ! Bottom row of next box
      ENDIF
      !
    ENDDO BOX2
    !
    !   Populate AI where A has nonzero entries
    POPNZ: DO M=1,NNZ
      I = UTL_GETIROW(A,M)
      J = UTL_GETICOL(A,M)
      MP = UTL_GETPOS(AI,I,J)
      IF (MP>0) AI%DVAL(MP) = A%DVAL(M)
    ENDDO POPNZ
    !
    RETURN
  END SUBROUTINE UTL_PREPINVERSE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_PREPLINE(LINE)
    !   Remove spaces and tab characters adjacent to an "=" sign in a string,
    !   unless the "=" sign is enclosed in single or double quotes.
    IMPLICIT NONE
    !
    !   Argument-list variable
    CHARACTER(LEN=*), INTENT(INOUT) :: LINE
    !
    !   Local variables
    INTEGER :: I, KDQ, KSQ, LB, LBEGIN, LEQUAL, LTRIM, MKDQ, MKSQ
    CHARACTER(LEN=1) :: CH1, TAB
    !
    KDQ = 0
    KSQ = 0
    !
    !   Eliminate blanks and tabs on either side of any "=" signs
    LEQUAL = INDEX(LINE,'=')
    LTRIM = LEN_TRIM(LINE)
    IF (LEQUAL>0 .AND. LTRIM>1) THEN
      TAB = CHAR(9)
      LB = 1
      500 CONTINUE
      LBEGIN = LB
      LTRIM = LEN_TRIM(LINE)
      LEQUAL = INDEX(LINE(LBEGIN:LTRIM),'=')
      LEQUAL = LEQUAL+LBEGIN-1
      MKDQ = 0
      MKSQ = 0
      !   Eliminate any blanks and tabs that precede "="
      IF (LEQUAL>LBEGIN) THEN
        KDQ = UTL_COUNTSUBS(LINE(1:LEQUAL),'"')
        MKDQ = MOD(KDQ,2)
        KSQ = UTL_COUNTSUBS(LINE(1:LEQUAL),'''')
        MKSQ = MOD(KSQ,2)
        IF ((MKDQ+MKSQ)==0) THEN
          PRE: DO I=LEQUAL-1,LBEGIN,-1
            CH1 = LINE(I:I)
            IF (CH1==' ' .OR. CH1==TAB) THEN
              LINE(I:LTRIM-1) = LINE(I+1:LTRIM)
              LINE(LTRIM:LTRIM) = ' '
              LTRIM = LTRIM-1
              LEQUAL = LEQUAL-1
           ELSE
              LB = MIN(I+2,LTRIM)
              EXIT PRE
            ENDIF
          ENDDO PRE
        ENDIF
      ENDIF
      !   Eliminate any blanks and tabs that follow "="
      IF (LEQUAL<LTRIM .AND. LEQUAL>0) THEN
        IF ((MKDQ+MKSQ)==0) THEN
          I=LEQUAL+1
          POST: DO WHILE (LTRIM.GE.I)
            CH1 = LINE(I:I)
            IF (CH1==' ' .OR. CH1==TAB) THEN
              LINE(I:LTRIM-1) = LINE(I+1:LTRIM)
              LINE(LTRIM:LTRIM) = ' '
              LTRIM = LTRIM-1
            ELSE
              EXIT POST
            ENDIF
          ENDDO POST
        ENDIF
      ENDIF
      IF (LB>LBEGIN) GOTO 500
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_PREPLINE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READ_CMPMATRIX_CDM(ANAME,INUNIT,IOUT,CDM)
    !   Read a matrix from a file coded as a compressed matrix into a
    !   CDMATRIX structure.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=12), INTENT(IN)    :: ANAME
    INTEGER,           INTENT(IN)    :: INUNIT
    INTEGER,           INTENT(IN)    :: IOUT
    TYPE (CDMATRIX),   INTENT(INOUT) :: CDM
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME, LINE
    INTEGER :: I, ICLOSE, ICOL, ICOLD, IPRN, ISTART,   &
               ISTOP, LLOC, LOCAT, N, NC, NNZ, NR4
    INTEGER(KIND=8) :: IC, IPOS, NR
    DOUBLE PRECISION :: CNSTNT, DVAL, R
    INTEGER(KIND=8), ALLOCATABLE, DIMENSION(:) :: IPOSA
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DVALA
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: VAL2D
    CHARACTER(LEN=MAX_STRING_LEN) :: FMTIN
    CHARACTER(LEN=2500) :: CNTRL
    !
    !   Format statements
    10 FORMAT(/,1X,   &
        '*** ERROR: End-of-file encountered while attempting to read:',/,   &
        1X,A,/,1X,'from unit ',I6)
    20 FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
!    30 FORMAT(I10,F10.0,A20,I10)
    40 FORMAT(1X,/1X,A,' =',1P,G14.6)
    60 FORMAT(/,1X,'Reading ',A,/,1X,'on unit ',I4,' with format: ',A)
    80 FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
    100 FORMAT(1X,'Error reading compressed matrix "',A,   &
               '" (UTL_READ_CMPMATRIX_CDM)')
    120 FORMAT(1X,'Control record must start with one of: "INTERNAL",',   &
        ' or "OPEN/CLOSE".')
    !
    CNSTNT = 1.0D0
    !
    READ(INUNIT,'(A)')LINE
    LLOC = 1
    CALL UTL_RWORD(IOUT,2,.TRUE.,LLOC,LINE,ISTART,ISTOP,NNZ,R,INUNIT)
    CALL UTL_RWORD(IOUT,2,.TRUE.,LLOC,LINE,ISTART,ISTOP,NR,R,INUNIT)
    CALL UTL_RWORD(IOUT,2,.TRUE.,LLOC,LINE,ISTART,ISTOP,NC,R,INUNIT)
    CALL UTL_RWORD(IOUT,1,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
    NR4 = NR
    !
    !   Allocate arrays in CDMATRIX structure
    CALL UTL_CONSTRUCTCDMATRIX(NNZ,NR4,NC,CDM,ANAME)
    !
    !   Read data and populate array components of CDM
    IF (LINE(ISTART:ISTOP)=='CONTROLRECORD') THEN
      !
      !C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
      CNTRL = UTL_GETLINE(INUNIT,0)
      IF (CNTRL==' ') THEN
        WRITE(IOUT,10) TRIM(ANAME),INUNIT
        CALL UTL_STOP(' ')
      ENDIF
      !
      ICLOSE=0
      ICOL=1
      !   Read keyword from array control record, indicating how to define
      !   contents of CDM
      CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
      IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
        LOCAT=INUNIT
      ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
        CALL UTL_RWORD(IOUT,0,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
        FNAME=CNTRL(ISTART:ISTOP)
        LOCAT=UTL_GETUNIT(7,1000)
        IF (IVERB>3) WRITE(IOUT,20) LOCAT,TRIM(FNAME)
        ICLOSE=1
      ELSE
        !
        !C2A-----ERROR: DID NOT FIND A RECOGNIZED WORD
        WRITE(IOUT,100)TRIM(ANAME)
        WRITE(IOUT,120)
        CALL UTL_STOP()
      END IF
      !
      !C3------READ REMAINING FIELDS:  CNSTNT, FMTIN, and IPRN
      CALL UTL_RWORD(IOUT,3,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,CNSTNT,INUNIT)
      IF(LOCAT.NE.0) THEN
        CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
        FMTIN=CNTRL(ISTART:ISTOP)
        IF(ICLOSE.NE.0) THEN
          OPEN(UNIT=LOCAT,FILE=FNAME,ACTION='READ')
        END IF
        CALL UTL_RWORD(IOUT,2,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,IPRN,R,INUNIT)
      END IF
      !
      !C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT == 0) THEN
        !C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        ICOLD = 0
        DO I=1,NNZ
          CDM%IPOS(I) = I
          CDM%DVAL(I) = CNSTNT
          IC = (CDM%IPOS(I)-1)/NR+1
          CDM%NNZ = CDM%NNZ+1
          IF (IC.NE.ICOLD) CDM%ICOL(IC) = I
          ICOLD = IC
        ENDDO
        IF (IVERB>3) WRITE(IOUT,40) ANAME,CNSTNT
        RETURN
      ENDIF
      !
      !C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
      IF (IVERB>3) WRITE(IOUT,60) TRIM(ANAME),LOCAT,TRIM(FMTIN)
      !
      ALLOCATE (IPOSA(NNZ),DVALA(NNZ))
      IF(FMTIN.EQ.'(FREE)') THEN
        !   Free-format list
        READ(LOCAT,*) (IPOSA(I),DVALA(I),I=1,NNZ)
      ELSE
        !   Formatted list
        READ(LOCAT,FMTIN) (IPOSA(I),DVALA(I),I=1,NNZ)
      END IF
      !
      !   Populate CDM
      ICOLD = 0
      DO I=1,NNZ
        CDM%IPOS(I) = IPOSA(I)
        CDM%DVAL(I) = DVALA(I)*CNSTNT
        IC = (CDM%IPOS(I)-1)/NR+1
        CDM%NNZ = CDM%NNZ+1
        IF (IC.NE.ICOLD) CDM%ICOL(IC) = I
        ICOLD = IC
      ENDDO
      DEALLOCATE(IPOSA,DVALA)
      !
      !C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
      IF (IPRN .GE. 0) THEN
        ALLOCATE(VAL2D(NR,NC))
        VAL2D = CDM
        CALL UTL_WRITE2D(IOUT,IPRN,NR4,NC,VAL2D,ANAME)
        DEALLOCATE(VAL2D)
      ENDIF
      !
      RETURN
      !
      !C8------CONTROL RECORD ERROR.
      WRITE(IOUT,80) TRIM(ANAME)
      WRITE(IOUT,'(1X,A)') TRIM(CNTRL)
      CALL UTL_STOP(' ')
    ELSE
      !   No control record
      ICOLD = 0
      DO I=1,NNZ
        READ(INUNIT,'(A)')LINE
        LLOC = 1
        CALL UTL_RWORD(IOUT,2,.TRUE.,LLOC,LINE,ISTART,ISTOP,IPOS,R,INUNIT)
        CALL UTL_RWORD(IOUT,3,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,DVAL,INUNIT)
        CDM%IPOS(I) = IPOS
        CDM%DVAL(I) = DVAL
        IC = (CDM%IPOS(I)-1)/NR+1
        CDM%NNZ = CDM%NNZ+1
        IF (IC.NE.ICOLD) CDM%ICOL(IC) = I
        ICOLD = IC
      ENDDO
    ENDIF
    !
    RETURN
    !
  END SUBROUTINE UTL_READ_CMPMATRIX_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READ_STDMATRIX_CDM(ANAME,INUNIT,IOUT,CDM)
    !   Read a matrix from a file coded as a compressed matrix into a
    !   CDMATRIX structure.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=12), INTENT(IN)    :: ANAME
    INTEGER,           INTENT(IN)    :: INUNIT
    INTEGER,           INTENT(IN)    :: IOUT
    TYPE (CDMATRIX),   INTENT(INOUT) :: CDM
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: DBUF
    DOUBLE PRECISION :: R
    INTEGER :: I, IPRN, ISTART, ISTAT, ISTOP, J, LLOC, N, NC, NR
    !
    !   Format statements
    110 FORMAT(1X,'Error: Memory allocation failure (UTL_READ_STDMATRIX_CDM)')
    !
    READ(INUNIT,'(A)')LINE
    LLOC = 1
    CALL UTL_RWORD(IOUT,2,.TRUE.,LLOC,LINE,ISTART,ISTOP,NR,R,INUNIT)
    CALL UTL_RWORD(IOUT,2,.TRUE.,LLOC,LINE,ISTART,ISTOP,NC,R,INUNIT)
    CALL UTL_RWORD(IOUT,1,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
    ALLOCATE(DBUF(NR,NC),STAT=ISTAT)
    IF (ISTAT.NE.0) THEN
      IF (IOUT>0) THEN
        WRITE(IOUT,110)
      ELSE
        WRITE(*,110)
      ENDIF
      GOTO 900
    ENDIF
    IF (LINE(ISTART:ISTOP)=='CONTROLRECORD') THEN
      CALL UTL_READ2D(ANAME,NR,NC,INUNIT,IOUT,DBUF,IPRN)
    ELSE
      !   Read each row of matrix in free format
      DO I=1,NR
        READ(INUNIT,*) (DBUF(I,J),J=1,NC)
      ENDDO
    ENDIF
    !
    !   Store non-zero values of DBUF in a CDMATRIX structure
    CALL UTL_ARR2CDMATRIX(NR,NC,DBUF,CDM,1,ANAME)
    !
    DEALLOCATE(DBUF)
    RETURN
    !
    900 CONTINUE
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_READ_STDMATRIX_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READ2D_D(ANAME,NR,NC,INUNIT,IOUT,BUF,IPRN)
    !     ******************************************************************
    !     ROUTINE TO INPUT A 2-D DOUBLE PRECISION DATA MATRIX
    !       BUF IS ARRAY TO INPUT
    !       ANAME IS A CHARACTER DESCRIPTION OF BUF
    !       NR IS NO. OF ROWS
    !       NC IS NO. OF COLS
    !       INUNIT IS INPUT UNIT
    !       IOUT IS OUTPUT UNIT
    !     ******************************************************************
    !
    !        SPECIFICATIONS:
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),                    INTENT(IN)  :: ANAME
    INTEGER,                             INTENT(IN)  :: NR
    INTEGER,                             INTENT(IN)  :: NC
    INTEGER,                             INTENT(IN)  :: INUNIT
    INTEGER,                             INTENT(IN)  :: IOUT
    DOUBLE PRECISION, DIMENSION (NR,NC), INTENT(OUT) :: BUF
    INTEGER,                             INTENT(OUT) :: IPRN
    !
    !   Local variables
    INTEGER I, ICLOSE, ICOL, IFREE, ISTART, ISTOP, J, LOCAT,  N
    DOUBLE PRECISION :: CNSTNT, R, DZERO
    CHARACTER(LEN=MAX_STRING_LEN) :: FMTIN
    CHARACTER(LEN=2500) :: CNTRL
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    !   Format statements
    10 FORMAT(/,1X,   &
        '*** ERROR: End-of-file encountered while attempting to read:',/,   &
        1X,A,/,1X,'from unit ',I6)
    20 FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
    30 FORMAT(I10,F10.0,A20,I10)
    40 FORMAT(1X,/1X,A,' =',1P,G14.6)
    60 FORMAT(/,1X,'Reading ',A,/,1X,'on unit ',I4,' with format: ',A)
    80 FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
    !
    !C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
    CNTRL = UTL_GETLINE(INUNIT,0)
    IF (CNTRL==' ') THEN
      WRITE(IOUT,10) TRIM(ANAME),INUNIT
      CALL UTL_STOP(' ')
    ENDIF
    !
    !C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
    !C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
    ICLOSE=0
    IFREE=1
    ICOL=1
    CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
    IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
      LOCAT=0
    ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
      LOCAT=INUNIT
    ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
      CALL UTL_RWORD(IOUT,0,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
      FNAME=CNTRL(ISTART:ISTOP)
      LOCAT=UTL_GETUNIT(7,1000)
      IF (IVERB>3) WRITE(IOUT,20) LOCAT,TRIM(FNAME)
      ICLOSE=1
    ELSE
      !
      !C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
      !C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
      IFREE=0
      READ(CNTRL,30,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    END IF
    !
    !C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
    IF(IFREE.NE.0) THEN
      CALL UTL_RWORD(IOUT,3,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,CNSTNT,INUNIT)
      IF(LOCAT.NE.0) THEN
        CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
        FMTIN=CNTRL(ISTART:ISTOP)
        IF(ICLOSE.NE.0) THEN
          OPEN(UNIT=LOCAT,FILE=FNAME,ACTION='READ')
        END IF
        CALL UTL_RWORD(IOUT,2,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,IPRN,R,INUNIT)
      END IF
    END IF
    !
    !C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
    IF(LOCAT == 0) THEN
      !C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      BUF=CNSTNT
      IF (IVERB>3) WRITE(IOUT,40) ANAME,CNSTNT
      RETURN
    ENDIF
    !
    !C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
    IF (IVERB>3) WRITE(IOUT,60) TRIM(ANAME),LOCAT,TRIM(FMTIN)
    !
    DO I=1,NR
      IF(FMTIN.EQ.'(FREE)') THEN
        READ(LOCAT,*) (BUF(I,J),J=1,NC)
      ELSE
        READ(LOCAT,FMTIN) (BUF(I,J),J=1,NC)
      END IF
    ENDDO
    !
    !C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
    IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
    DZERO=0.0D0
    IF (CNSTNT.NE.DZERO) THEN
      DO I=1,NR
        DO J=1,NC
          BUF(I,J)=BUF(I,J)*CNSTNT
        ENDDO
      ENDDO
    ENDIF
    !
    !C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
    IF(IPRN.GE.0) CALL UTL_WRITE2D(IOUT,IPRN,NR,NC,BUF,ANAME)
    !
    RETURN
    !
    !C8------CONTROL RECORD ERROR.
    500 CONTINUE
    WRITE(IOUT,80) TRIM(ANAME)
    WRITE(IOUT,'(1X,A)') TRIM(CNTRL)
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_READ2D_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READ2D_I(ANAME,NR,NC,INUNIT,IOUT,BUF,IPRN)
    !     ******************************************************************
    !     ROUTINE TO INPUT A 2-D DOUBLE PRECISION DATA MATRIX
    !       BUF IS ARRAY TO INPUT
    !       ANAME IS A CHARACTER DESCRIPTION OF BUF
    !       NR IS NO. OF ROWS
    !       NC IS NO. OF COLS
    !       INUNIT IS INPUT UNIT
    !       IOUT IS OUTPUT UNIT
    !     ******************************************************************
    !
    !        SPECIFICATIONS:
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),           INTENT(IN)  :: ANAME
    INTEGER,                    INTENT(IN)  :: NR
    INTEGER,                    INTENT(IN)  :: NC
    INTEGER,                    INTENT(IN)  :: INUNIT
    INTEGER,                    INTENT(IN)  :: IOUT
    INTEGER, DIMENSION (NR,NC), INTENT(OUT) :: BUF
    INTEGER,                    INTENT(OUT) :: IPRN
    !
    !   Local variables
    INTEGER :: I, ICLOSE, ICOL, IFREE, ISTART, ISTOP, J, LOCAT,  N
    INTEGER :: CNSTNT, ZERO
    DOUBLE PRECISION  :: R
    CHARACTER(LEN=MAX_STRING_LEN) :: FMTIN
    CHARACTER(LEN=2500) :: CNTRL
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    !   Format statements
    10 FORMAT(/,1X,   &
        '*** ERROR: End-of-file encountered while attempting to read:',/,   &
        1X,A,/,1X,'from unit ',I6)
    20 FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
    30 FORMAT(I10,I10,A20,I10)
    40 FORMAT(1X,/1X,A,' = ',I10)
    60 FORMAT(/,1X,'Reading ',A,/,1X,'on unit ',I4,' with format: ',A)
    80 FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
    !
    !C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
    CNTRL = UTL_GETLINE(INUNIT,0)
    IF (CNTRL==' ') THEN
      WRITE(IOUT,10) TRIM(ANAME),INUNIT
      CALL UTL_STOP(' ')
    ENDIF
    !
    !C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
    !C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
    ICLOSE=0
    IFREE=1
    ICOL=1
    CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
    IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
      LOCAT=0
    ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
      LOCAT=INUNIT
    ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
      CALL UTL_RWORD(IOUT,0,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
      FNAME=CNTRL(ISTART:ISTOP)
      LOCAT=UTL_GETUNIT(7,1000)
      IF (IVERB>3) WRITE(IOUT,20) LOCAT,TRIM(FNAME)
      ICLOSE=1
    ELSE
      !
      !C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
      !C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
      IFREE=0
      READ(CNTRL,30,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    END IF
    !
    !C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
    IF(IFREE.NE.0) THEN
      CALL UTL_RWORD(IOUT,2,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,CNSTNT,R,INUNIT)
      IF(LOCAT.NE.0) THEN
        CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
        FMTIN=CNTRL(ISTART:ISTOP)
        IF(ICLOSE.NE.0) THEN
          OPEN(UNIT=LOCAT,FILE=FNAME,ACTION='READ')
        END IF
        CALL UTL_RWORD(IOUT,2,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,IPRN,R,INUNIT)
      END IF
    END IF
    !
    !C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
    IF(LOCAT == 0) THEN
      !C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      BUF=CNSTNT
      IF (IVERB>3) WRITE(IOUT,40) ANAME,CNSTNT
      RETURN
    ENDIF
    !
    !C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
    IF (IVERB>3) WRITE(IOUT,60) TRIM(ANAME),LOCAT,TRIM(FMTIN)
    !
    DO I=1,NR
      IF(FMTIN.EQ.'(FREE)') THEN
        READ(LOCAT,*) (BUF(I,J),J=1,NC)
      ELSE
        READ(LOCAT,FMTIN) (BUF(I,J),J=1,NC)
      END IF
    ENDDO
    !
    !C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
    IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
    ZERO=0
    IF (CNSTNT .NE. ZERO) THEN
      DO I=1,NR
        DO J=1,NC
          BUF(I,J)=BUF(I,J)*CNSTNT
        ENDDO
      ENDDO
    ENDIF
    !
    !C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
    IF (IPRN .GE. 0) CALL UTL_WRITE2D(IOUT,IPRN,NR,NC,BUF,ANAME)
    !
    RETURN
    !
    !C8------CONTROL RECORD ERROR.
    500 CONTINUE
    WRITE(IOUT,80) TRIM(ANAME)
    WRITE(IOUT,'(1X,A)') TRIM(CNTRL)
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_READ2D_I
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READ2D_R(ANAME,NR,NC,INUNIT,IOUT,BUF,IPRN)
    !     ******************************************************************
    !     ROUTINE TO INPUT A 2-D REAL DATA MATRIX
    !       BUF IS ARRAY TO INPUT
    !       ANAME IS A CHARACTER DESCRIPTION OF A\BUF
    !       NR IS NO. OF ROWS
    !       NC IS NO. OF COLS
    !       INUNIT IS INPUT UNIT
    !       IOUT IS OUTPUT UNIT
    !     ******************************************************************
    !
    !        SPECIFICATIONS:
    USE GLOBAL_DATA, ONLY: IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),        INTENT(IN)  :: ANAME
    INTEGER,                 INTENT(IN)  :: NR
    INTEGER,                 INTENT(IN)  :: NC
    INTEGER,                 INTENT(IN)  :: INUNIT
    INTEGER,                 INTENT(IN)  :: IOUT
    REAL, DIMENSION (NR,NC), INTENT(OUT) :: BUF
    INTEGER,                 INTENT(OUT) :: IPRN
    !
    !   Local variables
    INTEGER I, ICLOSE, ICOL, IFREE, ISTART, ISTOP, J, LOCAT,  N
    DOUBLE PRECISION :: CNSTNT, R, DZERO
    CHARACTER(LEN=MAX_STRING_LEN) :: FMTIN
    CHARACTER(LEN=2500) :: CNTRL
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    !   Format statements
    10 FORMAT(/,1X,   &
        '*** ERROR: End-of-file encountered while attempting to read:',/,   &
        1X,A,/,1X,'from unit ',I6)
    20 FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
    30 FORMAT(I10,F10.0,A20,I10)
    40 FORMAT(1X,/1X,A,' =',1P,G14.6)
    60 FORMAT(/,1X,'Reading ',A,/,1X,'on unit ',I4,' with format: ',A)
    80 FORMAT(1X,/1X,'ERROR READING ARRAY CONTROL RECORD FOR ',A,':')
    !
    !C1------READ ARRAY CONTROL RECORD AS CHARACTER DATA.
    CNTRL = UTL_GETLINE(INUNIT,0)
    IF (CNTRL==' ') THEN
      WRITE(IOUT,10) TRIM(ANAME),INUNIT
      CALL UTL_STOP(' ')
    ENDIF
    !
    !C2------LOOK FOR ALPHABETIC WORD THAT INDICATES THAT THE RECORD IS FREE
    !C2------FORMAT.  SET A FLAG SPECIFYING IF FREE FORMAT OR FIXED FORMAT.
    ICLOSE=0
    IFREE=1
    ICOL=1
    CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
    IF (CNTRL(ISTART:ISTOP).EQ.'CONSTANT') THEN
      LOCAT=0
    ELSE IF(CNTRL(ISTART:ISTOP).EQ.'INTERNAL') THEN
      LOCAT=INUNIT
    ELSE IF(CNTRL(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
      CALL UTL_RWORD(IOUT,0,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
      FNAME=CNTRL(ISTART:ISTOP)
      LOCAT=UTL_GETUNIT(7,1000)
      IF (IVERB>3) WRITE(IOUT,20) LOCAT,TRIM(FNAME)
      ICLOSE=1
    ELSE
      !
      !C2A-----DID NOT FIND A RECOGNIZED WORD, SO NOT USING FREE FORMAT.
      !C2A-----READ THE CONTROL RECORD THE ORIGINAL WAY.
      IFREE=0
      READ(CNTRL,30,ERR=500) LOCAT,CNSTNT,FMTIN,IPRN
    END IF
    !
    !C3------FOR FREE FORMAT CONTROL RECORD, READ REMAINING FIELDS.
    IF(IFREE.NE.0) THEN
      CALL UTL_RWORD(IOUT,3,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,CNSTNT,INUNIT)
      IF(LOCAT.NE.0) THEN
        CALL UTL_RWORD(IOUT,1,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,N,R,INUNIT)
        FMTIN=CNTRL(ISTART:ISTOP)
        IF(ICLOSE.NE.0) THEN
          OPEN(UNIT=LOCAT,FILE=FNAME,ACTION='READ')
        END IF
        CALL UTL_RWORD(IOUT,2,.TRUE.,ICOL,CNTRL,ISTART,ISTOP,IPRN,R,INUNIT)
      END IF
    END IF
    !
    !C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
    IF(LOCAT == 0) THEN
      !C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
      BUF=CNSTNT
      IF (IVERB>3) WRITE(IOUT,40) ANAME,CNSTNT
      RETURN
    ENDIF
    !
    !C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
    IF (IVERB>3) WRITE(IOUT,60) TRIM(ANAME),LOCAT,TRIM(FMTIN)
    !
    DO I=1,NR
      IF(FMTIN.EQ.'(FREE)') THEN
        READ(LOCAT,*) (BUF(I,J),J=1,NC)
      ELSE
        READ(LOCAT,FMTIN) (BUF(I,J),J=1,NC)
      END IF
    ENDDO
    !
    !C5------IF CNSTNT NOT ZERO THEN MULTIPLY ARRAY VALUES BY CNSTNT.
    IF(ICLOSE.NE.0) CLOSE(UNIT=LOCAT)
    DZERO=0.0D0
    IF (CNSTNT.NE.DZERO) THEN
      DO I=1,NR
        DO J=1,NC
          BUF(I,J)=BUF(I,J)*CNSTNT
        ENDDO
      ENDDO
    ENDIF
    !
    !C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
    IF(IPRN.GE.0) CALL UTL_WRITE2D(IOUT,IPRN,NR,NC,BUF,ANAME)
    !
    RETURN
    !
    !C8------CONTROL RECORD ERROR.
    500 CONTINUE
    WRITE(IOUT,80) TRIM(ANAME)
    WRITE(IOUT,'(1X,A)') TRIM(CNTRL)
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_READ2D_R
  !-----------------------------------------------------------------------------
  RECURSIVE SUBROUTINE UTL_READBLOCK(NDEFCOLS,BLABEL,COLNAMES,INUNIT,IOUT,  &
                                     KITEM,REQUIRED,HEAD,TAIL,KLISTS)
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: AMESSAGE, IVERB, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(IN)    :: NDEFCOLS
    CHARACTER(LEN=*), INTENT(IN)    :: BLABEL      ! Block label
    CHARACTER(LEN=40), DIMENSION(NDEFCOLS), INTENT(IN) :: COLNAMES
    INTEGER,          INTENT(IN)    :: INUNIT
    INTEGER,          INTENT(IN)    :: IOUT
    CHARACTER(LEN=*), INTENT(IN)    :: KITEM       ! Key item
    LOGICAL,          INTENT(IN)    :: REQUIRED
    TYPE (LLIST), POINTER           :: HEAD ! Pointer to head of list for one KEYITEM
    TYPE (LLIST), POINTER           :: TAIL ! Pointer to tail of list for one KEYITEM
    INTEGER,          INTENT(INOUT) :: KLISTS
    !
    !   Local variables
    INTEGER :: ISTART, ISTOP, LENKEY, LENLABEL, LLOC, N
    DOUBLE PRECISION :: R
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE    ! Line read from input file
    CHARACTER(LEN=40) :: WORD1, WORD2, UPWORD1, UPWORD2
    CHARACTER(LEN=40) :: BLOCKLABEL, KEYITEM
    LOGICAL :: READINGBLOCK
    !
    !   Format statements
     20 FORMAT(/,1X,'Programmer error: ',A,' argument too long (UTL_READBLOCK)')
     30 FORMAT(1X,'Looking for input block "',A,'" on unit ',I5)
     50 FORMAT(1X,'Current line is: ',/,1X,A)
    140 FORMAT(/,1X,'*** ERROR (UTL_READBLOCK):',   &
        ' End-of-file encountered while looking for ',/,1X,A,' block')
    150 FORMAT(/,1X,'*** ERROR (UTL_READBLOCK):',   &
        ' End-of-file encountered when reading ',A,' block')
    180 FORMAT(1X,'NOTE: Input block named "',A,'" will be ignored.')
    300 FORMAT(/,1X,I7,1X,'Item(s) of type ',A,' were read from ',A,' block ')
    400 FORMAT(1X,'No items of type ',A,' were read from ',A,' block ')
    450 FORMAT(/,1X,'Error (UTL_READBLOCK) -- expected END ',A,' but found:')
    460 FORMAT(1X,'Error (UTL_READBLOCK) -- Expected input block',   &
               ' starting with BEGIN but found:',/,1X,A)
    !
    BLOCKLABEL = ' '
    LENLABEL = LEN_TRIM(BLABEL)
    IF (LENLABEL > LEN(BLOCKLABEL)) THEN
      WRITE(IOUT,20)'BLOCKLABEL'
      CALL UTL_STOP(' ')
    ENDIF
    IF (IVERB>3) WRITE(IOUT,30) TRIM(BLABEL),INUNIT
    CALL UTL_CASE(BLABEL,BLOCKLABEL,1)
    !
    LENKEY = LEN_TRIM(KITEM)
    IF (LENKEY > LEN(KEYITEM)) THEN
      WRITE(IOUT,20)'KEYITEM'
      CALL UTL_STOP(' ')
    ENDIF
    CALL UTL_CASE(KITEM,KEYITEM,1)
    !
    READINGBLOCK = .FALSE.
    500 CONTINUE
    !   Find non-comment line containing at least one non-blank character
    LINE = UTL_GETLINE(INUNIT,IOUT)
    !
    !   Ensure that line contains BEGIN [BLOCKLABEL]
    IF (LINE == ' ') THEN    ! When EOF, LINE = ' '
      IF (REQUIRED) THEN
        WRITE(IOUT,140)BLOCKLABEL(1:LENLABEL)
        CALL UTL_STOP(' ')
      ELSE
        GOTO 900
      ENDIF
    ELSE
      WORD1 = ' '
      WORD2 = ' '
      LLOC = 1
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD1 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD1,UPWORD1,1)
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD2 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD2,UPWORD2,1)
      IF (UPWORD1=='BEGIN' .AND. UPWORD2==BLOCKLABEL) THEN
        READINGBLOCK = .TRUE.
        CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
        WORD1 = LINE(ISTART:ISTOP)
        CALL UTL_CASE(WORD1,UPWORD1,1)
        IF (UPWORD1=='TABLE') THEN
          CALL UTL_READBLOCKTABLE(NDEFCOLS,BLOCKLABEL,COLNAMES,INUNIT,IOUT,  &
              KEYITEM,HEAD,TAIL,KLISTS)
          !   Read END line
          LINE = UTL_GETLINE(INUNIT,IOUT)
        ELSEIF (UPWORD1=='FILES') THEN
          CALL UTL_READBLOCKFILES(NDEFCOLS,BLOCKLABEL,COLNAMES,INUNIT,IOUT,  &
              KEYITEM,HEAD,TAIL,KLISTS,LINE)
        ELSEIF (UPWORD1=='KEYWORDS' .OR. UPWORD1==' ') THEN
          CALL UTL_READBLOCKKEYS(BLOCKLABEL,HEAD,INUNIT,IOUT,KEYITEM,  &
              KLISTS,LINE,TAIL)
        ELSE
          AMESSAGE = 'ERROR: Invalid BLOCKFORMAT "'//TRIM(WORD1)//   &
              '" found in '//TRIM(UPWORD2)//' input block.  '//   &
              'BLOCKFORMAT must be one of: "KEYWORDS", "TABLE", or "FILES".'
          CALL UTL_STOP()
        ENDIF
      ELSEIF (UPWORD1=='BEGIN' .AND. UPWORD2.NE.BLOCKLABEL) THEN
        IF (REQUIRED) THEN
          IF (IVERB.GE.2) THEN
            WRITE(IOUT,180) TRIM(UPWORD2)
          ENDIF
          GOTO 500
        ELSE
          BACKSPACE(INUNIT)
          GOTO 900
        ENDIF
      ELSEIF (UPWORD1=='BEGIN') THEN
        IF (.NOT. REQUIRED) THEN
          BACKSPACE(INUNIT)
          GOTO 900
        ENDIF
      ELSE
        IF (REQUIRED) THEN  ! Modification of 11/21/05 -- ERB
          GOTO 500
        ENDIF
        WRITE(IOUT,460)TRIM(LINE)
        CALL UTL_STOP()
      ENDIF
    ENDIF
    !
    IF (READINGBLOCK .AND. IVERB>3) THEN
      IF (KLISTS > 0) THEN
        WRITE(IOUT,300)KLISTS,KEYITEM(1:LENKEY),BLOCKLABEL(1:LENLABEL)
      ELSE
        WRITE(IOUT,400)KEYITEM(1:LENKEY),BLOCKLABEL(1:LENLABEL)
      ENDIF
    ENDIF
    !
    !   Ensure that line contains END [BLOCKLABEL]
    IF (LINE == ' ') THEN    ! When EOF, LINE = ' '
      WRITE(IOUT,150)BLOCKLABEL(1:LENLABEL)
      CALL UTL_STOP(' ')
    ELSE
      WORD1 = ' '
      WORD2 = ' '
      LLOC = 1
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD1 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD1,UPWORD1,1)
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD2 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD2,UPWORD2,1)
      IF (UPWORD1=='END' .AND. UPWORD2==BLOCKLABEL) THEN
        CONTINUE
      ELSE
        IF (READINGBLOCK) THEN
          WRITE(IOUT,450) BLOCKLABEL(1:LENLABEL)
          WRITE(IOUT,50) TRIM(LINE)
          CALL UTL_STOP(' ')
        ELSE
          GOTO 500
        ENDIF
      ENDIF
    ENDIF
    !
    900 CONTINUE
    RETURN
  END SUBROUTINE UTL_READBLOCK
  !-----------------------------------------------------------------------------
  RECURSIVE SUBROUTINE UTL_READBLOCKFILES(NDEFCOLS,BLOCKLABEL,COLNAMES,   &
      INUNIT,IOUT,KITEM,HEAD,TAIL,KLISTS,LINE)
    !   Open one or more files listed in an input block and read block data
    !   from each
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                  INTENT(IN)    :: NDEFCOLS
    CHARACTER(LEN=40),                        INTENT(IN)    :: BLOCKLABEL
    CHARACTER(LEN=40), DIMENSION(NDEFCOLS), INTENT(IN)    :: COLNAMES
    INTEGER,                                  INTENT(IN)    :: INUNIT
    INTEGER,                                  INTENT(IN)    :: IOUT
    CHARACTER(LEN=*),                         INTENT(IN)    :: KITEM
    TYPE (LLIST),                             POINTER       :: HEAD
    TYPE (LLIST),                             POINTER       :: TAIL
    INTEGER,                                  INTENT(INOUT) :: KLISTS
    CHARACTER(LEN=*),                         INTENT(OUT)   :: LINE
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: WORD1, WORD2, UPWORD1, UPWORD2
    INTEGER :: LLOC, IFIL, ISTART, ISTAT, ISTOP, N
    DOUBLE PRECISION :: R
    LOGICAL :: LEX, LOP
    !
    !   Format statements
    160 FORMAT(/,1X,'*** Error reading "',A,'" block.  BLOCKLABEL ',  &
        'on END line does',/,' not match BLOCKLABEL on BEGIN line.  ',   &
        'Last line read was:',/,1X,A,/,1X,   &
        '-- STOP EXECUTION (UTL_READBLOCKFILES)')
    200 FORMAT(/,1X,'*** ERROR: File "',A,'" does not exist',/,1X,   &
        '-- STOP EXECUTION (UTL_READBLOCKFILES)')
    210 FORMAT(/,1X,'*** Error opening file "',A,'" ',/,1X,   &
        'File is already open -- STOP EXECUTION (UTL_READBLOCKFILES)')
    220 FORMAT(/,1X,'*** Error opening file "',A,'" ',/,1X,   &
        '-- STOP EXECUTION (UTL_READBLOCKFILES)')
    !
    !   Process each file in list
    LINE = ' '
    FILES: DO
      LINE = UTL_GETLINE(INUNIT,IOUT)       ! Get next line
      LLOC = 1
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD1 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD1,UPWORD1,1)
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD2 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD2,UPWORD2,1)
      IF (UPWORD1 == 'END') THEN
        IF (UPWORD2 == BLOCKLABEL) THEN
          EXIT FILES
        ELSE
          IF (BLOCKLABEL==' ') THEN
            CYCLE FILES
          ELSE
            WRITE(IOUT,160)TRIM(BLOCKLABEL),TRIM(LINE)
            CALL UTL_STOP(' ')
          ENDIF
        ENDIF
      ENDIF
      !
      !   Open a file
      INQUIRE(FILE=WORD1,EXIST=LEX,OPENED=LOP)
      IF (.NOT. LEX) THEN
        WRITE(IOUT,200) TRIM(WORD1)
        CALL UTL_STOP(' ')
      ENDIF
      IF (LOP) THEN
        WRITE(IOUT,210) TRIM(WORD1)
        CALL UTL_STOP(' ')
      ENDIF
      IFIL = UTL_GETUNIT(7,1000)
      OPEN(IFIL,FILE=WORD1,STATUS='OLD',IOSTAT=ISTAT)
      IF (ISTAT .NE. 0) THEN
        WRITE(IOUT,220) TRIM(WORD1)
        CALL UTL_STOP(' ')
      ENDIF
      !
      !   Call UTL_READBLOCK to read the file
      CALL UTL_READBLOCK(NDEFCOLS,BLOCKLABEL,COLNAMES,IFIL,IOUT,KITEM,  &
          .TRUE.,HEAD,TAIL,KLISTS)
    ENDDO FILES
    !
    RETURN
  END SUBROUTINE UTL_READBLOCKFILES
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READBLOCKKEYS(BLOCKLABEL,HEAD,INUNIT,IOUT,KEYITEM,KLISTS,   &
      LINE,TAIL)
    !   Read an input block and store information in nested linked lists.
    USE GLOBAL_DATA, ONLY: IVERB
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*), INTENT(IN)    :: BLOCKLABEL ! Block label
    TYPE (LLIST), POINTER           :: HEAD       ! Pointer to head of list for one KEYITEM
    INTEGER,          INTENT(IN)    :: INUNIT
    INTEGER,          INTENT(IN)    :: IOUT
    CHARACTER(LEN=*), INTENT(IN)    :: KEYITEM    ! Key item
    INTEGER,          INTENT(INOUT) :: KLISTS
    CHARACTER(LEN=*), INTENT(INOUT) :: LINE       ! Line read from input file
    TYPE (LLIST), POINTER           :: TAIL       ! Pointer to tail of list for one KEYITEM
    !
    !   Local variables
    INTEGER :: IERR, ISTART, ISTOP, KL1, LENKEY, LENLABEL, LLOC, N
    DOUBLE PRECISION :: R
    CHARACTER(LEN=40) :: WORD1, WORD2, UPWORD1, UPWORD2
    CHARACTER(LEN=41) :: KEYEQUAL
    LOGICAL :: BLOCKERR = .FALSE., ENDKEYS = .FALSE.
    !
    !   Format statements
    200 FORMAT(1X,'Error reading ',A,' block')
    250 FORMAT(/,1X,65('*'),/,   &
        1X,'*',2X,'WARNING: Expected to find keyitem "',A,'"',T66,'*',/,   &
        1X,'*',4X,'in input block labeled "',A,'"',T66,'*',/,   &
        1X,'*',4X,'for which blockformat = KEYWORDS (the default),',T66,'*',/, &
        1X,'*',4X,'but keyitem "',A,'" was not found.',T66,'*',/,   &
        1X,'*',2X,'Is blockformat specified correctly?',T66,'*',/,   &
        1x,65('*'),/)
    500 FORMAT(1X,I3,' Error(s) reading ',A,   &
        ' block -- STOP EXECUTION (UTL_READBLOCKKEYS)')
    !
    IERR = 0
    LENLABEL = LEN_TRIM(BLOCKLABEL)
    LENKEY = LEN_TRIM(KEYITEM)
    KEYEQUAL = KEYITEM(1:LENKEY) // '='
    LENKEY= LENKEY+1
    KL1 = KLISTS
    !
    INPUT: DO
      !   Ensure that line passed in contains BEGIN [BLOCKLABEL]
      IF (LINE == ' ') EXIT INPUT    ! When EOF, LINE = ' '
      WORD1 = ' '
      WORD2 = ' '
      LLOC = 1
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD1 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD1,UPWORD1,1)
      CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
      WORD2 = LINE(ISTART:ISTOP)
      CALL UTL_CASE(WORD2,UPWORD2,1)
      IF (UPWORD1=='BEGIN' .AND. UPWORD2==BLOCKLABEL) THEN
        !   UTL_READLIST expects LINE to contain an input-file line containing
        !   a KEYITEM=value entry as the first string, so get that line
        CALL UTL_GETKEYLINE(BLOCKLABEL,ENDKEYS,INUNIT,IOUT,KEYEQUAL,LINE)
        IF (ENDKEYS) EXIT INPUT
        READL: DO  ! This loop is executed once for each keyitem
          !   Add list associated with this keyitem to upper-level linked
          !   list -- Assign GROUP as ' '
          CALL UTL_NEWLIST(' ',HEAD,IOUT,TAIL)
          CALL UTL_READLIST(BLOCKERR,BLOCKLABEL,ENDKEYS,INUNIT,IOUT,KEYEQUAL, &
              LINE,TAIL)
          !   On return,  LINE holds keyline for next KEYITEM
          IF (BLOCKERR) THEN
            WRITE(IOUT,200) BLOCKLABEL(1:LENLABEL)
            CALL UTL_STOP(' ')
          ENDIF
          KLISTS = KLISTS+1
          IF (ENDKEYS) EXIT READL
        ENDDO READL
        EXIT INPUT
      ELSE
        EXIT INPUT
      ENDIF
    ENDDO INPUT
    !
    IF (KLISTS==KL1) THEN
      IF (IVERB>0) WRITE(IOUT,250)TRIM(KEYITEM),TRIM(BLOCKLABEL),TRIM(KEYITEM)
    ENDIF
    IF (IERR > 0) THEN
      WRITE(IOUT,500) IERR,BLOCKLABEL(1:LENLABEL)
      CALL UTL_STOP(' ')
    ENDIF
    RETURN
  END SUBROUTINE UTL_READBLOCKKEYS
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READBLOCKTABLE(NDEFCOLS,BLOCKLABEL,COLNAMES,INUNIT,IOUT,   &
                                KITEM,HEAD,TAIL,KLISTS)
    !   Read data from a block in TABLE format into a linked list
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(IN)    :: NDEFCOLS
    CHARACTER(LEN=*), INTENT(IN)    :: BLOCKLABEL      ! Block label
    CHARACTER(LEN=40), DIMENSION(NDEFCOLS), INTENT(IN) :: COLNAMES
    INTEGER,          INTENT(IN)    :: INUNIT
    INTEGER,          INTENT(IN)    :: IOUT
    CHARACTER(LEN=*), INTENT(IN)    :: KITEM           ! Key item
    TYPE (LLIST), POINTER :: HEAD ! Pointer to head of list for one KEYITEM
    TYPE (LLIST), POINTER :: TAIL ! Pointer to tail of main list
    INTEGER,          INTENT(INOUT) :: KLISTS
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    INTEGER :: GPOS, I, IERR, INDAT, ISTART, ISTAT, ISTOP, K, KEYPOS,   &
               KROW, LLOC, LENLABEL, N, NCOL, NFILES, NROW, NSKIP
    DOUBLE PRECISION :: R
    CHARACTER(LEN=12) :: GPNAM
    CHARACTER(LEN=40) :: WORD1, WORD2, UPWORD1, UPWORD2
    LOGICAL :: COLLABELS, EOF
    CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE :: LABELS
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(:), ALLOCATABLE :: VALUES
    CHARACTER(LEN=1) :: CHAR1
    !
    !   Format statements
    300 FORMAT(/,1X,'*** ERROR: Expected ',I3,' column names but only ',I3,  &
        ' were listed in ',/,1X,A,   &
        ' block -- STOP EXECUTION (UTL_READBLOCKTABLE)')
    400 FORMAT(/,1X,   &
        '*** ERROR reading block in TABLE format:',/,   &
        ' When COLUMNLABELS is not specified, NCOL',  &
        ' must equal the default',   &
        ' number of',/,' columns.  However, NCOL was specified as ',I6,   &
        ' and the default number of'/,   &
        ' columns for the ',A,' block is ',I3,/,   &
        ' -- STOP EXECUTION (UTL_READBLOCKTABLE)',//,   &
        1X,'NOTE: The default columns for the ',A,' block, in order, are:')
    410 FORMAT(1X,A,5(2X,A))
    500 FORMAT(/,1X,  &
        '*** ERROR: COLUMNLABELS must be defined for ',A,' block type;',/,  &
        ' (No default column order defined)', &
        ' -- STOP EXECUTION (UTL_READBLOCKTABLE)')
    550 FORMAT(/,1X,   &
        '*** ERROR: Column label GROUPNAME used in conjunction with',   &
        ' GROUPNAME= option',/,   &
        ' in TABLE format, in block "',A,'"',/,   &
        ' -- STOP EXECUTION (UTL_READBLOCKTABLE)')
    600 FORMAT(/,1X,'*** ERROR: List of column names does not include "',A,  &
        '"',/,' in block "',A,'" -- STOP EXECUTION (UTL_READBLOCKTABLE)')
    700 FORMAT(/,1X,'*** ERROR: Can''t open file "',A,'"',/,1X,   &
        '-- STOP EXECUTION (UTL_READBLOCKTABLE)')
    800 FORMAT(/,1X,I6,' Errors found -- STOP EXECUTION (UTL_READBLOCKTABLE)')
    !
    !   Get the first non-blank, non-comment line after the BEGIN line
    LINE = UTL_GETLINE(INUNIT,IOUT)
    WORD1 = ' '
    WORD2 = ' '
    GPNAM = ' '
    LLOC = 1
    NROW = 0
    NCOL = 0
    ISTART = 1
    ISTOP = 0
    NFILES = 0
    COLLABELS = .FALSE.
    LENLABEL = LEN_TRIM(BLOCKLABEL)
    K=0
    !   Read header to define NROW and NCOL and to look for COLUMNLABELS option
    READHEAD: DO
      K = K+1
      !   UTL_PREPLINE condenses equalities not enclosed in parentheses.  The
      !   first call allows keyword = value to be recognized by UTL_GETTOKEN as
      !   a single token.  The second call ensures that "keyword = value" will
      !   be interpreted correctly as keyword=value in the following code.
      CALL UTL_PREPLINE(LINE)
      CALL UTL_GETTOKEN(IERR,INUNIT,IOUT,0,LLOC,LINE,ISTART,ISTOP,WORD1)
      CALL UTL_PREPLINE(WORD1)
      CALL UTL_CASE(WORD1,UPWORD1,1)
      IF (UPWORD1(1:5) == 'NROW=') THEN
        READ(UPWORD1(6:LEN_TRIM(UPWORD1)),*) NROW
      ELSEIF (UPWORD1(1:5) == 'NCOL=') THEN
        READ(UPWORD1(6:LEN_TRIM(UPWORD1)),*) NCOL
      ELSEIF (UPWORD1 == 'COLUMNLABELS') THEN
        COLLABELS = .TRUE.
      ELSEIF (UPWORD1(1:10) == 'DATAFILES=') THEN
        READ(UPWORD1(11:LEN_TRIM(UPWORD1)),*) NFILES
      ELSEIF (UPWORD1(1:10) == 'GROUPNAME=') THEN
        GPNAM = WORD1(11:LEN_TRIM(UPWORD1))
      ELSEIF (UPWORD1 .NE. ' ') THEN
        CYCLE READHEAD
      ELSEIF (UPWORD1 == ' ') THEN
        EXIT READHEAD
      ENDIF
    ENDDO READHEAD
    !
    IF (COLLABELS) THEN
      !   COLUMNLABELS is specified; read NCOL column labels
      ALLOCATE(LABELS(NCOL))
      READ(INUNIT,'(A)') LINE
      LLOC = 1
      DO I=1,NCOL
        CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
        WORD1 = LINE(ISTART:ISTOP)
        IF (WORD1 .NE. ' ') THEN
          LABELS(I) = WORD1
        ELSE
          WRITE(IOUT,300) NCOL,I-1,BLOCKLABEL(1:LENLABEL)
          CALL UTL_STOP(' ')
        ENDIF
      ENDDO
    ELSE
      !   COLUMNLABELS was not specified; use default column order
      IF (NDEFCOLS > 0) THEN
        IF (NCOL .NE. NDEFCOLS) THEN
          WRITE(IOUT,400) NCOL,BLOCKLABEL(1:LENLABEL),NDEFCOLS,   &
              BLOCKLABEL(1:LENLABEL)
          WRITE(IOUT,410) (TRIM(COLNAMES(I)),I=1,NDEFCOLS)
          CALL UTL_STOP(' ')
        ENDIF
        ALLOCATE(LABELS(NCOL))
        DO I=1,NCOL
          LABELS(I) = COLNAMES(I)
        ENDDO
      ELSE
        WRITE(IOUT,500) BLOCKLABEL(1:LENLABEL)
        CALL UTL_STOP(' ')
      ENDIF
    ENDIF
    !
    !   Determine position of keyitem and GROUPNAME in list of column names
    KEYPOS = 0
    GPOS = 0
    DO I=1,NCOL
      CALL UTL_CASE(LABELS(I),WORD1,1)
      LABELS(I) = WORD1
      IF (WORD1 == KITEM) KEYPOS = I
      IF (WORD1 == 'GROUPNAME') THEN
        GPOS = I
        IF (GPNAM .NE. ' ') THEN
          WRITE(IOUT,550) BLOCKLABEL(1:LENLABEL)
          CALL UTL_STOP(' ')
        ENDIF
      ENDIF
    ENDDO
    IF (KEYPOS == 0) THEN
      IF (KITEM.NE.'*') THEN
        WRITE(IOUT,600) KITEM,BLOCKLABEL(1:LENLABEL)
        CALL UTL_STOP(' ')
      ELSE
        KEYPOS = 1
      ENDIF
    ENDIF
    !
    !   Allocate memory for data values
    ALLOCATE(VALUES(NCOL))
    !
    !   Read each row of table, and each row's data in a linked list
    IERR = 0
    IF (NFILES==0) THEN
      !   Read data from file where block is defined
      DO I=1,NROW
        CALL UTL_READROW(EOF,GPNAM,GPOS,HEAD,IERR,INUNIT,IOUT,ISTAT,KEYPOS,   &
            KITEM,KLISTS,LABELS,NCOL,TAIL,VALUES)
      ENDDO
    ELSE
      !   Read data from one or more external data files
      KROW = 0
      DATFILES: DO K=1,NFILES
        READ(INUNIT,'(A)') LINE
        LLOC = 1
        CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
        WORD1 = LINE(ISTART:ISTOP)
        CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
        WORD2 = LINE(ISTART:ISTOP)
        CALL UTL_CASE(WORD2,UPWORD2,1)
        NSKIP = 0
        IF(UPWORD2(1:5)=='SKIP=') THEN
          READ(UPWORD2(6:LEN_TRIM(UPWORD2)),*) NSKIP
        ENDIF
        INDAT = UTL_GETUNIT(7,1000)
        OPEN(INDAT,FILE=WORD1,STATUS='OLD',IOSTAT=ISTAT)
        IF (ISTAT .NE. 0) THEN
          WRITE(IOUT,700) WORD1
          CALL UTL_STOP(' ')
        ENDIF
        EOF = .FALSE.
        IF (NSKIP>0) READ(INDAT,'(A)') (CHAR1,I=1,NSKIP)
        DO WHILE (KROW<NROW)
          CALL UTL_READROW(EOF,GPNAM,GPOS,HEAD,IERR,INDAT,IOUT,ISTAT,KEYPOS,   &
              KITEM,KLISTS,LABELS,NCOL,TAIL,VALUES)
          IF (ISTAT .NE. 0) THEN
            CLOSE(INDAT)
            CYCLE DATFILES
          ENDIF
          KROW = KROW+1
        ENDDO
        IF (KROW==NROW) EXIT DATFILES
      ENDDO DATFILES
    ENDIF
    IF (IERR>0) THEN
      WRITE(IOUT,800) IERR
      CALL UTL_STOP(' ')
    ENDIF
    !
    !   Deallocate memory
    DEALLOCATE(LABELS,VALUES)
    !
    RETURN
  END SUBROUTINE UTL_READBLOCKTABLE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READLIST(BLOCKERR,BLOCKLABEL,ENDKEYS,INUNIT,IOUT,KEYEQUAL,  &
      LINE,TAIL)
    !   Read list entries associated with one keyitem,
    !   and store information in a linked list.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: BLANKS, MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    LOGICAL,           INTENT(OUT)   :: BLOCKERR
    CHARACTER(LEN=40), INTENT(IN)    :: BLOCKLABEL
    LOGICAL,           INTENT(INOUT) :: ENDKEYS
    INTEGER,           INTENT(IN)    :: INUNIT
    INTEGER,           INTENT(IN)    :: IOUT
    CHARACTER(LEN=41), INTENT(IN)    :: KEYEQUAL
    CHARACTER(LEN=*),  INTENT(INOUT) :: LINE
    TYPE (LLIST),      INTENT(INOUT) :: TAIL
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: WORD1, WORD2, UPWORD1, UPWORD2
    CHARACTER(LEN=1) :: CH1, CH2
    INTEGER :: IEQ, IEQP1, IERR, ISTART, ISTOP, KENTRIES,   &
               LENLABEL, LENKEY, LENWORD, LINEENTRIES, LLOC, N
    DOUBLE PRECISION :: R
    !
    !   Format statements
    50  FORMAT(1X,'*** ERROR: Keyitem ',A,' out of place in block ',A,': ',A)
    100 FORMAT(1X,'*** ERROR: Did not find "END ',A,'" -- (UTL_READLIST)')
    130 FORMAT(1X,'*** ERROR: No value following "',A,'" in ',A,' block')
    150 FORMAT(1X,I5,' Errors found -- STOP EXECUTION (UTL_READLIST)')
    160 FORMAT(/,1X,'*** Error reading "',A,'" block.  BLOCKLABEL ',  &
        'on END line does',/,' not match BLOCKLABEL on BEGIN line.  ',   &
        'Last line read was:',/,1X,A,/,1X,   &
        '-- STOP EXECUTION (UTL_READLIST)')
    !
    BLOCKERR = .FALSE.
    LENKEY = LEN_TRIM(KEYEQUAL)
    LENLABEL = LEN_TRIM(BLOCKLABEL)
    LLOC = 1
    IERR = 0
    KENTRIES = 0
    LINEENTRIES = 0

    LOOPENTRIES: DO

      !   UTL_PREPLINE condenses equalities not enclosed in parentheses.  The
      !   first call allows keyword = value to be recognized by UTL_GETTOKEN as
      !   a single token.  The second call ensures that "keyword = value" will
      !   be interpreted correctly as keyword=value in the following code.
      CALL UTL_PREPLINE(LINE)
      CALL UTL_GETTOKEN(IERR,INUNIT,IOUT,0,LLOC,LINE,ISTART,ISTOP,WORD1)
      CALL UTL_PREPLINE(WORD1)
      !
      CALL UTL_CASE(WORD1,UPWORD1,1)
      IEQ = INDEX(WORD1,'=')
      !   Remove single or double quote following "=" and its matching character
      IEQP1 = IEQ+1
      LENWORD = LEN_TRIM(WORD1)
      IF (IEQP1<LENWORD) THEN
        CH1 = WORD1(IEQP1:IEQP1)
        CH2 = WORD1(LENWORD:LENWORD)
        IF (CH1==CHAR(39) .OR. CH1=='"') THEN
          IF (CH1==CH2) THEN
            WORD1(IEQP1:LENWORD-2) = WORD1(IEQ+2:LENWORD-1)
            WORD1(LENWORD-1:LENWORD) = '  '
            LENWORD = LENWORD-2
          ENDIF
        ENDIF
      ENDIF
      !
      IF (INDEX(UPWORD1,KEYEQUAL(1:LENKEY)) > 0 .AND. KENTRIES > 0) THEN
        IF (LINEENTRIES > 0) THEN
          WRITE(IOUT,50) KEYEQUAL(1:LEN_TRIM(KEYEQUAL)-1),  &
              BLOCKLABEL(1:LENLABEL), WORD1
          IERR = IERR+1
        ENDIF
        EXIT LOOPENTRIES
      ELSEIF (UPWORD1 == 'END') THEN
        CALL UTL_RWORD(IOUT,0,.TRUE.,LLOC,LINE,ISTART,ISTOP,N,R,INUNIT)
        WORD2 = LINE(ISTART:ISTOP)
        CALL UTL_CASE(WORD2,UPWORD2,1)
        IF (UPWORD2 == BLOCKLABEL) THEN
          ENDKEYS = .TRUE.
          EXIT LOOPENTRIES
        ELSE
          WRITE(IOUT,160)TRIM(BLOCKLABEL),TRIM(LINE)
          CALL UTL_STOP(' ')
        ENDIF
      ELSEIF (WORD1 == ' ') THEN        ! End of line -- go to next line
        CONTINUE
      ELSEIF (IEQ > 1 .AND. LEN_TRIM(WORD1) > IEQ) THEN
        !   Entry has form KEYWORD=VALUE.  Store keyword and value.
        KENTRIES = KENTRIES+1
        LINEENTRIES = LINEENTRIES+1
        CALL UTL_ADDNODE(IOUT,UPWORD1(1:(IEQ-1)),TAIL,   &
            WORD1((IEQ+1):LEN_TRIM(WORD1)))
        CYCLE LOOPENTRIES
      ELSEIF (IEQ > 1 .AND. LEN_TRIM(WORD1) == IEQ) THEN
        !   Entry has form KEYWORD=  .  If this is the keyitem, it's an error.
        IF (WORD1 == KEYEQUAL) THEN
          IERR = IERR+1
          WRITE(IOUT,130) KEYEQUAL(1:LENKEY), BLOCKLABEL(1:LENLABEL)
        ENDIF
      ELSE                              ! Entry not of form KEYWORD=VALUE
        CYCLE LOOPENTRIES               ! Get next word on this line
      ENDIF

      LINE = UTL_GETLINE(INUNIT,IOUT)       ! Get next line
      LLOC = 1
      LINEENTRIES = 0

      IF (LINE == ' ') THEN
        WRITE(IOUT,100) BLOCKLABEL(1:LENLABEL)
        ENDKEYS = .TRUE.
        BLOCKERR = .TRUE.
        EXIT LOOPENTRIES
      ENDIF

    ENDDO LOOPENTRIES

    IF (IERR > 0) THEN
      WRITE(IOUT,150) IERR
      CALL UTL_STOP(' ')
    ENDIF

    RETURN
  END SUBROUTINE UTL_READLIST
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READMATRIX_CDM(INUNIT,IOUT,CDM)
    !   Read a matrix from a file coded as either a standard matrix or a
    !   compressed matrix into a CDMATRIX structure.
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,         INTENT(IN)    :: INUNIT
    INTEGER,         INTENT(IN)    :: IOUT
    TYPE (CDMATRIX), INTENT(INOUT) :: CDM
    !
    !   Local variables
    INTEGER :: ISTART, ISTAT, ISTOP, KERR, LLOC
    LOGICAL :: LEX, LOP
    CHARACTER(LEN=10)             :: FRM, RD
    CHARACTER(LEN=12)             :: ANAME
    CHARACTER(LEN=16)             :: MTYPE
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    !
    !   Format statements
    100 FORMAT(1X,'Error getting status of unit ',i7,   &
        ' (UTL_READMATRIX_CDM)')
    110 FORMAT(1X,'Error: Unit ',I7,' does not exist (UTL_READMATRIX_CDM)')
    120 FORMAT(1X,'Error: Unit ',I7,' not opened (UTL_READMATRIX_CDM)')
    130 FORMAT(1X,'Error: File not opened for formatted input',/,   &
        1X,' on unit ',I7,' (UTL_READMATRIX_CDM)')
    140 FORMAT(1X,'Error: File not opened for reading',/,   &
        1X,' on unit ',I7,' (UTL_READMATRIX_CDM)')
    150 FORMAT(1X,'Error: Reading of matrix in SKYLINE format is not',   &
        ' currently supported.',/,1X,'Error is in file opened on unit ',I7,   &
        ' (UTL_READMATRIX_CDM)')
    160 FORMAT(1X,'Error reading file opened on unit ',I7,':',/,   &
        ' Expected matrix-format keyword, either: COMPLETEMATRIX or',   &
        ' COMPRESSEDMATRIX,',/,   &
        ' but read: "',A,'".  (UTL_READMATRIX_CDM)')
    !
    ISTAT = 0
    LEX = .FALSE.
    LOP = .FALSE.
    FRM = ' '
    RD = ' '
    !
    INQUIRE(UNIT=INUNIT,IOSTAT=ISTAT,EXIST=LEX,OPENED=LOP,   &
            FORM=FRM,READ=RD)
    IF (ISTAT.NE.0) THEN
      WRITE(IOUT,100)INUNIT
      GOTO 900
    ENDIF
    IF (.NOT. LEX) THEN
      WRITE(IOUT,110)INUNIT
      GOTO 900
    ENDIF
    IF (.NOT. LOP) THEN
      WRITE(IOUT,120)INUNIT
      GOTO 900
    ENDIF
    IF (FRM .NE. 'FORMATTED') THEN
      WRITE(IOUT,130)INUNIT
      GOTO 900
    ENDIF
    IF (RD .NE. 'YES') THEN
      WRITE(IOUT,140)INUNIT
      GOTO 900
    ENDIF
    !
    KERR = 0
    MTYPE = ' '
    LINE = UTL_GETLINE(INUNIT,IOUT)
    LLOC = 1
    CALL UTL_GETTOKEN(KERR,INUNIT,IOUT,1,LLOC,LINE,ISTART,ISTOP,MTYPE)
    CALL UTL_GETTOKEN(KERR,INUNIT,IOUT,0,LLOC,LINE,ISTART,ISTOP,ANAME)
    IF (KERR>0) GOTO 900
    IF (MTYPE=='COMPLETEMATRIX') THEN
      !   Read matrix in standard format
      CALL UTL_READ_STDMATRIX(ANAME,INUNIT,IOUT,CDM)
    ELSEIF (MTYPE=='COMPRESSEDMATRIX') THEN
      !   Read matrix in compressed format
      CALL UTL_READ_CMPMATRIX(ANAME,INUNIT,IOUT,CDM)
    ELSEIF (MTYPE=='SKYLINEMATRIX') THEN
      WRITE(IOUT,150)INUNIT
      GOTO 900
    ELSE
      WRITE(IOUT,160)INUNIT,MTYPE
      GOTO 900
    ENDIF
    !
    RETURN
    !
    900 CONTINUE
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_READMATRIX_CDM
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_READROW(EOF,GPNAM,GPOS,HEAD,IERR,INUNIT,IOUT,ISTAT,KEYPOS,   &
      KITEM,KLISTS,LABELS,NCOL,TAIL,VALUES)
    !   Read a row of data for TABLE block format
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    LOGICAL :: EOF
    CHARACTER(LEN=12), INTENT(INOUT)               :: GPNAM
    INTEGER,           INTENT(IN)                  :: GPOS, INUNIT, IOUT, KEYPOS, NCOL
    INTEGER,           INTENT(INOUT)               :: IERR, KLISTS
    CHARACTER(LEN=*),  INTENT(IN)                  :: KITEM       ! Key item
    TYPE (LLIST),      POINTER                     :: HEAD ! Pointer to head of list for one KEYITEM
    CHARACTER(LEN=MAX_STRING_LEN)                  :: WORD1
    CHARACTER(LEN=40),             DIMENSION(NCOL) :: LABELS
    CHARACTER(LEN=MAX_STRING_LEN), DIMENSION(NCOL) :: VALUES
    TYPE (LLIST),      POINTER                     :: TAIL
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: LINE
    INTEGER :: ISTART, ISTAT, ISTOP, J, LENGPNAM, LLOC
    !
    !   Format statements
    200 FORMAT(1X,'*** ERROR reading line from unit ',I5,':',/,1X,A)
    !
    READ(INUNIT,'(A)',IOSTAT=ISTAT,END=20) LINE
    GOTO 40
    20 CONTINUE
    EOF = .TRUE.
    GOTO 900
    40 CONTINUE
    IF (ISTAT .NE. 0) THEN
      WRITE(IOUT,200) INUNIT, LINE
      IERR=IERR+1
      GOTO 900
    ENDIF
    LLOC = 1
    DO J=1,NCOL
      CALL UTL_GETTOKEN(IERR,INUNIT,IOUT,0,LLOC,LINE,ISTART,ISTOP,WORD1)
      VALUES(J) = WORD1
    ENDDO
    IF (GPOS > 0) THEN
      GPNAM = VALUES(GPOS)
    ENDIF
    IF (GPNAM==' ') THEN
      LENGPNAM = 1
    ELSE
      LENGPNAM = LEN_TRIM(GPNAM)
    ENDIF
    !   Add a new list for this row
    CALL UTL_NEWLIST(GPNAM(1:LENGPNAM),HEAD,IOUT,TAIL)
    KLISTS = KLISTS+1
    !
    !   Add a node to the list for the key item
    IF (KITEM.NE.'*') THEN
      CALL UTL_ADDNODE(IOUT,TRIM(KITEM),TAIL,VALUES(KEYPOS))
    ENDIF
    !
    !   Add a node to the list for the group name, if defined
    IF (GPNAM .NE. ' ') CALL UTL_ADDNODE(IOUT,'GROUPNAME',TAIL,GPNAM)
    !
    !   Add a node for each column other than the KEYPOS column
    DO J=1,NCOL
      IF (J == KEYPOS .AND. KITEM.NE.'*') CYCLE
      CALL UTL_ADDNODE(IOUT,LABELS(J),TAIL,VALUES(J))
    ENDDO
    !
    900 CONTINUE
    RETURN
  END SUBROUTINE UTL_READROW
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_REMCHAR(ASTRING,ACH)

       IMPLICIT NONE

       CHARACTER(LEN=*), INTENT(INOUT) :: ASTRING
       CHARACTER(LEN=*), INTENT(IN)    :: ACH

       INTEGER LL,II,ICOUNT

       ICOUNT=0
       LL=LEN_TRIM(ACH)

10     II=INDEX(ASTRING,ACH)
       IF(II.EQ.0) THEN
         IF(ICOUNT.EQ.0)RETURN
         GO TO 20
       END IF
       ICOUNT=ICOUNT+1
       ASTRING(II:II-1+LL)=' '
       GO TO 10

20     ASTRING=ADJUSTL(ASTRING)
       RETURN

  END SUBROUTINE UTL_REMCHAR
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_RWORD_8D(IOUT,NCODE,RECQUOTES,ICOL,LINE,ISTART,ISTOP,N,R,INUNIT)
    !   Routine to extract a word from a line of text, and optionally
    !   convert the word to a number.
    !      ISTART and ISTOP will be returned with the starting and
    !        ending character positions of the word.
    !      The last character in LINE is set to blank so that if any
    !        problems occur with finding a word, ISTART and ISTOP will
    !        point to this blank character.  Thus, a word will always be
    !        returned unless there is a numeric conversion error.  Be sure
    !        that the last character in LINE is not an important character
    !        because it will always be set to blank.
    !      A word starts with the first character that is not a space, comma, or
    !        TAB character and ends when a subsequent character is a space,
    !        comma, or TAB character.  Note that these parsing rules do not
    !        treat two commas separated by one or more spaces as a null word.
    !      When RECQUOTES is true, For a word that begins with a single quote
    !        (') or double quote ("), the word starts with the character after
    !        the quote and ends with the character preceding a subsequent,
    !        matching quote.  Thus, a quoted word can include spaces, commas,
    !         and TAB characters.  The quoted word cannot contain a quote
    !        character that matches the initial quote character.
    !      When RECQUOTES is false, quote characters are treated as ordinary
    !        characters.
    !      If NCODE is 1, the word is converted to upper case.
    !      If NCODE IS 2, the word is converted to an integer.
    !      If NCODE IS 3, the word is converted to a double precision number.
    !      If NCODE is any other value, no conversion is performed.
    !      Words to be converted to numbers may have up to 42 characters.  This
    !        should allow reading of data recorded as quad precision values.
    !      Number conversion error is written to unit IOUT if IOUT is
    !        positive; error is written to default output if IOUT is 0.
    !        No error message is written if IOUT is negative.
    !      If INUNIT is present and greater than 0, error messages identify
    !        INUNIT as the unit number from which LINE was read.
    !---------------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,           INTENT(IN)    :: IOUT
    INTEGER,           INTENT(IN)    :: NCODE
    LOGICAL,           INTENT(IN)    :: RECQUOTES
    INTEGER,           INTENT(INOUT) :: ICOL
    CHARACTER(LEN=*),  INTENT(INOUT) :: LINE
    INTEGER,           INTENT(OUT)   :: ISTART
    INTEGER,           INTENT(OUT)   :: ISTOP
    INTEGER(KIND=8),   INTENT(OUT)   :: N
    DOUBLE PRECISION,  INTENT(OUT)   :: R
    INTEGER, OPTIONAL, INTENT(IN)    :: INUNIT
    !
    !   Local variables
    INTEGER :: I, IDIFF, ISTAT, IU, J, K, L, LINLEN
    CHARACTER(LEN=1)  :: QCHAR, TAB
    CHARACTER(LEN=25) :: STRING
    CHARACTER(LEN=42) :: RW
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    TAB=CHAR(9)
    IU=0
    IF (PRESENT(INUNIT)) IU=INUNIT
    !
    !C1------Set last char in LINE to blank and set ISTART and ISTOP to point
    !C1------to this blank as a default situation when no word is found.  If
    !C1------starting location in LINE is out of bounds, do not look for a
    !C1------word.
    LINLEN=LEN(LINE)
    LINE(LINLEN:LINLEN)=' '
    ISTART=LINLEN
    ISTOP=LINLEN
    LINLEN=LINLEN-1
    IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
    !
    !C2------Find start of word, which is indicated by first character that
    !C2------is not a blank, a comma, or a tab.
    DO I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' .AND. LINE(I:I).NE.TAB)   &
          GO TO 20
    ENDDO
    ICOL=LINLEN+1
    GO TO 100
    !
    !C3------Found start of word.  Look for end.
    !C3A-----When word is quoted, only a quote can terminate it.
    20 CONTINUE
    IF(RECQUOTES .AND. (LINE(I:I).EQ.'''' .OR. LINE(I:I).EQ.'"')) THEN
      QCHAR = LINE(I:I)
      I=I+1
      IF(I.LE.LINLEN) THEN
        DO J=I,LINLEN
          IF(LINE(J:J).EQ.QCHAR) GO TO 40
        ENDDO
      END IF
    !
    !C3B-----When word is not quoted, space, comma, or tab will terminate.
    ELSE
      DO J=I,LINLEN
        IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' .OR. LINE(J:J).EQ.TAB)   &
            GO TO 40
      ENDDO
    END IF
    !
    !C3C-----End of line without finding end of word; set end of word to
    !C3C-----end of line.
    J=LINLEN+1
    !
    !C4------Found end of word; set J to point to last character in WORD and
    !-------set ICOL to point to location for scanning for another word.
    40 CONTINUE
    ICOL=J+1
    J=J-1
    IF(J.LT.I) GO TO 100
    ISTART=I
    ISTOP=J
    !
    !C5------Convert word to upper case and RETURN if NCODE is 1.
    IF(NCODE.EQ.1) THEN
      IDIFF=ICHAR('a')-ICHAR('A')
      DO K=ISTART,ISTOP
        IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')   &
            LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
      ENDDO
      RETURN
    END IF
    !
    !C6------Convert word to a number if requested.
    100 CONTINUE
    IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
      RW=' '
      L=42-ISTOP+ISTART
      IF(L.LT.1) GO TO 200
      RW(L:42)=LINE(ISTART:ISTOP)
      IF(NCODE.EQ.2) READ(RW,'(I42)',ERR=200) N
      IF(NCODE.EQ.3) READ(RW,'(F42.0)',ERR=200) R
    END IF
    RETURN
    !
    !C7------Number conversion error.
    200 CONTINUE
    IF(NCODE.EQ.3) THEN
      STRING= 'A DOUBLE PRECISION NUMBER'
      L=25
    ELSE
      STRING= 'AN INTEGER'
      L=10
    END IF
    !
    !C7A-----If output unit is negative, set last character of string to 'E'.
    IF(IOUT.LT.0) THEN
      N=0
      R=0.
      LINE(LINLEN+1:LINLEN+1)='E'
      RETURN
    !
    !C7B-----If output unit is positive; write a message to output unit.
    ELSE IF(IOUT.GT.0) THEN
      IF(IU.GT.0) THEN
        ISTAT = 0
        INQUIRE(UNIT=IU,NAME=FNAME,IOSTAT=ISTAT)
        IF (ISTAT==0) THEN
          WRITE(IOUT,203) TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
        ELSE
          WRITE(IOUT,201) IU,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
        ENDIF
      ELSE
        WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      END IF
      201 FORMAT(1X,/1X,'FILE UNIT ',I4,': ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
      202 FORMAT(1X,/1X,'KEYBOARD INPUT: ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
      203 FORMAT(1X,/1X,'FILE "',A,'": ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
    !
    !C7C-----If output unit is 0; write a message to default output.
    ELSE
      IF (IU.GT.0) THEN
        WRITE(*,201) IU,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      ELSE
        WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      END IF
    END IF
    !
    !C7D-----STOP after writing message.
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_RWORD_8D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_RWORD_D(IOUT,NCODE,RECQUOTES,ICOL,LINE,ISTART,ISTOP,N,R,INUNIT)
    !   Routine to extract a word from a line of text, and optionally
    !   convert the word to a number.
    !      ISTART and ISTOP will be returned with the starting and
    !        ending character positions of the word.
    !      The last character in LINE is set to blank so that if any
    !        problems occur with finding a word, ISTART and ISTOP will
    !        point to this blank character.  Thus, a word will always be
    !        returned unless there is a numeric conversion error.  Be sure
    !        that the last character in LINE is not an important character
    !        because it will always be set to blank.
    !      A word starts with the first character that is not a space, comma, or
    !        TAB character and ends when a subsequent character is a space,
    !        comma, or TAB character.  Note that these parsing rules do not
    !        treat two commas separated by one or more spaces as a null word.
    !      When RECQUOTES is true, For a word that begins with a single quote
    !        (') or double quote ("), the word starts with the character after
    !        the quote and ends with the character preceding a subsequent,
    !        matching quote.  Thus, a quoted word can include spaces, commas,
    !         and TAB characters.  The quoted word cannot contain a quote
    !        character that matches the initial quote character.
    !      When RECQUOTES is false, quote characters are treated as ordinary
    !        characters.
    !      If NCODE is 1, the word is converted to upper case.
    !      If NCODE IS 2, the word is converted to an integer.
    !      If NCODE IS 3, the word is converted to a double precision number.
    !      If NCODE is any other value, no conversion is performed.
    !      Words to be converted to numbers may have up to 42 characters.  This
    !        should allow reading of data recorded as quad precision values.
    !      Number conversion error is written to unit IOUT if IOUT is
    !        positive; error is written to default output if IOUT is 0.
    !        No error message is written if IOUT is negative.
    !      If INUNIT is present and greater than 0, error messages identify
    !        INUNIT as the unit number from which LINE was read.
    !---------------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,           INTENT(IN)    :: IOUT
    INTEGER,           INTENT(IN)    :: NCODE
    LOGICAL,           INTENT(IN)    :: RECQUOTES
    INTEGER,           INTENT(INOUT) :: ICOL
    CHARACTER(LEN=*),  INTENT(INOUT) :: LINE
    INTEGER,           INTENT(OUT)   :: ISTART
    INTEGER,           INTENT(OUT)   :: ISTOP
    INTEGER,           INTENT(OUT)   :: N
    DOUBLE PRECISION,  INTENT(OUT)   :: R
    INTEGER, OPTIONAL, INTENT(IN)    :: INUNIT
    !
    !   Local variables
    INTEGER :: I, IDIFF, ISTAT, IU, J, K, L, LINLEN
    CHARACTER(LEN=1)  :: QCHAR, TAB
    CHARACTER(LEN=25) :: STRING
    CHARACTER(LEN=42) :: RW
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    TAB=CHAR(9)
    IU=0
    IF (PRESENT(INUNIT)) IU=INUNIT
    !
    !C1------Set last char in LINE to blank and set ISTART and ISTOP to point
    !C1------to this blank as a default situation when no word is found.  If
    !C1------starting location in LINE is out of bounds, do not look for a
    !C1------word.
    LINLEN=LEN(LINE)
    LINE(LINLEN:LINLEN)=' '
    ISTART=LINLEN
    ISTOP=LINLEN
    LINLEN=LINLEN-1
    IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
    !
    !C2------Find start of word, which is indicated by first character that
    !C2------is not a blank, a comma, or a tab.
    DO I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' .AND. LINE(I:I).NE.TAB)   &
          GO TO 20
    ENDDO
    ICOL=LINLEN+1
    GO TO 100
    !
    !C3------Found start of word.  Look for end.
    !C3A-----When word is quoted, only a quote can terminate it.
    20 CONTINUE
    IF(RECQUOTES .AND. (LINE(I:I).EQ.'''' .OR. LINE(I:I).EQ.'"')) THEN
      QCHAR = LINE(I:I)
      I=I+1
      IF(I.LE.LINLEN) THEN
        DO J=I,LINLEN
          IF(LINE(J:J).EQ.QCHAR) GO TO 40
        ENDDO
      END IF
    !
    !C3B-----When word is not quoted, space, comma, or tab will terminate.
    ELSE
      DO J=I,LINLEN
        IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' .OR. LINE(J:J).EQ.TAB)   &
            GO TO 40
      ENDDO
    END IF
    !
    !C3C-----End of line without finding end of word; set end of word to
    !C3C-----end of line.
    J=LINLEN+1
    !
    !C4------Found end of word; set J to point to last character in WORD and
    !-------set ICOL to point to location for scanning for another word.
    40 CONTINUE
    ICOL=J+1
    J=J-1
    IF(J.LT.I) GO TO 100
    ISTART=I
    ISTOP=J
    !
    !C5------Convert word to upper case and RETURN if NCODE is 1.
    IF(NCODE.EQ.1) THEN
      IDIFF=ICHAR('a')-ICHAR('A')
      DO K=ISTART,ISTOP
        IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')   &
            LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
      ENDDO
      RETURN
    END IF
    !
    !C6------Convert word to a number if requested.
    100 CONTINUE
    IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
      RW=' '
      L=42-ISTOP+ISTART
      IF(L.LT.1) GO TO 200
      RW(L:42)=LINE(ISTART:ISTOP)
      IF(NCODE.EQ.2) READ(RW,'(I42)',ERR=200) N
      IF(NCODE.EQ.3) READ(RW,'(F42.0)',ERR=200) R
    END IF
    RETURN
    !
    !C7------Number conversion error.
    200 CONTINUE
    IF(NCODE.EQ.3) THEN
      STRING= 'A DOUBLE PRECISION NUMBER'
      L=25
    ELSE
      STRING= 'AN INTEGER'
      L=10
    END IF
    !
    !C7A-----If output unit is negative, set last character of string to 'E'.
    IF(IOUT.LT.0) THEN
      N=0
      R=0.
      LINE(LINLEN+1:LINLEN+1)='E'
      RETURN
    !
    !C7B-----If output unit is positive; write a message to output unit.
    ELSE IF(IOUT.GT.0) THEN
      IF(IU.GT.0) THEN
        ISTAT = 0
        INQUIRE(UNIT=IU,NAME=FNAME,IOSTAT=ISTAT)
        IF (ISTAT==0) THEN
          WRITE(IOUT,203) TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
        ELSE
          WRITE(IOUT,201) IU,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
        ENDIF
      ELSE
        WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      END IF
      201 FORMAT(1X,/1X,'FILE UNIT ',I4,': ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
      202 FORMAT(1X,/1X,'KEYBOARD INPUT: ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
      203 FORMAT(1X,/1X,'FILE "',A,'": ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
    !
    !C7C-----If output unit is 0; write a message to default output.
    ELSE
      IF (IU.GT.0) THEN
        WRITE(*,201) IU,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      ELSE
        WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      END IF
    END IF
    !
    !C7D-----STOP after writing message.
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_RWORD_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_RWORD_R(IOUT,NCODE,RECQUOTES,ICOL,LINE,ISTART,ISTOP,N,R,INUNIT)
    !   Routine to extract a word from a line of text, and optionally
    !   convert the word to a number.
    !      ISTART and ISTOP will be returned with the starting and
    !        ending character positions of the word.
    !      The last character in LINE is set to blank so that if any
    !        problems occur with finding a word, ISTART and ISTOP will
    !        point to this blank character.  Thus, a word will always be
    !        returned unless there is a numeric conversion error.  Be sure
    !        that the last character in LINE is not an important character
    !        because it will always be set to blank.
    !      A word starts with the first character that is not a space, comma, or
    !        TAB character and ends when a subsequent character is a space,
    !        comma, or TAB character.  Note that these parsing rules do not
    !        treat two commas separated by one or more spaces as a null word.
    !      When RECQUOTES is true, For a word that begins with a single quote
    !        (') or double quote ("), the word starts with the character after
    !        the quote and ends with the character preceding a subsequent,
    !        matching quote.  Thus, a quoted word can include spaces, commas,
    !         and TAB characters.  The quoted word cannot contain a quote
    !        character that matches the initial quote character.
    !      When RECQUOTES is false, quote characters are treated as ordinary
    !        characters.
    !      If NCODE is 1, the word is converted to upper case.
    !      If NCODE IS 2, the word is converted to an integer.
    !      If NCODE IS 3, the word is converted to a double precision number.
    !      If NCODE is any other value, no conversion is performed.
    !      Words to be converted to numbers may have up to 42 characters.  This
    !        should allow reading of data recorded as quad precision values.
    !      Number conversion error is written to unit IOUT if IOUT is
    !        positive; error is written to default output if IOUT is 0.
    !        No error message is written if IOUT is negative.
    !      If INUNIT is present and greater than 0, error messages identify
    !        INUNIT as the unit number from which LINE was read.
    !---------------------------------------------------------------------------
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,           INTENT(IN)    :: IOUT
    INTEGER,           INTENT(IN)    :: NCODE
    LOGICAL,           INTENT(IN)    :: RECQUOTES
    INTEGER,           INTENT(INOUT) :: ICOL
    CHARACTER(LEN=*),  INTENT(INOUT) :: LINE
    INTEGER,           INTENT(OUT)   :: ISTART
    INTEGER,           INTENT(OUT)   :: ISTOP
    INTEGER,           INTENT(OUT)   :: N
    REAL,              INTENT(OUT)   :: R
    INTEGER, OPTIONAL, INTENT(IN)    :: INUNIT
    !
    !   Local variables
    INTEGER :: I, IDIFF, ISTAT, IU, J, K, L, LINLEN
    CHARACTER(LEN=1)  :: QCHAR, TAB
    CHARACTER(LEN=25) :: STRING
    CHARACTER(LEN=42) :: RW
    CHARACTER(LEN=MAX_STRING_LEN) :: FNAME
    !
    TAB=CHAR(9)
    IU=0
    IF (PRESENT(INUNIT)) IU=INUNIT
    !
    !C1------Set last char in LINE to blank and set ISTART and ISTOP to point
    !C1------to this blank as a default situation when no word is found.  If
    !C1------starting location in LINE is out of bounds, do not look for a
    !C1------word.
    LINLEN=LEN(LINE)
    LINE(LINLEN:LINLEN)=' '
    ISTART=LINLEN
    ISTOP=LINLEN
    LINLEN=LINLEN-1
    IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
    !
    !C2------Find start of word, which is indicated by first character that
    !C2------is not a blank, a comma, or a tab.
    DO I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' .AND. LINE(I:I).NE.TAB)   &
          GO TO 20
    ENDDO
    ICOL=LINLEN+1
    GO TO 100
    !
    !C3------Found start of word.  Look for end.
    !C3A-----When word is quoted, only a quote can terminate it.
    20 CONTINUE
    IF(RECQUOTES .AND. (LINE(I:I).EQ.'''' .OR. LINE(I:I).EQ.'"')) THEN
      QCHAR = LINE(I:I)
      I=I+1
      IF(I.LE.LINLEN) THEN
        DO J=I,LINLEN
          IF(LINE(J:J).EQ.QCHAR) GO TO 40
        ENDDO
      END IF
    !
    !C3B-----When word is not quoted, space, comma, or tab will terminate.
    ELSE
      DO J=I,LINLEN
        IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' .OR. LINE(J:J).EQ.TAB)   &
            GO TO 40
      ENDDO
    END IF
    !
    !C3C-----End of line without finding end of word; set end of word to
    !C3C-----end of line.
    J=LINLEN+1
    !
    !C4------Found end of word; set J to point to last character in WORD and
    !-------set ICOL to point to location for scanning for another word.
    40 CONTINUE
    ICOL=J+1
    J=J-1
    IF(J.LT.I) GO TO 100
    ISTART=I
    ISTOP=J
    !
    !C5------Convert word to upper case and RETURN if NCODE is 1.
    IF(NCODE.EQ.1) THEN
      IDIFF=ICHAR('a')-ICHAR('A')
      DO K=ISTART,ISTOP
        IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z')   &
            LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
      ENDDO
      RETURN
    END IF
    !
    !C6------Convert word to a number if requested.
    100 CONTINUE
    IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
      RW=' '
      L=42-ISTOP+ISTART
      IF(L.LT.1) GO TO 200
      RW(L:42)=LINE(ISTART:ISTOP)
      IF(NCODE.EQ.2) READ(RW,'(I42)',ERR=200) N
      IF(NCODE.EQ.3) READ(RW,'(F42.0)',ERR=200) R
    END IF
    RETURN
    !
    !C7------Number conversion error.
    200 CONTINUE
    IF(NCODE.EQ.3) THEN
      STRING= 'A REAL NUMBER'
      L=25
    ELSE
      STRING= 'AN INTEGER'
      L=10
    END IF
    !
    !C7A-----If output unit is negative, set last character of string to 'E'.
    IF(IOUT.LT.0) THEN
      N=0
      R=0.
      LINE(LINLEN+1:LINLEN+1)='E'
      RETURN
    !
    !C7B-----If output unit is positive; write a message to output unit.
    ELSE IF(IOUT.GT.0) THEN
      IF(IU.GT.0) THEN
        ISTAT = 0
        INQUIRE(UNIT=IU,NAME=FNAME,IOSTAT=ISTAT)
        IF (ISTAT==0) THEN
          WRITE(IOUT,203) TRIM(FNAME),LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
        ELSE
          WRITE(IOUT,201) IU,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
        ENDIF
      ELSE
        WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      END IF
      201 FORMAT(1X,/1X,'FILE UNIT ',I4,': ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
      202 FORMAT(1X,/1X,'KEYBOARD INPUT: ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
      203 FORMAT(1X,/1X,'FILE "',A,'": ERROR CONVERTING "',A,   &
          '" TO ',A,' IN LINE:',/1X,A)
    !
    !C7C-----If output unit is 0; write a message to default output.
    ELSE
      IF (IU.GT.0) THEN
        WRITE(*,201) IU,LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      ELSE
        WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),TRIM(LINE)
      END IF
    END IF
    !
    !C7D-----STOP after writing message.
    CALL UTL_STOP(' ')
  END SUBROUTINE UTL_RWORD_R
  !-----------------------------------------------------------------------------
  FUNCTION UTL_SAMENAME(NAME1,NAME2) RESULT(SAME)
    !   Make a case-insensitive comparison of two names
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    CHARACTER(LEN=*), INTENT(IN) :: NAME1 ! Name to be compared
    CHARACTER(LEN=*), INTENT(IN) :: NAME2 ! Name to be compared
    LOGICAL :: SAME
    !
    !   Local variables
    CHARACTER(LEN=MAX_STRING_LEN) :: NAME1UP, NAME2UP
    !
    !   Convert both names to upper case, and make comparison
    CALL UTL_CASE(NAME1,NAME1UP,1)
    CALL UTL_CASE(NAME2,NAME2UP,1)
    IF (NAME1UP==NAME2UP) THEN
      SAME = .TRUE.
    ELSE
      SAME = .FALSE.
    ENDIF
    !
    RETURN
  END FUNCTION UTL_SAMENAME
!=======================================================================
  SUBROUTINE UTL_SHELLSORT(N,X)
    !    Algorithm obtained from http://www.nist.gov/dads/HTML/shellsort.html
    !    Thank you NIST!
    !  ALGORITHM AS 304.8 APPL.STATIST. (1996), VOL.45, NO.3
    !  Sorts the N values stored in array X in ascending order
    !  Based on an "adaptation by Marlene Metzner" this algorithm is referred to
    !  as the Shell-Metzner sort by John P. Grillo, A Comparison of Sorts,
    !  Creative Computing, 2:76-80, Nov/Dec 1976. Grillo cites Fredric Stuart,
    !  FORTRAN Programming, John Wiley and Sons, New York, 1969, page 294. In
    !  crediting "one of the fastest" programs for sorting, Stuart says in a
    !  footnote, "Published by Marlene Metzner, Pratt & Whitney Aircraft
    !  Company. From a method described by D. L. Shell."
    !
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                        INTENT(IN)    ::  N
    CHARACTER(LEN=*), DIMENSION(N), INTENT(INOUT) ::  X
    !
    ! Local variables
    CHARACTER(LEN=MAX_STRING_LEN)   ::  TEMP
    INTEGER                         ::  I
    INTEGER                         ::  J
    INTEGER                         ::  INCR = 1
    !
    !    Loop : calculate the increment
    !
    10 INCR = 3 * INCR + 1
    IF (INCR .LE. N) GOTO 10
    !
    !    Loop : Shell-Metzner sort
    !
    20 INCR = INCR / 3
    I = INCR + 1
    30 IF (I .GT. N) GOTO 60
    TEMP = X(I)
    J = I
    40 IF (X(J - INCR) .LT. TEMP) GOTO 50
    X(J) = X(J - INCR)
    J = J - INCR
    IF (J .GT. INCR) GOTO 40
    50 X(J) = TEMP
    I = I + 1
    GOTO 30
    60 IF (INCR .GT. 1) GOTO 20
    !
    RETURN
  END SUBROUTINE UTL_SHELLSORT
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SSVD1(A,N,NP,D,E)
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, TRED2S,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION
    !     ******************************************************************
    !   Modified 11/20/03 to use CDMATRIX structure for A
    USE DATATYPES
    !
    !   Argument-list variables
    TYPE (CDMATRIX),                 INTENT(INOUT) :: A
    INTEGER,                         INTENT(IN)    :: N
    INTEGER,                         INTENT(IN)    :: NP
    DOUBLE PRECISION, DIMENSION(NP), INTENT(OUT)   :: D
    DOUBLE PRECISION, DIMENSION(NP), INTENT(OUT)   :: E
    !
    !   Local variables
    DOUBLE PRECISION :: F, G, H, HH, SCALE
    INTEGER :: I, ICOL, IROW, J, K, L, M, NNZ
    !
    IF (N.GT.1) THEN
      NNZ = A%NNZ
      DO 80 I = N, 2, -1               ! iterate up rows, all but first row
        L = I - 1
        H = 0.0D0
        SCALE = 0.0D0
        IF (L.GT.1) THEN               ! true if I>2
          DO 10 K = 1, L               ! iterate across row left of diagonal
            SCALE = SCALE + ABS(UTL_GETVAL(A,I,K))
 10       CONTINUE
          IF (SCALE.EQ.0.0D0) THEN
            E(I) = UTL_GETVAL(A,I,L)
          ELSE
            !
            !   Execute for elements in row I left of diagonal:
            DOM: DO M=1,NNZ
              ICOL = UTL_GETICOL(A,M)
              IF (ICOL .GE. I) EXIT DOM
              IROW = UTL_GETIROW(A,M)
              IF (IROW .NE. I) CYCLE DOM
              A%DVAL(M) = A%DVAL(M)/SCALE
              H = H+(A%DVAL(M))**2
            ENDDO DOM
            !
            F = UTL_GETVAL(A,I,L)
            G = -SIGN(SQRT(H),F)  ! G is nonzero if any A left of diag in row I is nonzero
            E(I) = SCALE*G
            H = H - F*G
            M = UTL_GETPOS(A,I,L)
            IF (M>0) A%DVAL(M) = F - G     ! A(i,l) is element immediately left of diagonal;
            F = 0.0D0             ! if all A left of diagonal = 0, A(i,l) is 0
            DO 50 J = 1, L        ! iterate across row left of diag, or down column above diag
              M = UTL_GETPOS(A,J,I)
              IF (M>0) A%DVAL(M) = UTL_GETVAL(A,I,J)/H
              G = 0.0D0
              DO 30 K = 1, J              ! iterate across row to column j
                G = G + UTL_GETVAL(A,J,K)*UTL_GETVAL(A,I,K)
 30           CONTINUE
              IF (L.GT.J) THEN
                DO 40 K = J+1, L
                  G = G + UTL_GETVAL(A,K,J)*UTL_GETVAL(A,I,K)
 40             CONTINUE
              ENDIF
              E(J) = G/H
              F = F + E(J)*UTL_GETVAL(A,I,J)
 50         CONTINUE
            HH = F/(H+H)
            DO 70 J = 1, L        ! iterate across row left of diag or down column above diag
              F = UTL_GETVAL(A,I,J)
              G = E(J) - HH*F
              E(J) = G
              DO 60 K = 1, J      ! iterate across row to column j
                M = UTL_GETPOS(A,J,K)
                IF (M>0) THEN
                  A%DVAL(M) = UTL_GETVAL(A,J,K) - F*E(K) - G*UTL_GETVAL(A,I,K)
                ENDIF
 60           CONTINUE
 70         CONTINUE
          ENDIF
        ELSE
          E(I) = UTL_GETVAL(A,I,L)
        ENDIF
        D(I) = H
 80   CONTINUE
    ENDIF
    D(1) = 0.0D0
    E(1) = 0.0D0
    DO 130 I = 1, N              ! iterate across rows or down columns
      L = I - 1
      IF (D(I).NE.0.0D0) THEN
        DO 110 J = 1, L          ! iterate across row left of diagonal
          G = 0.0D0
          DO 90 K = 1, L         ! iterate across row left of diag or down column above diagonal
            G = G + UTL_GETVAL(A,I,K)*UTL_GETVAL(A,K,J)
 90       CONTINUE
          DO 100 K = 1, L                  ! iterate down column above diagonal
            M = UTL_GETPOS(A,K,J)
            IF (M>0) A%DVAL(M) = UTL_GETVAL(A,K,J) - G*UTL_GETVAL(A,K,I)
100       CONTINUE
110     CONTINUE
      ENDIF
      M = UTL_GETPOS(A,I,I)
      D(I) = A%DVAL(M)
      A%DVAL(M) = 1.0D0
      IF (L.GE.1) THEN
        DO 120 J = 1, L      ! iterate across row left of diag or down column above diag
          M = UTL_GETPOS(A,I,J)
          IF (M>0) A%DVAL(M) = 0.0D0
          M = UTL_GETPOS(A,J,I)
          IF (M>0) A%DVAL(M) = 0.0D0
120     CONTINUE
      ENDIF
130 CONTINUE
    RETURN
  END SUBROUTINE UTL_SSVD1
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SSVD2(N,NP,D,E,Z)
    !     ******************************************************************
    !     COPYRIGHT (C) 1986 NUMERICAL RECIPES SOFTWARE, TQLIS,
    !     REPRODUCED BY PERMISSION FROM THE BOOK NUMERICAL RECIPES: THE ART
    !     OF SCIENTIFIC COMPUTING, PUBLISHED BY CAMBRIDGE UNIVERSITY PRESS
    !     MODIFIED FOR DOUBLE PRECISION
    !     ******************************************************************
    !   Modified 11/21/03 to use CDMATRIX structure for A
    !   8/29/2006 -- Write revised message and stop when ITER==30.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                         INTENT(IN)    :: N
    INTEGER,                         INTENT(IN)    :: NP
    DOUBLE PRECISION, DIMENSION(NP), INTENT(INOUT) :: D
    DOUBLE PRECISION, DIMENSION(NP), INTENT(INOUT) :: E
    TYPE (CDMATRIX),                 INTENT(INOUT) :: Z
    !
    !   Local variables
    DOUBLE PRECISION :: B, C, DD, F, G, P, R, S
    INTEGER :: I, ITER, K, L, M, MP
    !
    !   Format statements
    100 FORMAT(/,   &
    1X,'ERROR: Too many iterations in UTL_SSVD2 when calculating ',   &
    'eigenvalues.  Check',/,   &
    1X,'for parameter values outside of their physical limits.  Adjust ',   &
    'program control',/,   &
    1X,'parameters to avoid this condition.  For example, log ',   &
    'transform parameters',/,   &
    1X,'that cannot be negative.')
    !
    IF (N.GT.1) THEN
      DO 10 I = 2, N
        E(I-1) = E(I)
 10   CONTINUE
      E(N) = 0.0D0
      DO 70 L = 1, N
        ITER = 0
 20     CONTINUE
        DO 30 M = L, N-1
          DD = DABS(D(M)) + DABS(D(M+1))
          IF (ABS(E(M))+DD.EQ.DD) GOTO 40
 30     CONTINUE
        M = N
 40     CONTINUE
        IF (M.NE.L) THEN
          IF (ITER.EQ.30) THEN
            WRITE (*,100)
            CALL UTL_STOP('Error in UTL_SSVD2')
          ENDIF
          ITER = ITER + 1
          G = (D(L+1)-D(L))/(2.0D0*E(L))
          R = DSQRT(G**2+1.0D0)
          G = D(M) - D(L) + E(L)/(G+SIGN(R,G))
          S = 1.0D0
          C = 1.0D0
          P = 0.0D0
          DO 60 I = M-1, L, -1
            F = S*E(I)
            B = C*E(I)
            IF (DABS(F).GE.DABS(G)) THEN
              C = G/F
              R = DSQRT(C**2+1.0D0)
              E(I+1) = F*R
              S = 1.0D0/R
              C = C*S
            ELSE
              S = F/G
              R = DSQRT(S**2+1.0D0)
              E(I+1) = G*R
              C = 1.0D0/R
              S = S*C
            ENDIF
            G = D(I+1) - P
            R = (D(I)-G)*S + 2.0D0*C*B
            P = S*R
            D(I+1) = G + P
            G = C*R - B
            DO 50 K = 1, N
              F = UTL_GETVAL(Z,K,I+1)
              MP = UTL_GETPOS(Z,K,I+1)
              IF (MP>0) THEN
                Z%DVAL(MP) = S*UTL_GETVAL(Z,K,I) + C*F
              ENDIF
              MP = UTL_GETPOS(Z,K,I)
              IF (MP>0) THEN
                Z%DVAL(MP) = C*UTL_GETVAL(Z,K,I) - S*F
              ENDIF
 50         CONTINUE
 60       CONTINUE
          D(L) = D(L) - P
          E(L) = G
          E(M) = 0.0D0
          GOTO 20
        ENDIF
 70   CONTINUE
    ENDIF
    RETURN
  END SUBROUTINE UTL_SSVD2
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SSVD3(N,A,C,B)
    !     ******************************************************************
    !     CALCULATE THE INVERSE (B) OF A SYMMETRIC MATRIX WITH THE EIGEN-
    !     VECTORS STORED IN A AND THE EIGENVALUES IN C BY
    !     B = A * C-1 * A(TRANSPOSE)
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                        INTENT(IN)    :: N
    TYPE (CDMATRIX),                INTENT(IN)    :: A
    DOUBLE PRECISION, DIMENSION(N), INTENT(IN)    :: C
    TYPE (CDMATRIX),                INTENT(INOUT) :: B
    !
    !   Local variables
    DOUBLE PRECISION :: TMP
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: BROW
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: AROW
    INTEGER :: I, J, K, M, MP, NNZ
    !
    ALLOCATE(BROW(B%NC),AROW(A%NC))
    NNZ = A%NNZ
    DO MP=1,NNZ
      J = UTL_GETICOL(A,MP)
      B%DVAL(MP) = A%DVAL(MP)/C(J)
    ENDDO
    !
    DO I = 1, N
      CALL UTL_GETROW(A,I,AROW)
      DO J = I, N
        MP = UTL_GETPOS(B,I,J)
        IF (MP>0) THEN
          CALL UTL_GETROW(B,J,BROW)
          TMP = 0.0D0
          DO K = 1, N
            TMP = TMP + BROW(K)*AROW(K)
          ENDDO
          B%DVAL(MP) = TMP
        ENDIF
      ENDDO
    ENDDO
    !
    DO MP=1,NNZ
      I = UTL_GETIROW(B,MP)
      J = UTL_GETICOL(B,MP)
      IF (J>I) THEN
        M = UTL_GETPOS(B,J,I)
        B%DVAL(M) = B%DVAL(MP)
      ENDIF
    ENDDO
    !
    DEALLOCATE(BROW,AROW)
    RETURN
  END SUBROUTINE UTL_SSVD3
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_STOP(STOPMESS,IOUT)
    !  STOP PROGRAM, WITH OPTION TO PRINT MESSAGE BEFORE STOPPING
    USE GLOBAL_DATA
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: STOPMESS
    INTEGER,          OPTIONAL, INTENT(IN) :: IOUT
    !
    !   Format statements
    10 FORMAT(/,1X,A)
    !
    IF (AMESSAGE .NE. ' ') THEN
      IF (PRESENT(IOUT)) THEN
        CALL UTL_WRITE_MESSAGE(IUNIT=IOUT)
      ELSE
        CALL UTL_WRITE_MESSAGE()
      ENDIF
    ENDIF
    !
    IF (PRESENT(STOPMESS)) THEN
      IF (STOPMESS .NE. ' ') THEN
        IF (PRESENT(IOUT)) THEN
          WRITE(IOUT,10) STOPMESS
        ELSE
          WRITE(*,10) STOPMESS
        ENDIF
      ENDIF
    ENDIF
    STOP
  END SUBROUTINE UTL_STOP
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_STRING2ARR(IDIM,STRING,CHARARR)
    !   Store a character string in an array of characters.  If the non-blank
    !   length of STRING exceeds IDIM, only the first IDIM characters are
    !   stored in CHARARR
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN)  :: IDIM  ! Dimension of CHARARR
    CHARACTER(LEN=*),                  INTENT(IN)  :: STRING
    CHARACTER(LEN=1), DIMENSION(IDIM), INTENT(OUT) :: CHARARR
    !
    !   Local variables
    INTEGER :: I, LENSTR, N
    !
    CHARARR = ' '
    LENSTR = LEN_TRIM(STRING)
    N = MIN(LENSTR,IDIM)
    DO I=1,N
      CHARARR(I) = STRING(I:I)
    ENDDO
    RETURN
  END SUBROUTINE UTL_STRING2ARR
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_STUD_T(IDOF,TST)
    !     DETERMINE THE VALUE OF THE T STATISTIC NEEDED TO CALCULATE LINEAR
    !     INDIVIDUAL CONFIDENCE INTERVALS FOR A TWO-SIDED SIGNIFICANCE LEVEL
    !     OF 0.05
    !
    !   Argument-list variables
    INTEGER,          INTENT(IN)  :: IDOF
    DOUBLE PRECISION, INTENT(OUT) :: TST
    !
    !   Local variables
    DOUBLE PRECISION :: TABLE
    INTEGER :: I, ITABLE
    !
    !     ------------------------------------------------------------------
    DIMENSION ITABLE(35), TABLE(35)
    DATA (ITABLE(I),I=1,35)/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,    &
          13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, &
          28, 29, 30, 40, 60, 120, 240, 500/
    DATA (TABLE(I),I=1,35)/12.706D0, 4.303D0, 3.182D0, 2.776D0, 2.571D0,   &
          2.447D0, 2.365D0, 2.306D0, 2.262D0, 2.228D0, 2.201D0, 2.179D0,   &
          2.160D0, 2.145D0, 2.131D0, 2.120D0, 2.110D0, 2.101D0, 2.093D0,   &
          2.086D0, 2.080D0, 2.074D0, 2.069D0, 2.064D0, 2.060D0, 2.056D0,   &
          2.052D0, 2.048D0, 2.045D0, 2.042D0, 2.021D0, 2.000D0, 1.980D0,   &
          1.970D0, 1.960D0/
    !     ------------------------------------------------------------------
    !
    IF (IDOF .LE. 30) THEN
      TST=TABLE(IDOF)
      RETURN
    ENDIF
    DO I=31,35
      IF(IDOF .LE. ITABLE(I)) THEN
        TST = TABLE(I-1)+(TABLE(I)-TABLE(I-1))*                       &
             DBLE(IDOF-ITABLE(I-1))/DBLE(ITABLE(I)-ITABLE(I-1))
        RETURN
      ENDIF
    ENDDO
    TST=TABLE(35)
    RETURN
  END SUBROUTINE UTL_STUD_T
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SUBERROR(subname)

    ! -- Subroutine UTL_SUBERROR names the subroutine causing a run-time error.

    ! -- Arguments are as follows:-
    !       subname:  name of offending subroutine

    ! -- Revision history:-
    !       June-November, 1995: version 1.

    IMPLICIT NONE
    CHARACTER (LEN=*)               ::SUBNAME

    WRITE(*,10) TRIM(SUBNAME)
10      FORMAT(/,' *** PROGRAMMING ERROR CALLING SUBROUTINE ',a,' ***')
    CALL UTL_STOP()

  END SUBROUTINE UTL_SUBERROR
  !-----------------------------------------------------------------------------
  FUNCTION UTL_SUBSTITUTE(NPE,NPT,IPTR,PARVALSET,PVAL) RESULT (PVTMP)
    !   Populate an array of dimension NPT with parameter values
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                          INTENT(IN) :: NPE
    INTEGER,                          INTENT(IN) :: NPT
    INTEGER, DIMENSION(NPE),          INTENT(IN) :: IPTR
    DOUBLE PRECISION, DIMENSION(NPE), INTENT(IN) :: PARVALSET
    DOUBLE PRECISION, DIMENSION(NPT), INTENT(IN) :: PVAL
    DOUBLE PRECISION, DIMENSION(NPT)             :: PVTMP
    !
    !   Local variables
    INTEGER :: I
    !
    !   Assign PVTMP with current parameter values
    PVTMP = PVAL
    !
    !   Substitute values of adjustable parameters
    IF (NPE>0) THEN
      DO I=1,NPE
        PVTMP(IPTR(I)) = PARVALSET(I)
      ENDDO
    ENDIF
    RETURN
    !
  END FUNCTION UTL_SUBSTITUTE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SUBSTITUTE_SUB(NPE,NPT,IPTR,PARVALSET,PVAL,PVTMP)
    !   Populate an array of dimension NPT with parameter values
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                          INTENT(IN)  :: NPE
    INTEGER,                          INTENT(IN)  :: NPT
    INTEGER, DIMENSION(NPE),          INTENT(IN)  :: IPTR
    DOUBLE PRECISION, DIMENSION(NPE), INTENT(IN)  :: PARVALSET
    DOUBLE PRECISION, DIMENSION(NPT), INTENT(IN)  :: PVAL
    DOUBLE PRECISION, DIMENSION(NPT), INTENT(OUT) :: PVTMP
    !
    !   Local variables
    INTEGER :: I
    !
    !   Assign PVTMP with current parameter values
    PVTMP = PVAL
    !
    !   Substitute values of adjustable parameters
    IF (NPE>0) THEN
      DO I=1,NPE
        PVTMP(IPTR(I)) = PARVALSET(I)
      ENDDO
    ENDIF
    RETURN
    !
  END SUBROUTINE UTL_SUBSTITUTE_SUB
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SVD(IFAIL,A,AS,DTLA)
    !-----VERSION 20000615 MCH
    !     CALCULATE THE INVERSE AND THE SQUARE-ROOT OF THE INVERSE OF A
    !     SQUARE MATRIX USING SVD (I.E. DECOMPOSE THE MATRIX INTO A MATRIX
    !     OF EIGENVECTORS AND A DIAGONAL MATRIX OF CORRESPONDING
    !     EIGENVALUES).
    !   Modified 11/20/2003 to use CDMATRIX structures and take advantage of
    !   automatic-array feature of Fortran 90 - ERB
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(INOUT) :: IFAIL
    TYPE (CDMATRIX),  INTENT(INOUT) :: A     ! Input matrix on invocation; inverse on return
    TYPE (CDMATRIX),  INTENT(INOUT) :: AS    ! Square-root of inverse
    DOUBLE PRECISION, INTENT(OUT)   :: DTLA  ! Log-determinant of the matrix
    !
    !   Local variables
    INTEGER :: I
    INTEGER :: NRC
    TYPE (CDMATRIX) :: BUFF1
    TYPE (CDMATRIX) :: AI
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: BUFF3, E
    CHARACTER(LEN=12) :: ANAME, ASNAME
    !
    ! Format statements
    100 FORMAT(/,1X,'Programmer error: "A" matrix not square in UTL_SVD')
    !
    CALL TYP_NULL(BUFF1)
    CALL TYP_NULL(AI)
    IFAIL = 0
    ANAME = A%ARRAYNAME
    ASNAME = AS%ARRAYNAME
    NRC = A%NR
    !
    !   Check for errors
    IF (NRC .NE. A%NC) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    !
    !   Construct a new CDMATRIX structure like A, but with space allocated for
    !   all elements required in the inverted matrix
    CALL UTL_PREPINVERSE(A,AI,ANAME)
    !
    !   Initialize CDMATRIX structures BUFF1 and AS
    BUFF1 = AI   ! Invokes UTL_ASSIGN, which overloads "=" operator
    AS = AI      ! Invokes UTL_ASSIGN, which overloads "=" operator
    AS%ARRAYNAME = ASNAME
    AS%DVAL = 0.0D0
    !
    !   E is used as temporary storage of the off-diagonal elements of
    !   the returned (by SSVD1) tridiagonal matrix used by SSVD2
    ALLOCATE (BUFF3(NRC),E(NRC))
    CALL UTL_SSVD1(BUFF1,NRC,NRC,BUFF3,E)
    CALL UTL_SSVD2(NRC,NRC,BUFF3,E,BUFF1)
    !   Calculate inverse matrix
    CALL UTL_SSVD3(NRC,BUFF1,BUFF3,AI)
    !
    !   Calculate log-determinant of the matrix
    DTLA = 0.
    DO I = 1, NRC
      IF (BUFF3(I) <= 0.D0) THEN
        IFAIL = 1
        RETURN
      ENDIF
      DTLA = DTLA + LOG(BUFF3(I))
    ENDDO
    !
    !  Calculate square-root of the inverse matrix
    DO I = 1, NRC
      ! Note that following check was done in preceding DO loop - ERB 12/8/2006
      !IF (BUFF3(I) <= 0.D0) THEN
      !  IFAIL = 1
      !  RETURN
      !ENDIF
      BUFF3(I) = SQRT(BUFF3(I))
    ENDDO
    CALL UTL_SSVD3(NRC,BUFF1,BUFF3,AS)
    !
    !   Assign inverted matrix to argument-list variable
    A = AI   ! Invokes UTL_ASSIGN
    !
    !   Deallocate local CDMATRIX structures and arrays
    CALL TYP_DEALLOC(BUFF1)
    CALL TYP_DEALLOC(AI)
    DEALLOCATE(BUFF3)
    DEALLOCATE(E)
    !
    RETURN
  END SUBROUTINE UTL_SVD
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SYSTEM(COMMAND)
    !   Make a system call.  Edit this subroutine to call compiler-dependent
    !   extension to standard Fortran 90/95
    IMPLICIT NONE
    !
    !   Argument-list variable
    CHARACTER(LEN=*), INTENT(IN) :: COMMAND
    !
    CALL SYSTEM(COMMAND)
    RETURN
  END SUBROUTINE UTL_SYSTEM
  !-----------------------------------------------------------------------------
  LOGICAL FUNCTION UTL_SYSTEM_FUNC(COMMAND,ERRORMSG)
    !   Make a system call.  Edit this subroutine to call compiler-dependent
    !   extension to standard Fortran 90/95
    !
    ! The following statement (USE IFPORT) is needed when the Intel Visual
    ! Fortran compiler is used.  For compilers that do not support IFPORT,
    ! it may be possible to compile this function by commenting-out the
    ! USE IFPORT statement.
    USE IFPORT
    !
    IMPLICIT NONE
    INTEGER(4) :: I, ERRNUM
    !
    !   Argument-list variable
    CHARACTER(LEN=*), INTENT(IN)  :: COMMAND
    CHARACTER(LEN=*), INTENT(OUT) :: ERRORMSG
    !
    UTL_SYSTEM_FUNC = .FALSE.
    ERRORMSG = ''
    I = SYSTEM(COMMAND)
    IF (I == -1) THEN
      ERRORMSG = 'Error running command: "' // TRIM(COMMAND) // '"'
    ELSE
      UTL_SYSTEM_FUNC = .TRUE.
    ENDIF
    RETURN
  END FUNCTION UTL_SYSTEM_FUNC
  !-----------------------------------------------------------------------------
  FUNCTION UTL_TABLESECTIONS(NCOLUMNS,NCSECTION) RESULT (NSECTS)
    !   Return (integer) number of table sections required for printing
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER, INTENT(IN) :: NCOLUMNS   ! Number of columns in entire table
    INTEGER, INTENT(IN) :: NCSECTION  ! Number of columns per table section
    INTEGER :: NSECTS                 ! Number of table sections required
    !
    !   Local variables
    DOUBLE PRECISION :: A
    !
    A = (NCOLUMNS-0.1)/DBLE(NCSECTION)
    NSECTS = INT(A) + 1
    RETURN
  END FUNCTION UTL_TABLESECTIONS
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_TABREP(LINE)
    ! -- SUBROUTINE UTL_TABREP REPLACES A TAB BY BLANK SPACE(S)
    IMPLICIT NONE
    !
    !   Argument-list variable
    CHARACTER (LEN=*), INTENT(INOUT) :: LINE
    !
    !   Local variables
    INTEGER LLEN,I,J,K,NBLC

    LLEN=LEN(LINE)
    NBLC=LEN_TRIM(LINE)
    IF(NBLC.EQ.0) RETURN

    I=0
    30 I=I+1
    IF(I.GT.NBLC)RETURN
    IF(ICHAR(LINE(I:I)).NE.9) GO TO 30
    J=((I-1)/8+1)*8-I
    IF(J.EQ.0) THEN
      LINE(I:I)=' '
    ELSE
      LINE(I:I)=' '
      NBLC=NBLC+J
      IF(NBLC.GT.LLEN) NBLC=LLEN
      DO 50 K=NBLC,((I-1)/8+1)*8,-1
        LINE(K:K)=LINE(K-J:K-J)
      50 CONTINUE
      DO 60 K=I+1,MIN(NBLC,I+J)
        LINE(K:K)=' '
      60 CONTINUE
      I=I+J
    END IF
    GO TO 30
    !
  END SUBROUTINE UTL_TABREP
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_VEC2CDMATRIX_R(IVEC,VEC,CDM,IDIMOPT,ANAME)
    !   Convert a REAL vector to a double-precision matrix of type CDMATRIX.
    !   Nonzero entries in VEC are stored in diagonal positions in CDM.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                    INTENT(IN)    :: IVEC
    REAL, DIMENSION(IVEC),      INTENT(IN)    :: VEC
    TYPE (CDMATRIX),            INTENT(INOUT) :: CDM
    INTEGER, OPTIONAL,          INTENT(IN)    :: IDIMOPT
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)    :: ANAME
    !
    !   Local variables
    INTEGER :: K, NNZ
    INTEGER(KIND=8) :: I, IDIM
    REAL :: ZERO = 0.0
    DOUBLE PRECISION :: DZERO = 0.0D0
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: IVEC<1 in UTL_VEC2CDMATRIX_R')
    !
    IF (IVEC<1) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    IF (PRESENT(ANAME)) THEN
      CDM%ARRAYNAME = ANAME
    ELSE
      CDM%ARRAYNAME = ' '
    ENDIF
    !
    !   If arrays in CDM are allocated, deallocate them
    IF (ASSOCIATED(CDM%DVAL)) DEALLOCATE(CDM%DVAL)
    IF (ASSOCIATED(CDM%IPOS)) DEALLOCATE(CDM%IPOS)
    IF (ASSOCIATED(CDM%ICOL)) DEALLOCATE(CDM%ICOL)
    !
    !   Determine number of nonzero entries in VEC
    NNZ = 0
    DO I =1,IVEC
      IF (VEC(I).NE.ZERO) NNZ = NNZ+1
    ENDDO
    IDIM = NNZ
    IF (PRESENT(IDIMOPT) .AND. IDIMOPT>NNZ) IDIM = IDIMOPT
    IF (IDIM<1) IDIM = 1
    !
    !   Construct CDM structure:
    !   Assign dimensions of CDM component arrays
    CDM%NR = IVEC
    CDM%NC = IVEC
    CDM%NNZ = NNZ
    CDM%IDIM = IDIM
    ALLOCATE(CDM%DVAL(IDIM),CDM%IPOS(IDIM),CDM%ICOL(IVEC))
    IF (NNZ==0) THEN
      !   Special case: VEC contains no nonzero entries.  Allocate arrays
      !   with one element each and initialize arrays appropriately
      CDM%DVAL(1) = DZERO
      CDM%IPOS(1) = 1
      CDM%ICOL = 0
    ELSE
      CDM%ICOL = 0
      K = 0
      !   Populate DVAL with values from VEC; assign IPOS and ICOL accordingly
      EACHELEM: DO I=1,IVEC
        IF (VEC(I).NE.ZERO) THEN
          K = K+1
          CDM%DVAL(K) = DBLE(VEC(I))
          CDM%IPOS(K) = (I-1)*IDIM+I
          CDM%ICOL(I) = K
          IF (K==NNZ) EXIT EACHELEM
        ENDIF
      ENDDO EACHELEM
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_VEC2CDMATRIX_R
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_VEC2CDMATRIX_D(IVEC,VEC,CDM,IDIMOPT,ANAME)
    !   Convert a DOUBLE PRECISION vector to a double-precision matrix of
    !   type CDMATRIX.  Nonzero entries in VEC are stored in diagonal
    !   positions in CDM.
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list and result variables
    INTEGER,                           INTENT(IN)    :: IVEC
    DOUBLE PRECISION, DIMENSION(IVEC), INTENT(IN)    :: VEC
    TYPE (CDMATRIX),                   INTENT(INOUT) :: CDM
    INTEGER, OPTIONAL,                 INTENT(IN)    :: IDIMOPT
    CHARACTER(LEN=*), OPTIONAL,        INTENT(IN)    :: ANAME
    !
    !   Local variables
    INTEGER :: K, NNZ
    INTEGER(KIND=8) :: I, IDIM
    DOUBLE PRECISION :: DZERO = 0.0D0
    !
    !   Format statements
    100 FORMAT(1X,'ERROR: IVEC<1 in UTL_VEC2CDMATRIX_D')
    !
    IF (IVEC<1) THEN
      WRITE(*,100)
      CALL UTL_STOP(' ')
    ENDIF
    IF (PRESENT(ANAME)) THEN
      CDM%ARRAYNAME = ANAME
    ELSE
      CDM%ARRAYNAME = ' '
    ENDIF
    !
    !   If arrays in CDM are allocated, deallocate them
    IF (ASSOCIATED(CDM%DVAL)) DEALLOCATE(CDM%DVAL)
    IF (ASSOCIATED(CDM%IPOS)) DEALLOCATE(CDM%IPOS)
    IF (ASSOCIATED(CDM%ICOL)) DEALLOCATE(CDM%ICOL)
    !
    !   Determine number of nonzero entries in VEC
    NNZ = 0
    DO I =1,IVEC
      IF (VEC(I).NE.DZERO) NNZ = NNZ+1
    ENDDO
    IDIM = NNZ
    IF (PRESENT(IDIMOPT) .AND. IDIMOPT>NNZ) IDIM = IDIMOPT
    IF (IDIM<1) IDIM = 1
    !
    !   Construct CDM structure:
    !   Assign dimensions of CDM component arrays
    CDM%NR = IVEC
    CDM%NC = IVEC
    CDM%NNZ = NNZ
    CDM%IDIM = IDIM
    ALLOCATE(CDM%DVAL(IDIM),CDM%IPOS(IDIM),CDM%ICOL(IVEC))
    IF (NNZ==0) THEN
      !   Special case: VEC contains no nonzero entries.  Allocate arrays
      !   with one element each and initialize arrays appropriately
      CDM%DVAL(1) = DZERO
      CDM%IPOS(1) = 1
      CDM%ICOL = 0
    ELSE
      CDM%ICOL = 0
      K = 0
      !   Populate DVAL with values from VEC; assign IPOS and ICOL accordingly
      EACHELEM: DO I=1,IVEC
        IF (VEC(I).NE.DZERO) THEN
          K = K+1
          CDM%DVAL(K) = VEC(I)
          CDM%IPOS(K) = (I-1)*IDIM+I
          CDM%ICOL(I) = K
          IF (K==NNZ) EXIT EACHELEM
        ENDIF
      ENDDO EACHELEM
    ENDIF
    !
    RETURN
  END SUBROUTINE UTL_VEC2CDMATRIX_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_VERSION_CHECK(MINVERSION,ISTAT)
    !   Determine if the current version of the API (VERSIONID of the
    !   Global Data Module) is as new or newer than the version required
    !   by the calling program unit
    USE GLOBAL_DATA, ONLY: AMESSAGE, VERSIONID
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),  INTENT(IN)  :: MINVERSION
    INTEGER, OPTIONAL, INTENT(OUT) :: ISTAT
    !
    !   Local variables
    INTEGER, DIMENSION(3) :: IVERAPI, IVERMIN
    INTEGER :: I, ISTT
    !
    ISTT = 0
    CALL UTL_VERSION_PARSE(VERSIONID,IVERAPI)
    CALL UTL_VERSION_PARSE(MINVERSION,IVERMIN)
    !
    DO I=1,3
      IF (IVERMIN(I)>IVERAPI(I)) THEN
        ISTT = -1
        EXIT
      ELSEIF (IVERMIN(I)<IVERAPI(I)) THEN
        EXIT
      ENDIF
    ENDDO
    !
    IF (PRESENT(ISTAT)) THEN
      ISTAT = ISTT
    ELSE
      IF (ISTT==-1) THEN
        AMESSAGE = ' Programming error:  Version '//TRIM(MINVERSION)//   &
                   ' of JUPITER API is required.  Version in use is '   &
                   //TRIM(VERSIONID)//' -- Please download newer version '   &
                   //'of JUPITER API from U.S. Geological Survey'//   &
                   ' JUPITER web site.'
        CALL UTL_STOP()
      ENDIF
    ENDIF
    RETURN
  END SUBROUTINE UTL_VERSION_CHECK
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_VERSION_PARSE(VID,INTS)
    !   Parse a version string to get three integers
    IMPLICIT NONE
    !
    !   Argument-list variables
    CHARACTER(LEN=*),      INTENT(IN)  :: VID
    INTEGER, DIMENSION(3), INTENT(OUT) :: INTS
    !
    !   Local variables
    CHARACTER(LEN=1) :: DOT = '.'
    INTEGER :: I, L1, L2, LENV
    !
    LENV = LEN_TRIM(VID)
    L1 = 0
    I = 1
    !
    !   Locate delimiting dots
    PARSE: DO WHILE (I<LENV)
      I = I+1
      IF (L1==0) THEN
        IF (VID(I:I)==DOT) THEN
          L1 = I
          CYCLE PARSE
        ENDIF
      ELSE
        IF (VID(I:I)==DOT) THEN
          L2 = I
          EXIT PARSE
        ENDIF
      ENDIF
    ENDDO PARSE
    !
    !   Read three integers
    IF (L1>1 .AND. L2>(L1+1) .AND. LENV>L2) THEN
      READ(VID(1:L1-1),'(I10)',ERR=300) INTS(1)
      READ(VID(L1+1:L2-1),'(I10)',ERR=300) INTS(2)
      READ(VID(L2+1:LENV),'(I10)',ERR=300) INTS(3)
      RETURN
    ENDIF
    !
    300 CONTINUE
    CALL UTL_STOP(' Programmer error: Invalid VID in UTL_VERSION_PARSE')
  END SUBROUTINE UTL_VERSION_PARSE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WHICH1(IFAIL,NSTR,ISTR,ASTR,TSTR)

    ! -- SUBROUTINE UTL_WHICH1 FINDS A STRING IN AN ARRAY OF STRINGS
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    implicit none

    integer, intent(out)                :: ifail   ! error indicator
    integer, intent(in)                 :: NSTR    ! number of parameters
    integer, intent(inout)              :: ISTR    ! where to start the search
    character (len=*), intent(in), dimension(NSTR)  :: ASTR    ! array of strings
    character (len=*), intent(inout)    :: TSTR    ! string to look for

    integer                             :: i
    CHARACTER (LEN=MAX_STRING_LEN)      :: STRING

    IFAIL=0
    IF((ISTR.LT.1).OR.(ISTR.GT.NSTR)) ISTR=1
    call UTL_casetrans(TSTR,'hi')
    CALL UTL_CASE(ASTR(ISTR),STRING,1)
    IF(TSTR.EQ.STRING) RETURN
    IF(ISTR.NE.NSTR)THEN
      DO 20 I=ISTR+1,NSTR
        CALL UTL_CASE(ASTR(I),STRING,1)
        IF(TSTR.EQ.STRING)THEN
          ISTR=I
          RETURN
        END IF
20    CONTINUE
    END IF
    IF(ISTR.NE.1)THEN
      DO 40 I=ISTR-1,1,-1
        CALL UTL_CASE(ASTR(I),STRING,1)
        IF(TSTR.EQ.STRING) THEN
          ISTR=I
          RETURN
        END IF
40    CONTINUE
    END IF
    IFAIL=1
    RETURN
  END SUBROUTINE UTL_WHICH1
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITE2D_D(IOUT,IPRN,NR,NC,BUF,TEXT)
    !   Write a 2-D DOUBLE PRECISION array
    USE GLOBAL_DATA, ONLY: HYPHENS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                            INTENT(IN) :: IOUT
    INTEGER,                            INTENT(IN) :: IPRN
    INTEGER,                            INTENT(IN) :: NR
    INTEGER,                            INTENT(IN) :: NC
    DOUBLE PRECISION, DIMENSION(NR,NC), INTENT(IN) :: BUF
    CHARACTER(LEN=*),                   INTENT(IN) :: TEXT
    !
    !   Local variables
    INTEGER :: I, IP, J, LENTEXT
    !
    !   Format statements
    1    FORMAT(/2X,A,/,2X,A)
    !
    !C1------PRINT A HEADER
    LENTEXT = LEN_TRIM(TEXT)
    IF (LENTEXT>100) LENTEXT = 100
    WRITE(IOUT,1) TEXT,HYPHENS(1:LENTEXT)
    !
    !C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
    !C2------BETWEEN 1 AND 21.
    IP=IPRN
    IF(IP.LT.1 .OR. IP.GT.21) IP=12
    !
    !C3------CALL THE UTILITY MODULE UTL_COLNO TO PRINT COLUMN NUMBERS.
    IF(IP.EQ.1) CALL UTL_COLNO(1,NC,0,11,10,IOUT,4)
    IF(IP.EQ.2) CALL UTL_COLNO(1,NC,0,9,13,IOUT,4)
    IF(IP.GE.3 .AND. IP.LE.6) CALL UTL_COLNO(1,NC,3,15,7,IOUT,1)
    IF(IP.GE.7 .AND. IP.LE.11) CALL UTL_COLNO(1,NC,3,20,5,IOUT,1)
    IF(IP.EQ.12) CALL UTL_COLNO(1,NC,0,10,11,IOUT,4)
    IF(IP.GE.13 .AND. IP.LE.18) CALL UTL_COLNO(1,NC,3,10,6,IOUT,1)
    IF(IP.EQ.19) CALL UTL_COLNO(1,NC,0,5,12,IOUT,4)
    IF(IP.EQ.20) CALL UTL_COLNO(1,NC,0,6,11,IOUT,4)
    IF(IP.EQ.21) CALL UTL_COLNO(1,NC,0,7,9,IOUT,4)
!C
!C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
    DO I=1,NR
      SELECT CASE(IP)
        CASE(1)
          !   FORMAT 11G10.3
          WRITE(IOUT,11) I,(BUF(I,J),J=1,NC)
          11 FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
        CASE(2)
          !   FORMAT 9G13.6
          WRITE(IOUT,21) I,(BUF(I,J),J=1,NC)
          21 FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
        CASE(3)
          !   FORMAT 15F7.1
          WRITE(IOUT,31) I,(BUF(I,J),J=1,NC)
          31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
        CASE(4)
          !   FORMAT 15F7.2
          WRITE(IOUT,41) I,(BUF(I,J),J=1,NC)
          41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
        CASE(5)
          !   FORMAT 15F7.3
          WRITE(IOUT,51) I,(BUF(I,J),J=1,NC)
          51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
        CASE(6)
          !   FORMAT 15F7.4
          WRITE(IOUT,61) I,(BUF(I,J),J=1,NC)
          61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
        CASE(7)
          !   FORMAT 20F5.0
          WRITE(IOUT,71) I,(BUF(I,J),J=1,NC)
          71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
        CASE(8)
          !   FORMAT 20F5.1
          WRITE(IOUT,81) I,(BUF(I,J),J=1,NC)
          81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
        CASE(9)
          !   FORMAT 20F5.2
          WRITE(IOUT,91) I,(BUF(I,J),J=1,NC)
          91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
        CASE(10)
          !   FORMAT 20F5.3
          WRITE(IOUT,101) I,(BUF(I,J),J=1,NC)
          101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
        CASE(11)
          !   FORMAT 20F5.4
          WRITE(IOUT,111) I,(BUF(I,J),J=1,NC)
          111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
        CASE(12)
          !   FORMAT 10G11.4
          WRITE(IOUT,121) I,(BUF(I,J),J=1,NC)
          121 FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
        CASE(13)
          !   FORMAT 10F6.0
          WRITE(IOUT,131) I,(BUF(I,J),J=1,NC)
          131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
        CASE(14)
          !   FORMAT 10F6.1
          WRITE(IOUT,141) I,(BUF(I,J),J=1,NC)
          141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
        CASE(15)
          !   FORMAT 10F6.2
          WRITE(IOUT,151) I,(BUF(I,J),J=1,NC)
          151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
        CASE(16)
          !   FORMAT 10F6.3
          WRITE(IOUT,161) I,(BUF(I,J),J=1,NC)
          161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
        CASE(17)
          !   FORMAT 10F6.4
          WRITE(IOUT,171) I,(BUF(I,J),J=1,NC)
          171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
        CASE(18)
          !   FORMAT 10F6.5
          WRITE(IOUT,181) I,(BUF(I,J),J=1,NC)
          181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
        CASE(19)
          !  FORMAT 5G12.5
          WRITE(IOUT,191) I,(BUF(I,J),J=1,NC)
          191 FORMAT(1X,I3,2X,1PG12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))
        CASE(20)
          !  FORMAT 6G11.4
          WRITE(IOUT,201) I,(BUF(I,J),J=1,NC)
          201 FORMAT(1X,I3,2X,1PG11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))
        CASE(21)
          !  FORMAT 7G9.2
          WRITE(IOUT,211) I,(BUF(I,J),J=1,NC)
          211 FORMAT(1X,I3,2X,1PG9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))
      END SELECT
    ENDDO
    !C
    !C5------RETURN
    RETURN
  END SUBROUTINE UTL_WRITE2D_D
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITE2D_I(IOUT,IPRN,NR,NC,BUF,TEXT)
    !   Write a 2-D INTEGER array
    USE GLOBAL_DATA, ONLY: HYPHENS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                   INTENT(IN) :: IOUT
    INTEGER,                   INTENT(IN) :: IPRN
    INTEGER,                   INTENT(IN) :: NR
    INTEGER,                   INTENT(IN) :: NC
    INTEGER, DIMENSION(NR,NC), INTENT(IN) :: BUF
    CHARACTER(LEN=*),          INTENT(IN) :: TEXT
    !
    !   Local variables
    INTEGER :: I, IP, J, LENTEXT
    !
    !   Format statements
    1    FORMAT(/2X,A,/,2X,A)
    !
    !C1------PRINT A HEADER
    LENTEXT = LEN_TRIM(TEXT)
    IF (LENTEXT>100) LENTEXT = 100
    WRITE(IOUT,1) TEXT,HYPHENS(1:LENTEXT)
    !
    !C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
    !C2------BETWEEN 1 AND 21.
    IP=IPRN
    IF(IP<1 .OR. IP>9) IP=6
    !
    !C3------CALL THE UTILITY MODULE UTL_COLNO TO PRINT COLUMN NUMBERS.
    IF(IP==1) CALL UTL_COLNO(1,NC,3,60,1,IOUT,0)
    IF(IP==2) CALL UTL_COLNO(1,NC,3,40,2,IOUT,0)
    IF(IP==3) CALL UTL_COLNO(1,NC,3,30,3,IOUT,0)
    IF(IP==4) CALL UTL_COLNO(1,NC,3,25,4,IOUT,0)
    IF(IP==5) CALL UTL_COLNO(1,NC,3,20,5,IOUT,0)
    IF(IP==6) CALL UTL_COLNO(1,NC,3,10,11,IOUT,0)
    IF(IP==7) CALL UTL_COLNO(1,NC,3,25,2,IOUT,0)
    IF(IP==8) CALL UTL_COLNO(1,NC,3,15,4,IOUT,0)
    IF(IP==9) CALL UTL_COLNO(1,NC,3,10,6,IOUT,0)
    !
    !   LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
    DO I=1,NR
      SELECT CASE (IP)
        CASE(1)
          !   FORMAT 60I1
          WRITE(IOUT,11) I,(BUF(I,J),J=1,NC)
          11 FORMAT(1X,I3,1X,I1,59(1X,I1):/(5X,60(1X,I1)))
        CASE(2)
          !   FORMAT 40I2
          WRITE(IOUT,21) I,(BUF(I,J),J=1,NC)
          21 FORMAT(1X,I3,1X,I2,39(1X,I2):/(5X,40(1X,I2)))
        CASE(3)
          !   FORMAT 30I3
          WRITE(IOUT,31) I,(BUF(I,J),J=1,NC)
          31 FORMAT(1X,I3,1X,I3,29(1X,I3):/(5X,30(1X,I3)))
        CASE(4)
          !   FORMAT 25I4
          WRITE(IOUT,41) I,(BUF(I,J),J=1,NC)
          41 FORMAT(1X,I3,1X,I4,24(1X,I4):/(5X,25(1X,I4)))
        CASE(5)
          !   FORMAT 20I5
          WRITE(IOUT,51) I,(BUF(I,J),J=1,NC)
          51 FORMAT(1X,I3,1X,I5,19(1X,I5):/(5X,20(1X,I5)))
        CASE(6)
          !   FORMAT 10I11
          WRITE(IOUT,61) I,(BUF(I,J),J=1,NC)
          61 FORMAT(1X,I3,1X,I11,9(1X,I11):/(5X,10(1X,I11)))
        CASE(7)
          !   FORMAT 25I2
          WRITE(IOUT,71) I,(BUF(I,J),J=1,NC)
          71 FORMAT(1X,I3,1X,I2,24(1X,I2):/(5X,25(1X,I2)))
        CASE(8)
          !   FORMAT 15I4
          WRITE(IOUT,81) I,(BUF(I,J),J=1,NC)
          81 FORMAT(1X,I3,1X,I4,14(1X,I4):/(5X,15(1X,I4)))
        CASE(9)
          !   FORMAT 10I6
          WRITE(IOUT,91) I,(BUF(I,J),J=1,NC)
          91 FORMAT(1X,I3,1X,I6,9(1X,I6):/(5X,10(1X,I6)))
      END SELECT
    ENDDO
    !
    RETURN
  END SUBROUTINE UTL_WRITE2D_I
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITE2D_R(IOUT,IPRN,NR,NC,BUF,TEXT)
    !   Write a 2-D REAL array
    USE GLOBAL_DATA, ONLY: HYPHENS
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                INTENT(IN) :: IOUT
    INTEGER,                INTENT(IN) :: IPRN
    INTEGER,                INTENT(IN) :: NR
    INTEGER,                INTENT(IN) :: NC
    REAL, DIMENSION(NR,NC), INTENT(IN) :: BUF
    CHARACTER(LEN=*),       INTENT(IN) :: TEXT
    !
    !   Local variables
    INTEGER :: I, IP, J, LENTEXT
    !
    !   Format statements
    1    FORMAT(/2X,A,/,2X,A)
    !
    !C1------PRINT A HEADER
    LENTEXT = LEN_TRIM(TEXT)
    IF (LENTEXT>100) LENTEXT = 100
    WRITE(IOUT,1) TEXT,HYPHENS(1:LENTEXT)
    !
    !C2------MAKE SURE THE FORMAT CODE (IP OR IPRN) IS
    !C2------BETWEEN 1 AND 21.
    IP=IPRN
    IF(IP.LT.1 .OR. IP.GT.21) IP=12
    !
    !C3------CALL THE UTILITY MODULE UTL_COLNO TO PRINT COLUMN NUMBERS.
    IF(IP.EQ.1) CALL UTL_COLNO(1,NC,0,11,10,IOUT,4)
    IF(IP.EQ.2) CALL UTL_COLNO(1,NC,0,9,13,IOUT,4)
    IF(IP.GE.3 .AND. IP.LE.6) CALL UTL_COLNO(1,NC,3,15,7,IOUT,1)
    IF(IP.GE.7 .AND. IP.LE.11) CALL UTL_COLNO(1,NC,3,20,5,IOUT,1)
    IF(IP.EQ.12) CALL UTL_COLNO(1,NC,0,10,11,IOUT,4)
    IF(IP.GE.13 .AND. IP.LE.18) CALL UTL_COLNO(1,NC,3,10,6,IOUT,1)
    IF(IP.EQ.19) CALL UTL_COLNO(1,NC,0,5,12,IOUT,4)
    IF(IP.EQ.20) CALL UTL_COLNO(1,NC,0,6,11,IOUT,4)
    IF(IP.EQ.21) CALL UTL_COLNO(1,NC,0,7,9,IOUT,4)
!C
!C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
    DO I=1,NR
      SELECT CASE(IP)
        CASE(1)
          !   FORMAT 11G10.3
          WRITE(IOUT,11) I,(BUF(I,J),J=1,NC)
          11 FORMAT(1X,I3,2X,1PG10.3,10(1X,G10.3):/(5X,11(1X,G10.3)))
        CASE(2)
          !   FORMAT 9G13.6
          WRITE(IOUT,21) I,(BUF(I,J),J=1,NC)
          21 FORMAT(1X,I3,2X,1PG13.6,8(1X,G13.6):/(5X,9(1X,G13.6)))
        CASE(3)
          !   FORMAT 15F7.1
          WRITE(IOUT,31) I,(BUF(I,J),J=1,NC)
          31 FORMAT(1X,I3,1X,15(1X,F7.1):/(5X,15(1X,F7.1)))
        CASE(4)
          !   FORMAT 15F7.2
          WRITE(IOUT,41) I,(BUF(I,J),J=1,NC)
          41 FORMAT(1X,I3,1X,15(1X,F7.2):/(5X,15(1X,F7.2)))
        CASE(5)
          !   FORMAT 15F7.3
          WRITE(IOUT,51) I,(BUF(I,J),J=1,NC)
          51 FORMAT(1X,I3,1X,15(1X,F7.3):/(5X,15(1X,F7.3)))
        CASE(6)
          !   FORMAT 15F7.4
          WRITE(IOUT,61) I,(BUF(I,J),J=1,NC)
          61 FORMAT(1X,I3,1X,15(1X,F7.4):/(5X,15(1X,F7.4)))
        CASE(7)
          !   FORMAT 20F5.0
          WRITE(IOUT,71) I,(BUF(I,J),J=1,NC)
          71 FORMAT(1X,I3,1X,20(1X,F5.0):/(5X,20(1X,F5.0)))
        CASE(8)
          !   FORMAT 20F5.1
          WRITE(IOUT,81) I,(BUF(I,J),J=1,NC)
          81 FORMAT(1X,I3,1X,20(1X,F5.1):/(5X,20(1X,F5.1)))
        CASE(9)
          !   FORMAT 20F5.2
          WRITE(IOUT,91) I,(BUF(I,J),J=1,NC)
          91 FORMAT(1X,I3,1X,20(1X,F5.2):/(5X,20(1X,F5.2)))
        CASE(10)
          !   FORMAT 20F5.3
          WRITE(IOUT,101) I,(BUF(I,J),J=1,NC)
          101 FORMAT(1X,I3,1X,20(1X,F5.3):/(5X,20(1X,F5.3)))
        CASE(11)
          !   FORMAT 20F5.4
          WRITE(IOUT,111) I,(BUF(I,J),J=1,NC)
          111 FORMAT(1X,I3,1X,20(1X,F5.4):/(5X,20(1X,F5.4)))
        CASE(12)
          !   FORMAT 10G11.4
          WRITE(IOUT,121) I,(BUF(I,J),J=1,NC)
          121 FORMAT(1X,I3,2X,1PG11.4,9(1X,G11.4):/(5X,10(1X,G11.4)))
        CASE(13)
          !   FORMAT 10F6.0
          WRITE(IOUT,131) I,(BUF(I,J),J=1,NC)
          131 FORMAT(1X,I3,1X,10(1X,F6.0):/(5X,10(1X,F6.0)))
        CASE(14)
          !   FORMAT 10F6.1
          WRITE(IOUT,141) I,(BUF(I,J),J=1,NC)
          141 FORMAT(1X,I3,1X,10(1X,F6.1):/(5X,10(1X,F6.1)))
        CASE(15)
          !   FORMAT 10F6.2
          WRITE(IOUT,151) I,(BUF(I,J),J=1,NC)
          151 FORMAT(1X,I3,1X,10(1X,F6.2):/(5X,10(1X,F6.2)))
        CASE(16)
          !   FORMAT 10F6.3
          WRITE(IOUT,161) I,(BUF(I,J),J=1,NC)
          161 FORMAT(1X,I3,1X,10(1X,F6.3):/(5X,10(1X,F6.3)))
        CASE(17)
          !   FORMAT 10F6.4
          WRITE(IOUT,171) I,(BUF(I,J),J=1,NC)
          171 FORMAT(1X,I3,1X,10(1X,F6.4):/(5X,10(1X,F6.4)))
        CASE(18)
          !   FORMAT 10F6.5
          WRITE(IOUT,181) I,(BUF(I,J),J=1,NC)
          181 FORMAT(1X,I3,1X,10(1X,F6.5):/(5X,10(1X,F6.5)))
        CASE(19)
          !  FORMAT 5G12.5
          WRITE(IOUT,191) I,(BUF(I,J),J=1,NC)
          191 FORMAT(1X,I3,2X,1PG12.5,4(1X,G12.5):/(5X,5(1X,G12.5)))
        CASE(20)
          !  FORMAT 6G11.4
          WRITE(IOUT,201) I,(BUF(I,J),J=1,NC)
          201 FORMAT(1X,I3,2X,1PG11.4,5(1X,G11.4):/(5X,6(1X,G11.4)))
        CASE(21)
          !  FORMAT 7G9.2
          WRITE(IOUT,211) I,(BUF(I,J),J=1,NC)
          211 FORMAT(1X,I3,2X,1PG9.2,6(1X,G9.2):/(5X,7(1X,G9.2)))
      END SELECT
    ENDDO
    !C
    !C5------RETURN
    RETURN
  END SUBROUTINE UTL_WRITE2D_R
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITEBLOCK(HEAD,IOUT)
    !   Print table of keywords and values for all entries in nest of linked
    !   lists (generally populated from an input block).
    USE DATATYPES
    USE GLOBAL_DATA, ONLY: MAX_STRING_LEN
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (LLIST), POINTER    :: HEAD
    INTEGER,      INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I
    TYPE (LLIST), POINTER :: PTR
    TYPE (LNODE), POINTER :: PIPTR
    !
    !   Format statements
    300 FORMAT(/,1X,'Keyword',33X,'Value',8X,'Group = ',A,   &
               /,1X,38('-'),2X,38('-'))
    400 FORMAT(1X,A38,2X,2000(A1))
    500 FORMAT(1X,78('-'))
    !
    !   Write out the values
    NULLIFY(PTR,PIPTR)
    PTR => HEAD
    OUTPUT: DO
      IF (.NOT. ASSOCIATED(PTR)) EXIT OUTPUT  ! Pointer valid?
      WRITE(IOUT,300) PTR%GROUP
      PIPTR => PTR%LHEAD
      INFO: DO
        IF (.NOT. ASSOCIATED(PIPTR)) EXIT INFO
        WRITE(IOUT,400)PIPTR%KEYWORD,(PIPTR%STRING(I),I=1,PIPTR%NCHAR)
        PIPTR => PIPTR%NEXTNODE               ! Get pointer to next entry
      ENDDO INFO
      WRITE(IOUT,500)
      PTR => PTR%NEXTLIST                     ! Get pointer to next KEYITEM
    ENDDO OUTPUT
  END SUBROUTINE UTL_WRITEBLOCK
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITECDMATRIX(CDM,IFLAG,IOUT,INPUTFORMAT,COLNAM,ROWNAM)
    !   Write the contents of a CDMATRIX structure, either: (IFLAG<1) in
    !   compressed form; or (IFLAG>=1) in full-matrix form.  Note: this
    !   subroutine is intended as a debugging aid, not for generating
    !   final-application output
    USE DATATYPES
    IMPLICIT NONE
    !
    !   Argument-list variables
    TYPE (CDMATRIX),                               INTENT(IN) :: CDM
    INTEGER,                                       INTENT(IN) :: IFLAG
    INTEGER,                                       INTENT(IN) :: IOUT
    LOGICAL,                             OPTIONAL, INTENT(IN) :: INPUTFORMAT
    CHARACTER(LEN=*), DIMENSION(CDM%NC), OPTIONAL, INTENT(IN) :: COLNAM
    CHARACTER(LEN=*), DIMENSION(CDM%NR), OPTIONAL, INTENT(IN) :: ROWNAM
    !
    !   Local variables
    INTEGER :: I, IC, IR, J, K, NC, NNZ, NR
    INTEGER(KIND=8) :: IPOS, NR8
    INTEGER :: ROWNAMLEN
    !DOUBLE PRECISION, DIMENSION(CDM%NC) :: DAT
    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: DAT
    DOUBLE PRECISION :: DZERO = 0.0D0
    LOGICAL :: INPFMT
    !
    !   Format statements
    10 FORMAT(1X,'COMPRESSEDMATRIX',1X,A)
    11 FORMAT(1X,'COMPRESSEDMATRIX')
    20 FORMAT(3(1X,I12))
    30 FORMAT(1X,'COMPLETEMATRIX',1X,A)
    31 FORMAT(1X,'COMPLETEMATRIX')
    40 FORMAT(2(1X,I9))
    50 FORMAT(10000(1X,D24.16))
    80 FORMAT(/,1X,'CDMATRIX structure "',A,'" contains no nonzero entries')
    85 FORMAT(/,1X,'Contents of CDMATRIX structure:')
    90 FORMAT(/,1X,'Contents of CDMATRIX structure "',A,'":')
    100 FORMAT(   &
        1X,'IDIM = ',I12,/,   &
        1X,'NNZ = ',I12,/,   &
        1X,'NR = ',I9,/,   &
        1X,'NC = ',I9,//,   &
        4X,'IPOS',17X,'DVAL')
    110 FORMAT(1X,I12,2X,D24.16)
    120 FORMAT(/,3X,'ICOL')
    130 FORMAT(1X,I12)
    135 FORMAT(/,1X,'Full matrix generated from compressed structure:',/)
    140 FORMAT(/,1X,'Full matrix generated from compressed structure "',A,'":',/)
    164 FORMAT(14X,10000(6X,A12,7X))
    165 FORMAT(8X,10000(6X,A12,7X))
    169 FORMAT(14X,10000(10X,I5,10X))
    170 FORMAT(8X,10000(10X,I5,10X))
    175 FORMAT(1X,A12,1X,10000(1X,G24.16))
    176 FORMAT(1X,A20,1X,10000(1X,G24.16))
    180 FORMAT(1X,I6,1X,10000(1X,G24.16))
    !
    IF (IOUT<1) RETURN
    
    ! Find character length of ROWNAM argument.  Currently, the maximum
    ! supported length is 20.
    ROWNAMLEN = 0
    IF (PRESENT(ROWNAM)) THEN
      IF (CDM%NR >= 1) THEN
        ROWNAMLEN = LEN(ROWNAM(1))
      ENDIF
    ENDIF
    
    INPFMT = .FALSE.
    IF (PRESENT(INPUTFORMAT)) INPFMT = INPUTFORMAT
    NNZ = CDM%NNZ
    IF (NNZ<1 .AND. .NOT. INPFMT) THEN
      WRITE(IOUT,80)TRIM(CDM%ARRAYNAME)
      RETURN
    ENDIF
    !
    IF (IFLAG<1) THEN
      !   Write in compressed form, no column or row labels
      !   Write header or COMPRESSEDMATRIX line
      IF (INPFMT) THEN
        IF (CDM%ARRAYNAME==' ' .OR. CDM%ARRAYNAME=='') THEN
          WRITE(IOUT,11)
        ELSE
          WRITE(IOUT,10) TRIM(CDM%ARRAYNAME)
        ENDIF
      ELSE
        IF (CDM%ARRAYNAME==' ' .OR. CDM%ARRAYNAME=='') THEN
          WRITE(IOUT,85)
        ELSE
          WRITE(IOUT,90)TRIM(CDM%ARRAYNAME)
        ENDIF
      ENDIF
      !   Write contents of CDMATRIX structure
      IF (INPFMT) THEN
        WRITE(IOUT,20) CDM%NNZ,CDM%NR,CDM%NC
      ELSE
        WRITE(IOUT,100)CDM%IDIM,CDM%NNZ,CDM%NR,CDM%NC
      ENDIF
      WRITE(IOUT,110)(CDM%IPOS(I),CDM%DVAL(I),I=1,CDM%NNZ)
      IF (.NOT. INPFMT) THEN
        WRITE(IOUT,120)
        WRITE(IOUT,130)(CDM%ICOL(I),I=1,CDM%NC)
      ENDIF
    ELSE
      !   Write in full matrix form, optionally with column and row labels
      IF (INPFMT) THEN
        IF (CDM%ARRAYNAME==' ') THEN
          WRITE(IOUT,31)
        ELSE
          WRITE(IOUT,30) TRIM(CDM%ARRAYNAME)
        ENDIF
      ELSE
        IF (CDM%ARRAYNAME==' ') THEN
          WRITE(IOUT,135)
        ELSE
          WRITE(IOUT,140)TRIM(CDM%ARRAYNAME)
        ENDIF
      ENDIF
      NR = CDM%NR
      NC = CDM%NC
      IF (INPFMT) THEN
        WRITE(IOUT,40)NR,NC
      ELSE
        IF (PRESENT(COLNAM)) THEN
          !   Write column labels
          IF (PRESENT(ROWNAM)) THEN
            WRITE(IOUT,164)(COLNAM(I),I=1,NC)
          ELSE
            WRITE(IOUT,165)(COLNAM(I),I=1,NC)
          ENDIF
        ELSE
          !   Write column numbers
          IF (PRESENT(ROWNAM)) THEN
            WRITE(IOUT,169)(I,I=1,NC)
          ELSE
            WRITE(IOUT,170)(I,I=1,NC)
          ENDIF
        ENDIF
      ENDIF
      ALLOCATE(DAT(CDM%NC))
      NR8 = NR
      EACHROW: DO I=1,NR
        DAT = DZERO
        DO K=1,NNZ
          IPOS = CDM%IPOS(K)
          IR = MOD((IPOS-1),NR8)+1
          IF (IR==I) THEN
            IC = INT((DBLE(IPOS)-0.1)/DBLE(NR))+1
            DAT(IC) = CDM%DVAL(K)
          ENDIF
        ENDDO
        IF (INPFMT) THEN
          WRITE(IOUT,50) (DAT(J),J=1,NC)
        ELSE
          IF (PRESENT(ROWNAM) .AND. ROWNAMLEN <= 20) THEN
            !   Write row with row label
            IF (ROWNAMLEN <= 12) THEN
              WRITE(IOUT,175)ROWNAM(I),(DAT(J),J=1,NC)
            ELSE
              WRITE(IOUT,176)ROWNAM(I),(DAT(J),J=1,NC)
            ENDIF
          ELSE
            !   Write row with row number
            WRITE(IOUT,180)I,(DAT(J),J=1,NC)
          ENDIF
        ENDIF
      ENDDO EACHROW
      DEALLOCATE(DAT)
    ENDIF
    RETURN
  END SUBROUTINE UTL_WRITECDMATRIX
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITEMATRIX_DOUBLE(NROW,NCOL,BUF,ROWNAM,COLNAM,IPRC,IOUT)
    ! note that the order of ncol,nrow needs to be reversed; also IPRC,IOUT
    !   This routine needs to be wrung out - ERB 6/3/03.
    !C     ******************************************************************
    !C     PRINT ONE NROW*NCOL MATRIX WITH COLUMN AND ROW LABELS
    !C     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                                INTENT(IN) :: NROW
    INTEGER,                                INTENT(IN) :: NCOL
    DOUBLE PRECISION, DIMENSION(NROW,NCOL), INTENT(IN) :: BUF
    CHARACTER(LEN=*), DIMENSION(NROW),      INTENT(IN) :: ROWNAM
    CHARACTER(LEN=*), DIMENSION(NCOL),      INTENT(IN) :: COLNAM
    INTEGER,                                INTENT(IN) :: IPRC
    INTEGER,                                INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I, IP, J, NSP
    !
    !C2------MAKE SURE THE FORMAT CODE (IPRC) IS VALID
    IP=IPRC
    IF(IP.LT.1 .OR. IP.GT.10) IP=1
    NSP = LEN(ROWNAM)+1
    !
    !C3------LABEL COLUMNS WITH COLUMN NAMES.
    IF(IP.EQ.1) CALL UTL_COLLBL(NCOL,NSP,11,10,IOUT,COLNAM)
    IF(IP.EQ.2) CALL UTL_COLLBL(NCOL,NSP,10,11,IOUT,COLNAM)
    IF(IP.EQ.3) CALL UTL_COLLBL(NCOL,NSP,9,12,IOUT,COLNAM)
    IF(IP.EQ.4) CALL UTL_COLLBL(NCOL,NSP,8,13,IOUT,COLNAM)
    IF(IP.EQ.5) CALL UTL_COLLBL(NCOL,NSP,8,14,IOUT,COLNAM)
    IF(IP.EQ.6) CALL UTL_COLLBL(NCOL,NSP,6,10,IOUT,COLNAM)
    IF(IP.EQ.7) CALL UTL_COLLBL(NCOL,NSP,5,11,IOUT,COLNAM)
    IF(IP.EQ.8) CALL UTL_COLLBL(NCOL,NSP,5,12,IOUT,COLNAM)
    IF(IP.EQ.9) CALL UTL_COLLBL(NCOL,NSP,4,13,IOUT,COLNAM)
    IF(IP.EQ.10) CALL UTL_COLLBL(NCOL,NSP,4,14,IOUT,COLNAM)
    !
    !C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
    DO 1000 I=1,NROW
      IF (IP.EQ.1) THEN
        !C------------ FORMAT 11G10.3
        WRITE(IOUT,11) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        11 FORMAT(1X,A,1X,1PG10.3,10(1X,G10.3):/(11X,11(1X,G10.3)))
      ELSEIF (IP.EQ.2) THEN
        !C------------ FORMAT 10G11.4
        WRITE(IOUT,21) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        21 FORMAT(1X,A,1X,1PG11.4,9(1X,G11.4):/(11X,10(1X,G11.4)))
      ELSEIF (IP.EQ.3) THEN
        !C------------ FORMAT 9G12.5
          WRITE(IOUT,31) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        31 FORMAT(1X,A,1X,1PG12.5,8(1X,G12.5):/(11X,9(1X,G12.5)))
      ELSEIF (IP.EQ.4) THEN
        !C------------ FORMAT 8G13.6
        WRITE(IOUT,41) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        41 FORMAT(1X,A,1X,1PG13.6,7(1X,G13.6):/(11X,8(1X,G13.6)))
      ELSEIF (IP.EQ.5) THEN
        !C------------ FORMAT 8G14.7
        WRITE(IOUT,51) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        51 FORMAT(1X,A,1X,1PG14.7,7(1X,G14.7):/(11X,8(1X,G14.7)))
      ELSEIF (IP.EQ.6) THEN
        !C------------ FORMAT 6G10.3
        WRITE(IOUT,61) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        61 FORMAT(1X,A,1X,1PG10.3,5(1X,G10.3):/(11X,6(1X,G10.3)))
      ELSEIF (IP.EQ.7) THEN
        !C------------ FORMAT 5G11.4
        WRITE(IOUT,71) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        71 FORMAT(1X,A,1X,1PG11.4,4(1X,G11.4):/(11X,5(1X,G11.4)))
      ELSEIF (IP.EQ.8) THEN
        !C------------ FORMAT 5G12.5
        WRITE(IOUT,81) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        81 FORMAT(1X,A,1X,1PG12.5,4(1X,G12.5):/(11X,5(1X,G12.5)))
      ELSEIF (IP.EQ.9) THEN
        !C------------ FORMAT 4G13.6
        WRITE(IOUT,91) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        91 FORMAT(1X,A,1X,1PG13.6,3(1X,G13.6):/(11X,4(1X,G13.6)))
      ELSEIF (IP.EQ.10) THEN
        !C------------ FORMAT 4G14.7
        WRITE(IOUT,101) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        101 FORMAT(1X,A,1X,1PG14.7,3(1X,G14.7):/(11X,4(1X,G14.7)))
      ENDIF
    1000 CONTINUE
    !
    RETURN
  END SUBROUTINE UTL_WRITEMATRIX_DOUBLE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITEMATRIX_REAL(NROW,NCOL,BUF,ROWNAM,COLNAM,IPRC,IOUT)
    ! note that the order of ncol,nrow needs to be reversed; also IPRC,IOUT
    !   This routine needs to be wrung out - ERB 6/3/03.
    !C     ******************************************************************
    !C     PRINT ONE NROW*NCOL MATRIX WITH COLUMN AND ROW LABELS
    !C     ******************************************************************
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,                           INTENT(IN) :: NROW
    INTEGER,                           INTENT(IN) :: NCOL
    REAL, DIMENSION(NROW,NCOL),        INTENT(IN) :: BUF
    CHARACTER(LEN=*), DIMENSION(NROW), INTENT(IN) :: ROWNAM
    CHARACTER(LEN=*), DIMENSION(NCOL), INTENT(IN) :: COLNAM
    INTEGER,                           INTENT(IN) :: IPRC
    INTEGER,                           INTENT(IN) :: IOUT
    !
    !   Local variables
    INTEGER :: I, IP, J, NSP
    !
    !C2------MAKE SURE THE FORMAT CODE (IPRC) IS VALID
    IP=IPRC
    IF(IP.LT.1 .OR. IP.GT.10) IP=1
    !
    NSP = LEN(ROWNAM)+1
    !C3------LABEL COLUMNS WITH COLUMN NAMES.
    IF(IP.EQ.1) CALL UTL_COLLBL(NCOL,NSP,11,10,IOUT,COLNAM)
    IF(IP.EQ.2) CALL UTL_COLLBL(NCOL,NSP,10,11,IOUT,COLNAM)
    IF(IP.EQ.3) CALL UTL_COLLBL(NCOL,NSP,9,12,IOUT,COLNAM)
    IF(IP.EQ.4) CALL UTL_COLLBL(NCOL,NSP,8,13,IOUT,COLNAM)
    IF(IP.EQ.5) CALL UTL_COLLBL(NCOL,NSP,8,14,IOUT,COLNAM)
    IF(IP.EQ.6) CALL UTL_COLLBL(NCOL,NSP,6,10,IOUT,COLNAM)
    IF(IP.EQ.7) CALL UTL_COLLBL(NCOL,NSP,5,11,IOUT,COLNAM)
    IF(IP.EQ.8) CALL UTL_COLLBL(NCOL,NSP,5,12,IOUT,COLNAM)
    IF(IP.EQ.9) CALL UTL_COLLBL(NCOL,NSP,4,13,IOUT,COLNAM)
    IF(IP.EQ.10 ) CALL UTL_COLLBL(NCOL,NSP,4,14,IOUT,COLNAM)
    !
    !C4------LOOP THROUGH THE ROWS PRINTING EACH ONE IN ITS ENTIRETY.
    DO 1000 I=1,NROW
      IF (IP.EQ.1) THEN
        !C------------ FORMAT 11G10.3
        WRITE(IOUT,11) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        11 FORMAT(1X,A11,1X,1PG10.3,10(1X,G10.3):/(11X,11(1X,G10.3)))
      ELSEIF (IP.EQ.2) THEN
        !C------------ FORMAT 10G11.4
        WRITE(IOUT,21) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        21 FORMAT(1X,A12,1X,1PG11.4,9(1X,G11.4):/(11X,10(1X,G11.4)))
      ELSEIF (IP.EQ.3) THEN
        !C------------ FORMAT 9G12.5
          WRITE(IOUT,31) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        31 FORMAT(1X,A13,1X,1PG12.5,8(1X,G12.5):/(11X,9(1X,G12.5)))
      ELSEIF (IP.EQ.4) THEN
        !C------------ FORMAT 8G13.6
        WRITE(IOUT,41) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        41 FORMAT(1X,A14,1X,1PG13.6,7(1X,G13.6):/(11X,8(1X,G13.6)))
      ELSEIF (IP.EQ.5) THEN
        !C------------ FORMAT 8G14.7
        WRITE(IOUT,51) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        51 FORMAT(1X,A15,1X,1PG14.7,7(1X,G14.7):/(11X,8(1X,G14.7)))
      ELSEIF (IP.EQ.6) THEN
        !C------------ FORMAT 6G10.3
        WRITE(IOUT,61) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        61 FORMAT(1X,A11,1X,1PG10.3,5(1X,G10.3):/(11X,6(1X,G10.3)))
      ELSEIF (IP.EQ.7) THEN
        !C------------ FORMAT 5G11.4
        WRITE(IOUT,71) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        71 FORMAT(1X,A12,1X,1PG11.4,4(1X,G11.4):/(11X,5(1X,G11.4)))
      ELSEIF (IP.EQ.8) THEN
        !C------------ FORMAT 5G12.5
        WRITE(IOUT,81) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        81 FORMAT(1X,A13,1X,1PG12.5,4(1X,G12.5):/(11X,5(1X,G12.5)))
      ELSEIF (IP.EQ.9) THEN
        !C------------ FORMAT 4G13.6
        WRITE(IOUT,91) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        91 FORMAT(1X,A14,1X,1PG13.6,3(1X,G13.6):/(11X,4(1X,G13.6)))
      ELSEIF (IP.EQ.10) THEN
        !C------------ FORMAT 4G14.7
        WRITE(IOUT,101) ROWNAM(I),(BUF(I,J),J=1,NCOL)
        101 FORMAT(1X,A15,1X,1PG14.7,3(1X,G14.7):/(11X,4(1X,G14.7)))
      ENDIF
    1000 CONTINUE
    !
    RETURN
  END SUBROUTINE UTL_WRITEMATRIX_REAL
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRITE_MESSAGE(IUNIT,ERROR,LEADSPACE,ENDSPACE)
    ! -- Subroutine UTL_WRITE_MESSAGE formats and writes a message.
    !
    ! -- Arguments are as follows:-
    !       IUNIT        : the unit number to which the message is written
    !       ERROR        : if "yes" precede message with "Error"
    !       LEADSPACE    : if "yes" precede message with blank line
    !       ENDSPACE     : if "yes" follow message by blank line
    !
    USE GLOBAL_DATA, ONLY: AMESSAGE,ERRSUB
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER, INTENT(IN), OPTIONAL           ::IUNIT
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL ::ERROR
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL ::LEADSPACE
    CHARACTER (LEN=*), INTENT(IN), OPTIONAL ::ENDSPACE
    !
    !   Local variables
    INTEGER            ::JEND,I,NBLC,JUNIT,LEADBLANK
    INTEGER            ::ITAKE,J
    CHARACTER (LEN=20) :: ABLANK
    !
    IF (AMESSAGE==' ') RETURN
    IF(AMESSAGE(1:1).NE.' ')AMESSAGE=' '//TRIM(AMESSAGE)
    ABLANK=' '
    ITAKE=0
    J=0
    IF(PRESENT(IUNIT))THEN
      JUNIT=IUNIT
    ELSE
      JUNIT=0
    END IF
    IF(PRESENT(LEADSPACE))THEN
      IF(UTL_SAMENAME(LEADSPACE,'YES')) THEN
        IF (JUNIT>0) THEN
          WRITE(JUNIT,*)
        ELSE
          WRITE(*,*)
        ENDIF
      ENDIF
    ENDIF
    IF(PRESENT(ERROR))THEN
      IF(UTL_SAMENAME(ERROR,'YES'))THEN
        NBLC=LEN_TRIM(AMESSAGE)
        AMESSAGE=ADJUSTR(AMESSAGE(1:NBLC+8))
        IF(NBLC+8.LT.LEN(AMESSAGE)) AMESSAGE(NBLC+9:)=' '
        AMESSAGE(1:8)=' Error: '
      END IF
    END IF
    !
    DO I=1,20
      IF(AMESSAGE(I:I).NE.' ')EXIT
    END DO
    LEADBLANK=I-1
    NBLC=LEN_TRIM(AMESSAGE)
    !
  5 CONTINUE
    JEND=J+78-ITAKE
    IF(JEND.GE.NBLC) GO TO 100
    DO I=JEND,J+1,-1
      IF(AMESSAGE(I:I).EQ.' ') THEN
        IF(ITAKE.EQ.0) THEN
          IF (JUNIT>0) THEN
            WRITE(JUNIT,'(A)',ERR=200) AMESSAGE(J+1:I)
          ELSE
            WRITE(*,'(A)',ERR=200) AMESSAGE(J+1:I)
          ENDIF
          ITAKE=2+LEADBLANK
        ELSE
          IF (JUNIT>0) THEN
            WRITE(JUNIT,'(A)',ERR=200) ABLANK(1:LEADBLANK+2)//AMESSAGE(J+1:I)
          ELSE
            WRITE(*,'(A)',ERR=200) ABLANK(1:LEADBLANK+2)//AMESSAGE(J+1:I)
          ENDIF
        END IF
        J=I
        GO TO 5
      END IF
    END DO
    IF(ITAKE.EQ.0)THEN
      IF (JUNIT>0) THEN
        WRITE(JUNIT,'(A)',ERR=200) AMESSAGE(J+1:JEND)
      ELSE
        WRITE(*,'(A)',ERR=200) AMESSAGE(J+1:JEND)
      ENDIF
      ITAKE=2+LEADBLANK
    ELSE
      IF (JUNIT>0) THEN
        WRITE(JUNIT,'(A)',ERR=200) ABLANK(1:LEADBLANK+2)//AMESSAGE(J+1:JEND)
      ELSE
        WRITE(*,'(A)',ERR=200) ABLANK(1:LEADBLANK+2)//AMESSAGE(J+1:JEND)
      ENDIF
    END IF
    J=JEND
    GO TO 5
    !
100 CONTINUE
    JEND=NBLC
    IF(ITAKE.EQ.0)THEN
      IF (JUNIT>0) THEN
        WRITE(JUNIT,'(A)',ERR=200) AMESSAGE(J+1:JEND)
      ELSE
        WRITE(*,'(A)',ERR=200) AMESSAGE(J+1:JEND)
      ENDIF
    ELSE
      IF (JUNIT>0) THEN
        WRITE(JUNIT,'(A)',ERR=200) ABLANK(1:LEADBLANK+2)//AMESSAGE(J+1:JEND)
      ELSE
        WRITE(*,'(A)',ERR=200) ABLANK(1:LEADBLANK+2)//AMESSAGE(J+1:JEND)
      ENDIF
    END IF
    !
    IF(PRESENT(ENDSPACE))THEN
      IF(UTL_SAMENAME(ENDSPACE,'YES')) THEN
        IF (JUNIT>0) THEN
          WRITE(JUNIT,*)
        ELSE
          WRITE(*,*)
        ENDIF
      ENDIF
    END IF
    RETURN
    !
200 CONTINUE
    CALL UTL_STOP()
    !
  END SUBROUTINE UTL_WRITE_MESSAGE
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_WRTSIG(IFAIL,VAL,WORD,NW,PRECPR,TVAL,NOPNT)
    ! --
    ! -- SUBROUTINE UTL_WRTSIG WRITES A NUMBER INTO A CONFINED SPACE WITH
    ! -- MAXIMUM PRECISION
    ! --

    ! -- Revision history:-
    !       July, 1993: version 1.
    !       August 1994: modified for unix version (#ifdef's added)
    !       August, 1995: #ifdefs commented out for inclusion in Groundwater
    !                     Data Utilities

    !    failure criteria:
    !        ifail= 1 ...... number too large or small for single precision type
    !        ifail= 2 ...... number too large or small for double precision type
    !        ifail= 3 ...... field width too small to represent number
    !        ifail=-1 ...... internal error type 1
    !        ifail=-2 ...... internal error type 2
    !        ifail=-3 ...... internal error type 3
    USE GLOBAL_DATA, ONLY: AMESSAGE
    IMPLICIT NONE
    !
    !   Argument-list variables
    INTEGER,          INTENT(OUT) :: IFAIL
    DOUBLE PRECISION, INTENT(IN)  :: VAL    ! Value to be written
    CHARACTER(LEN=*), INTENT(OUT) :: WORD   ! Character representation of precision-limited value
    INTEGER,          INTENT(IN)  :: NW     ! Available field width
    INTEGER,          INTENT(IN)  :: PRECPR ! 0: single prec. (E) output;
                                            ! 1: double prec. (E) output;
                                            ! 2: double prec. (D) output
    DOUBLE PRECISION, INTENT(OUT) :: TVAL   ! Value written, truncated to available precision
    INTEGER,          INTENT(IN)  :: NOPNT  ! 1: decimal point optional; 0: dec. pt. required
    !
    !   Local variables
    INTEGER LW, POS, INC, D, P, W, J, JJ, K, JEXP, N, JFAIL, &
            EPOS, PP, KEXP, IFLAG, LEXP
    CHARACTER(LEN=29) :: TWORD,TTWORD,FMT*14
    !
    LEXP=0
    IFLAG=0
    WORD=' '
    POS=1
    IF(VAL.LT.0.0D0)POS=0
!#ifdef USE_D_FORMAT
!        WRITE(TWORD,'(1PD23.15D3)') VAL
!#else
    WRITE(TWORD,'(1PE23.15E3)') VAL
!#endif
    READ(TWORD(20:23),'(I4)') JEXP
    EPOS=1
    IF(JEXP.LT.0)EPOS=0

    JFAIL=0
    IFAIL=0
    IF(PRECPR.EQ.0)THEN
      LW=MIN(15,NW)
    ELSE
      LW=MIN(23,NW)
    END IF

    N=0
    IF(NOPNT.EQ.1)N=N+1
    IF(POS.EQ.1)N=N+1
    IF(PRECPR.EQ.0)THEN
      IF(ABS(JEXP).GT.38)THEN
        IFAIL=1
        RETURN
      END IF
      IF(POS.EQ.1) THEN
        IF(LW.GE.13) THEN
          WRITE(WORD,'(1PE13.7)',ERR=80) VAL
          GO TO 200
        END IF
      ELSE
        IF(LW.GE.14)THEN
          WRITE(WORD,'(1PE14.7)',ERR=80) VAL
          GO TO 200
        END IF
      END IF
      IF(LW.GE.14-N) THEN
        LW=14-N
        GO TO 80
      END IF
    ELSE
      IF(ABS(JEXP).GT.275)THEN
        IFAIL=2
        RETURN
      END IF
      IF(POS.EQ.1) THEN
        IF(LW.GE.22) THEN
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD22.15D3)',ERR=80) VAL
!#else
          WRITE(WORD,'(1PE22.15E3)',ERR=80) VAL
!#endif
          GO TO 200
        END IF
      ELSE
        IF(LW.GE.23) THEN
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD23.15D3)',ERR=80) VAL
!#else
          WRITE(WORD,'(1PE23.15E3)',ERR=80) VAL
!#endif
          GO TO 200
        END IF
      END IF
      IF(LW.GE.23-N)THEN
        LW=23-N
        GO TO 80
      END IF
    END IF

    IF(NOPNT.EQ.1)THEN
      IF((JEXP.EQ.LW-2+POS).OR.(JEXP.EQ.LW-3+POS))THEN
        WRITE(FMT,15)LW+1
15          FORMAT('(F',I2,'.0)')
        WRITE(WORD,FMT,ERR=19) VAL
        IF(INDEX(WORD,'*').NE.0) GO TO 19
        IF(WORD(1:1).EQ.' ') GO TO 19
        WORD(LW+1:LW+1)=' '
        GO TO 200
      END IF
    END IF
19      D=MIN(LW-2+POS,LW-JEXP-3+POS)
20      IF(D.LT.0) GO TO 80
    WRITE(FMT,30) LW,D
30      FORMAT('(F',I2,'.',I2,')')
    WRITE(WORD,FMT,ERR=80) VAL
    IF(INDEX(WORD,'*').NE.0) THEN
      D=D-1
      GO TO 20
    END IF
    K=INDEX(WORD,'.')
    IF(K.EQ.0)THEN
      IFAIL=-1
      RETURN
    END IF
    IF((K.EQ.1).OR.((POS.EQ.0).AND.(K.EQ.2)))THEN
      DO 70 J=1,3
      IF(K+J.GT.LW) GO TO 75
      IF(WORD(K+J:K+J).NE.'0') GO TO 200
70        CONTINUE
      GO TO 80
75        IFAIL=3
      RETURN
    END IF
    GO TO 200

80      WORD=' '
    IF(NOPNT.EQ.0)THEN
      D=LW-7
      IF(POS.EQ.1) D=D+1
      IF(EPOS.EQ.1) D=D+1
      IF(ABS(JEXP).LT.100) D=D+1
      IF(ABS(JEXP).LT.10) D=D+1
      IF((JEXP.GE.100).AND.(JEXP-(D-1).LT.100))THEN
        P=1+(JEXP-99)
        D=D+1
        LEXP=99
      ELSE IF((JEXP.GE.10).AND.(JEXP-(D-1).LT.10))THEN
        P=1+(JEXP-9)
        D=D+1
        LEXP=9
      ELSE IF((JEXP.EQ.-10).OR.(JEXP.EQ.-100)) THEN
        IFLAG=1
        D=D+1
      ELSE
        P=1
      END IF
      INC=0
85        IF(D.LE.0) GO TO 300
      IF(IFLAG.EQ.0)THEN
        WRITE(FMT,100,ERR=300) P,D+7,D-1
      ELSE
        WRITE(FMT,100,ERR=300) 0,D+8,D
      END IF
      WRITE(TWORD,FMT) VAL
      IF(IFLAG.EQ.1) GO TO 87
      READ(TWORD(D+4:D+7),'(I4)',ERR=500) KEXP
      IF(((KEXP.EQ.10).AND.((JEXP.EQ.9).OR.(LEXP.EQ.9))).OR. &
      ((KEXP.EQ.100).AND.((JEXP.EQ.99).OR.LEXP.EQ.99))) THEN
        IF(INC.EQ.0)THEN
          IF(LEXP.EQ.0)THEN
        IF(D-1.EQ.0) THEN
          D=D-1
        ELSE
          P=P+1
        END IF
          ELSE IF(LEXP.EQ.9)THEN
        IF(JEXP-(D-2).LT.10) THEN
          P=P+1
        ELSE
          D=D-1
        END IF
          ELSE IF(LEXP.EQ.99)THEN
        IF(JEXP-(D-2).LT.100)THEN
          P=P+1
        ELSE
          D=D-1
        END IF
          END IF
          INC=INC+1
          GO TO 85
        END IF
      END IF
!#ifdef USE_D_FORMAT
!87        J=INDEX(TWORD,'D')
!#else
87        J=INDEX(TWORD,'E')
!#endif
      GO TO 151
    END IF
    INC=0
    P=LW-2
    PP=JEXP-(P-1)
    IF(PP.GE.10)THEN
      P=P-1
      IF(PP.GE.100)P=P-1
    ELSE IF(PP.LT.0)THEN
      P=P-1
      IF(PP.LE.-10)THEN
        P=P-1
        IF(PP.LE.-100)P=P-1
      END IF
    END IF
    IF(POS.EQ.0)P=P-1
90      CONTINUE
    D=P-1
    W=D+8
    WRITE(FMT,100) P,W,D
    IF(D.LT.0)THEN
      IF(JFAIL.EQ.1) GO TO 300
      JFAIL=1
      P=P+1
      GO TO 90
    END IF
!#ifdef USE_D_FORMAT
!100     FORMAT('(',I2,'pD',I2,'.',I2,'D3)')
!#else
100     FORMAT('(',I2,'pE',I2,'.',I2,'E3)')
!#endif
    WRITE(TWORD,FMT) VAL
!#ifdef USE_D_FORMAT
!        J=INDEX(TWORD,'D')
!#else
    J=INDEX(TWORD,'E')
!#endif
    IF(TWORD(J-1:J-1).NE.'.')THEN
      IFAIL=-1
      RETURN
    END IF
    N=1
    IF(TWORD(J+1:J+1).EQ.'-') N=N+1
    IF(TWORD(J+2:J+2).NE.'0') THEN
      N=N+2
      GO TO 120
    END IF
    IF(TWORD(J+3:J+3).NE.'0') N=N+1
120     N=N+1
    IF(J+N-2-POS.LT.LW)THEN
      IF(INC.EQ.-1) GO TO 150
      TTWORD=TWORD
      P=P+1
      INC=1
      GO TO 90
    ELSE IF(J+N-2-POS.EQ.LW) THEN
      GO TO 150
    ELSE
      IF(INC.EQ.1)THEN
        TWORD=TTWORD
        GO TO 150
      END IF
      IF(JFAIL.EQ.1) GO TO 300
      P=P-1
      INC=-1
      GO TO 90
    END IF

150     J=INDEX(TWORD,'.')
151     IF(POS.EQ.0)THEN
      K=1
    ELSE
     K=2
    END IF
    WORD(1:J-K)=TWORD(K:J-1)
    JJ=J
    J=J-K+1
    IF(PRECPR == 0 .OR. PRECPR == 1)THEN
      WORD(J:J)='E'
    ELSEIF(PRECPR == 2) THEN
      WORD(J:J)='D'
    ELSE
      AMESSAGE = 'Programmer ERROR in UTL_WRTSIG--PRECPR must '//   &
                 'be 0, 1, or 2'
      CALL UTL_STOP()
    END IF
    JJ=JJ+2
    IF(NOPNT.EQ.0) JJ=JJ-1
    IF(TWORD(JJ:JJ).EQ.'-')THEN
      J=J+1
      WORD(J:J)='-'
    END IF
    IF(TWORD(JJ+1:JJ+1).NE.'0')THEN
      J=J+2
      WORD(J-1:J)=TWORD(JJ+1:JJ+2)
      GO TO 180
    END IF
    IF(TWORD(JJ+2:JJ+2).NE.'0')THEN
      J=J+1
      WORD(J:J)=TWORD(JJ+2:JJ+2)
    END IF
180     J=J+1
    WORD(J:J)=TWORD(JJ+3:JJ+3)
    IF(IFLAG.EQ.1)THEN
      IF(POS.EQ.1)THEN
        JJ=1
      ELSE
        JJ=2
      END IF
      N=len_trim(WORD)
      DO 190 J=JJ,N-1
        WORD(J:J)=WORD(J+1:J+1)
      190 CONTINUE
      WORD(N:N)=' '
    END IF

200     IF(len_trim(WORD).GT.LW)THEN
      IFAIL=-2
      RETURN
    END IF
    WRITE(FMT,30) LW,0
    READ(WORD,FMT,ERR=400) TVAL
    RETURN
300     IFAIL=3
    RETURN
400     IFAIL=-3
    RETURN
500     IFAIL=-2
    RETURN
  END SUBROUTINE UTL_WRTSIG
  !-----------------------------------------------------------------------------
  ! Three versions of UTL_SLEEP are provided.  Leave only one version
  ! uncommented. The first can be used by the Intel Fortran compiler.  This
  ! version, or one similar with modifications for other compilers that support
  ! "SLEEP"ing, is preferred because during sleep, no CPU cycles are used.  
  !
  ! The second version can be used by the gfortran compiler.
  !
  ! For compilers that do not support a SLEEP routine or equivalent, another
  ! version (labeled "For compilers that do not support SLEEP"), which uses CPU 
  ! cycles until the desired time has elapsed, can be used; however, use of this 
  ! version may degrade performance substantially.
  !-----------------------------------------------------------------------------
  SUBROUTINE UTL_SLEEP(SECS)   ! Version for Intel compiler
    ! Suspend execution for SECS seconds.
    USE IFPORT
    IMPLICIT NONE
    !
    !   Argument-list variables
    DOUBLE PRECISION, INTENT(IN) :: SECS
    !
    !   Local variables
    INTEGER(4) :: MILLISECS
    !
    MILLISECS = NINT(SECS * 1000)
    CALL SLEEPQQ(MILLISECS)
    RETURN
  END SUBROUTINE UTL_SLEEP
  !-----------------------------------------------------------------------------
!  SUBROUTINE UTL_SLEEP(SECS)   ! Version for gfortran compiler
!    ! Suspend execution for SECS seconds.
!    IMPLICIT NONE
!    !
!    !   Argument-list variables
!    DOUBLE PRECISION, INTENT(IN) :: SECS
!    !
!    CALL SLEEP(NINT(SECS))
!    RETURN
!  END SUBROUTINE UTL_SLEEP
  !-----------------------------------------------------------------------------
!  SUBROUTINE UTL_SLEEP(SECS)  ! For compilers that do not support SLEEP
!    IMPLICIT NONE
!    !
!    !   Argument-list variables
!    DOUBLE PRECISION :: SECS
!    !
!    !   Local variables
!    CHARACTER(LEN=8)  :: DATE
!    CHARACTER(LEN=10) :: TIME
!    CHARACTER(LEN=5)  :: ZONE
!    INTEGER, DIMENSION(8) :: VALUES0, VALUES1
!    DOUBLE PRECISION :: DIFF, YRSEC0, YRSEC1
!    DOUBLE PRECISION, PARAMETER :: SPD = 86400.D0
!    DOUBLE PRECISION, PARAMETER :: SPH = 3600.D0
!    DOUBLE PRECISION, PARAMETER :: SPM = 60.D0
!    !
!    10 CONTINUE
!    CALL DATE_AND_TIME(DATE,TIME,ZONE,VALUES0)
!    !   Calculate seconds since beginning of month
!    YRSEC0 = (VALUES0(3)-1)*SPD + VALUES0(5)*SPH + VALUES0(6)*SPM   &
!             + VALUES0(7)*1.D0 + VALUES0(8)*1.0D-3
!    !
!    DO WHILE (.TRUE.)
!      CALL DATE_AND_TIME(DATE,TIME,ZONE,VALUES1)
!      IF (VALUES0(2) .NE. VALUES1(2)) GOTO 10
!      !   Again calculate seconds since beginning of month
!      YRSEC1 = (VALUES1(3)-1)*SPD + VALUES1(5)*SPH + VALUES1(6)*SPM   &
!               + VALUES1(7)*1.D0 + VALUES1(8)*1.0D-3
!      DIFF = YRSEC1-YRSEC0
!      IF (DIFF .GE. SECS) RETURN
!    ENDDO
!    !
!    RETURN
!  END SUBROUTINE UTL_SLEEP
  !-----------------------------------------------------------------------------
  !
END MODULE UTILITIES





