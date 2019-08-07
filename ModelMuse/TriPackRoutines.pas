// The procedures in this file are used to create a constrained or
// unconstrained Delaunay triangulation.
//
// Original source code
// http://www.netlib.org/toms/751
//
// Output from the procedures in this file are set to @link(TriPackMessages).
// You can alter the procedures in @link(TriPackMessages) if you wish to
// capture the output in a different way.
//
// Although this file is in the public domain, the algorithm it contains
// is not.
// For conditions of use see
// http://www.netlib.org/toms/
//
// Translation from Fortran by Richard B. Winston.
// @author(Richard B. Winston <rbwinst@usgs.gov>)
unit TriPackRoutines;

interface

uses Classes, GoPhastTypes;

Type
  TReal6 = array[0..5] of Real;
  TReal3 = array[0..2] of Real;
//  TRealArray = array of Real;
  TIntArray = array of longint;

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE CREATES A THIESSEN TRIANGULATION OF N
// ARBITRARILY SPACED POINTS IN THE PLANE REFERRED TO AS
// NODES.  THE TRIANGULATION IS OPTIMAL IN THE SENSE THAT IT
// IS AS NEARLY EQUIANGULAR AS POSSIBLE.  TRMESH IS PART OF
// AN INTERPOLATION PACKAGE WHICH ALSO PROVIDES SUBROUTINES
// TO REORDER THE NODES, ADD A NEW NODE, DELETE AN ARC, PLOT
// THE MESH, AND PRINT THE DATA STRUCTURE.
//   UNLESS THE NODES ARE ALREADY ORDERED IN SOME REASONABLE
// FASHION, THEY SHOULD BE REORDERED BY SUBROUTINE REORDR FOR
// INCREASED EFFICIENCY BEFORE CALLING TRMESH.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//                            N >= 3.
//
//                      X,Y - N-VECTORS OF COORDINATES.
//                            (X(I),Y(I)) DEFINES NODE I.
//
//                     IADJ - VECTOR OF LENGTH >= 6*N-9.
//
//                     IEND - VECTOR OF LENGTH >= N.
//
// N, X, AND Y ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - IADJ - ADJACENCY LISTS OF NEIGHBORS IN
//                            COUNTERCLOCKWISE ORDER.  THE
//                            LIST FOR NODE I+1 FOLLOWS THAT
//                            FOR NODE I WHERE X AND Y DEFINE
//                            THE ORDER.  THE VALUE 0 DENOTES
//                            THE BOUNDARY (OR A PSEUDO-NODE
//                            AT INFINITY) AND IS ALWAYS THE
//                            LAST NEIGHBOR OF A BOUNDARY
//                            NODE.  IADJ IS UNCHANGED IF IER
//                            .NE. 0.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS (SETS OF
//                            NEIGHBORS) IN IADJ.  THE
//                            NEIGHBORS OF NODE 1 BEGIN IN
//                            IADJ(1).  FOR K .GT. 1, THE
//                            NEIGHBORS OF NODE K BEGIN IN
//                            IADJ(IEND(K-1)+1) AND K HAS
//                            IEND(K) - IEND(K-1) NEIGHBORS
//                            INCLUDING (POSSIBLY) THE
//                            BOUNDARY.  IADJ(IEND(K)) .EQ. 0
//                            IFF NODE K IS ON THE BOUNDARY.
//                            IEND IS UNCHANGED IF IER = 1.
//                            IF IER = 2 IEND CONTAINS THE
//                            INDICES OF A SEQUENCE OF N
//                            NODES ORDERED FROM LEFT TO
//                            RIGHT WHERE LEFT AND RIGHT ARE
//                            DEFINED BY ASSUMING NODE 1 IS
//                            TO THE LEFT OF NODE 2.
//
//                      IER - ERROR INDICATOR
//                            IER = 0 IF NO ERRORS WERE
//                                    ENCOUNTERED.
//                            IER = 1 IF N .LT. 3.
//                            IER = 2 IF N >= 3 AND ALL
//                                    NODES ARE COLLINEAR.
//
// MODULES REFERENCED BY TRMESH - SHIFTD, ADNODE, TRFIND,
//                                INTADD, BDYADD, SWPTST,
//                                SWAP, INDEX
//
//***********************************************************
//
//)
Procedure TRMESH (const N: longint; const X,Y: TRealArray; var IADJ: TIntArray;
  var IEND: TIntArray; var IER: longint);

// @preformatted(
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   THIS ROUTINE DETERMINES THE NUMBER OF ARCS IN A TRIANGU-
//C LATION WHICH ARE NOT LOCALLY OPTIMAL AS DEFINED BY LOGICAL
//C FUNCTION SWPTST.  A THIESSEN TRIANGULATION (SEE SUBROUTINE
//C TRMESH) IS CHARACTERIZED BY THE PROPERTY THAT ALL ARCS ARE
//C LOCALLY OPTIMAL.  THE ALGORITHM CONSISTS OF APPLYING THE
//C SWAP TEST TO ALL INTERIOR ARCS.
//C
//C INPUT PARAMETERS -
//C
//C       N - NUMBER OF NODES IN THE TRIANGULATION.  N .GE. 3.
//C
//C       X,Y - COORDINATES OF THE NODES.
//C
//C       IADJ,IEND - DATA STRUCTURE CONTAINING THE TRIANGULA-
//C                   TION.  SEE SUBROUTINE TRMESH.
//C
//C THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//C
//C       LL - NUMBER OF COLUMNS RESERVED FOR LIST (SEE THE
//C            OUTPUT VALUE OF LIST).  LL .GE. 0.
//C
//C       LIST - INTEGER ARRAY DIMENSIONED 2 BY LL (OR VECTOR
//C              OF LENGTH .GE. 2*LL) IF LL .GT. 0.
//C
//C OUTPUT PARAMETERS -
//C
//C       LL - NUMBER OF ARCS WHICH ARE NOT LOCALLY OPTIMAL
//C            UNLESS IER = 1.
//C
//C       LIST - COLUMNS CONTAIN THE ENDPOINT NODAL INDICES
//C              (SMALLER INDEX IN THE FIRST ROW) OF THE FIRST
//C              K NONOPTIMAL ARCS ENCOUNTERED, WHERE K IS THE
//C              MINIMUM OF THE INPUT AND OUTPUT VALUES OF LL,
//C              IF IER = 0 AND K .GT. 0.  THE NUMBER OF INTE-
//C              RIOR ARCS IS 3N-2NB-3 .LE. 3(N-3) WHERE NB IS
//C              THE NUMBER OF BOUNDARY NODES.  BOUNDARY ARCS
//C              ARE OPTIMAL BY DEFINITION.
//C
//C       IER - ERROR INDICATOR
//C             IER = 0 IF NO ERRORS WERE ENCOUNTERED.
//C             IER = 1 IF N .LT. 3 OR LL .LT. 0.
//C
//C MODULES REQUIRED BY ARCTST - SWPTST
//C
//C***********************************************************
//C
//)
procedure ARCTST (const N: longint; const X,Y: TRealArray;
  var IADJ, IEND: TIntArray; var LL: longint;
  var LIST: TIntArray; var IER: longint); stdcall;
  
// @preformatted(
//
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF N POINTS IN THE PLANE, THIS
// ROUTINE RETURNS A VECTOR CONTAINING THE INDICES, IN
// COUNTERCLOCKWISE ORDER, OF THE NODES ON THE BOUNDARY OF
// THE CONVEX HULL OF THE SET OF POINTS.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//
//                     IADJ - SET OF ADJACENCY LISTS OF
//                            NODES IN THE MESH.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS IN IADJ FOR
//                            EACH NODE IN THE MESH.
//
//                    NODES - VECTOR OF LENGTH >= NB.
//                            (NB <= N).
//
//   IADJ AND IEND MAY BE CREATED BY TRMESH AND ARE NOT
// ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS -   NB - NUMBER OF BOUNDARY NODES.
//
//                    NA,NT - NUMBER OF ARCS AND TRIANGLES,
//                            RESPECTIVELY, IN THE MESH.
//
//                    NODES - VECTOR OF NB BOUNDARY NODE
//                            INDICES RANGING FROM 1 TO N.
//
// MODULES REFERENCED BY BNODES - NONE
//
//***********************************************************
//
//)
procedure BNODES (const N: longint; const IADJ: TIntArray;
    const IEND: TIntArray; var NB,NA,NT: longint; var NODES: TIntArray);

// @preformatted(
//      SUBROUTINE DELETE (NN,NOUT1,NOUT2, IADJ,IEND, IER)
//      INTEGER NN, NOUT1, NOUT2, IADJ(1), IEND(NN), IER
//      EXTERNAL INDEX
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   THIS ROUTINE DELETES A BOUNDARY EDGE FROM A TRIANGU-
//C LATION OF A SET OF POINTS IN THE PLANE.  IT MAY BE NEC-
//C ESSARY TO FORCE CERTAIN EDGES TO BE PRESENT BEFORE CALL-
//C ING DELETE (SEE SUBROUTINE EDGE).  NOTE THAT SUBROUTINES
//C EDGE, TRFIND, AND THE ROUTINES WHICH CALL TRFIND (ADNODE,
//C UNIF, INTRC1, AND INTRC0) SHOULD NOT BE CALLED FOLLOWING
//C A DELETION.
//C
//C INPUT PARAMETERS -    NN - NUMBER OF NODES IN THE TRIAN-
//C                            GULATION.
//C
//C              NOUT1,NOUT2 - PAIR OF ADJACENT NODES ON THE
//C                            BOUNDARY DEFINING THE ARC TO
//C                            BE REMOVED.  NOUT2 MUST BE THE
//C                            LAST NONZERO NEIGHBOR OF NOUT1.
//C
//C THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//C
//C                IADJ,IEND - DATA STRUCTURE DEFINING THE
//C                            TRIANGULATION (SEE SUBROUTINE
//C                            TRMESH).
//C
//C OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE REMOVAL
//C                                 OF THE ARC NOUT1-NOUT2
//C                                 IF IER .EQ. 0.
//C
//C                           IER - ERROR INDICATOR
//C                                 IER = 0 IF NO ERRORS WERE
//C                                         ENCOUNTERED.
//C                                 IER = 1 IF NOUT1 OR NOUT2
//C                                         IS NOT ON THE
//C                                         BOUNDARY.
//C                                 IER = 2 IF NOUT1 OR NOUT2
//C                                         HAS ONLY 2 NONZERO
//C                                         NEIGHBORS.
//C                                 IER = 3 IF NOUT2 IS NOT
//C                                         THE LAST NEIGHBOR
//C                                         OF NOUT1.
//C                                 IER = 4 IF A DELETION
//C                                         WOULD DIVIDE THE
//C                                         MESH INTO TWO
//C                                         REGIONS.
//)
procedure DELETE (const NN,NOUT1,NOUT2: longint;
  var IADJ: TIntArray; var IEND: TIntArray; var IER: longint);
{
@name is similar to @link(DELETE) but it only requires that the nodes
share an edge in order to delete the edge. The mesh is not guaranteed to
be valid after calling @name. For example, some nodes may no longer be part
of any triangle.

See @link(DELETE) for the meanings of the parameters.
}
procedure DELETE2 (const NN,NOUT1,NOUT2: longint;
  var IADJ: TIntArray; var IEND: TIntArray; var IER: longint);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO
// REORDER THE REAL ARRAY A INTO INCREASING ORDER.  A RECORD
// OF THE PERMUTATIONS APPLIED TO A IS STORED IN IND, AND
// THESE PERMUTATIONS MAY BE APPLIED TO ONE OR TWO ADDITIONAL
// VECTORS BY THIS ROUTINE.  ANY OTHER VECTOR V MAY BE PER-
// MUTED IN THE SAME FASHION BY CALLING SUBROUTINE PERMUT
// WITH N, IND, AND V AS PARAMETERS.
//   A SET OF NODES (X(I),Y(I)) AND DATA VALUES Z(I) MAY BE
// PREPROCESSED BY REORDR FOR INCREASED EFFICIENCY IN THE
// TRIANGULATION ROUTINE TRMESH.  EFFICIENCY IS INCREASED BY
// A FACTOR OF APPROXIMATELY SQRT(N)/6 FOR RANDOMLY DISTRIB-
// UTED NODES, AND THE PREPROCESSING IS ALSO USEFUL FOR
// DETECTING DUPLICATE NODES.  EITHER X OR Y MAY BE USED AS
// THE SORT KEY (ASSOCIATED WITH A).
//
// INPUT PARAMETERS - N - NUMBER OF NODES.
//
//                IFLAG - NUMBER OF VECTORS TO BE PERMUTED.
//                        IFLAG .LE. 0 IF A, B, AND C ARE TO
//                                     REMAIN UNALTERED.
//                        IFLAG .EQ. 1 IF ONLY A IS TO BE
//                                     PERMUTED.
//                        IFLAG .EQ. 2 IF A AND B ARE TO BE
//                                     PERMUTED.
//                        IFLAG >= 3 IF A, B, AND C ARE TO
//                                     BE PERMUTED.
//
//                A,B,C - VECTORS OF LENGTH N TO BE SORTED
//                        (ON THE COMPONENTS OF A), OR DUMMY
//                        PARAMETERS, DEPENDING ON IFLAG.
//
//                  IND - VECTOR OF LENGTH >= N.
//
// N, IFLAG, AND ANY DUMMY PARAMETERS ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS - A,B,C - SORTED OR UNALTERED VECTORS.
//
//                       IND - SEQUENCE OF INDICES 1,...,N
//                             PERMUTED IN THE SAME FASHION
//                             AS THE REAL VECTORS.  THUS,
//                             THE ORDERING MAY BE APPLIED TO
//                             A REAL VECTOR V AND STORED IN
//                             W BY SETTING W(I) = V(IND(I)),
//                             OR V MAY BE OVERWRITTEN WITH
//                             THE ORDERING BY A CALL TO PER-
//                             MUT.
//
// MODULES REFERENCED BY REORDR - QSORT, PERMUT
//
//***********************************************************
//)
Procedure REORDR (const N,IFLAG: longint; var A,B,C: TRealArray;
  var IND: TIntArray);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE TESTS THE VALIDITY OF THE DATA STRUCTURE
// REPRESENTING A THIESSEN TRIANGULATION CREATED BY SUBROU-
// TINE TRMESH.  THE FOLLOWING PROPERTIES ARE TESTED --
//   1)  IEND(1) >= 3 AND IEND(K) >= IEND(K-1)+3 FOR K =
//       2,...,N (EACH NODE HAS AT LEAST THREE NEIGHBORS).
//   2)  0 .LE. IADJ(K) .LE. N FOR K = 1,...,IEND(N) (IADJ
//       ENTRIES ARE NODAL INDICES OR ZEROS REPRESENTING THE
//       BOUNDARY).
//   3)  NB >= 3, NT = 2N-NB-2, AND NA = 3N-NB-3 WHERE NB,
//       NT, AND NA ARE THE NUMBERS OF BOUNDARY NODES, TRI-
//       ANGLES, AND ARCS, RESPECTIVELY.
//   4)  EACH CIRCUMCIRCLE DEFINED BY THE VERTICES OF A TRI-
//       ANGLE CONTAINS NO NODES IN ITS INTERIOR.  THIS PROP-
//       ERTY DISTINGUISHES A THIESSEN TRIANGULATION FROM AN
//       ARBITRARY TRIANGULATION OF THE NODES.
// NOTE THAT NO TEST IS MADE FOR THE PROPERTY THAT A TRIANGU-
// LATION COVERS THE CONVEX HULL OF THE NODES, AND THUS A
// TEST ON A DATA STRUCTURE ALTERED BY SUBROUTINE DELETE
// SHOULD NOT RESULT IN AN ERROR.
//
// INPUT PARAMETERS -
//
//       N - NUMBER OF NODES.  N >= 3.
//
//       X,Y - NODAL COORDINATES.
//
//       IADJ,IEND - TRIANGULATION DATA STRUCTURE.  SEE SUB-
//                   ROUTINE TRMESH.
//
//       TOL - NONNEGATIVE TOLERANCE TO ALLOW FOR FLOATING-
//             POINT ERRORS IN THE CIRCUMCIRCLE TEST.  AN
//             ERROR SITUATION IS DEFINED AS R**2 - D**2 .GT.
//             TOL WHERE R IS THE RADIUS OF A CIRCUMCIRCLE
//             AND D IS THE DISTANCE FROM THE CIRCUMCENTER
//             TO THE NEAREST NODE.  A REASONABLE VALUE
//             FOR TOL IS 10*EPS WHERE EPS IS THE MACHINE
//             PRECISION.  THE TEST IS EFFECTIVELY BYPASSED
//             BY MAKING TOL LARGER THAN THE DIAMETER OF THE
//             CONVEX HULL OF THE NODES.
//
//       LUN - LOGICAL UNIT NUMBER FOR PRINTING ERROR MES-
//             SAGES.  IF LUN .LT. 1 OR LUN .GT. 99, NO MES-
//             SAGES ARE PRINTED.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETER -
//
//       IER - ERROR INDICATOR
//             IER = -1 IF ONE OR MORE NULL TRIANGLES (AREA =
//                      0) ARE PRESENT BUT NO (OTHER) ERRORS
//                      WERE ENCOUNTERED.  A NULL TRIANGLE IS
//                      AN ERROR ONLY IF IT OCCURS IN THE
//                      THE INTERIOR.
//             IER = 0 IF NO ERRORS OR NULL TRIANGLES WERE
//                     ENCOUNTERED.
//             IER = 1 IF N .LT. 3 OR TOL .LT. 0.
//             IER = 2 IF AN IEND OR IADJ ENTRY IS OUT OF
//                     RANGE.
//             IER = 3 IF THE TRIANGULATION PARAMETERS (NB,
//                     NT, AND NA) ARE INCONSISTENT.
//             IER = 4 IF A TRIANGLE CONTAINS A NODE INTERIOR
//                     TO ITS CIRCUMCIRCLE.
//             THE ERROR SITUATIONS ARE TESTED IN THE ORDER
//               DEFINED BY THE (POSITIVE) IER VALUES.
//
// MODULE REQUIRED BY TRMTST - CIRCUM
//
//***********************************************************
//)
procedure TRMTST(const N: longint; const X,Y: TRealArray;
    const IADJ: TIntArray; const IEND: TIntArray; const TOL: Real;
    const LUN: longint; var IER: longint);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A THIESSEN TRIANGULATION OF N NODES AND AN ARRAY
// NPTS CONTAINING THE INDICES OF L-1 NODES ORDERED BY
// EUCLIDEAN DISTANCE FROM NPTS(1), THIS SUBROUTINE SETS
// NPTS(L) TO THE INDEX OF THE NEXT NODE IN THE SEQUENCE --
// THE NODE, OTHER THAN NPTS(1),...,NPTS(L-1), WHICH IS
// CLOSEST TO NPTS(1).  THUS, THE ORDERED SEQUENCE OF K
// CLOSEST NODES TO N1 (INCLUDING N1) MAY BE DETERMINED BY
// K-1 CALLS TO GETNP WITH NPTS(1) = N1 AND L = 2,3,...,K
// FOR K >= 2.
//   THE ALGORITHM USES THE FACT THAT, IN A THIESSEN TRIAN-
// GULATION, THE K-TH CLOSEST NODE TO A GIVEN NODE N1 IS A
// NEIGHBOR OF ONE OF THE K-1 CLOSEST NODES TO N1.
//
// INPUT PARAMETERS - X,Y - VECTORS OF LENGTH N CONTAINING
//                          THE CARTESIAN COORDINATES OF THE
//                          NODES.
//
//                   IADJ - SET OF ADJACENCY LISTS OF NODES
//                          IN THE TRIANGULATION.
//
//                   IEND - POINTERS TO THE ENDS OF ADJACENCY
//                          LISTS FOR EACH NODE IN THE TRI-
//                          ANGULATION.
//
//                      L - NUMBER OF NODES IN THE SEQUENCE
//                          ON OUTPUT.  2 .LE. L .LE. N.
//
//                   NPTS - ARRAY OF LENGTH >= L CONTAIN-
//                          ING THE INDICES OF THE L-1 CLOS-
//                          EST NODES TO NPTS(1) IN THE FIRST
//                          L-1 LOCATIONS.
//
// IADJ AND IEND MAY BE CREATED BY SUBROUTINE TRMESH.
//
// INPUT PARAMETERS OTHER THAN NPTS ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS - NPTS - UPDATED WITH THE INDEX OF THE
//                            L-TH CLOSEST NODE TO NPTS(1) IN
//                            POSITION L UNLESS IER = 1.
//
//                       DS - SQUARED EUCLIDEAN DISTANCE BE-
//                            TWEEN NPTS(1) AND NPTS(L)
//                            UNLESS IER = 1.
//
//                      IER - ERROR INDICATOR
//                            IER = 0 IF NO ERRORS WERE EN-
//                                    COUNTERED.
//                            IER = 1 IF L IS OUT OF RANGE.
//
// MODULES REFERENCED BY GETNP - NONE
//
// INTRINSIC FUNCTION CALLED BY GETNP - IABS
//
//***********************************************************
//
//)
procedure GETNP (const X,Y: TRealArray; const IADJ: TIntArray;
  const IEND: TIntArray; const L: longint; var NPTS: TIntArray;
  var DS: Real; var IER: longint);

// @preformatted(
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A SEQUENCE OF NB POINTS IN THE PLANE, THIS
//C FUNCTION COMPUTES THE AREA BOUNDED BY THE CLOSED POLY-
//C GONAL CURVE WHICH PASSES THROUGH THE POINTS IN THE
//C SPECIFIED ORDER.  EACH SIMPLE CLOSED CURVE IS POSITIVELY
//C ORIENTED (BOUNDS POSITIVE AREA) IF AND ONLY IF THE POINTS
//C ARE SPECIFIED IN COUNTERCLOCKWISE ORDER.  THE LAST POINT
//C OF THE CURVE IS TAKEN TO BE THE FIRST POINT SPECIFIED, AND
//C THUS THIS POINT NEED NOT BE SPECIFIED TWICE.  HOWEVER, ANY
//C POINT MAY BE SPECIFIED MORE THAN ONCE IN ORDER TO DEFINE A
//C MULTIPLY CONNECTED DOMAIN.
//C   THE AREA OF A TRIANGULATION MAY BE COMPUTED BY CALLING
//C AREA WITH VALUES OF NB AND NODES DETERMINED BY SUBROUTINE
//C BNODES.
//C
//C INPUT PARAMETERS -   X,Y - N-VECTORS OF COORDINATES OF
//C                            POINTS IN THE PLANE FOR N >=
//C                            NB.  NODE I HAS COORDINATES
//C                            (X(I),Y(I)) FOR I = 1, 2, ...,
//C                            N.
//C
//C                       NB - LENGTH OF NODES.
//C
//C                    NODES - VECTOR OF NODE INDICES IN THE
//C                            RANGE 1 TO N DEFINING THE
//C                            POLYGONAL CURVE.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER -  AREA - SIGNED AREA BOUNDED BY THE
//C                            POLYGONAL CURVE DEFINED
//C                            ABOVE.
//C
//C MODULES REFERENCED BY AREA - NONE
//C
//C***********************************************************
//)
function AREA (const X,Y: TRealArray;  const NB: longint;
    const NODES: TIntArray): Real;


// @preformatted(
//      SUBROUTINE EDGE (IN1,IN2,X,Y, LWK,IWK,IADJ,IEND, IER)
//      LOGICAL SWPTST
//      INTEGER IN1, IN2, LWK, IWK(2,1), IADJ(1), IEND(1), IER
//      REAL    X(1), Y(1)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A TRIANGULATION OF N NODES AND A PAIR OF NODAL
//C INDICES IN1 AND IN2, THIS ROUTINE SWAPS ARCS AS NECESSARY
//C TO FORCE IN1 AND IN2 TO BE ADJACENT.  ONLY ARCS WHICH
//C INTERSECT IN1-IN2 ARE SWAPPED OUT.  IF A THIESSEN TRIANGU-
//C LATION IS INPUT, THE RESULTING TRIANGULATION IS AS CLOSE
//C AS POSSIBLE TO A THIESSEN TRIANGULATION IN THE SENSE THAT
//C ALL ARCS OTHER THAN IN1-IN2 ARE LOCALLY OPTIMAL.
//C   A SEQUENCE OF CALLS TO EDGE MAY BE USED TO FORCE THE
//C PRESENCE OF A SET OF EDGES DEFINING THE BOUNDARY OF A NON-
//C CONVEX REGION.  SUBSEQUENT DELETION OF EDGES OUTSIDE THIS
//C REGION (BY SUBROUTINE DELETE) RESULTS IN A NONCONVEX TRI-
//C ANGULATION WHICH MAY SERVE AS A FINITE ELEMENT GRID.
//C (EDGE SHOULD NOT BE CALLED AFTER A CALL TO DELETE.)  IF,
//C ON THE OTHER HAND, INTERPOLATION IS TO BE PERFORMED IN THE
//C NONCONVEX REGION, EDGES MUST NOT BE DELETED, BUT IT IS
//C STILL ADVANTAGEOUS TO HAVE THE NONCONVEX BOUNDARY PRESENT
//C IF IT IS DESIRABLE THAT INTERPOLATED VALUES BE INFLUENCED
//C BY THE GEOMETRY.  NOTE THAT SUBROUTINE GETNP WHICH IS USED
//C TO SELECT THE NODES ENTERING INTO LOCAL DERIVATIVE ESTI-
//C MATES WILL NOT NECESSARILY RETURN CLOSEST NODES IF THE
//C TRIANGULATION HAS BEEN RENDERED NONOPTIMAL BY A CALL TO
//C EDGE.  HOWEVER, THE EFFECT WILL BE MERELY TO FURTHER EN-
//C HANCE THE INFLUENCE OF THE NONCONVEX GEOMETRY ON INTERPO-
//C LATED VALUES.
//C
//C INPUT PARAMETERS - IN1,IN2 - INDICES (OF X AND Y) IN THE
//C                              RANGE 1,...,N DEFINING A PAIR
//C                              OF NODES TO BE CONNECTED BY
//C                              AN ARC.
//C
//C                        X,Y - N-VECTORS CONTAINING CARTE-
//C                              SIAN COORDINATES OF THE
//C                              NODES.
//C
//C THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//C
//C                        LWK - NUMBER OF COLUMNS RESERVED
//C                              FOR IWK.  THIS MUST BE AT
//C                              LEAST NI -- THE NUMBER OF
//C                              ARCS WHICH INTERSECT IN1-IN2.
//C                              (NI IS BOUNDED BY N-3).
//C
//C                        IWK - INTEGER WORK ARRAY DIMENSION-
//C                              ED 2 BY LWK (OR VECTOR OF
//C                              LENGTH .GE. 2*LWK).
//C
//C                  IADJ,IEND - DATA STRUCTURE DEFINING THE
//C                              TRIANGULATION.  SEE SUBROU-
//C                              TINE TRMESH.
//C
//C OUTPUT PARAMETERS - LWK - NUMBER OF IWK COLUMNS REQUIRED
//C                           IF IER = 0 OR IER = 2.  LWK = 0
//C                           IFF IN1 AND IN2 WERE ADJACENT
//C                           ON INPUT.
//C
//C                     IWK - CONTAINS THE INDICES OF THE END-
//C                           POINTS OF THE NEW ARCS OTHER
//C                           THAN IN1-IN2 UNLESS IER .GT. 0
//C                           OR LWK = 0.  NEW ARCS TO THE
//C                           LEFT OF IN1->IN2 ARE STORED IN
//C                           THE FIRST K-1 COLUMNS (LEFT POR-
//C                           TION OF IWK), COLUMN K CONTAINS
//C                           ZEROS, AND NEW ARCS TO THE RIGHT
//C                           OF IN1->IN2 OCCUPY COLUMNS K+1,
//C                           ...,LWK.  (K CAN BE DETERMINED
//C                           BY SEARCHING IWK FOR THE ZEROS.)
//C
//C               IADJ,IEND - UPDATED IF NECESSARY TO REFLECT
//C                           THE PRESENCE OF AN ARC CONNECT-
//C                           ING IN1 AND IN2, UNALTERED IF
//C                           IER .NE. 0.
//C
//C                     IER - ERROR INDICATOR
//C                           IER = 0 IF NO ERRORS WERE EN-
//C                                   COUNTERED.
//C                           IER = 1 IF IN1 .LT. 1, IN2 .LT.
//C                                   1, IN1 = IN2, OR LWK
//C                                   .LT. 0 ON INPUT.
//C                           IER = 2 IF MORE SPACE IS REQUIR-
//C                                   ED IN IWK.  SEE LWK.
//C                           IER = 3 IF IN1 AND IN2 COULD NOT
//C                                   BE CONNECTED DUE TO AN
//C                                   INVALID DATA STRUCTURE.
//C
//C MODULES REFERENCED BY EDGE - SWAP, INDEX, SHIFTD, SWPTST
//C
//C***********************************************************
//C
//)
procedure EDGE (const IN1,IN2: longint; const X,Y: TRealArray;
    var LWK: longint; var IWK: TIntArray;
    var IADJ: TIntArray; var IEND: TIntArray; var IER: longint);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF N NODES IN THE PLANE WITH
// ASSOCIATED DATA VALUES, THIS ROUTINE USES A GLOBAL METHOD
// TO COMPUTE ESTIMATED GRADIENTS AT THE NODES.  THE METHOD
// CONSISTS OF MINIMIZING A QUADRATIC FUNCTIONAL Q(G) OVER
// THE N-VECTOR G OF GRADIENTS WHERE Q APPROXIMATES THE LIN-
// EARIZED CURVATURE OF AN INTERPOLANT F OVER THE TRIANGULA-
// TION.  THE RESTRICTION OF F TO AN ARC OF THE TRIANGULATION
// IS TAKEN TO BE THE HERMITE CUBIC INTERPOLANT OF THE DATA
// VALUES AND TANGENTIAL GRADIENT COMPONENTS AT THE END-
// POINTS OF THE ARC, AND Q IS THE SUM OF THE LINEARIZED
// CURVATURES OF F ALONG THE ARCS -- THE INTEGRALS OVER THE
// ARCS OF D2F(T)**2 WHERE D2F(T) IS THE SECOND DERIVATIVE
// OF F WITH RESPECT TO DISTANCE T ALONG THE ARC.  THIS MIN-
// IMIZATION PROBLEM CORRESPONDS TO AN ORDER 2N SYMMETRIC
// POSITIVE-DEFINITE SPARSE LINEAR SYSTEM WHICH IS SOLVED FOR
// THE X AND Y PARTIAL DERIVATIVES BY THE BLOCK GAUSS-SEIDEL
// METHOD WITH 2 BY 2 BLOCKS.
//   AN ALTERNATIVE METHOD, SUBROUTINE GRADL, COMPUTES A
// LOCAL APPROXIMATION TO THE PARTIALS AT A SINGLE NODE AND
// MAY BE MORE ACCURATE, DEPENDING ON THE DATA VALUES AND
// DISTRIBUTION OF NODES (NEITHER METHOD EMERGED AS SUPERIOR
// IN TESTS FOR ACCURACY).  HOWEVER, IN TESTS RUN ON AN IBM
// 370, GRADG WAS FOUND TO BE ABOUT 3.6 TIMES AS FAST FOR
// NIT = 4.
//
// INPUT PARAMETERS - N - NUMBER OF NODES.  N .GE. 3.
//
//                  X,Y - CARTESIAN COORDINATES OF THE NODES.
//
//                    Z - DATA VALUES AT THE NODES.  Z(I) IS
//                        ASSOCIATED WITH (X(I),Y(I)).
//
//            IADJ,IEND - DATA STRUCTURE DEFINING THE TRIAN-
//                        GULATION.  SEE SUBROUTINE TRMESH.
//
//                  EPS - NONNEGATIVE CONVERGENCE CRITERION.
//                        THE METHOD IS TERMINATED WHEN THE
//                        MAXIMUM CHANGE IN A GRADIENT COMPO-
//                        NENT BETWEEN ITERATIONS IS AT MOST
//                        EPS.  EPS = 1.E-2 IS SUFFICIENT FOR
//                        EFFECTIVE CONVERGENCE.
//
// THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
//                  NIT - MAXIMUM NUMBER OF GAUSS-SEIDEL
//                        ITERATIONS TO BE APPLIED.  THIS
//                        MAXIMUM WILL LIKELY BE ACHIEVED IF
//                        EPS IS SMALLER THAN THE MACHINE
//                        PRECISION.  OPTIMAL EFFICIENCY WAS
//                        ACHIEVED IN TESTING WITH EPS = 0
//                        AND NIT = 3 OR 4.
//
//                 ZXZY - 2 BY N ARRAY WHOSE COLUMNS CONTAIN
//                        INITIAL ESTIMATES OF THE PARTIAL
//                        DERIVATIVES (ZERO VECTORS ARE
//                        SUFFICIENT).
//
// OUTPUT PARAMETERS - NIT - NUMBER OF GAUSS-SEIDEL ITERA-
//                           TIONS EMPLOYED.
//
//                    ZXZY - ESTIMATED X AND Y PARTIAL DERIV-
//                           ATIVES AT THE NODES WITH X PAR-
//                           TIALS IN THE FIRST ROW.  ZXZY IS
//                           NOT CHANGED IF IER = 2.
//
//                     IER - ERROR INDICATOR
//                           IER = 0 IF THE CONVERGENCE CRI-
//                                   TERION WAS ACHIEVED.
//                           IER = 1 IF CONVERGENCE WAS NOT
//                                   ACHIEVED WITHIN NIT
//                                   ITERATIONS.
//                           IER = 2 IF N OR EPS IS OUT OF
//                                   RANGE OR NIT .LT. 0 ON
//                                   INPUT.
//
// MODULES REFERENCED BY GRADG - NONE
//
// INTRINSIC FUNCTIONS CALLED BY GRADG - SQRT, AMAX1, ABS
//
//***********************************************************
//)
procedure GRADG (const N: longint; const X,Y,Z: TRealArray;
    const IADJ: TIntArray; const IEND: TIntArray; const EPS: Real;
    var NIT: longint; var ZXZY: TRealArray; var IER: longint);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
// THIS ROUTINE COMPUTES THE VALUE AT (PX,PY) OF A PIECEWISE
// LINEAR SURFACE WHICH INTERPOLATES DATA VALUES AT THE
// VERTICES OF THE TRIANGLES.  THE SURFACE IS EXTENDED IN A
// CONTINUOUS FASHION BEYOND THE BOUNDARY OF THE TRIANGULAR
// MESH, ALLOWING EXTRAPOLATION.  INTRC0 IS PART OF AN
// INTERPOLATION PACKAGE WHICH PROVIDES ROUTINES TO GENERATE,
// UPDATE, AND PLOT THE MESH.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//                            N .GE. 3.
//
//                    PX,PY - POINT AT WHICH THE INTERPOLATED
//                            VALUE IS DESIRED.
//
//                      X,Y - VECTORS OF COORDINATES OF THE
//                            NODES IN THE MESH.
//
//                        Z - VECTOR OF DATA VALUES AT THE
//                            NODES.
//
//                     IADJ - SET OF ADJACENCY LISTS OF NODES
//                            IN THE MESH.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS IN IADJ FOR
//                            EACH NODE IN THE MESH.
//
//                      IST - INDEX OF THE STARTING NODE IN
//                            THE SEARCH FOR A TRIANGLE CON-
//                            TAINING (PX,PY).  1 .LE. IST
//                            .LE. N.  THE OUTPUT VALUE OF
//                            IST FROM A PREVIOUS CALL MAY
//                            BE A GOOD CHOICE.
//
// IADJ AND IEND MAY BE CREATED BY TRMESH.
//
// INPUT PARAMETERS OTHER THAN IST ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS -  IST - INDEX OF ONE OF THE VERTICES OF
//                            THE TRIANGLE CONTAINING (PX,PY)
//                            UNLESS IER .LT. 0.
//
//                       PZ - VALUE OF THE INTERPOLATORY
//                            SURFACE AT (PX,PY) OR ZERO
//                            IF IER .LT. 0.
//
//                      IER - ERROR INDICATOR
//                            IER = 0 IF NO ERRORS WERE
//                                    ENCOUNTERED.
//                            IER = 1 IF NO ERRORS WERE EN-
//                                    COUNTERED AND EXTRAPO-
//                                    LATION WAS PERFORMED.
//                            IER = -1 IF N OR IST IS OUT OF
//                                     RANGE.
//                            IER = -2 IF THE NODES ARE COL-
//                                     LINEAR.
//
// MODULES REFERENCED BY INTRC0 - TRFIND, COORDS
//
//***********************************************************
//)
procedure INTRC0 (const N: longint; const PX,PY: Real;
  const X,Y,Z: TRealArray; const IADJ: TIntArray; const IEND: TIntArray;
  var IST: longint; var PZ: Real; var IER: longint);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A THIESSEN TRIANGULATION OF N POINTS IN THE PLANE
// WITH ASSOCIATED DATA VALUES Z, THIS SUBROUTINE ESTIMATES
// X AND Y PARTIAL DERIVATIVES AT NODE K.  THE DERIVATIVES
// ARE TAKEN TO BE THE PARTIALS AT K OF A QUADRATIC FUNCTION
// WHICH INTERPOLATES Z(K) AND FITS THE DATA VALUES AT A SET
// OF NEARBY NODES IN A WEIGHTED LEAST SQUARES SENSE. A MAR-
// QUARDT STABILIZATION FACTOR IS USED IF NECESSARY TO ENSURE
// A WELL-CONDITIONED SYSTEM AND A LINEAR FITTING FUNCTION IS
// USED IF N .LT. 6.  THUS, A UNIQUE SOLUTION EXISTS UNLESS
// THE NODES ARE COLLINEAR.
//   AN ALTERNATIVE ROUTINE, GRADG, EMPLOYS A GLOBAL METHOD
// TO COMPUTE THE PARTIAL DERIVATIVES AT ALL OF THE NODES AT
// ONCE.  THAT METHOD IS MORE EFFICIENT (WHEN ALL PARTIALS
// ARE NEEDED) AND MAY BE MORE ACCURATE, DEPENDING ON THE
// DATA.
//
// INPUT PARAMETERS - N - NUMBER OF NODES IN THE TRIANGULA-
//                        TION.  N >= 3.
//
//                    K - NODE AT WHICH DERIVATIVES ARE
//                        SOUGHT.  1 .LE. K .LE. N.
//
//                  X,Y - N-VECTORS CONTAINING THE CARTESIAN
//                        COORDINATES OF THE NODES.
//
//                    Z - N-VECTOR CONTAINING THE DATA VALUES
//                        ASSOCIATED WITH THE NODES.
//
//                 IADJ - SET OF ADJACENCY LISTS.
//
//                 IEND - POINTERS TO THE ENDS OF ADJACENCY
//                        LISTS FOR EACH NODE.
//
// IADJ AND IEND MAY BE CREATED BY SUBROUTINE TRMESH.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - DX,DY - ESTIMATED PARTIAL DERIVATIVES
//                             AT NODE K UNLESS IER .LT. 0.
//
//                       IER - ERROR INDICATOR
//                             IER .GT. 0 IF NO ERRORS WERE
//                                      ENCOUNTERED.  IER
//                                      CONTAINS THE NUMBER
//                                      OF NODES (INCLUDING
//                                      K) USED IN THE FIT.
//                                      IER = 3, 4, OR 5 IM-
//                                      PLIES A LINEAR FIT.
//                             IER = -1 IF N OR K IS OUT OF
//                                      RANGE.
//                             IER = -2 IF ALL NODES ARE
//                                      COLLINEAR.
//
// MODULES REFERENCED BY GRADL - GETNP, SETUP, GIVENS,
//                               ROTATE
//
// INTRINSIC FUNCTIONS CALLED BY GRADL - MIN0, FLOAT, SQRT,
//                                       AMIN1, ABS
//
//***********************************************************
//)
procedure GRADL (const N,K: longint; const X,Y,Z: TRealArray;
    const IADJ: TIntArray; const IEND: TIntArray; var DX,DY: Real;
    var IER: longint);

// @preformatted(
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A THIESSEN TRIANGULATION OF A SET OF POINTS IN THE
// PLANE WITH CORRESPONDING DATA VALUES, THIS ROUTINE INTERP-
// OLATES THE DATA VALUES TO A SET OF RECTANGULAR GRID POINTS
// FOR SUCH APPLICATIONS AS CONTOURING.  THE INTERPOLANT IS
// ONCE CONTINUOUSLY DIFFERENTIABLE.  EXTRAPOLATION IS PER-
// FORMED AT GRID POINTS EXTERIOR TO THE TRIANGULATION.
//
// INPUT PARAMETERS - N - NUMBER OF NODES IN THE TRIANGULA-
//                        TION.  N >= 3
//
//                X,Y,Z - N-VECTORS OF NODAL COORDINATES AND
//                        DATA VALUES.
//
//            IADJ,IEND - TRIANGULATION DATA STRUCTURE -- MAY
//                        BE CREATED BY TRMESH.
//
//                 NROW - NUMBER OF ROWS IN THE DIMENSION
//                        STATEMENT OF ZZ.
//
//                NX,NY - NUMBER OF ROWS AND COLUMNS, RESPEC-
//                        TIVELY, IN THE RECTANGULAR GRID.
//                        1 .LE. NX .LE. NROW AND 1 .LE. NY.
//
//                PX,PY - VECTORS OF LENGTH NX AND NY, RE-
//                        SPECTIVELY, CONTAINING THE COORDI-
//                        NATES OF THE GRID LINES.
//
//                IFLAG - OPTION INDICATOR
//                        IFLAG = 0 IF DERIVATIVE ESTIMATES
//                                  AT THE VERTICES OF A
//                                  TRIANGLE ARE TO BE RECOM-
//                                  PUTED FOR EACH GRID POINT
//                                  IN THE TRIANGLE AND NOT
//                                  SAVED.
//                        IFLAG = 1 IF DERIVATIVE ESTIMATES
//                                  ARE INPUT IN ZXZY.
//                        IFLAG = 2 IF DERIVATIVE ESTIMATES
//                                  ARE TO BE COMPUTED ONCE
//                                  FOR EACH NODE (BY GRADL)
//                                  AND SAVED IN ZXZY.
//
//                 ZXZY - NOT USED IF IFLAG = 0, 2 BY N ARRAY
//                        WHOSE COLUMNS CONTAIN THE X AND Y
//                        PARTIAL DERIVATIVE ESTIMATES (X
//                        PARTIALS IN THE FIRST ROW) IF
//                        IFLAG = 1, OR 2 BY N ARRAY (OR WORK
//                        SPACE OF LENGTH >= 2*N) IF IFLAG
//                        = 2.
//
// DERIVATIVE ESTIMATES MAY BE COMPUTED BY GRADL OR GRADG.
//
//                   ZZ - NROW BY NCOL ARRAY WITH NROW >=
//                        NX AND NCOL >= NY.
//
// NONE OF THE INPUT PARAMETERS ARE ALTERED EXCEPT AS
//   NOTED BELOW.
//
// OUTPUT PARAMETERS - ZXZY - 2 BY N ARRAY WHOSE COLUMNS CON-
//                            TAIN X AND Y PARTIAL DERIVATIVE
//                            ESTIMATES AT THE NODES IF IFLAG
//                            = 2 AND IER >= 0, NOT ALTERED
//                            IF IFLAG .NE. 2.
//
//                       ZZ - INTERPOLATED VALUES AT THE GRID
//                            POINTS IF IER >= 0.
//                            ZZ(I,J) = F(PX(I),PY(J)) FOR
//                            I = 1,...,NX AND J = 1,...,NY.
//
//                      IER - ERROR INDICATOR
//                            IER >= 0 IF NO ERRORS WERE
//                                       ENCOUNTERED.  IER
//                                       CONTAINS THE NUMBER
//                                       OF GRID POINTS EXT-
//                                       ERIOR TO THE TRIAN-
//                                       GULATION BOUNDARY.
//                            IER  =  -1 IF N, NX, NY, OR
//                                       IFLAG IS OUT OF
//                                       RANGE.
//                            IER  =  -2 IF THE NODES ARE
//                                       COLLINEAR.
//
// MODULES REFERENCED BY UNIF - INTRC1, TRFIND, TVAL,
//           (AND OPTIONALLY)   GRADL, GETNP, SETUP, GIVENS,
//                                AND ROTATE
//
//***********************************************************
//)
procedure UNIF (const N: longint; const X,Y,Z: TRealArray; const IADJ: TIntArray;
  const IEND: TIntArray; const NROW,NX,NY: longint; const PX: TRealArray;
  const PY: TRealArray; const IFLAG: longint; var ZXZY: TRealArray; var ZZ: TRealArray;
  var IER: longint);

// @preformatted(
//      FUNCTION VOLUME (N,X,Y,Z,IADJ,IEND)
//      INTEGER N, IADJ(1), IEND(N)
//      REAL    X(N), Y(N), Z(N)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A SET OF N DATA POINTS (X(I),Y(I)) AND FUNCTION
//C VALUES Z(I)=F(X(I),Y(I)) AND A TRIANGULATION COVERING THE
//C CONVEX HULL H OF THE DATA POINTS, THIS FUNCTION APPROXI-
//C MATES THE INTEGRAL OF F OVER H BY INTEGRATING THE PIECE-
//C WISE LINEAR INTERPOLANT OF THE DATA VALUES.  VOLUME IS
//C PART OF AN INTERPOLATION PACKAGE WHICH PROVIDES ROUTINES
//C TO CREATE, UPDATE, AND PLOT THE TRIANGULAR MESH.
//C
//C INPUT PARAMETERS -      N - NUMBER OF NODES IN THE MESH.
//C                             N .GE. 3.
//C
//C                       X,Y - VECTORS OF COORDINATES OF
//C                             THE NODES IN THE MESH.
//C
//C                         Z - VECTOR OF DATA VALUES AT THE
//C                             NODES.
//C
//C                      IADJ - SET OF ADJACENCY LISTS OF
//C                             NODES IN THE MESH.
//C
//C                      IEND - POINTERS TO THE ENDS OF
//C                             ADJACENCY LISTS IN IADJ FOR
//C                             EACH NODE IN THE MESH.
//C
//C IADJ AND IEND MAY BE CREATED BY TRMESH.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER - VOLUME - SUM OF THE VOLUMES OF THE
//C                             LINEAR INTERPOLANTS ON THE
//C                             TRIANGLES.
//C
//C MODULE REFERENCED BY VOLUME - TRVOL
//C
//C***********************************************************
//C
//)
function Volume(const N: longint; const X,Y,Z: TRealArray;
  const IADJ, IEND: TIntArray): Real;

// @preformatted(
//      INTEGER FUNCTION LOPTST (N1,N2,X,Y,IADJ,IEND)
//      INTEGER N1, N2, IADJ(1), IEND(1)
//      REAL    X(1), Y(1)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A PAIR OF INDICES DEFINING A TRIANGULATION ARC,
//C THIS FUNCTION DETERMINES WHETHER OR NOT THE ARC IS LOCALLY
//C OPTIMAL AS DEFINED BY LOGICAL FUNCTION SWPTST.
//C
//C INPUT PARAMETERS -
//C
//C       N1,N2 - X,Y INDICES OF A PAIR OF ADJACENT NODES.
//C
//C       X,Y - NODAL COORDINATES.
//C
//C       IADJ,IEND - DATA STRUCTURE CONTAINING THE TRIANGULA-
//C                   TION.  SEE SUBROUTINE TRMESH.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER -
//C
//C       LOPTST = -2 IF N1 AND N2 ARE NOT ADJACENT,
//C              = -1 IF N1-N2 IS AN INTERIOR ARC WHICH IS
//C                   NOT LOCALLY OPTIMAL,
//C              =  0 IF N1-N2 SATISFIES THE NEUTRAL CASE (THE
//C                   VERTICES OF THE CORRESPONDING QUADRILAT-
//C                   ERAL LIE ON A COMMON CIRCLE),
//C              =  1 IF N1-N2 IS A LOCALLY OPTIMAL INTERIOR
//C                   ARC,
//C              =  2 IF N1-N2 IS A BOUNDARY ARC.
//C      NOTE THAT N1-N2 IS LOCALLY OPTIMAL IFF LOPTST .GE. 0.
//C
//C MODULES REQUIRED BY LOPTST - NONE
//C
//C***********************************************************
//C
//)
FUNCTION LOPTST (const N1,N2: longint; const X,Y: TRealArray;
  const IADJ,IEND: TIntArray): INTEGER;

// @preformatted(
//      SUBROUTINE TRPRNT (N,LUNIT,X,Y,IADJ,IEND,IFLAG,IFORTRAN)
//      DLL_IMPORT ADJACENCY_SETS, NEIGHBORS, X_NODE, ADJACENCY,
//     &  Positions, BlankLine, NewPage, BOUNDARY_NODES,
//     &  OUT_OF_RANGE_TRPRNT
//      INTEGER N, LUNIT, IADJ(1), IEND(N), IFLAG
//      REAL    X(N), Y(N)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
//C THIS ROUTINE PRINTS THE ADJACENCY LISTS AND, OPTIONALLY,
//C THE NODAL COORDINATES.  THE NUMBERS OF BOUNDARY NODES,
//C TRIANGLES, AND ARCS ARE ALSO PRINTED.
//C
//C INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//C                            3 .LE. N .LE. 9999.
//C
//C                    LUNIT - LOGICAL UNIT FOR OUTPUT.  1
//C                            .LE. LUNIT .LE. 99.  OUTPUT IS
//C                            PRINTED ON UNIT 6 IF LUNIT IS
//C                            OUT OF RANGE.
//C
//C                      X,Y - VECTORS OF COORDINATES OF THE
//C                            NODES IN THE MESH.  NOT USED
//C                            UNLESS IFLAG = 0.
//C
//C                     IADJ - SET OF ADJACENCY LISTS OF NODES
//C                            IN THE MESH.
//C
//C                     IEND - POINTERS TO THE ENDS OF
//C                            ADJACENCY LISTS IN IADJ FOR
//C                            EACH NODE IN THE MESH.
//C
//C                    IFLAG - OPTION INDICATOR
//C                            IFLAG = 0 IF X AND Y ARE TO BE
//C                                      PRINTED (TO 6 DECIMAL
//C                                      PLACES).
//C                            IFLAG = 1 IF X AND Y ARE NOT
//C                                      TO BE PRINTED.
//C
//C IADJ AND IEND MAY BE CREATED BY TRMESH.
//C
//C NONE OF THE PARAMETERS ARE ALTERED BY THIS ROUTINE.
//C
//C MODULES REFERENCED BY TRPRNT - NONE
//C
//C***********************************************************
//C
//      INTEGER NN, NMAX, LUN, NODE, INDF, INDL, NL, NLMAX,
//     .        INC, I, NB, NT, NA
//      INTEGER IFORTRAN
//      DATA    NMAX/9999/, NLMAX/60/
//C
//C LOCAL PARAMETERS -
//C
//C NN =        LOCAL COPY OF N
//C NMAX =      UPPER BOUND ON N
//C LUN =       LOCAL COPY OF LUNIT
//C NODE =      INDEX OF A NODE
//C INDF,INDL = IADJ INDICES OF THE FIRST AND LAST NEIGHBORS
//C               OF NODE
//C NL =        NUMBER OF LINES PRINTED ON A PAGE
//C NLMAX =     MAXIMUM NUMBER OF PRINT LINES PER PAGE EXCEPT
//C               FOR THE LAST PAGE WHICH HAS 3 ADDITIONAL
//C               LINES
//C INC =       INCREMENT FOR NL
//C I =         IADJ INDEX FOR IMPLIED DO-LOOP
//C NB =        NUMBER OF BOUNDARY NODES
//C NT =        NUMBER OF TRIANGLES
//C NA =        NUMBER OF ARCS (UNDIRECTED EDGES)
//C
//)
procedure TRPRNT (const N, LUNIT: longint;
   const X,Y: TRealArray; const IADJ: TIntArray;
  const IEND: TIntArray; const IFLAG: longint);
  
implementation

Uses Math, TriPackMessages;

{$WARNINGS OFF}

function TRVOL (const X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3: Real): Real;
//      FUNCTION TRVOL (X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3)
//      REAL X1, X2, X3, Y1, Y2, Y3, Z1, Z2, Z3
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   THIS FUNCTION COMPUTES THE INTEGRAL OVER A TRIANGLE OF
//C THE PLANAR SURFACE WHICH INTERPOLATES DATA VALUES AT THE
//C VERTICES.
//C
//C INPUT PARAMETERS - X1,X2,X3 - X COORDINATES OF THE
//C                               VERTICES OF THE TRIANGLE IN
//C                               COUNTERCLOCKWISE ORDER.
//C
//C                    Y1,Y2,Y3 - Y COORDINATES OF THE
//C                               VERTICES OF THE TRIANGLE IN
//C                               THE SAME ORDER AS THE X
//C                               COORDINATES.
//C
//C                    Z1,Z2,Z3 - DATA VALUES AT THE VERTICES
//C                               IN THE SAME ORDER AS THE
//C                               COORDINATES.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER -    TRVOL - VOLUME UNDER THE INTERPOLA-
//C                               TORY SURFACE ABOVE THE
//C                               TRIANGLE OR ZERO IF THE
//C                               COORDINATES ARE INCORRECTLY
//C                               ORDERED OR COLLINEAR.
//C
//C MODULES REFERENCED BY TRVOL - NONE
//C
//C***********************************************************
//C
var
  T1, T2, T3, AREA: Real;
begin
      T1 := X2*Y3 - X3*Y2;
      T2 := X3*Y1 - X1*Y3;
      T3 := X1*Y2 - X2*Y1;
      AREA := T1 + T2 + T3;
      IF (AREA < 0) then AREA := 0.;


//      T1 = X2*Y3 - X3*Y2
//      T2 = X3*Y1 - X1*Y3
//      T3 = X1*Y2 - X2*Y1
//      AREA = T1 + T2 + T3
//      IF (AREA .LT. 0.) AREA = 0.
//C
//C AREA IS TWICE THE AREA OF THE TRIANGLE.  TRVOL IS THE MEAN
//C   OF THE DATA VALUES TIMES THE AREA OF THE TRIANGLE.
//C
      result := (Z1 + Z2 + Z3)*AREA/6.
//      TRVOL = (Z1 + Z2 + Z3)*AREA/6.
//      RETURN
//      END
end;

function Volume(const N: longint; const X,Y,Z: TRealArray;
  const IADJ, IEND: TIntArray): Real;
//      FUNCTION VOLUME (N,X,Y,Z,IADJ,IEND)
//      INTEGER N, IADJ(1), IEND(N)
//      REAL    X(N), Y(N), Z(N)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A SET OF N DATA POINTS (X(I),Y(I)) AND FUNCTION
//C VALUES Z(I)=F(X(I),Y(I)) AND A TRIANGULATION COVERING THE
//C CONVEX HULL H OF THE DATA POINTS, THIS FUNCTION APPROXI-
//C MATES THE INTEGRAL OF F OVER H BY INTEGRATING THE PIECE-
//C WISE LINEAR INTERPOLANT OF THE DATA VALUES.  VOLUME IS
//C PART OF AN INTERPOLATION PACKAGE WHICH PROVIDES ROUTINES
//C TO CREATE, UPDATE, AND PLOT THE TRIANGULAR MESH.
//C
//C INPUT PARAMETERS -      N - NUMBER OF NODES IN THE MESH.
//C                             N .GE. 3.
//C
//C                       X,Y - VECTORS OF COORDINATES OF
//C                             THE NODES IN THE MESH.
//C
//C                         Z - VECTOR OF DATA VALUES AT THE
//C                             NODES.
//C
//C                      IADJ - SET OF ADJACENCY LISTS OF
//C                             NODES IN THE MESH.
//C
//C                      IEND - POINTERS TO THE ENDS OF
//C                             ADJACENCY LISTS IN IADJ FOR
//C                             EACH NODE IN THE MESH.
//C
//C IADJ AND IEND MAY BE CREATED BY TRMESH.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER - VOLUME - SUM OF THE VOLUMES OF THE
//C                             LINEAR INTERPOLANTS ON THE
//C                             TRIANGLES.
//C
//C MODULE REFERENCED BY VOLUME - TRVOL
//C
//C***********************************************************
//C
var
  NN, NM2, N1, N2, N3, INDF, INDL, INDX: integer;
  SUM, XN1, YN1, ZN1: Real;
//      INTEGER NN, NM2, N1, N2, N3, INDF, INDL, INDX
//      REAL    SUM, XN1, YN1, ZN1
//C
//C LOCAL PARAMETERS -
//C
//C NN =          LOCAL COPY OF N
//C NM2 =         N-2
//C N1,N2,N3 =    VERTICES OF A TRIANGLE IN COUNTERCLOCKWISE
//C                 ORDER
//C INDF =        IADJ INDEX OF THE FIRST NEIGHBOR OF N1
//C INDL =        IADJ INDEX OF THE LAST NEIGHBOR OF N1
//C INDX =        IADJ INDEX VARYING FROM INDF TO INDL
//C SUM =         TEMPORARY STORAGE FOR ACCUMULATED VOLUME
//C XN1,YN1,ZN1 = X(N1), Y(N1), Z(N1) -- STORED LOCALLY FOR
//C                 EFFICIENCY
//C
begin
  NN := N;
  result := 0;
//      NN = N
  if NN >= 3 then
  begin
  
//      IF (NN .LT. 3) GO TO 3
//C
//C INITIALIZATION
//C
      NM2 := NN-2;
      INDF := 0;
      SUM := 0.;
//      NM2 = NN-2
//      INDF = 1
//      SUM = 0.
//C
//C LOOP ON TRIANGLES (N1,N2,N3) SUCH THAT N2 AND N3 ARE
//C   ADJACENT NEIGHBORS OF N1 WHICH ARE BOTH LARGER THAN N1
//C
//      DO 2 N1 = 1,NM2
      for N1 := 0 to NM2 - 1 do
      begin

        XN1 := X[N1];
        YN1 := Y[N1];
        ZN1 := Z[N1];
        INDL := IEND[N1]-1;

//        XN1 = X(N1)
//        YN1 = Y(N1)
//        ZN1 = Z(N1)
//        INDL = IEND(N1)
//        DO 1 INDX = INDF,INDL
        for INDX := INDF to INDL do
        begin

          N2 := IADJ[INDX]-1;
          N3 := IADJ[INDX+1]-1;
          IF (INDX = INDL) then N3 := IADJ[INDF]-1;
          IF ((N2 < N1) or (N3 < N1)) then Continue;
          SUM := SUM + TRVOL(XN1,X[N2],X[N3],YN1,Y[N2],Y[N3],
                           ZN1,Z[N2],Z[N3]);

//          N2 = IADJ(INDX)
//          N3 = IADJ(INDX+1)
//          IF (INDX .EQ. INDL) N3 = IADJ(INDF)
//          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 1
//          SUM = SUM + TRVOL(XN1,X(N2),X(N3),YN1,Y(N2),Y(N3),
//     .                      ZN1,Z(N2),Z(N3))
        end;
//    1     CONTINUE
        INDF := INDL+1;
//        INDF = INDL + 1
      end;
//    2   CONTINUE
//C
      result := SUM;
//      VOLUME = SUM
//      RETURN
//C
//C N IS OUT OF RANGE
//C
//    3 VOLUME = 0.
  end;
//      RETURN
//      END
end;


function AREA (const X,Y: TRealArray;  const NB: longint;
    const NODES: TIntArray): Real;
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A SEQUENCE OF NB POINTS IN THE PLANE, THIS
//C FUNCTION COMPUTES THE AREA BOUNDED BY THE CLOSED POLY-
//C GONAL CURVE WHICH PASSES THROUGH THE POINTS IN THE
//C SPECIFIED ORDER.  EACH SIMPLE CLOSED CURVE IS POSITIVELY
//C ORIENTED (BOUNDS POSITIVE AREA) IF AND ONLY IF THE POINTS
//C ARE SPECIFIED IN COUNTERCLOCKWISE ORDER.  THE LAST POINT
//C OF THE CURVE IS TAKEN TO BE THE FIRST POINT SPECIFIED, AND
//C THUS THIS POINT NEED NOT BE SPECIFIED TWICE.  HOWEVER, ANY
//C POINT MAY BE SPECIFIED MORE THAN ONCE IN ORDER TO DEFINE A
//C MULTIPLY CONNECTED DOMAIN.
//C   THE AREA OF A TRIANGULATION MAY BE COMPUTED BY CALLING
//C AREA WITH VALUES OF NB AND NODES DETERMINED BY SUBROUTINE
//C BNODES.
//C
//C INPUT PARAMETERS -   X,Y - N-VECTORS OF COORDINATES OF
//C                            POINTS IN THE PLANE FOR N >=
//C                            NB.  NODE I HAS COORDINATES
//C                            (X(I),Y(I)) FOR I = 1, 2, ...,
//C                            N.
//C
//C                       NB - LENGTH OF NODES.
//C
//C                    NODES - VECTOR OF NODE INDICES IN THE
//C                            RANGE 1 TO N DEFINING THE
//C                            POLYGONAL CURVE.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER -  AREA - SIGNED AREA BOUNDED BY THE
//C                            POLYGONAL CURVE DEFINED
//C                            ABOVE.
//C
//C MODULES REFERENCED BY AREA - NONE
//C
//C***********************************************************
var
  NNB, ND, I: longint;
  A, X0, Y0, DX1, DY1, DX2, DY2: Real;
begin
//C
//C LOCAL PARAMETERS -
//C
//C NNB =     LOCAL COPY OF NB
//C ND =      ELEMENT OF NODES
//C I =       DO-LOOP AND NODES INDEX
//C A =       PARTIAL SUM OF SIGNED (AND DOUBLED) TRIANGLE
//C             AREAS
//C X0,Y0 =   X(NODES(1)), Y(NODES(1))
//C DX1,DY1 = COMPONENTS OF THE VECTOR NODES(1)-NODES(I) FOR
//C             I = 2, 3, ..., NB-1
//C DX2,DY2 = COMPONENTS OF THE VECTOR NODES(1)-NODES(I) FOR
//C             I = 3, 4, ..., NB

  NNB := NB;
  A := 0.;
  IF (NNB >= 3) then
  begin

    // INITIALIZATION

    ND := NODES[0];
    X0 := X[ND-1];
    Y0 := Y[ND-1];
    ND := NODES[1];
    DX1 := X[ND-1] - X0;
    DY1 := Y[ND-1] - Y0;

    // LOOP ON TRIANGLES (NODES(1),NODES(I),NODES(I+1)),
    //   I = 2, 3, ..., NB-1, ADDING TWICE THEIR SIGNED
    //   AREAS TO A

    for I := 2 to NNB - 1 do
    begin
      ND := NODES[I];
      DX2 := X[ND-1] - X0;
      DY2 := Y[ND-1] - Y0;
      A := A + DX1*DY2 - DX2*DY1;
      DX1 := DX2;
      DY1 := DY2;
    end;

  // A CONTAINS TWICE THE SIGNED AREA OF THE REGION
  end;
  result := A/2;
end;

procedure BNODES (const N: longint; const IADJ: TIntArray;
    const IEND: TIntArray; var NB,NA,NT: longint; var NODES: TIntArray);
//
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF N POINTS IN THE PLANE, THIS
// ROUTINE RETURNS A VECTOR CONTAINING THE INDICES, IN
// COUNTERCLOCKWISE ORDER, OF THE NODES ON THE BOUNDARY OF
// THE CONVEX HULL OF THE SET OF POINTS.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//
//                     IADJ - SET OF ADJACENCY LISTS OF
//                            NODES IN THE MESH.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS IN IADJ FOR
//                            EACH NODE IN THE MESH.
//
//                    NODES - VECTOR OF LENGTH >= NB.
//                            (NB <= N).
//
//   IADJ AND IEND MAY BE CREATED BY TRMESH AND ARE NOT
// ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS -   NB - NUMBER OF BOUNDARY NODES.
//
//                    NA,NT - NUMBER OF ARCS AND TRIANGLES,
//                            RESPECTIVELY, IN THE MESH.
//
//                    NODES - VECTOR OF NB BOUNDARY NODE
//                            INDICES RANGING FROM 1 TO N.
//
// MODULES REFERENCED BY BNODES - NONE
//
//***********************************************************
//
var
  NST, INDL, K, N0, INDF: longint;
//
// LOCAL PARAMETERS -
//
// NST =  FIRST ELEMENT OF NODES -- ARBITRARILY CHOSEN
// INDL = IADJ INDEX OF THE LAST NEIGHBOR OF NST
// K =    NODES INDEX
// N0 =   BOUNDARY NODE TO BE ADDED TO NODES
// INDF = IADJ INDEX OF THE FIRST NEIGHBOR OF N0
begin
  // SET NST TO THE FIRST BOUNDARY NODE ENCOUNTERED

  NST := 1;
  repeat
     INDL := IEND[NST-1];
     IF (IADJ[INDL-1] = 0) then break;
     Inc(NST);
  until (False);

  // INITIALIZATION

  NODES[0] := NST;
  K := 1;
  N0 := NST;

  // TRAVERSE THE BOUNDARY IN COUNTERCLOCKWISE ORDER

  repeat
    INDF := 1;
    IF (N0 > 1) then INDF := IEND[N0-2] + 1;
    N0 := IADJ[INDF-1];
    IF (N0 = NST) then
    begin
      break;
    end;
    K := K + 1;
    NODES[K-1] := N0;
  until (False);

  // TERMINATION

  NB := K;
  NT := 2*N - NB - 2;
  NA := NT + N - 1;
end;

FUNCTION INDEX_Pascal (const NVERTX,NABOR: longint;
    const IADJ: TIntArray; const IEND: TIntArray):longint;
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS FUNCTION RETURNS THE INDEX OF NABOR IN THE
// ADJACENCY LIST FOR NVERTX.
//
// INPUT PARAMETERS - NVERTX - NODE WHOSE ADJACENCY LIST IS
//                             TO BE SEARCHED.
//
//                     NABOR - NODE WHOSE INDEX IS TO BE
//                             RETURNED.  NABOR MUST BE
//                             CONNECTED TO NVERTX.
//
//                      IADJ - SET OF ADJACENCY LISTS.
//
//                      IEND - POINTERS TO THE ENDS OF
//                             ADJACENCY LISTS IN IADJ.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//
// OUTPUT PARAMETER -  INDEX - IADJ(INDEX) = NABOR.
//
// MODULES REFERENCED BY INDEX - NONE
//
//***********************************************************
var
  NB, INDX: longint;
// LOCAL PARAMETERS -
//
// NB =   LOCAL COPY OF NABOR
// INDX = INDEX FOR IADJ
begin
  NB := NABOR;

  // INITIALIZATION

  INDX := IEND[NVERTX-1] + 1;

  // SEARCH THE LIST OF NVERTX NEIGHBORS FOR NB

  repeat
    INDX := INDX - 1;
  until (IADJ[INDX-1] = NB);

  result := INDX;
end;

procedure GETNP (const X,Y: TRealArray; const IADJ: TIntArray;
  const IEND: TIntArray; const L: longint; var NPTS: TIntArray;
  var DS: Real; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A THIESSEN TRIANGULATION OF N NODES AND AN ARRAY
// NPTS CONTAINING THE INDICES OF L-1 NODES ORDERED BY
// EUCLIDEAN DISTANCE FROM NPTS(1), THIS SUBROUTINE SETS
// NPTS(L) TO THE INDEX OF THE NEXT NODE IN THE SEQUENCE --
// THE NODE, OTHER THAN NPTS(1),...,NPTS(L-1), WHICH IS
// CLOSEST TO NPTS(1).  THUS, THE ORDERED SEQUENCE OF K
// CLOSEST NODES TO N1 (INCLUDING N1) MAY BE DETERMINED BY
// K-1 CALLS TO GETNP WITH NPTS(1) = N1 AND L = 2,3,...,K
// FOR K >= 2.
//   THE ALGORITHM USES THE FACT THAT, IN A THIESSEN TRIAN-
// GULATION, THE K-TH CLOSEST NODE TO A GIVEN NODE N1 IS A
// NEIGHBOR OF ONE OF THE K-1 CLOSEST NODES TO N1.
//
// INPUT PARAMETERS - X,Y - VECTORS OF LENGTH N CONTAINING
//                          THE CARTESIAN COORDINATES OF THE
//                          NODES.
//
//                   IADJ - SET OF ADJACENCY LISTS OF NODES
//                          IN THE TRIANGULATION.
//
//                   IEND - POINTERS TO THE ENDS OF ADJACENCY
//                          LISTS FOR EACH NODE IN THE TRI-
//                          ANGULATION.
//
//                      L - NUMBER OF NODES IN THE SEQUENCE
//                          ON OUTPUT.  2 .LE. L .LE. N.
//
//                   NPTS - ARRAY OF LENGTH >= L CONTAIN-
//                          ING THE INDICES OF THE L-1 CLOS-
//                          EST NODES TO NPTS(1) IN THE FIRST
//                          L-1 LOCATIONS.
//
// IADJ AND IEND MAY BE CREATED BY SUBROUTINE TRMESH.
//
// INPUT PARAMETERS OTHER THAN NPTS ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS - NPTS - UPDATED WITH THE INDEX OF THE
//                            L-TH CLOSEST NODE TO NPTS(1) IN
//                            POSITION L UNLESS IER = 1.
//
//                       DS - SQUARED EUCLIDEAN DISTANCE BE-
//                            TWEEN NPTS(1) AND NPTS(L)
//                            UNLESS IER = 1.
//
//                      IER - ERROR INDICATOR
//                            IER = 0 IF NO ERRORS WERE EN-
//                                    COUNTERED.
//                            IER = 1 IF L IS OUT OF RANGE.
//
// MODULES REFERENCED BY GETNP - NONE
//
// INTRINSIC FUNCTION CALLED BY GETNP - IABS
//
//***********************************************************
//
var
  LM1, N1, I, NI, NP, INDF, INDL, INDX, NB: longint;
  X1, Y1, DNP, DNB: Real;
// LOCAL PARAMETERS -
//
// LM1 =     L - 1
// N1 =      NPTS(1)
// I =       NPTS INDEX AND DO-LOOP INDEX
// NI =      NPTS(I)
// NP =      CANDIDATE FOR NPTS(L)
// INDF =    IADJ INDEX OF THE FIRST NEIGHBOR OF NI
// INDL =    IADJ INDEX OF THE LAST NEIGHBOR OF NI
// INDX =    IADJ INDEX IN THE RANGE INDF,...,INDL
// NB =      NEIGHBOR OF NI AND CANDIDATE FOR NP
// X1,Y1 =   COORDINATES OF N1
// DNP,DNB = SQUARED DISTANCES FROM N1 TO NP AND NB,
//             RESPECTIVELY
begin
  LM1 := L - 1;
  IF (LM1 < 1) then
  begin
  // L IS OUT OF RANGE
    IER := 1;
    Exit;
  end;
  IER := 0;
  N1 := NPTS[0];
  X1 := X[N1-1];
  Y1 := Y[N1-1];

  // MARK THE ELEMENTS OF NPTS

  for I := 0 to LM1 - 1 do
  begin
    NI := NPTS[I];
    IEND[NI-1] := -IEND[NI-1];
  end;

  // CANDIDATES FOR NP = NPTS(L) ARE THE UNMARKED NEIGHBORS
  //   OF NODES IN NPTS.  NP=0 IS A FLAG TO SET NP TO THE
  //   FIRST CANDIDATE ENCOUNTERED.

  NP := 0;
  DNP := 0.;

  //C LOOP ON NODES NI IN NPTS

  for I := 0 to LM1 - 1 do
  begin
    NI := NPTS[I];
    INDF := 1;
    IF (NI > 1) then INDF := ABS(IEND[NI-2]) + 1;
    INDL := -IEND[NI-1];

  // LOOP ON NEIGHBORS NB OF NI

    for INDX := INDF-1 to INDL - 1 do
    begin

      NB := IADJ[INDX];
      IF (NB = 0)  OR  (IEND[NB-1] < 0) then
      begin
        Continue;
      end;

  //C NB IS AN UNMARKED NEIGHBOR OF NI.  REPLACE NP IF NB IS
  //C   CLOSER TO N1 OR IS THE FIRST CANDIDATE ENCOUNTERED.

      DNB := Sqr(X[NB-1]-X1) + Sqr(Y[NB-1]-Y1);
      IF ((NP <> 0)  AND  (DNB >= DNP)) then
      begin
        Continue;
      end;

      NP := NB;
      DNP := DNB;
    end;
  end;

  NPTS[L-1] := NP;
  DS := DNP;

  //C UNMARK THE ELEMENTS OF NPTS

  for I := 0 to LM1 - 1 do
  begin
    NI := NPTS[I];
    IEND[NI-1] := -IEND[NI-1];
  end;
end;

type
  TRealSorter = class(TObject)
    Real1: Real;
    Real2: Real;
    Int: longint;
  end;

function SortReals(Item1, Item2: Pointer): Integer;
var
  AReal1: TRealSorter;
  AReal2: TRealSorter;
  Difference: double;
begin
  AReal1 := Item1;
  AReal2 := Item2;
  Difference := AReal1.Real1 - AReal2.Real1;
  if Difference < 0 then
  begin
    result := -1;
  end
  else if Difference > 0 then
  begin
    result := 1;
  end
  else
  begin
    Difference := AReal1.Real2 - AReal2.Real2;
    if Difference < 0 then
    begin
      result := -1;
    end
    else if Difference > 0 then
    begin
      result := 1;
    end
    else
    begin
      result := 0;
    end;
  end
end;

procedure QSORT (const N: longint; const X, Y: TRealArray;
  var IND: TIntArray);  stdcall;
// RBW: The comments are from the original procedure but the source code is new.
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO
// SORT THE REAL ARRAY X INTO INCREASING ORDER.  THE ALGOR-
// ITHM IS AS FOLLOWS.  IND IS INITIALIZED TO THE ORDERED
// SEQUENCE OF INDICES 1,...,N, AND ALL INTERCHANGES ARE
// APPLIED TO IND.  X IS DIVIDED INTO TWO PORTIONS BY PICKING
// A CENTRAL ELEMENT T.  THE FIRST AND LAST ELEMENTS ARE COM-
// PARED WITH T, AND INTERCHANGES ARE APPLIED AS NECESSARY SO
// THAT THE THREE VALUES ARE IN ASCENDING ORDER.  INTER-
// CHANGES ARE THEN APPLIED SO THAT ALL ELEMENTS GREATER THAN
// T ARE IN THE UPPER PORTION OF THE ARRAY AND ALL ELEMENTS
// LESS THAN T ARE IN THE LOWER PORTION.  THE UPPER AND LOWER
// INDICES OF ONE OF THE PORTIONS ARE SAVED IN LOCAL ARRAYS,
// AND THE PROCESS IS REPEATED ITERATIVELY ON THE OTHER
// PORTION.  WHEN A PORTION IS COMPLETELY SORTED, THE PROCESS
// BEGINS AGAIN BY RETRIEVING THE INDICES BOUNDING ANOTHER
// UNSORTED PORTION.
//
// INPUT PARAMETERS -   N - LENGTH OF THE ARRAY X.
//
//                      X - VECTOR OF LENGTH N TO BE SORTED.
//
//                    IND - VECTOR OF LENGTH >= N.
//
// N AND X ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETER - IND - SEQUENCE OF INDICES 1,...,N
//                          PERMUTED IN THE SAME FASHION AS X
//                          WOULD BE.  THUS, THE ORDERING ON
//                          X IS DEFINED BY Y(I) = X(IND(I)).
//
// MODULES REFERENCED BY QSORT - NONE
//
// INTRINSIC FUNCTIONS CALLED BY QSORT - IFIX, FLOAT
//
//***********************************************************
var
  Index: integer;
  List: TList;
  IntStorage: TRealSorter;
begin
  List := TList.Create;
  try
    List.Capacity := N;
    for Index := 0 to N - 1 do
    begin
      IntStorage := TRealSorter.Create;
      IntStorage.Int := Index + 1;
      IntStorage.Real1 := X[Index];
      IntStorage.Real2 := Y[Index];
      List.Add(IntStorage);
    end;
    List.Sort(SortReals);
    for Index := 0 to N - 1 do
    begin
      IntStorage := List[Index];
      IND[Index] := IntStorage.Int;
    end;
  finally
    for Index := 0 to N - 1 do
    begin
      IntStorage := List[Index];
      IntStorage.Free;
    end;
    List.Free;
  end;
end;

procedure PERMUT (const NN: longint; const IP: TIntArray; var A: TRealArray);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE APPLIES A SET OF PERMUTATIONS TO A VECTOR.
//
// INPUT PARAMETERS - NN - LENGTH OF A AND IP.
//
//                    IP - VECTOR CONTAINING THE SEQUENCE OF
//                         INTEGERS 1,...,NN PERMUTED IN THE
//                         SAME FASHION THAT A IS TO BE PER-
//                         MUTED.
//
//                     A - VECTOR TO BE PERMUTED.
//
// NN AND IP ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - A - REORDERED VECTOR REFLECTING THE
//                         PERMUTATIONS DEFINED BY IP.
//
// MODULES REFERENCED BY PERMUT - NONE
//
//***********************************************************
var
  N, K, J, IPJ: longint;
  TEMP: Real;
  ContinueLoop: Boolean;
//C LOCAL PARAMETERS -
//C
//C N =    LOCAL COPY OF NN
//C K =    INDEX FOR IP AND FOR THE FIRST ELEMENT OF A IN A
//C          PERMUTATION
//C J =    INDEX FOR IP AND A, J >= K
//C IPJ =  IP(J)
//C TEMP = TEMPORARY STORAGE FOR A(K)
begin
  N := NN;
  IF (N < 2) then
  begin
    Exit;
  end;
  K := 1;

  //C LOOP ON PERMUTATIONS

  repeat
    J := K;
    TEMP := A[K-1];

  //C APPLY PERMUTATION TO A.  IP(J) IS MARKED (MADE NEGATIVE)
  //C   AS BEING INCLUDED IN THE PERMUTATION.

    repeat
      IPJ := IP[J-1];
      IP[J-1] := -IPJ;
      IF (IPJ = K) then
      begin
        break;
      end;
      A[J-1] := A[IPJ-1];
      J := IPJ;
    until (False);

    A[J-1] := TEMP;

  //C SEARCH FOR AN UNMARKED ELEMENT OF IP

    ContinueLoop := False;
    repeat

      K := K + 1;
      IF (K > N) then
      begin
        break;
      end;
      IF (IP[K-1] > 0) then
      begin
        ContinueLoop := True;
        break;
      end;
    until (False);
    if not ContinueLoop then
    begin
      break;
    end;
  until False;

  //C ALL PERMUTATIONS HAVE BEEN APPLIED.  UNMARK IP.

  for K := 0 to N-1 do
  begin
    IP[K] := -IP[K];
  end;
end;


Procedure REORDR (const N,IFLAG: longint; var A,B,C: TRealArray;
  var IND: TIntArray);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO
// REORDER THE REAL ARRAY A INTO INCREASING ORDER.  A RECORD
// OF THE PERMUTATIONS APPLIED TO A IS STORED IN IND, AND
// THESE PERMUTATIONS MAY BE APPLIED TO ONE OR TWO ADDITIONAL
// VECTORS BY THIS ROUTINE.  ANY OTHER VECTOR V MAY BE PER-
// MUTED IN THE SAME FASHION BY CALLING SUBROUTINE PERMUT
// WITH N, IND, AND V AS PARAMETERS.
//   A SET OF NODES (X(I),Y(I)) AND DATA VALUES Z(I) MAY BE
// PREPROCESSED BY REORDR FOR INCREASED EFFICIENCY IN THE
// TRIANGULATION ROUTINE TRMESH.  EFFICIENCY IS INCREASED BY
// A FACTOR OF APPROXIMATELY SQRT(N)/6 FOR RANDOMLY DISTRIB-
// UTED NODES, AND THE PREPROCESSING IS ALSO USEFUL FOR
// DETECTING DUPLICATE NODES.  EITHER X OR Y MAY BE USED AS
// THE SORT KEY (ASSOCIATED WITH A).
//
// INPUT PARAMETERS - N - NUMBER OF NODES.
//
//                IFLAG - NUMBER OF VECTORS TO BE PERMUTED.
//                        IFLAG .LE. 0 IF A, B, AND C ARE TO
//                                     REMAIN UNALTERED.
//                        IFLAG .EQ. 1 IF ONLY A IS TO BE
//                                     PERMUTED.
//                        IFLAG .EQ. 2 IF A AND B ARE TO BE
//                                     PERMUTED.
//                        IFLAG >= 3 IF A, B, AND C ARE TO
//                                     BE PERMUTED.
//
//                A,B,C - VECTORS OF LENGTH N TO BE SORTED
//                        (ON THE COMPONENTS OF A), OR DUMMY
//                        PARAMETERS, DEPENDING ON IFLAG.
//
//                  IND - VECTOR OF LENGTH >= N.
//
// N, IFLAG, AND ANY DUMMY PARAMETERS ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS - A,B,C - SORTED OR UNALTERED VECTORS.
//
//                       IND - SEQUENCE OF INDICES 1,...,N
//                             PERMUTED IN THE SAME FASHION
//                             AS THE REAL VECTORS.  THUS,
//                             THE ORDERING MAY BE APPLIED TO
//                             A REAL VECTOR V AND STORED IN
//                             W BY SETTING W(I) = V(IND(I)),
//                             OR V MAY BE OVERWRITTEN WITH
//                             THE ORDERING BY A CALL TO PER-
//                             MUT.
//
// MODULES REFERENCED BY REORDR - QSORT, PERMUT
//
//***********************************************************
var
  NN, NV: longint;
// LOCAL PARAMETERS -
//
// NN = LOCAL COPY OF N
// NV = LOCAL COPY OF IFLAG
begin
  NN := N;
  NV := IFLAG;

  //rbw begin new code
  Assert(NV >= 2);
  QSORT(NN,A,B, IND);
  // rbw end new code
  IF (NV <= 0) then
  begin
    Exit;
  end;

  PERMUT(NN,IND, A);
  IF (NV = 1) then
  begin
    Exit;
  end;

  PERMUT(NN,IND, B);
  IF (NV = 2) then
  begin
    Exit;
  end;

  PERMUT(NN,IND, C);
end;


procedure SHIFTD (const NFRST,NLAST,KK: longint; var IARR: TIntArray);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE SHIFTS A SET OF CONTIGUOUS ELEMENTS OF AN
// INTEGER ARRAY KK POSITIONS DOWNWARD (UPWARD IF KK .LT. 0).
// THE LOOPS ARE UNROLLED IN ORDER TO INCREASE EFFICIENCY.
//
// INPUT PARAMETERS - NFRST,NLAST - BOUNDS ON THE PORTION OF
//                                  IARR TO BE SHIFTED.  ALL
//                                  ELEMENTS BETWEEN AND
//                                  INCLUDING THE BOUNDS ARE
//                                  SHIFTED UNLESS NFRST .GT.
//                                  NLAST, IN WHICH CASE NO
//                                  SHIFT OCCURS.
//
//                             KK - NUMBER OF POSITIONS EACH
//                                  ELEMENT IS TO BE SHIFTED.
//                                  IF KK .LT. 0 SHIFT UP.
//                                  IF KK .GT. 0 SHIFT DOWN.
//
//                           IARR - INTEGER ARRAY OF LENGTH
//                                  >= NLAST + MAX(KK,0).
//
// NFRST, NLAST, AND KK ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETER -        IARR - SHIFTED ARRAY.
//
// MODULES REFERENCED BY SHIFTD - NONE
//
//***********************************************************
const
  INC = 5;
var
  K, NF, NL, NLP1, NS, NSL, I, IBAK, INDX, IMAX: longint;
// LOCAL PARAMETERS -
//
// INC =  DO-LOOP INCREMENT (UNROLLING FACTOR) -- IF INC IS
//          CHANGED, STATEMENTS MUST BE ADDED TO OR DELETED
//          FROM THE DO-LOOPS
// K =    LOCAL COPY OF KK
// NF =   LOCAL COPY OF NFRST
// NL =   LOCAL COPY OF NLAST
// NLP1 = NL + 1
// NS =   NUMBER OF SHIFTS
// NSL =  NUMBER OF SHIFTS DONE IN UNROLLED DO-LOOP (MULTIPLE
//          OF INC)
// I =    DO-LOOP INDEX AND INDEX FOR IARR
// IBAK = INDEX FOR DOWNWARD SHIFT OF IARR
// INDX = INDEX FOR IARR
// IMAX = BOUND ON DO-LOOP INDEX
begin
  K := KK;
  NF := NFRST;
  NL := NLAST;
  IF (NF > NL)  OR  (K = 0) then
  begin
    Exit;
  end;
  NLP1 := NL + 1;
  NS := NLP1 - NF;
  NSL := INC*(NS div INC);
  IF ( K >= 0) then
  begin

  //C SHIFT DOWNWARD STARTING FROM THE BOTTOM

    IF (NSL > 0) then
    begin
      for I := 1 to (NSL+Inc-1) div Inc do
      begin
        IBAK := NLP1 - ((I-1)*Inc+1);
        INDX := IBAK + K;
        IARR[INDX-1] := IARR[IBAK-1];
        IARR[INDX-1-1] := IARR[IBAK-1-1];
        IARR[INDX-2-1] := IARR[IBAK-2-1];
        IARR[INDX-3-1] := IARR[IBAK-3-1];
        IARR[INDX-4-1] := IARR[IBAK-4-1];
      end;
    end;

  //C PERFORM THE REMAINING NS-NSL SHIFTS ONE AT A TIME

    IBAK := NLP1 - NSL;
    while (IBAK > NF) do
    begin
      IBAK := IBAK - 1;
      INDX := IBAK + K;
      IARR[INDX-1] := IARR[IBAK-1];
    end;
    Exit;
  end;

  //C SHIFT UPWARD STARTING FROM THE TOP

  IF (NSL > 0) then
  begin
    IMAX := NLP1 - INC;
    I := NF;
    while I <= IMAX do
    begin
      INDX := I + K;
      IARR[INDX-1] := IARR[I-1];
      IARR[INDX+1-1] := IARR[I+1-1];
      IARR[INDX+2-1] := IARR[I+2-1];
      IARR[INDX+3-1] := IARR[I+3-1];
      IARR[INDX+4-1] := IARR[I+4-1];
      I := I + INC;
    end;
  end;

  //C PERFORM THE REMAINING NS-NSL SHIFTS ONE AT A TIME

  I := NSL + NF;
  while (I <= NL) do
  begin
    INDX := I + K;
    IARR[INDX-1] := IARR[I-1];
    I := I + 1;
  end;
end;

FUNCTION SWPTST (const IN1,IN2,IO1,IO2: longint; const X,Y: TRealArray): boolean;
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS FUNCTION DECIDES WHETHER OR NOT TO REPLACE A
// DIAGONAL ARC IN A QUADRILATERAL WITH THE OTHER DIAGONAL.
// THE DETERMINATION IS BASED ON THE SIZES OF THE ANGLES
// CONTAINED IN THE 2 TRIANGLES DEFINED BY THE DIAGONAL.
// THE DIAGONAL IS CHOSEN TO MAXIMIZE THE SMALLEST OF THE
// SIX ANGLES OVER THE TWO PAIRS OF TRIANGLES.
//
// INPUT PARAMETERS -  IN1,IN2,IO1,IO2 - NODE INDICES OF THE
//                              FOUR POINTS DEFINING THE
//                              QUADRILATERAL.  IO1 AND IO2
//                              ARE CURRENTLY CONNECTED BY A
//                              DIAGONAL ARC.  THIS ARC
//                              SHOULD BE REPLACED BY AN ARC
//                              CONNECTING IN1, IN2 IF THE
//                              DECISION IS MADE TO SWAP.
//                              IN1,IO1,IO2 MUST BE IN
//                              COUNTERCLOCKWISE ORDER.
//
//                        X,Y - VECTORS OF NODAL COORDINATES.
//                              (X(I),Y(I)) ARE THE COORD-
//                              INATES OF NODE I FOR I = IN1,
//                              IN2, IO1, OR IO2.
//
// NONE OF THE INPUT PARAMETERS ARE ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETER -  SWPTST - .TRUE. IFF THE ARC CONNECTING
//                              IO1 AND IO2 IS TO BE REPLACED
//
// MODULES REFERENCED BY SWPTST - NONE
//
//***********************************************************
var
       DX11, DX12, DX22, DX21, DY11, DY12, DY22, DY21,
          SIN1, SIN2, COS1, COS2, SIN12: Real;
// LOCAL PARAMETERS -
//
// DX11,DY11 = X,Y COORDINATES OF THE VECTOR IN1-IO1
// DX12,DY12 = X,Y COORDINATES OF THE VECTOR IN1-IO2
// DX22,DY22 = X,Y COORDINATES OF THE VECTOR IN2-IO2
// DX21,DY21 = X,Y COORDINATES OF THE VECTOR IN2-IO1
// SIN1 =      CROSS PRODUCT OF THE VECTORS IN1-IO1 AND
//               IN1-IO2 -- PROPORTIONAL TO SIN(T1) WHERE T1
//               IS THE ANGLE AT IN1 FORMED BY THE VECTORS
// COS1 =      INNER PRODUCT OF THE VECTORS IN1-IO1 AND
//               IN1-IO2 -- PROPORTIONAL TO COS(T1)
// SIN2 =      CROSS PRODUCT OF THE VECTORS IN2-IO2 AND
//               IN2-IO1 -- PROPORTIONAL TO SIN(T2) WHERE T2
//               IS THE ANGLE AT IN2 FORMED BY THE VECTORS
// COS2 =      INNER PRODUCT OF THE VECTORS IN2-IO2 AND
//               IN2-IO1 -- PROPORTIONAL TO COS(T2)
// SIN12 =     SIN1*COS2 + COS1*SIN2 -- PROPORTIONAL TO
//               SIN(T1+T2)
begin
  result := false;

  // COMPUTE THE VECTORS CONTAINING THE ANGLES T1, T2

  DX11 := X[IO1-1] - X[IN1-1];
  DX12 := X[IO2-1] - X[IN1-1];
  DX22 := X[IO2-1] - X[IN2-1];
  DX21 := X[IO1-1] - X[IN2-1];

  DY11 := Y[IO1-1] - Y[IN1-1];
  DY12 := Y[IO2-1] - Y[IN1-1];
  DY22 := Y[IO2-1] - Y[IN2-1];
  DY21 := Y[IO1-1] - Y[IN2-1];

  // COMPUTE INNER PRODUCTS

  COS1 := DX11*DX12 + DY11*DY12;
  COS2 := DX22*DX21 + DY22*DY21;

  // THE DIAGONALS SHOULD BE SWAPPED IFF (T1+T2) .GT. 180
  //   DEGREES.  THE FOLLOWING TWO TESTS INSURE NUMERICAL
  //   STABILITY.

  IF (COS1 >= 0.0)  AND  (COS2 >= 0.0) then
  begin
    Exit;
  end;
  IF (COS1 < 0.0)  AND  (COS2 < 0.0) then
  begin
    result := True;
    Exit;
  end;

  // COMPUTE VECTOR CROSS PRODUCTS

  SIN1 := DX11*DY12 - DX12*DY11;
  SIN2 := DX22*DY21 - DX21*DY22;
  SIN12 := SIN1*COS2 + COS1*SIN2;
  IF (SIN12 >= 0.0) then
  begin
    Exit;
  end;
  result := True;
end;

procedure TRFIND (const NST: longint; const PX,PY: Real; const X,Y: TRealArray;
  const IADJ: TIntArray; const IEND: TIntArray; var I1,I2,I3: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE LOCATES A POINT P IN A THIESSEN TRIANGU-
// LATION, RETURNING THE VERTEX INDICES OF A TRIANGLE WHICH
// CONTAINS P.  TRFIND IS PART OF AN INTERPOLATION PACKAGE
// WHICH PROVIDES SUBROUTINES FOR CREATING THE MESH.
//
// INPUT PARAMETERS -    NST - INDEX OF NODE AT WHICH TRFIND
//                             BEGINS SEARCH.  SEARCH TIME
//                             DEPENDS ON THE PROXIMITY OF
//                             NST TO P.
//
//                     PX,PY - X AND Y-COORDINATES OF THE
//                             POINT TO BE LOCATED.
//
//                       X,Y - VECTORS OF COORDINATES OF
//                             NODES IN THE MESH.  (X(I),Y(I))
//                             DEFINES NODE I FOR I = 1,...,N
//                             WHERE N >= 3.
//
//                      IADJ - SET OF ADJACENCY LISTS OF
//                             NODES IN THE MESH.
//
//                      IEND - POINTERS TO THE ENDS OF
//                             ADJACENCY LISTS IN IADJ FOR
//                             EACH NODE IN THE MESH.
//
// IADJ AND IEND MAY BE CREATED BY TRMESH.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - I1,I2,I3 - VERTEX INDICES IN COUNTER-
//                                CLOCKWISE ORDER - VERTICES
//                                OF A TRIANGLE CONTAINING P
//                                IF P IS AN INTERIOR NODE.
//                                IF P IS OUTSIDE OF THE
//                                BOUNDARY OF THE MESH, I1
//                                AND I2 ARE THE FIRST (RIGHT
//                                -MOST) AND LAST (LEFTMOST)
//                                NODES WHICH ARE VISIBLE
//                                FROM P, AND I3 = 0.  IF P
//                                AND ALL OF THE NODES LIE ON
//                                A SINGLE LINE THEN I1 = I2
//                                = I3 = 0.
//
// MODULES REFERENCED BY TRFIND - NONE
//
// INTRINSIC FUNCTION CALLED BY TRFIND - MAX0
//
//***********************************************************
var
  N0, N1, N2, N3, N4, INDX, IND, NF,
  NL, NEXT: longint;
  XP, YP: Real;
  GoTo4: Boolean;
  GoTo16: Boolean;
  GoTo18: Boolean;
  GoTo7: Boolean;
  GoTo8: Boolean;
// LOCAL PARAMETERS -
//
// XP,YP =     LOCAL VARIABLES CONTAINING PX AND PY
// N0,N1,N2 =  NODES IN COUNTERCLOCKWISE ORDER DEFINING A
//               CONE (WITH VERTEX N0) CONTAINING P
// N3,N4 =     NODES OPPOSITE N1-N2 AND N2-N1, RESPECTIVELY
// INDX,IND =  INDICES FOR IADJ
// NF,NL =     FIRST AND LAST NEIGHBORS OF N0 IN IADJ, OR
//               FIRST (RIGHTMOST) AND LAST (LEFTMOST) NODES
//               VISIBLE FROM P WHEN P IS OUTSIDE THE
//               BOUNDARY
// NEXT =      CANDIDATE FOR I1 OR I2 WHEN P IS OUTSIDE OF
//               THE BOUNDARY
// LEFT =      STATEMENT FUNCTION WHICH COMPUTES THE SIGN OF
//               A CROSS PRODUCT (Z-COMPONENT).  LEFT(X1,...,
//               Y0) = .TRUE. IFF (X0,Y0) IS ON OR TO THE
//               LEFT OF THE VECTOR FROM (X1,Y1) TO (X2,Y2).
  function Left(const X1,Y1,X2,Y2,X0,Y0: Real): boolean;
  begin
    result := (X2-X1)*(Y0-Y1) >= (X0-X1)*(Y2-Y1);
  end;
begin
  XP := PX;
  YP := PY;

  // INITIALIZE VARIABLES AND FIND A CONE CONTAINING P

  N0 := MAX(NST,1);
  GoTo8 := False;
  repeat
    INDX := IEND[N0-1];
    NL := IADJ[INDX-1];
    INDX := 1;
    IF (N0 <> 1) then
    begin
      INDX := IEND[N0-1-1] + 1;
    end;
    NF := IADJ[INDX-1];
    N1 := NF;
    GoTo18 := False;
    GoTo16 := False;
    GoTo4 := False;
    IF (NL = 0) then
    begin

  // N0 IS A BOUNDARY NODE.  SET NL TO THE LAST NONZERO
  //   NEIGHBOR OF N0.

      IND := IEND[N0-1] - 1;
      NL := IADJ[IND-1];
      IF not ( LEFT(X[N0-1],Y[N0-1],X[NF-1],Y[NF-1],XP,YP) ) then
      begin

  // P IS OUTSIDE THE BOUNDARY

        NL := N0;
        GoTo16 := True;
        break;
      end;

      IF ( LEFT(X[NL-1],Y[NL-1],X[N0-1],Y[N0-1],XP,YP) ) then
      begin
        GoTo4 := True;
      end

  // P IS OUTSIDE THE BOUNDARY AND N0 IS THE RIGHTMOST
  //   VISIBLE BOUNDARY NODE
      else
      begin
        I1 := N0;
        GoTo18 := True;
        break;
      end;

  // N0 IS AN INTERIOR NODE.  FIND N1.

    end;
    GoTo7 := False;
    if not GoTo4 then
    begin
      while not ( LEFT(X[N0-1],Y[N0-1],X[N1-1],Y[N1-1],XP,YP) ) do
      begin
        INDX := INDX + 1;
        N1 := IADJ[INDX-1];
        IF (N1 = NL) then
        begin
          GoTo7 := True;
          break;
        end;
      end;
    end;
    if GoTo7 then
    begin
      break;
    end;

  // P IS TO THE LEFT OF ARC N0-N1.  INITIALIZE N2 TO THE NEXT
  //   NEIGHBOR OF N0.

    GoTo8 := False;
    repeat
      INDX := INDX + 1;
      N2 := IADJ[INDX-1];
      IF ( NOT LEFT(X[N0-1],Y[N0-1],X[N2-1],Y[N2-1],XP,YP) ) then
      begin
        GoTo8 := True;
        break;
      end;

      N1 := N2;
    until (N1 = NL);
    if GoTo8 then
    begin
      break;
    end;

    IF ( NOT LEFT(X[N0-1],Y[N0-1],X[NF-1],Y[NF-1],XP,YP) ) then
    begin
      break;
    end;

    IF not (XP = X[N0-1]) AND (YP = Y[N0-1]) then
    begin

  // P IS LEFT OF OR ON ARCS N0-NB FOR ALL NEIGHBORS NB
  //   OF N0.
  // ALL POINTS ARE COLLINEAR IFF P IS LEFT OF NB-N0 FOR
  //   ALL NEIGHBORS NB OF N0.  SEARCH THE NEIGHBORS OF N0
  //   IN REVERSE ORDER.  NOTE -- N1 = NL AND INDX POINTS TO
  //   NL.

      while LEFT(X[N1-1],Y[N1-1],X[N0-1],Y[N0-1],XP,YP) do
      begin
        IF (N1 = NF) then
        begin
  // ALL POINTS ARE COLLINEAR
          I1 := 0;
          I2 := 0;
          I3 := 0;
          Exit;
        end;
        INDX := INDX - 1;
        N1 := IADJ[INDX-1];
      end;
    end;
  // P IS TO THE RIGHT OF N1-N0, OR P=N0.  SET N0 TO N1 AND
  //   START OVER.

    N0 := N1;

  until False;
  if not GoTo18 then
  begin
    if not GoTo16 then
    begin
      if not GoTo8 then
      begin

  // P IS BETWEEN ARCS N0-N1 AND N0-NF

        N2 := NF;
      end;
  // P IS CONTAINED IN A CONE DEFINED BY LINE SEGMENTS N0-N1
  //   AND N0-N2 WHERE N1 IS ADJACENT TO N2

      N3 := N0;
      while not LEFT(X[N1-1],Y[N1-1],X[N2-1],Y[N2-1],XP,YP) do
      begin

  // SET N4 TO THE FIRST NEIGHBOR OF N2 FOLLOWING N1

        INDX := IEND[N2-1];
        GoTo16 := False;
        IF (IADJ[INDX-1] = N1) then
        begin

  // N1 IS THE LAST NEIGHBOR OF N2.
  // SET N4 TO THE FIRST NEIGHBOR.

          INDX := 1;
          IF (N2 <> 1) then INDX := IEND[N2-1-1] + 1;
          N4 := IADJ[INDX-1];

  // N1 IS NOT THE LAST NEIGHBOR OF N2

        end
        else
        begin
          repeat
            INDX := INDX-1;
          until (IADJ[INDX-1] = N1);
          N4 := IADJ[INDX+1-1];
          IF (N4 = 0) then
          begin

  // P IS OUTSIDE THE BOUNDARY

            NF := N2;
            NL := N1;
            GoTo16 := True;
            break;
          end;
        end;

  // DEFINE A NEW ARC N1-N2 WHICH INTERSECTS THE LINE
  //   SEGMENT N0-P

        IF not ( LEFT(X[N0-1],Y[N0-1],X[N4-1],Y[N4-1],XP,YP) ) then
        begin
          N3 := N2;
          N2 := N4;
        end
        else
        begin
          N3 := N1;
          N1 := N4;
        end;
      end;
      if not GoTo16 then
      begin

  // P IS IN THE TRIANGLE (N1,N2,N3) AND NOT ON N2-N3.  IF
  //   N3-N1 OR N1-N2 IS A BOUNDARY ARC CONTAINING P, TREAT P
  //   AS EXTERIOR.

        INDX := IEND[N1-1];
        IF (IADJ[INDX-1] <> 0) then
        begin

  //      C P DOES NOT LIE ON A BOUNDARY ARC.

          I1 := N1;
          I2 := N2;
          I3 := N3;
          Exit;
        end;

  // N1 IS A BOUNDARY NODE.  N3-N1 IS A BOUNDARY ARC IFF N3
  //   IS THE LAST NONZERO NEIGHBOR OF N1.

        IF (N3 = IADJ[INDX-1-1]) and

  // N3-N1 IS A BOUNDARY ARC

          LEFT(X[N1-1],Y[N1-1],X[N3-1],Y[N3-1],XP,YP) then
        begin

  // P LIES ON N1-N3

          I1 := N1;
          I2 := N3;
          I3 := 0;
          Exit;

        end;
  // N3-N1 IS NOT A BOUNDARY ARC CONTAINING P.  N1-N2 IS A
  //   BOUNDARY ARC IFF N2 IS THE FIRST NEIGHBOR OF N1.

        INDX := 1;
        IF (N1 <> 1) then INDX := IEND[N1-1-1] + 1;
        IF (N2 = IADJ[INDX-1]) and

  // N1-N2 IS A BOUNDARY ARC

          LEFT(X[N2-1],Y[N2-1],X[N1-1],Y[N1-1],XP,YP) then
        begin

  // P LIES ON N1-N2

          I1 := N2;
          I2 := N1;
          I3 := 0;
          Exit;
        end;
  // P DOES NOT LIE ON A BOUNDARY ARC.
        I1 := N1;
        I2 := N2;
        I3 := N3;
        Exit;
      end;
    end;

  // NF AND NL ARE ADJACENT BOUNDARY NODES WHICH ARE VISIBLE
  //   FROM P.  FIND THE FIRST VISIBLE BOUNDARY NODE.
  // SET NEXT TO THE FIRST NEIGHBOR OF NF.

    repeat
      INDX := 1;
      IF (NF <> 1) then
      begin
        INDX := IEND[NF-1-1] + 1;
      end;
      NEXT := IADJ[INDX-1];
      IF ( LEFT(X[NF-1],Y[NF-1],X[NEXT-1],Y[NEXT-1],XP,YP) ) then
      begin
        break;
      end;
      NF := NEXT;
    until (False);

  // NF IS THE FIRST (RIGHTMOST) VISIBLE BOUNDARY NODE

    I1 := NF;
  end;

  // FIND THE LAST VISIBLE BOUNDARY NODE.  NL IS THE FIRST
  //   CANDIDATE FOR I2.
  // SET NEXT TO THE LAST NEIGHBOR OF NL.

  repeat
    INDX := IEND[NL-1] - 1;
    NEXT := IADJ[INDX-1];
    IF ( LEFT(X[NEXT-1],Y[NEXT-1],X[NL-1],Y[NL-1],XP,YP) ) then
    begin
      break;
    end;
    NL := NEXT;
  until (False);

  // NL IS THE LAST (LEFTMOST) VISIBLE BOUNDARY NODE

  I2 := NL;
  I3 := 0;
end;

procedure BDYADD (const KK,I1,I2: longint;
  var IADJ: TIntArray; var IEND: TIntArray);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE ADDS A BOUNDARY NODE TO A TRIANGULATION
// OF A SET OF KK-1 POINTS IN THE PLANE.  IADJ AND IEND ARE
// UPDATED WITH THE INSERTION OF NODE KK.
//
// INPUT PARAMETERS -   KK - INDEX OF AN EXTERIOR NODE TO BE
//                           ADDED.  KK >= 4.
//
//                      I1 - FIRST (RIGHTMOST AS VIEWED FROM
//                           KK) BOUNDARY NODE IN THE MESH
//                           WHICH IS VISIBLE FROM KK - THE
//                           LINE SEGMENT KK-I1 INTERSECTS
//                           NO ARCS.
//
//                      I2 - LAST (LEFTMOST) BOUNDARY NODE
//                           WHICH IS VISIBLE FROM KK.
//
//                    IADJ - SET OF ADJACENCY LISTS OF NODES
//                           IN THE MESH.
//
//                    IEND - POINTERS TO THE ENDS OF
//                           ADJACENCY LISTS IN IADJ FOR
//                           EACH NODE IN THE MESH.
//
//   IADJ AND IEND MAY BE CREATED BY TRMESH AND MUST CONTAIN
// THE VERTICES I1 AND I2.  I1 AND I2 MAY BE DETERMINED BY
// TRFIND.
//
// KK, I1, AND I2 ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ADDITION
//                                 OF NODE KK AS THE LAST
//                                 ENTRY.  NODE KK WILL BE
//                                 CONNECTED TO I1, I2, AND
//                                 ALL BOUNDARY NODES BETWEEN
//                                 THEM.  NO OPTIMIZATION OF
//                                 THE MESH IS PERFORMED.
//
// MODULE REFERENCED BY BDYADD - SHIFTD
//
// INTRINSIC FUNCTIONS CALLED BY BDYADD - MIN0, MAX0
//
//***********************************************************
var
       K, KM1, NRIGHT, NLEFT, NF, NL, N1, N2, I,
            IMIN, IMAX, KEND, NEXT, INDX: longint;
// LOCAL PARAMETERS -
//
// K =            LOCAL COPY OF KK
// KM1 =          K - 1
// NRIGHT,NLEFT = LOCAL COPIES OF I1, I2
// NF,NL =        INDICES OF IADJ BOUNDING THE PORTION OF THE
//                  ARRAY TO BE SHIFTED
// N1 =           IADJ INDEX OF THE FIRST NEIGHBOR OF NLEFT
// N2 =           IADJ INDEX OF THE LAST NEIGHBOR OF NRIGHT
// I =            DO-LOOP INDEX
// IMIN,IMAX =    BOUNDS ON DO-LOOP INDEX -- FIRST AND LAST
//                  ELEMENTS OF IEND TO BE INCREMENTED
// KEND =         POINTER TO THE LAST NEIGHBOR OF K IN IADJ
// NEXT =         NEXT BOUNDARY NODE TO BE CONNECTED TO KK
// INDX =         INDEX FOR IADJ
begin
  K := KK;
  KM1 := K - 1;
  NRIGHT := I1;
  NLEFT := I2;

  // INITIALIZE VARIABLES

  NL := IEND[KM1-1];
  N1 := 1;
  IF (NLEFT <> 1) then
  begin
    N1 := IEND[NLEFT-1-1] + 1;
  end;
  N2 := IEND[NRIGHT-1];
  NF := MAX(N1,N2);

  // INSERT K AS A NEIGHBOR OF MAX(NRIGHT,NLEFT)

  SHIFTD(NF,NL,2, IADJ );
  IADJ[NF+1-1] := K;
  IMIN := MAX(NRIGHT,NLEFT);
  for I := IMIN-1 to KM1 - 1 do
  begin
    IEND[I] := IEND[I] + 2;
  end;

  // INITIALIZE KEND AND INSERT K AS A NEIGHBOR OF
  //   MIN(NRIGHT,NLEFT)

  KEND := NL + 3;
  NL := NF - 1;
  NF := MIN(N1,N2);
  SHIFTD(NF,NL,1, IADJ );
  IADJ[NF-1] := K;
  IMAX := IMIN - 1;
  IMIN := MIN(NRIGHT,NLEFT);
  for I := IMIN-1 to IMAX - 1 do
  begin
    IEND[I] := IEND[I] + 1;
  end;

  // INSERT NRIGHT AS THE FIRST NEIGHBOR OF K

  IADJ[KEND-1] := NRIGHT;

  // INITIALIZE INDX FOR LOOP ON BOUNDARY NODES BETWEEN NRIGHT
  //   AND NLEFT
  INDX := IEND[NRIGHT-1] - 2;
  repeat
    NEXT := IADJ[INDX-1];
    IF (NEXT = NLEFT) then
    begin
      break;
    end;

  // CONNECT NEXT AND K

    KEND := KEND + 1;
    IADJ[KEND-1] := NEXT;
    INDX := IEND[NEXT-1];
    IADJ[INDX-1] := K;
    INDX := INDX - 1;
  until (False);

  // INSERT NLEFT AND 0 AS THE LAST NEIGHBORS OF K

  IADJ[KEND+1-1] := NLEFT;
  KEND := KEND + 2;
  IADJ[KEND-1] := 0;
  IEND[K-1] := KEND;
end;

procedure INTADD (const KK,I1,I2,I3: longint; var IADJ: TIntArray;
  var IEND: TIntArray);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE ADDS AN INTERIOR NODE TO A TRIANGULATION
// OF A SET OF KK-1 POINTS IN THE PLANE.  IADJ AND IEND ARE
// UPDATED WITH THE INSERTION OF NODE KK IN THE TRIANGLE
// WHOSE VERTICES ARE I1, I2, AND I3.
//
// INPUT PARAMETERS -        KK - INDEX OF NODE TO BE
//                                INSERTED.  KK >= 4.
//
//                     I1,I2,I3 - INDICES OF THE VERTICES OF
//                                A TRIANGLE CONTAINING NODE
//                                KK -- IN COUNTERCLOCKWISE
//                                ORDER.
//
//                         IADJ - SET OF ADJACENCY LISTS
//                                OF NODES IN THE MESH.
//
//                         IEND - POINTERS TO THE ENDS OF
//                                ADJACENCY LISTS IN IADJ FOR
//                                EACH NODE IN THE MESH.
//
//   IADJ AND IEND MAY BE CREATED BY TRMESH AND MUST CONTAIN
// THE VERTICES I1, I2, AND I3.  I1,I2,I3 MAY BE DETERMINED
// BY TRFIND.
//
// KK, I1, I2, AND I3 ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ADDITION
//                                 OF NODE KK AS THE LAST
//                                 ENTRY.  NODE KK WILL BE
//                                 CONNECTED TO NODES I1, I2,
//                                 AND I3.  NO OPTIMIZATION
//                                 OF THE MESH IS PERFORMED.
//
// MODULE REFERENCED BY INTADD - SHIFTD
//
// INTRINSIC FUNCTION CALLED BY INTADD - MOD
//
//***********************************************************
var
  K, KM1, IP1, IP2, IP3, INDX, NF,
  NL, N1, N2, IMIN, IMAX, I, ITEMP: longint;
  N, NFT: array[0..2] of longint;
// LOCAL PARAMETERS -
//
// K =           LOCAL COPY OF KK
// KM1 =         K - 1
// N =           VECTOR CONTAINING I1, I2, I3
// NFT =         POINTERS TO THE TOPS OF THE 3 SETS OF IADJ
//                 ELEMENTS TO BE SHIFTED DOWNWARD
// IP1,IP2,IP3 = PERMUTATION INDICES FOR N AND NFT
// INDX =        INDEX FOR IADJ AND N
// NF,NL =       INDICES OF FIRST AND LAST ENTRIES IN IADJ
//                 TO BE SHIFTED DOWN
// N1,N2 =       FIRST 2 VERTICES OF A NEW TRIANGLE --
//                 (N1,N2,KK)
// IMIN,IMAX =   BOUNDS ON DO-LOOP INDEX -- FIRST AND LAST
//                 ELEMENTS OF IEND TO BE INCREMENTED
// I =           DO-LOOP INDEX
// ITEMP =       TEMPORARY STORAGE LOCATION
begin
  K := KK;

  // INITIALIZATION

  N[0] := I1;
  N[1] := I2;
  N[2] := I3;

  // SET UP NFT

  for I := 1 to 3 do
  begin
    N1 := N[I-1];
    INDX := I mod 3 + 1;
    N2 := N[INDX-1];
    INDX := IEND[N1-1] + 1;

  // FIND THE INDEX OF N2 AS A NEIGHBOR OF N1

    repeat
      INDX := INDX - 1;
    until (IADJ[INDX-1] = N2);
    NFT[I-1] := INDX + 1;
  end;

  // ORDER THE VERTICES BY DECREASING MAGNITUDE.
  //   N(IP(I+1)) PRECEDES N(IP(I)) IN IEND FOR
  //   I = 1,2.

  IP1 := 1;
  IP2 := 2;
  IP3 := 3;
  IF ( N[1] > N[0] ) then
  begin
    IP1 := 2;
    IP2 := 1;
  end;

  IF ( N[2] > N[IP1-1] ) then
  begin
    IP3 := IP1;
    IP1 := 3;
  end;

  IF ( N[IP3-1] > N[IP2-1] )  then
  begin
    ITEMP := IP2;
    IP2 := IP3;
    IP3 := ITEMP;
  end;

  // ADD NODE K TO THE ADJACENCY LISTS OF EACH VERTEX AND
  //   UPDATE IEND.  FOR EACH VERTEX, A SET OF IADJ ELEMENTS
  //   IS SHIFTED DOWNWARD AND K IS INSERTED.  SHIFTING STARTS
  //   AT THE END OF THE ARRAY.

  KM1 := K - 1;
  NL := IEND[KM1-1];
  NF := NFT[IP1-1];
  IF (NF <= NL) then SHIFTD(NF,NL,3, IADJ );
  IADJ[NF+2-1] := K;
  IMIN := N[IP1-1];
  IMAX := KM1;
  for I := IMIN to IMAX do
  begin
    IEND[I-1] := IEND[I-1] + 3;
  end;

  NL := NF - 1;
  NF := NFT[IP2-1];
  SHIFTD(NF,NL,2, IADJ );
  IADJ[NF+1-1] := K;
  IMAX := IMIN - 1;
  IMIN := N[IP2-1];
  for I := IMIN to IMAX do
  begin
    IEND[I-1] := IEND[I-1] + 2;
  end;

  NL := NF - 1;
  NF := NFT[IP3-1];
  SHIFTD(NF,NL,1, IADJ );
  IADJ[NF-1] := K;
  IMAX := IMIN - 1;
  IMIN := N[IP3-1];
  for I := IMIN to IMAX do
  begin
    IEND[I-1] := IEND[I-1] + 1;
  end;

  // ADD NODE K TO IEND AND ITS NEIGHBORS TO IADJ

  INDX := IEND[KM1-1];
  IEND[K-1] := INDX + 3;
  for I := 1 to 3 do
  begin
    INDX := INDX + 1;
    IADJ[INDX-1] := N[I-1];
  end;
end;

procedure SWAP (const NIN1,NIN2,NOUT1,NOUT2: longint;
  var IADJ, IEND: TIntArray);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS SUBROUTINE SWAPS THE DIAGONALS IN A CONVEX QUADRI-
// LATERAL.
//
// INPUT PARAMETERS -  NIN1,NIN2,NOUT1,NOUT2 - NODAL INDICES
//                            OF A PAIR OF ADJACENT TRIANGLES
//                            WHICH FORM A CONVEX QUADRILAT-
//                            ERAL.  NOUT1 AND NOUT2 ARE CON-
//                            NECTED BY AN ARC WHICH IS TO BE
//                            REPLACED BY THE ARC NIN1-NIN2.
//                            (NIN1,NOUT1,NOUT2) MUST BE TRI-
//                            ANGLE VERTICES IN COUNTERCLOCK-
//                            WISE ORDER.
//
// THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
//                IADJ,IEND - TRIANGULATION DATA STRUCTURE
//                            (SEE SUBROUTINE TRMESH).
//
// OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ARC
//                                 REPLACEMENT.
//
// MODULES REFERENCED BY SWAP - INDEX, SHIFTD
//
//***********************************************************
var
  IN_Array, IO: array[0..1] of longint;
  IP1, IP2, J, K, NF, NL, I,
  IMIN, IMAX: longint;
// LOCAL PARAMETERS -
//
// IN =        NIN1 AND NIN2 ORDERED BY INCREASING MAGNITUDE
//               (THE NEIGHBORS OF IN(1) PRECEDE THOSE OF
//               IN(2) IN IADJ)
// IO =        NOUT1 AND NOUT2 IN INCREASING ORDER
// IP1,IP2 =   PERMUTATION OF (1,2) SUCH THAT IO(IP1)
//               PRECEDES IO(IP2) AS A NEIGHBOR OF IN(1)
// J,K =       PERMUTATION OF (1,2) USED AS INDICES OF IN
//               AND IO
// NF,NL =     IADJ INDICES BOUNDARY A PORTION OF THE ARRAY
//               TO BE SHIFTED
// I =         IEND INDEX
// IMIN,IMAX = BOUNDS ON THE PORTION OF IEND TO BE INCRE-
//               MENTED OR DECREMENTED
//
begin
  IN_Array[0] := NIN1;
  IN_Array[1] := NIN2;
  IO[0] := NOUT1;
  IO[1] := NOUT2;
  IP1 := 1;

  // ORDER THE INDICES SO THAT IN_Array[0] .LT. IN_Array[1] AND IO[0] .LT.
  //   IO[1], AND CHOOSE IP1 AND IP2 SUCH THAT (IN_Array[0],IO(IP1),
  //   IO(IP2)) FORMS A TRIANGLE.

  IF (IN_Array[0] >= IN_Array[1]) then
  begin
    IN_Array[0] := IN_Array[1];
    IN_Array[1] := NIN1;
    IP1 := 2;
  end;

  IF (IO[0] >= IO[1]) then
  begin
    IO[0] := IO[1];
    IO[1] := NOUT1;
    IP1 := 3 - IP1;
  end;

  IP2 := 3 - IP1;
  IF (IO[1] < IN_Array[0]) then
  begin

  // THE VERTICES ARE ORDERED (IO[0],IO[1],IN_Array[0],IN_Array[1]).
  //   DELETE IO[1] BY SHIFTING UP BY 1

    NF := 1 + INDEX_Pascal(IO[0],IO[1],IADJ,IEND);
    NL := -1 + INDEX_Pascal(IO[1],IO[0],IADJ,IEND);
    IF (NF <= NL) then
    begin
      SHIFTD(NF,NL,-1, IADJ );
    end;
    IMIN := IO[0];
    IMAX := IO[1]-1;
    for I := IMIN-1 to IMAX-1 do
    begin
      IEND[I] := IEND[I] - 1;
    end;

  //   DELETE IO[0] BY SHIFTING UP BY 2 AND INSERT IN_Array[1]

    NF := NL + 2;
    NL := -1 + INDEX_Pascal(IN_Array[0],IO[IP2-1],IADJ,IEND);
    IF (NF <= NL) then SHIFTD(NF,NL,-2, IADJ );
    IADJ[NL-1-1] := IN_Array[1];
    IMIN := IO[1];
    IMAX := IN_Array[0]-1;
    for I := IMIN-1 to IMAX-1 do
    begin
      IEND[I] := IEND[I] - 2;
    end;

  //   SHIFT UP BY 1 AND INSERT IN_Array[0]

    NF := NL + 1;
    NL := -1 + INDEX_Pascal(IN_Array[1],IO[IP1-1],IADJ,IEND);
    SHIFTD(NF,NL,-1, IADJ );
    IADJ[NL-1] := IN_Array[0];
    IMIN := IN_Array[0];
    IMAX := IN_Array[1]-1;
    for I := IMIN-1 to IMAX-1 do
    begin
      IEND[I] := IEND[I] - 1;
    end;
  end
  else IF (IN_Array[1] < IO[0]) then
  begin

  // THE VERTICES ARE ORDERED (IN_Array[0],IN_Array[1],IO[0],IO[1]).
  //   DELETE IO[0] BY SHIFTING DOWN BY 1

    NF := 1 + INDEX_Pascal(IO[0],IO[1],IADJ,IEND);
    NL := -1 + INDEX_Pascal(IO[1],IO[0],IADJ,IEND);
    IF (NF <= NL) then
    begin
  //          KK_Temp := 1;
      SHIFTD(NF,NL,1, IADJ );
    end;
    IMIN := IO[0];
    IMAX := IO[1] - 1;
    for I := IMIN-1 to IMAX-1 do
    begin
     IEND[I] := IEND[I] + 1;
    end;

  //   DELETE IO[1] BY SHIFTING DOWN BY 2 AND INSERT IN_Array[0]

    NL := NF - 2;
    NF := 1 + INDEX_Pascal(IN_Array[1],IO[IP2-1],IADJ,IEND);
    IF (NF <= NL) then
    begin
      SHIFTD(NF,NL,2, IADJ );
    end;
    IADJ[NF+1-1] := IN_Array[0];
    IMIN := IN_Array[1];
    IMAX := IO[0] - 1;
    for I := IMIN-1 to IMAX-1 do
    begin
      IEND[I] := IEND[I] + 2;
    end;

  //   SHIFT DOWN BY 1 AND INSERT IN_Array[1]

    NL := NF - 1;
    NF := 1 + INDEX_Pascal(IN_Array[0],IO[IP1-1],IADJ,IEND);
    SHIFTD(NF,NL,1, IADJ );
    IADJ[NF-1] := IN_Array[1];
    IMIN := IN_Array[0];
    IMAX := IN_Array[1] - 1;
    for I := IMIN-1 to IMAX-1 do
    begin
      IEND[I] := IEND[I] + 1;
    end;
  end
  else
  begin

  // IN_Array[0] AND IO[0] PRECEDE IN_Array[1] AND IO[1].  FOR (J,K) =
  //   (1,2) AND (2,1), DELETE IO(K) AS A NEIGHBOR OF IO(J)
  //   BY SHIFTING A PORTION OF IADJ EITHER UP OR DOWN AND
  //   AND INSERT IN_Array(K) AS A NEIGHBOR OF IN_Array(J).

    for J := 1 to 2 do
    begin
      K := 3 - J;
      IF (IN_Array[J-1] <= IO[J-1]) then
      begin

    //   THE NEIGHBORS OF IN_Array(J) PRECEDE THOSE OF IO(J) -- SHIFT
    //     DOWN BY 1

        NF := 1 + INDEX_Pascal(IN_Array[J-1],IO[IP1-1],IADJ,IEND);
        NL := -1 + INDEX_Pascal(IO[J-1],IO[K-1],IADJ,IEND);
        IF (NF <= NL) then
        begin
          SHIFTD(NF,NL,1, IADJ );
        end;
        IADJ[NF-1] := IN_Array[K-1];
        IMIN := IN_Array[J-1];
        IMAX := IO[J-1]-1;
        for I := IMIN-1 to IMAX - 1 do
        begin
          IEND[I] := IEND[I] + 1;
        end;
      end
      else
      begin

    //   THE NEIGHBORS OF IO(J) PRECEDE THOSE OF IN_Array(J) -- SHIFT
    //     UP BY 1

        NF := 1 + INDEX_Pascal(IO[J-1],IO[K-1],IADJ,IEND);
        NL := -1 + INDEX_Pascal(IN_Array[J-1],IO[IP2-1],IADJ,IEND);
        IF (NF <= NL) then
        begin
          SHIFTD(NF,NL,-1, IADJ );
        end;
        IADJ[NL-1] := IN_Array[K-1];
        IMIN := IO[J-1];
        IMAX := IN_Array[J-1] - 1;
        for I := IMIN-1 to IMAX - 1 do
        begin
          IEND[I] := IEND[I] - 1;
        end;
      end;

    //   REVERSE (IP1,IP2) FOR (J,K) = (2,1)

      IP1 := IP2;
      IP2 := 3 - IP1;
    end;
  end;
end;


procedure ADNODE (const KK: longint; const X,Y: TRealArray; var IADJ: TIntArray;
    var IEND: TIntArray; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE ADDS NODE KK TO A TRIANGULATION OF A SET
// OF POINTS IN THE PLANE PRODUCING A NEW TRIANGULATION.  A
// SEQUENCE OF EDGE SWAPS IS THEN APPLIED TO THE MESH,
// RESULTING IN AN OPTIMAL TRIANGULATION.  ADNODE IS PART
// OF AN INTERPOLATION PACKAGE WHICH ALSO PROVIDES ROUTINES
// TO INITIALIZE THE DATA STRUCTURE, PLOT THE MESH, AND
// DELETE ARCS.
//
// INPUT PARAMETERS -   KK - INDEX OF THE NODE TO BE ADDED
//                           TO THE MESH.  KK >= 4.
//
//                     X,Y - VECTORS OF COORDINATES OF THE
//                           NODES IN THE MESH.  (X(I),Y(I))
//                           DEFINES NODE I FOR I = 1,..,KK.
//
//                    IADJ - SET OF ADJACENCY LISTS OF NODES
//                           1,..,KK-1.
//
//                    IEND - POINTERS TO THE ENDS OF
//                           ADJACENCY LISTS IN IADJ FOR
//                           EACH NODE IN THE MESH.
//
// IADJ AND IEND MAY BE CREATED BY TRMESH.
//
// KK, X, AND Y ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE ADDITION
//                                 OF NODE KK AS THE LAST
//                                 ENTRY.
//
//                           IER - ERROR INDICATOR
//                                 IER = 0 IF NO ERRORS
//                                         WERE ENCOUNTERED.
//                                 IER = 1 IF ALL NODES
//                                         (INCLUDING KK) ARE
//                                         COLLINEAR.
//
// MODULES REFERENCED BY ADNODE - TRFIND, INTADD, BDYADD,
//                                SHIFTD, INDEX, SWPTST,
//                                SWAP
//
//***********************************************************
//
var
  K, KM1, I1, I2, I3, INDKF, INDKL, NABOR1,
  IO1, IO2, IN1, INDK1, IND2F, IND21: longint;
  XK, YK: Real;
  GoTo4: Boolean;
// LOCAL PARAMETERS -
//
// K =        LOCAL COPY OF KK
// KM1 =      K - 1
// I1,I2,I3 = VERTICES OF A TRIANGLE CONTAINING K
// INDKF =    IADJ INDEX OF THE FIRST NEIGHBOR OF K
// INDKL =    IADJ INDEX OF THE LAST NEIGHBOR OF K
// NABOR1 =   FIRST NEIGHBOR OF K BEFORE ANY SWAPS OCCUR
// IO1,IO2 =  ADJACENT NEIGHBORS OF K DEFINING AN ARC TO
//              BE TESTED FOR A SWAP
// IN1 =      VERTEX OPPOSITE K -- FIRST NEIGHBOR OF IO2
//              WHICH PRECEDES IO1.  IN1,IO1,IO2 ARE IN
//              COUNTERCLOCKWISE ORDER.
// INDK1 =    INDEX OF IO1 IN THE ADJACENCY LIST FOR K
// IND2F =    INDEX OF THE FIRST NEIGHBOR OF IO2
// IND21 =    INDEX OF IO1 IN THE ADJACENCY LIST FOR IO2
// XK,YK =    X(K), Y(K)
//
begin
  IER := 0;
  K := KK;

  // INITIALIZATION

  KM1 := K - 1;
  XK := X[K-1];
  YK := Y[K-1];

  // ADD NODE K TO THE MESH

  TRFIND(KM1,XK,YK,X,Y,IADJ,IEND, I1,I2,I3);
  IF (I1 = 0) then
  begin
  // ALL NODES ARE COLLINEAR
    IER := 1;
    Exit;
  end;
  IF (I3 = 0) then
  begin
    BDYADD(K,I1,I2, IADJ,IEND );
  end
  else
  begin
    INTADD(K,I1,I2,I3, IADJ,IEND );
  end;

  // INITIALIZE VARIABLES FOR OPTIMIZATION OF THE MESH

  INDKF := IEND[KM1-1] + 1;
  INDKL := IEND[K-1];
  NABOR1 := IADJ[INDKF-1];
  IO2 := NABOR1;
  INDK1 := INDKF + 1;
  IO1 := IADJ[INDK1-1];

  // BEGIN LOOP -- FIND THE VERTEX OPPOSITE K

  repeat
    IND2F := 1;
    IF (IO2 <> 1) then
    begin
      IND2F := IEND[IO2-1-1] + 1;
    end;
    IND21 := INDEX_Pascal(IO2,IO1,IADJ,IEND);
    GoTo4 := False;
    IF (IND2F <> IND21) then
    begin
      IN1 := IADJ[IND21-1-1];
    end
    else
    begin

  // IN1 IS THE LAST NEIGHBOR OF IO2

      IND21 := IEND[IO2-1];
      IN1 := IADJ[IND21-1];
      GoTo4 := IN1 = 0;
    end;

    if not GoTo4 then
    begin

  // SWAP TEST -- IF A SWAP OCCURS, TWO NEW ARCS ARE OPPOSITE K
  //              AND MUST BE TESTED.  INDK1 AND INDKF MUST BE
  //              DECREMENTED.


      IF SWPTST(IN1,K,IO1,IO2,X,Y) then
      begin
        SWAP(IN1,K,IO1,IO2, IADJ,IEND );
        IO1 := IN1;
        INDK1 := INDK1 - 1;
        INDKF := INDKF - 1;
        Continue
      end;
    end;

  // NO SWAP OCCURRED.  RESET IO2 AND IO1, AND TEST FOR
  //   TERMINATION.

    IF (IO1 = NABOR1) then
    begin
      Exit;
    end;
    IO2 := IO1;
    INDK1 := INDK1 + 1;
    IF (INDK1 > INDKL) then
    begin
      INDK1 := INDKF;
    end;
    IO1 := IADJ[INDK1-1];
  until (IO1 = 0);
end;


Procedure TRMESH (const N: longint; const X,Y: TRealArray; var IADJ: TIntArray;
    var IEND: TIntArray; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE CREATES A THIESSEN TRIANGULATION OF N
// ARBITRARILY SPACED POINTS IN THE PLANE REFERRED TO AS
// NODES.  THE TRIANGULATION IS OPTIMAL IN THE SENSE THAT IT
// IS AS NEARLY EQUIANGULAR AS POSSIBLE.  TRMESH IS PART OF
// AN INTERPOLATION PACKAGE WHICH ALSO PROVIDES SUBROUTINES
// TO REORDER THE NODES, ADD A NEW NODE, DELETE AN ARC, PLOT
// THE MESH, AND PRINT THE DATA STRUCTURE.
//   UNLESS THE NODES ARE ALREADY ORDERED IN SOME REASONABLE
// FASHION, THEY SHOULD BE REORDERED BY SUBROUTINE REORDR FOR
// INCREASED EFFICIENCY BEFORE CALLING TRMESH.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//                            N >= 3.
//
//                      X,Y - N-VECTORS OF COORDINATES.
//                            (X(I),Y(I)) DEFINES NODE I.
//
//                     IADJ - VECTOR OF LENGTH >= 6*N-9.
//
//                     IEND - VECTOR OF LENGTH >= N.
//
// N, X, AND Y ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - IADJ - ADJACENCY LISTS OF NEIGHBORS IN
//                            COUNTERCLOCKWISE ORDER.  THE
//                            LIST FOR NODE I+1 FOLLOWS THAT
//                            FOR NODE I WHERE X AND Y DEFINE
//                            THE ORDER.  THE VALUE 0 DENOTES
//                            THE BOUNDARY (OR A PSEUDO-NODE
//                            AT INFINITY) AND IS ALWAYS THE
//                            LAST NEIGHBOR OF A BOUNDARY
//                            NODE.  IADJ IS UNCHANGED IF IER
//                            .NE. 0.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS (SETS OF
//                            NEIGHBORS) IN IADJ.  THE
//                            NEIGHBORS OF NODE 1 BEGIN IN
//                            IADJ(1).  FOR K .GT. 1, THE
//                            NEIGHBORS OF NODE K BEGIN IN
//                            IADJ(IEND(K-1)+1) AND K HAS
//                            IEND(K) - IEND(K-1) NEIGHBORS
//                            INCLUDING (POSSIBLY) THE
//                            BOUNDARY.  IADJ(IEND(K)) .EQ. 0
//                            IFF NODE K IS ON THE BOUNDARY.
//                            IEND IS UNCHANGED IF IER = 1.
//                            IF IER = 2 IEND CONTAINS THE
//                            INDICES OF A SEQUENCE OF N
//                            NODES ORDERED FROM LEFT TO
//                            RIGHT WHERE LEFT AND RIGHT ARE
//                            DEFINED BY ASSUMING NODE 1 IS
//                            TO THE LEFT OF NODE 2.
//
//                      IER - ERROR INDICATOR
//                            IER = 0 IF NO ERRORS WERE
//                                    ENCOUNTERED.
//                            IER = 1 IF N .LT. 3.
//                            IER = 2 IF N >= 3 AND ALL
//                                    NODES ARE COLLINEAR.
//
// MODULES REFERENCED BY TRMESH - SHIFTD, ADNODE, TRFIND,
//                                INTADD, BDYADD, SWPTST,
//                                SWAP, INDEX
//
//***********************************************************
//
var
  NN, K, KM1, NL, NR, IND, INDX, N0, ITEMP,
  IERR, KM1D2, KMI, I, KMIN: longint;
  XL, YL, XR, YR, DXR, DYR, XK, YK, DXK, DYK,
  CPROD, SPROD: Real;
  Restart: Boolean;
  NodeIsToLeft: boolean;
// LOCAL PARAMETERS -
//
// NN =          LOCAL COPY OF N
// K =           NODE (INDEX) TO BE INSERTED INTO IEND
// KM1 =         K-1 - (VARIABLE) LENGTH OF IEND
// NL,NR =       IEND(1), IEND(KM1) -- LEFTMOST AND RIGHTMOST
//                 NODES IN IEND AS VIEWED FROM THE RIGHT OF
//                 1-2 WHEN IEND CONTAINS THE INITIAL ORDERED
//                 SET OF NODAL INDICES
// XL,YL,XR,YR = X AND Y COORDINATES OF NL AND NR
// DXR,DYR =     XR-XL, YR-YL
// XK,YK =       X AND Y COORDINATES OF NODE K
// DXK,DYK =     XK-XL, YK-YL
// CPROD =       VECTOR CROSS PRODUCT OF NL-NR AND NL-K --
//                 USED TO DETERMINE THE POSITION OF NODE K
//                 WITH RESPECT TO THE LINE DEFINED BY THE
//                 NODES IN IEND
// SPROD =       SCALAR PRODUCT USED TO DETERMINE THE
//                 INTERVAL CONTAINING NODE K WHEN K IS ON
//                 THE LINE DEFINED BY THE NODES IN IEND
// IND,INDX =    INDICES FOR IEND AND IADJ, RESPECTIVELY
// N0,ITEMP =    TEMPORARY NODES (INDICES)
// IERR =        DUMMY PARAMETER FOR CALL TO ADNODE
// KM1D2,KMI,I = KM1/2, K-I, DO-LOOP INDEX -- USED IN IEND
//                 REORDERING LOOP
// KMIN =        FIRST NODE INDEX SENT TO ADNODE
begin
  NN := N;
  IER := 1;
  IF (NN < 3) then Exit;
  IER := 0;

// INITIALIZE IEND, NL, NR, AND K

  IEND[0] := 1;
  IEND[1] := 2;
  XL := X[0];
  YL := Y[0];
  XR := X[1] ;
  YR := Y[1] ;
  K := 2;

// BEGIN LOOP ON NODES 3,4,...

  Restart := True;
  NodeIsToLeft := False;
  DXR := 0;
  DYR := 0;
  repeat
    if Restart then
    begin
      DXR := XR-XL;
      DYR := YR-YL;
    end;

// NEXT LOOP BEGINS HERE IF NL AND NR ARE UNCHANGED

    IF (K = NN) then
    begin
// ALL NODES ARE COLLINEAR
      IER := 2;
      Exit;
    end;
    KM1 := K;
    K := KM1 + 1;
    XK := X[K-1];
    YK := Y[K-1];
    DXK := XK-XL;
    DYK := YK-YL;
    CPROD := DXR*DYK - DXK*DYR;
    IF (CPROD > 0.0) then
    begin
      NodeIsToLeft := True;
      break;
    end;
    IF (CPROD < 0.0) then
    begin
      NodeIsToLeft := False;
      break;
    end;

// NODE K LIES ON THE LINE CONTAINING NODES 1,2,...,K-1.
//   SET SPROD TO (NL-NR,NL-K).

    SPROD := DXR*DXK + DYR*DYK;
    IF (SPROD <= 0.0) then
    begin

// NODE K IS TO THE LEFT OF NL.  INSERT K AS THE FIRST
//   (LEFTMOST) NODE IN IEND AND SET NL TO K.

      SHIFTD(1,KM1,1, IEND);
      IEND[0] := K;
      XL := XK;
      YL := YK;
      Restart := True;
      Continue;
    end;

// NODE K IS TO THE RIGHT OF NL.  FIND THE LEFTMOST NODE
//   N0 WHICH LIES TO THE RIGHT OF K.
//   SET SPROD TO (N0-NL,N0-K).

    NodeIsToLeft := False;
    for IND := 1 to KM1-1 do
    begin
      N0 := IEND[IND];
      SPROD := (XL-X[N0-1])*(XK-X[N0-1]) +
             (YL-Y[N0-1])*(YK-Y[N0-1]);
      IF (SPROD >= 0.0) then
      begin
        NodeIsToLeft := True;
        break;
      end;
    end;

    if NodeIsToLeft then
    begin
// NODE K IS TO THE RIGHT OF NR.  INSERT K AS THE LAST
//   (RIGHTMOST) NODE IN IEND AND SET NR TO K.

      IEND[K-1] := K;
      XR := XK;
      YR := YK;
      Restart := True;
    end
    else
    begin
// NODE K LIES BETWEEN IEND(IND-1) AND IEND(IND).  INSERT K
//   IN IEND.

      SHIFTD(IND,KM1,1, IEND);
      IEND[IND-1] := K;
      Restart := False;
    end;
  until False;

// NODE K IS TO THE LEFT OF NL-NR.  REORDER IEND SO THAT NL
//   IS THE LEFTMOST NODE AS VIEWED FROM K.

  if NodeIsToLeft then
  begin

  KM1D2 := KM1 div 2;
  for I := 1 to KM1D2 do
  begin
    KMI := K-I;
    ITEMP := IEND[I-1];
    IEND[I-1] := IEND[KMI-1];
    IEND[KMI-1] := ITEMP;
  end;
  end;

// NODE K IS TO THE RIGHT OF NL-NR.  CREATE A TRIANGULATION
//   CONSISTING OF NODES 1,2,...,K.

  NL := IEND[0];
  NR := IEND[KM1-1];

// CREATE THE ADJACENCY LISTS FOR THE FIRST K-1 NODES.
//   INSERT NEIGHBORS IN REVERSE ORDER.  EACH NODE HAS FOUR
//   NEIGHBORS EXCEPT NL AND NR WHICH HAVE THREE.

  for IND := 1 to KM1 do
  begin
    N0 := IEND[IND-1];
    INDX := 4*N0;
    IF (N0 >= NL) then INDX := INDX-1;
    IF (N0 >= NR) then INDX := INDX-1;
    IADJ[INDX-1] := 0;
    INDX := INDX-1;
    IF (IND < KM1) then IADJ[INDX-1] := IEND[IND-1+1];
    IF (IND < KM1) then INDX := INDX-1;
    IADJ[INDX-1] := K;
    IF (IND = 1) then
    begin
      Continue;
    end;
    IADJ[INDX-1-1] := IEND[IND-1-1];

  end;

// CREATE THE ADJACENCY LIST FOR NODE K

  INDX := 5*KM1 - 1;
  IADJ[INDX-1] := 0;
  for IND := 0 to KM1 - 1 do
  begin
    INDX := INDX-1;
    IADJ[INDX-1] := IEND[IND];
  end;

// REPLACE IEND ELEMENTS WITH POINTERS TO IADJ

  INDX := 0;
  for IND := 1 to KM1 do
  begin
    INDX := INDX + 4;
    IF (IND = NL)  OR  (IND = NR) then
    begin
      INDX := INDX-1;
    end;
    IEND[IND-1] := INDX;
  end;

  INDX := INDX + K;
  IEND[K-1] := INDX;

// ADD THE REMAINING NODES TO THE TRIANGULATION

  IF (K = NN) then
  begin
    Exit;
  end;
  KMIN := K+1;
  for K := KMIN to NN do
  begin
    ADNODE(K,X,Y, IADJ,IEND, IERR);
  end;
end;

procedure SETUP (const XK,YK,ZK,XI,YI,ZI,S1,S2,R: Real; var ROW: TReal6);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE SETS UP THE I-TH ROW OF AN AUGMENTED RE-
// GRESSION MATRIX FOR A WEIGHTED LEAST-SQUARES FIT OF A
// QUADRATIC FUNCTION Q(X,Y) TO A SET OF DATA VALUES Z WHERE
// Q(XK,YK) = ZK.  THE FIRST 3 COLUMNS (QUADRATIC TERMS) ARE
// SCALED BY 1/S2 AND THE FOURTH AND FIFTH COLUMNS (LINEAR
// TERMS) ARE SCALED BY 1/S1.  THE WEIGHT IS (R-D)/(R*D) IF
// R .GT. D AND 0 IF R .LE. D, WHERE D IS THE DISTANCE
// BETWEEN NODES I AND K.
//
// INPUT PARAMETERS - XK,YK,ZK - COORDINATES AND DATA VALUE
//                               AT NODE K -- INTERPOLATED
//                               BY Q.
//
//                    XI,YI,ZI - COORDINATES AND DATA VALUE
//                               AT NODE I.
//
//                       S1,S2 - INVERSE SCALE FACTORS.
//
//                           R - RADIUS OF INFLUENCE ABOUT
//                               NODE K DEFINING THE WEIGHT.
//
//                         ROW - VECTOR OF LENGTH 6.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETER - ROW - VECTOR CONTAINING A ROW OF THE
//                          AUGMENTED REGRESSION MATRIX.
//
// MODULES REFERENCED BY SETUP - NONE
//
// INTRINSIC FUNCTION CALLED BY SETUP - SQRT
//
//***********************************************************
var
  I: longint;
  DX, DY, DXSQ, DYSQ, D, W, W1, W2: Real;
// LOCAL PARAMETERS -
//
// I =    DO-LOOP INDEX
// DX =   XI - XK
// DY =   YI - YK
// DXSQ = DX*DX
// DYSQ = DY*DY
// D =    DISTANCE BETWEEN NODES K AND I
// W =    WEIGHT ASSOCIATED WITH THE ROW
// W1 =   W/S1
// W2 =   W/S2
begin
      DX := XI - XK;
      DY := YI - YK;
      DXSQ := DX*DX;
      DYSQ := DY*DY;
      D := SQRT(DXSQ + DYSQ);
      IF not (D <= 0.0)  OR  (D >= R) then
      begin
        W := (R-D)/R/D;
        W1 := W/S1;
        W2 := W/S2;
        ROW[0] := DXSQ*W2;
        ROW[1] := DX*DY*W2;
        ROW[2] := DYSQ*W2;
        ROW[3] := DX*W1;
        ROW[4] := DY*W1;
        ROW[5] := (ZI - ZK)*W;
      end
      else
      begin

// NODES K AND I COINCIDE OR NODE I IS OUTSIDE OF THE RADIUS
//   OF INFLUENCE.  SET ROW TO THE ZERO VECTOR.

        for I := 0 to 5 do
        begin
          ROW[I] := 0.0;
        end;
      end;
end;

procedure GIVENS (var A,B, C,S: Real);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE CONSTRUCTS THE GIVENS PLANE ROTATION --
//     ( C  S)
// G = (     ) WHERE C*C + S*S = 1 -- WHICH ZEROS THE SECOND
//     (-S  C)
// ENTRY OF THE 2-VECTOR (A B)-TRANSPOSE.  A CALL TO GIVENS
// IS NORMALLY FOLLOWED BY A CALL TO ROTATE WHICH APPLIES
// THE TRANSFORMATION TO A 2 BY N MATRIX.  THIS ROUTINE WAS
// TAKEN FROM LINPACK.
//
// INPUT PARAMETERS - A,B - COMPONENTS OF THE 2-VECTOR TO BE
//                          ROTATED.
//
// OUTPUT PARAMETERS -  A - OVERWRITTEN BY R = +/-SQRT(A*A
//                          + B*B)
//
//                      B - OVERWRITTEN BY A VALUE Z WHICH
//                          ALLOWS C AND S TO BE RECOVERED
//                          AS FOLLOWS -
//                          C = SQRT(1-Z*Z), S=Z IF ABS(Z)
//                              .LE. 1.
//                          C = 1/Z, S = SQRT(1-C*C) IF
//                              ABS(Z) .GT. 1.
//
//                      C - +/-(A/R)
//
//                      S - +/-(B/R)
//
// MODULES REFERENCED BY GIVENS - NONE
//
// INTRINSIC FUNCTIONS CALLED BY GIVENS - ABS, SQRT
//
//***********************************************************
var
  AA, BB, R, U, V: Real;
//
// LOCAL PARAMETERS -
//
// AA,BB = LOCAL COPIES OF A AND B
// R =     C*A + S*B = +/-SQRT(A*A+B*B)
// U,V =   VARIABLES USED TO SCALE A AND B FOR COMPUTING R
begin
  AA := A;
  BB := B;
  IF (ABS(AA) > ABS(BB)) then
  begin

// ABS(A) > ABS(B)

    U := AA + AA;
    V := BB/U;
    R := SQRT(0.25 + V*V) * U;
    C := AA/R;
    S := V * (C + C);

// NOTE THAT R HAS THE SIGN OF A, C .GT. 0, AND S HAS
//   SIGN(A)*SIGN(B)

    B := S;
    A := R;

  end
// ABS(A) <= ABS(B)
  else if BB <> 0 then
  begin
    U := BB + BB;
    V := AA/U;

// STORE R IN A

    A := SQRT(0.25 + V*V) * U;
    S := BB/A;
    C := V * (S + S);

// NOTE THAT R HAS THE SIGN OF B, S .GT. 0, AND C HAS
//   SIGN(A)*SIGN(B)

    B := 1.0;
    IF (C <> 0.0) then
    begin
      B := 1.0/C;
    end;
  end
  else
  begin

// A = B = 0.
    C := 1.0;
    S := 0.0;
  end;
end;

procedure ROTATE (const N: longint; const C,S: Real; var X,Y: TReal6;
  const XIndex, YIndex: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//                                            ( C  S)
//   THIS ROUTINE APPLIES THE GIVENS ROTATION (     ) TO THE
//                                            (-S  C)
//               (X(1) ... X(N))
// 2 BY N MATRIX (             ).  THIS ROUTINE WAS TAKEN
//               (Y(1) ... Y(N))
// LINPACK.
//
// INPUT PARAMETERS -   N - NUMBER OF COLUMNS TO BE ROTATED.
//
//                    C,S - ELEMENTS OF THE GIVENS ROTATION.
//                          THESE MAY BE DETERMINED BY
//                          SUBROUTINE GIVENS.
//
//                    X,Y - VECTORS OF LENGTH >= N
//                          CONTAINING THE 2-VECTORS TO BE
//                          ROTATED.
//
//   THE PARAMETERS N, C, AND S ARE NOT ALTERED BY THIS
// ROUTINE.
//
// OUTPUT PARAMETERS - X,Y - ROTATED VECTORS
//
// MODULES REFERENCED BY ROTATE - NONE
//
//***********************************************************
var
  I: longint;
  XI, YI: Real;
// LOCAL PARAMETERS -
//
// I =     DO-LOOP INDEX
// XI,YI = X(I), Y(I)
begin
  IF (N <= 0) OR ((C = 1.0) AND (S = 0.0)) then
  begin
    Exit;
  end;
  for I := 1 to N do
  begin
    XI := X[I-1+XIndex];
    YI := Y[I-1+YIndex];
    X[I-1+XIndex] := C*XI + S*YI;
    Y[I-1+YIndex] := -S*XI + C*YI;
  end;
end;

procedure GRADL (const N,K: longint; const X,Y,Z: TRealArray;
    const IADJ: TIntArray; const IEND: TIntArray; var DX,DY: Real;
    var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A THIESSEN TRIANGULATION OF N POINTS IN THE PLANE
// WITH ASSOCIATED DATA VALUES Z, THIS SUBROUTINE ESTIMATES
// X AND Y PARTIAL DERIVATIVES AT NODE K.  THE DERIVATIVES
// ARE TAKEN TO BE THE PARTIALS AT K OF A QUADRATIC FUNCTION
// WHICH INTERPOLATES Z(K) AND FITS THE DATA VALUES AT A SET
// OF NEARBY NODES IN A WEIGHTED LEAST SQUARES SENSE. A MAR-
// QUARDT STABILIZATION FACTOR IS USED IF NECESSARY TO ENSURE
// A WELL-CONDITIONED SYSTEM AND A LINEAR FITTING FUNCTION IS
// USED IF N .LT. 6.  THUS, A UNIQUE SOLUTION EXISTS UNLESS
// THE NODES ARE COLLINEAR.
//   AN ALTERNATIVE ROUTINE, GRADG, EMPLOYS A GLOBAL METHOD
// TO COMPUTE THE PARTIAL DERIVATIVES AT ALL OF THE NODES AT
// ONCE.  THAT METHOD IS MORE EFFICIENT (WHEN ALL PARTIALS
// ARE NEEDED) AND MAY BE MORE ACCURATE, DEPENDING ON THE
// DATA.
//
// INPUT PARAMETERS - N - NUMBER OF NODES IN THE TRIANGULA-
//                        TION.  N >= 3.
//
//                    K - NODE AT WHICH DERIVATIVES ARE
//                        SOUGHT.  1 .LE. K .LE. N.
//
//                  X,Y - N-VECTORS CONTAINING THE CARTESIAN
//                        COORDINATES OF THE NODES.
//
//                    Z - N-VECTOR CONTAINING THE DATA VALUES
//                        ASSOCIATED WITH THE NODES.
//
//                 IADJ - SET OF ADJACENCY LISTS.
//
//                 IEND - POINTERS TO THE ENDS OF ADJACENCY
//                        LISTS FOR EACH NODE.
//
// IADJ AND IEND MAY BE CREATED BY SUBROUTINE TRMESH.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS - DX,DY - ESTIMATED PARTIAL DERIVATIVES
//                             AT NODE K UNLESS IER .LT. 0.
//
//                       IER - ERROR INDICATOR
//                             IER .GT. 0 IF NO ERRORS WERE
//                                      ENCOUNTERED.  IER
//                                      CONTAINS THE NUMBER
//                                      OF NODES (INCLUDING
//                                      K) USED IN THE FIT.
//                                      IER = 3, 4, OR 5 IM-
//                                      PLIES A LINEAR FIT.
//                             IER = -1 IF N OR K IS OUT OF
//                                      RANGE.
//                             IER = -2 IF ALL NODES ARE
//                                      COLLINEAR.
//
// MODULES REFERENCED BY GRADL - GETNP, SETUP, GIVENS,
//                               ROTATE
//
// INTRINSIC FUNCTIONS CALLED BY GRADL - MIN0, FLOAT, SQRT,
//                                       AMIN1, ABS
//
//***********************************************************
const
  LMN = 10;
  LMX = 30;
  RTOL = 1e-5;
  DTOL = 0.01;
  SF = 1.0;
var
  NN, KK, LMIN, LMAX, LM1, LNP,
  IERR, NP, I, J, IM1, JP1, IP1, L: longint;
  NPTS: TIntArray;
  SUM, DS, R, RS, AVSQ, AV, XK, YK, ZK,
    C, S, DMIN: Real;
  A: array [0..5] of TReal6;
  GoTo3: Boolean;
  GoTo14: Boolean;
// LOCAL PARAMETERS -
//
// NN,KK =     LOCAL COPIES OF N AND K
// LMN,LMX =   MINIMUM AND MAXIMUM VALUES OF LNP FOR N
//               SUFFICIENTLY LARGE.  IN MOST CASES LMN-1
//               NODES ARE USED IN THE FIT.  4 .LE. LMN .LE.
//               LMX.
// LMIN,LMAX = MIN(LMN,N), MIN(LMX,N)
// LM1 =       LMIN-1 OR LNP-1
// LNP =       LENGTH OF NPTS
// NPTS =      ARRAY CONTAINING THE INDICES OF A SEQUENCE OF
//               NODES ORDERED BY DISTANCE FROM K.  NPTS(1)=K
//               AND THE FIRST LNP-1 ELEMENTS OF NPTS ARE
//               USED IN THE LEAST SQUARES FIT.  UNLESS LNP
//               EXCEEDS LMAX, NPTS(LNP) DETERMINES R.
// IERR =      ERROR FLAG FOR CALLS TO GETNP (NOT CHECKED)
// NP =        ELEMENT OF NPTS TO BE ADDED TO THE SYSTEM
// I,J =       DO-LOOP INDICES
// IM1,JP1 =   I-1, J+1
// IP1 =       I+1
// L =         NUMBER OF COLUMNS OF A**T TO WHICH A ROTATION
//               IS APPLIED
// SUM =       SUM OF SQUARED EUCLIDEAN DISTANCES BETWEEN
//               NODE K AND THE NODES USED IN THE LEAST
//               SQUARES FIT
// DS =        SQUARED DISTANCE BETWEEN NODE K AND AN ELE-
//               MENT OF NPTS
// R =         DISTANCE BETWEEN NODE K AND NPTS(LNP) OR SOME
//               POINT FURTHER FROM K THAN NPTS(LMAX) IF
//               NPTS(LMAX) IS USED IN THE FIT.  R IS A
//               RADIUS OF INFLUENCE WHICH ENTERS INTO THE
//               WEIGHTS (SEE SUBROUTINE SETUP).
// RS =        R*R
// RTOL =      TOLERANCE FOR DETERMINING R.  IF THE RELATIVE
//               CHANGE IN DS BETWEEN TWO ELEMENTS OF NPTS IS
//               NOT GREATER THAN RTOL THEY ARE TREATED AS
//               BEING THE SAME DISTANCE FROM NODE K
// AVSQ =      AV*AV
// AV =        ROOT-MEAN-SQUARE DISTANCE BETWEEN K AND THE
//               NODES (OTHER THAN K) IN THE LEAST SQUARES
//               FIT.  THE FIRST 3 COLUMNS OF THE SYSTEM ARE
//               SCALED BY 1/AVSQ, THE NEXT 2 BY 1/AV.
// XK,YK,ZK =  COORDINATES AND DATA VALUE ASSOCIATED WITH K
// A =         TRANSPOSE OF THE AUGMENTED REGRESSION MATRIX
// C,S =       COMPONENTS OF THE PLANE ROTATION DETERMINED
//               BY SUBROUTINE GIVENS
// DMIN =      MINIMUM OF THE MAGNITUDES OF THE DIAGONAL
//               ELEMENTS OF THE REGRESSION MATRIX AFTER
//               ZEROS ARE INTRODUCED BELOW THE DIAGONAL
// DTOL =      TOLERANCE FOR DETECTING AN ILL-CONDITIONED
//               SYSTEM.  THE SYSTEM IS ACCEPTED WHEN DMIN
//               >= DTOL
// SF =        MARQUARDT STABILIZATION FACTOR USED TO DAMP
//               OUT THE FIRST 3 SOLUTION COMPONENTS (SECOND
//               PARTIALS OF THE QUADRATIC) WHEN THE SYSTEM
//               IS ILL-CONDITIONED.  AS SF INCREASES, THE
//               FITTING FUNCTION APPROACHES A LINEAR
begin
  SetLength(NPTS, 30);

  NN := N;
  KK := K;

  // CHECK FOR ERRORS AND INITIALIZE LMIN, LMAX

  IF (NN < 3)  OR  (KK < 1)  OR  (KK > NN) then
  begin
  // N OR K IS OUT OF RANGE

    IER := -1;
    Exit;
  end;


  LMIN := MIN(LMN,NN);
  LMAX := MIN(LMX,NN);

  // COMPUTE NPTS, LNP, AVSQ, AV, AND R.
  //   SET NPTS TO THE CLOSEST LMIN-1 NODES TO K.

  SUM := 0.0;
  NPTS[0] := KK;
  LM1 := LMIN - 1;
  for LNP := 2 to LM1 do
  begin
    GETNP (X,Y,IADJ,IEND,LNP, NPTS, DS,IERR);
    SUM := SUM + DS;
  end;

  // ADD ADDITIONAL NODES TO NPTS UNTIL THE RELATIVE INCREASE
  //   IN DS IS AT LEAST RTOL.

  GoTo3 := False;
  for LNP := LMIN to LMAX do
  begin
    GETNP (X,Y,IADJ,IEND,LNP, NPTS, RS,IERR);
    IF ((RS-DS)/DS <= RTOL) then
    begin
      Continue;
    end;
    IF (LNP > 6) then
    begin
      GoTo3 := True;
      break;
    end;
    SUM := SUM + RS;
  end;

  // USE ALL LMAX NODES IN THE LEAST SQUARES FIT.  RS IS
  //   ARBITRARILY INCREASED BY 10 PER CENT.

    if not GoTo3 then
    begin
      RS := 1.1*RS;
      LNP := LMAX + 1;
    end;

  // THERE ARE LNP-2 EQUATIONS CORRESPONDING TO NODES NPTS(2),
  //   ...,NPTS(LNP-1).

  AVSQ := SUM/(LNP-2);
  AV := SQRT(AVSQ);
  R := SQRT(RS);
  XK := X[KK-1];
  YK := Y[KK-1];
  ZK := Z[KK-1];
  GoTo14 := False;
  IF (LNP >= 7) then
  begin

  // SET UP THE FIRST 5 EQUATIONS OF THE AUGMENTED REGRESSION
  //   MATRIX (TRANSPOSED) AS THE COLUMNS OF A, AND ZERO OUT
  //   THE LOWER TRIANGLE (UPPER TRIANGLE OF A) WITH GIVENS
  //   ROTATIONS

    for I := 1 to 5 do
    begin
      NP := NPTS[I-1+1];
      SETUP (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],AV,AVSQ,
                 R, A[I-1]);
      IF (I = 1) then
      begin
        Continue;
      end;
      IM1 := I - 1;
      for J := 1 to IM1 do
      begin
        JP1 := J + 1;
        L := 6 - J;
        GIVENS (A[J-1,J-1],A[I-1,J-1],C,S);
        ROTATE (L,C,S,A[J-1],A[I-1], JP1-1, JP1-1);
      end;
    end;

  // ADD THE ADDITIONAL EQUATIONS TO THE SYSTEM USING
  //   THE LAST COLUMN OF A -- I .LE. LNP.

    I := 7;
    repeat

      repeat

        IF (I = LNP) then
        begin
          break;
        end;
        NP := NPTS[I-1];
        SETUP (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],AV,AVSQ,
                   R, A[5]);
        for J := 1 to 5 do
        begin
          JP1 := J + 1;
          L := 6 - J;
          GIVENS (A[j-1,J-1],A[5,J-1],C,S);
          ROTATE (L,C,S,A[J-1],A[5],JP1-1,JP1-1);
        end;
        I := I + 1;
      until (False);

  // TEST THE SYSTEM FOR ILL-CONDITIONING

      DMIN := Min( ABS(A[0,0]),ABS(A[1,1]));
      DMIN := Min( DMIN,ABS(A[2,2]));
      DMIN := Min( DMIN,ABS(A[3,3]));
      DMIN := Min( DMIN,ABS(A[4,4]));
      IF (DMIN >= DTOL) then
      begin
        DY := A[4,5]/A[4,4];
        DX := (A[3,5] - A[3,4]*DY)/A[3,3]/AV;
        DY := DY/AV;
        IER := LNP - 1;
        Exit;
      end;
      IF (LNP > LMAX) then
      begin
        Break;
      end;

  // ADD ANOTHER NODE TO THE SYSTEM AND INCREASE R --
  //   I .EQ. LNP

      LNP := LNP + 1;
      IF (LNP <= LMAX) then
      begin
        GETNP (X,Y,IADJ,IEND,LNP, NPTS, RS,IERR)
      end;
      R := SQRT(1.1*RS)
    until False;

  // STABILIZE THE SYSTEM BY DAMPING SECOND PARTIALS --ADD
  //   MULTIPLES OF THE FIRST THREE UNIT VECTORS TO THE FIRST
  //   THREE EQUATIONS.

    for I := 1 to 3 do
    begin
      A[5,I-1] := SF;
      IP1 := I + 1;
      for J := IP1 to 6 do
      begin
        A[5,J-1] := 0.0;
      end;
      for J := 1 to 5 do
      begin
        JP1 := J + 1;
        L := 6 - J;
        GIVENS (A[J-1,J-1],A[5, j-1],C,S);
        ROTATE (L,C,S,A[J-1],A[5],JP1-1,JP1-1);
      end;
    end;
    GoTo14 := True;

  // 4 .LE. LNP .LE. 6 (2, 3, OR 4 EQUATIONS) -- FIT A PLANE TO
  //   THE DATA USING THE LAST 3 COLUMNS OF A.

  end;
  if not GoTo14 then
  begin
    NP := NPTS[1];
    SETUP (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],AV,AVSQ,
               R, A[3]);
    NP := NPTS[2];
    SETUP (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],AV,AVSQ,
               R, A[4]);
    GIVENS (A[3,3],A[4,3],C,S);
    ROTATE (2,C,S,A[3],A[4],4,4);
    IF (LNP <> 4) then
    begin
      LM1 := LNP - 1;
      for I := 4 to LM1 do
      begin
        NP := NPTS[I-1];
        SETUP (XK,YK,ZK,X[NP-1],Y[NP-1],Z[NP-1],AV,AVSQ,
                   R, A[5]);
        GIVENS (A[3,3],A[5,3],C,S);
        ROTATE(2,C,S,A[3],A[5],4,4);
        GIVENS (A[4,4],A[5,4],C,S);
        ROTATE (1,C,S,A[5],A[5],4,5);
      end;
    end;

  // TEST THE LINEAR FIT FOR ILL-CONDITIONING

  end;

  DMIN := Min( ABS(A[3,3]),ABS(A[4,4]) );
  IF (DMIN < DTOL) then
  begin
  // NO UNIQUE SOLUTION DUE TO COLLINEAR NODES

    IER := -2;
    Exit;
  end;

  // SOLVE THE 2 BY 2 TRIANGULAR SYSTEM FOR THE DERIVATIVES

  DY := A[4,5]/A[4,4];
  DX := (A[3,5] - A[3,4]*DY)/A[3,3]/AV;
  DY := DY/AV;
  IER := LNP - 1;
end;

procedure TVAL(const X,Y,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,
  ZX2,ZX3,ZY1,ZY2,ZY3: Real; const IFLAG: longint; var W,WX,WY: Real;
  var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN FUNCTION VALUES AND FIRST PARTIAL DERIVATIVES AT
// THE THREE VERTICES OF A TRIANGLE, THIS ROUTINE DETERMINES
// A FUNCTION W WHICH AGREES WITH THE GIVEN DATA, RETURNING
// THE VALUE AND (OPTIONALLY) FIRST PARTIAL DERIVATIVES OF W
// AT A POINT (X,Y) IN THE TRIANGLE.  THE INTERPOLATION
// METHOD IS EXACT FOR QUADRATIC POLYNOMIAL DATA.  THE
// TRIANGLE IS PARTITIONED INTO THREE SUBTRIANGLES WITH
// EQUAL AREAS.  W IS CUBIC IN EACH SUBTRIANGLE AND ALONG
// THE EDGES, BUT HAS ONLY ONE CONTINUOUS DERIVATIVE ACROSS
// EDGES.  THE NORMAL DERIVATIVE OF W VARIES LINEARLY ALONG
// EACH OUTER EDGE.  THE VALUES AND PARTIAL DERIVATIVES OF W
// ALONG A TRIANGLE EDGE DEPEND ONLY ON THE DATA VALUES AT
// THE ENDPOINTS OF THE EDGE.  THUS THE METHOD YIELDS C-1
// CONTINUITY WHEN USED TO INTERPOLATE OVER A TRIANGULAR
// GRID.  THIS ALGORITHM IS DUE TO C. L. LAWSON.
//
// INPUT PARAMETERS -   X,Y - COORDINATES OF A POINT AT WHICH
//                            W IS TO BE EVALUATED.
//
//        X1,X2,X3,Y1,Y2,Y3 - COORDINATES OF THE VERTICES OF
//                            A TRIANGLE CONTAINING (X,Y).
//
//                 Z1,Z2,Z3 - FUNCTION VALUES AT THE VERTICES
//                            TO BE INTERPOLATED.
//
//              ZX1,ZX2,ZX3 - X-DERIVATIVE VALUES AT THE
//                            VERTICES.
//
//              ZY1,ZY2,ZY3 - Y-DERIVATIVE VALUES AT THE
//                            VERTICES.
//
//                    IFLAG - OPTION INDICATOR
//                            IFLAG = 0 IF ONLY W IS TO BE
//                                      COMPUTED.
//                            IFLAG = 1 IF W, WX, AND WY ARE
//                                      TO BE RETURNED.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS -   W - ESTIMATED VALUE OF THE INTERP-
//                           OLATORY FUNCTION AT (X,Y) IF
//                           IER = 0.  OTHERWISE W = 0.
//
//                   WX,WY - PARTIAL DERIVATIVES OF W AT
//                           (X,Y) IF IER = 0 AND IFLAG = 1,
//                           UNCHANGED IF IFLAG .NE. 1, ZERO
//                           IF IER .NE. 0 AND IFLAG = 1.
//
//                     IER - ERROR INDICATOR
//                           IER = 0 IF NO ERRORS WERE
//                                   ENCOUNTERED.
//                           IER = 1 IF THE VERTICES OF THE
//                                   TRIANGLE ARE COLLINEAR.
//
// MODULES REFERENCED BY TVAL - NONE
//
// INTRINSIC FUNCTION CALLED BY TVAL - AMIN1
//
//***********************************************************
var
  I, IP1, IP2, IP3: longint;
  AREA, XP, YP,
  RMIN, C1, C2: Real;
  U, V, SL,
  R, RX,
  RY, PHI, PHIX, PHIY,
  RO, ROX, ROY, F, G, GX,
  GY, P, PX, PY, Q, QX, QY,
  A, AX, AY, B, BX, BY, C,
  CX, CY: array[0..2] of Real;
// LOCAL PARAMETERS -
//
// I =               DO-LOOP INDEX
// IP1,IP2,IP3 =     PERMUTED INDICES FOR COMPUTING RO, ROX,
//                     AND ROY
// U(K) =            X-COMPONENT OF THE VECTOR REPRESENTING
//                     THE SIDE OPPOSITE VERTEX K
// V(K) =            Y-COMPONENT OF THE VECTOR REPRESENTING
//                     THE SIDE OPPOSITE VERTEX K
// SL(K) =           SQUARE OF THE LENGTH OF THE SIDE
//                     OPPOSITE VERTEX K
// AREA =            TWICE THE AREA OF THE TRIANGLE
// XP,YP =           X-X1, Y-Y1
// R(K) =            K-TH BARYCENTRIC COORDINATE
// RX(K),RY(K) =     X,Y PARTIAL DERIVATIVES OF R(K)
// PHI(K)            R(K-1)*R(K+1) -- QUADRATIC
// PHIX(K),PHIY(K) = X,Y PARTIALS OF PHI(K)
// RMIN =            MIN(R1,R2,R3)
// C1,C2 =           FACTORS FOR COMPUTING RO
// RO(K) =           FACTORS FOR COMPUTING G -- CUBIC
//                     CORRECTION TERMS
// ROX(K),ROY(K) =   X,Y PARTIALS OF RO(K)
// F(K) =            FACTORS FOR COMPUTING G, GX, AND GY --
//                     CONSTANT
// G(K) =            FACTORS FOR COMPUTING THE CARDINAL
//                     FUNCTIONS -- CUBIC
// GX(K),GY(K) =     X,Y PARTIALS OF G(K)
// P(K) =            G(K) + PHI(K)
// PX(K),PY(K) =     X,Y PARTIALS OF P(K)
// Q(K) =            G(K) - PHI(K)
// QX(K),QY(K) =     X,Y PARTIALS OF Q(K)
// A(K) =            CARDINAL FUNCTION WHOSE COEFFICIENT IS
//                     Z(K)
// AX(K),AY(K) =     X,Y PARTIALS OF A(K) -- CARDINAL
//                     FUNCTIONS FOR WX AND WY
// B(K) =            TWICE THE CARDINAL FUNCTION WHOSE
//                     COEFFICIENT IS ZX(K)
// BX(K),BY(K) =     X,Y PARTIALS OF B(K)
// C(K) =            TWICE THE CARDINAL FUNCTION WHOSE
//                     COEFFICIENT IS ZY(K)
// CX(K),CY(K) =     X,Y PARTIALS OF C(K)
begin
  U[0] := X3 - X2;
  U[1] := X1 - X3;
  U[2] := X2 - X1;

  V[0] := Y3 - Y2;
  V[1] := Y1 - Y3;
  V[2] := Y2 - Y1;

  for I := 0 to 2 do
  begin
    SL[I] := U[I]*U[I] + V[I]*V[I];
  end;

// AREA = 3-1 X 3-2

  AREA := U[0]*V[1] - U[1]*V[0];
  IF (AREA = 0.0) then
  begin
// VERTICES ARE COLLINEAR
    IER := 1;
    W := 0.;
    IF (IFLAG = 1) then
    begin
      WX := 0.0;
      WY := 0.0;
    end;
    Exit;
  end;

// R[0] = (2-3 X 2-(X,Y))/AREA, R[1] = (1-(X,Y) X 1-3)/AREA,
//   R[2] = (1-2 X 1-(X,Y))/AREA

  R[0] := (U[0]*(Y-Y2) - V[0]*(X-X2))/AREA;
  XP := X - X1;
  YP := Y - Y1;
  R[1] := (U[1]*YP - V[1]*XP)/AREA;
  R[2] := (U[2]*YP - V[2]*XP)/AREA;
  IER := 0;

  PHI[0] := R[1]*R[2];
  PHI[1] := R[2]*R[0];
  PHI[2] := R[0]*R[1];

  RMIN := MIN(R[0],R[1]);
  RMIN := MIN(RMIN,R[2]);
  IF (RMIN = R[0]) then
  begin
    IP1 := 0;
    IP2 := 1;
    IP3 := 2;
  end
  else

  IF (RMIN = R[1]) then
  begin
    IP1 := 1;
    IP2 := 2;
    IP3 := 0;
  end
  else
  begin
    IP1 := 2;
    IP2 := 0;
    IP3 := 1;
  end;

  C1 := RMIN*RMIN/2.0;
  C2 := RMIN/3.0;
  RO[IP1] := (PHI[IP1] + 5.*C1/3.0)*R[IP1] - C1;
  RO[IP2] := C1*(R[IP3] - C2);
  RO[IP3] := C1*(R[IP2] - C2);

  F[0] := 3.*(SL[1]-SL[2])/SL[0];
  F[1] := 3.*(SL[2]-SL[0])/SL[1];
  F[2] := 3.*(SL[0]-SL[1])/SL[2];

  G[0] := (R[1]-R[2])*PHI[0] + F[0]*RO[0] - RO[1] + RO[2];
  G[1] := (R[2]-R[0])*PHI[1] + F[1]*RO[1] - RO[2] + RO[0];
  G[2] := (R[0]-R[1])*PHI[2] + F[2]*RO[2] - RO[0] + RO[1];

  for I := 0 to 2 do
  begin
    P[I] := G[I] + PHI[I];
    Q[I] := G[I] - PHI[I];
  end;

  A[0] := R[0] + G[2] - G[1];
  A[1] := R[1] + G[0] - G[2];
  A[2] := R[2] + G[1] - G[0];

  B[0] := U[2]*P[2] + U[1]*Q[1];
  B[1] := U[0]*P[0] + U[2]*Q[2];
  B[2] := U[1]*P[1] + U[0]*Q[0];

  C[0] := V[2]*P[2] + V[1]*Q[1];
  C[1] := V[0]*P[0] + V[2]*Q[2];
  C[2] := V[1]*P[1] + V[0]*Q[0];

// W IS A LINEAR COMBINATION OF THE CARDINAL FUNCTIONS

  W := A[0]*Z1 + A[1]*Z2 + A[2]*Z3 + (B[0]*ZX1 + B[1]*ZX2
     + B[2]*ZX3 + C[0]*ZY1 + C[1]*ZY2 + C[2]*ZY3)/2.0;
  IF (IFLAG <> 1) then
  begin
    Exit;
  end;

// COMPUTE WX AND WY

  for I := 0 to 2 do
  begin
    RX[I] := -V[I]/AREA;
    RY[I] := U[I]/AREA;
  end;
  PHIX[0] := R[1]*RX[2] + RX[1]*R[2];
  PHIY[0] := R[1]*RY[2] + RY[1]*R[2];
  PHIX[1] := R[2]*RX[0] + RX[2]*R[0];
  PHIY[1] := R[2]*RY[0] + RY[2]*R[0];
  PHIX[2] := R[0]*RX[1] + RX[0]*R[1];
  PHIY[2] := R[0]*RY[1] + RY[0]*R[1];

  ROX[IP1] := RX[IP1]*(PHI[IP1] + 5.*C1) +
            R[IP1]*(PHIX[IP1] - RX[IP1]);
  ROY[IP1] := RY[IP1]*(PHI[IP1] + 5.*C1) +
            R[IP1]*(PHIY[IP1] - RY[IP1]);
  ROX[IP2] := RX[IP1]*(PHI[IP2] - C1) + C1*RX[IP3];
  ROY[IP2] := RY[IP1]*(PHI[IP2] - C1) + C1*RY[IP3];
  ROX[IP3] := RX[IP1]*(PHI[IP3] - C1) + C1*RX[IP2];
  ROY[IP3] := RY[IP1]*(PHI[IP3] - C1) + C1*RY[IP2];

  GX[0] := (RX[1] - RX[2])*PHI[0] + (R[1] - R[2])*PHIX[0]
         + F[0]*ROX[0] - ROX[1] + ROX[2];
  GY[0] := (RY[1] - RY[2])*PHI[0] + (R[1] - R[2])*PHIY[0]
         + F[0]*ROY[0] - ROY[1] + ROY[2];
  GX[1] := (RX[2] - RX[0])*PHI[1] + (R[2] - R[0])*PHIX[1]
         + F[1]*ROX[1] - ROX[2] + ROX[0];
  GY[1] := (RY[2] - RY[0])*PHI[1] + (R[2] - R[0])*PHIY[1]
         + F[1]*ROY[1] - ROY[2] + ROY[0];
  GX[2] := (RX[0] - RX[1])*PHI[2] + (R[0] - R[1])*PHIX[2]
         + F[2]*ROX[2] - ROX[0] + ROX[1];
  GY[2] := (RY[0] - RY[1])*PHI[2] + (R[0] - R[1])*PHIY[2]
         + F[2]*ROY[2] - ROY[0] + ROY[1];

  for I := 0 to 2 do
  begin
    PX[I] := GX[I] + PHIX[I];
    PY[I] := GY[I] + PHIY[I];
    QX[I] := GX[I] - PHIX[I];
    QY[I] := GY[I] - PHIY[I];
  end;

  AX[0] := RX[0] + GX[2] - GX[1];
  AY[0] := RY[0] + GY[2] - GY[1];
  AX[1] := RX[1] + GX[0] - GX[2];
  AY[1] := RY[1] + GY[0] - GY[2];
  AX[2] := RX[2] + GX[1] - GX[0];
  AY[2] := RY[2] + GY[1] - GY[0];

  BX[0] := U[2]*PX[2] + U[1]*QX[1];
  BY[0] := U[2]*PY[2] + U[1]*QY[1];
  BX[1] := U[0]*PX[0] + U[2]*QX[2];
  BY[1] := U[0]*PY[0] + U[2]*QY[2];
  BX[2] := U[1]*PX[1] + U[0]*QX[0];
  BY[2] := U[1]*PY[1] + U[0]*QY[0];

  CX[0] := V[2]*PX[2] + V[1]*QX[1];
  CY[0] := V[2]*PY[2] + V[1]*QY[1];
  CX[1] := V[0]*PX[0] + V[2]*QX[2];
  CY[1] := V[0]*PY[0] + V[2]*QY[2];
  CX[2] := V[1]*PX[1] + V[0]*QX[0];
  CY[2] := V[1]*PY[1] + V[0]*QY[0];

// WX AND WY ARE LINEAR COMBINATIONS OF THE CARDINAL
//   FUNCTIONS

  WX := AX[0]*Z1 + AX[1]*Z2 + AX[2]*Z3 + (BX[0]*ZX1 +
      BX[1]*ZX2 + BX[2]*ZX3 + CX[0]*ZY1 + CX[1]*ZY2 +
      CX[2]*ZY3)/2.0;
  WY := AY[0]*Z1 + AY[1]*Z2 + AY[2]*Z3 + (BY[0]*ZX1 +
      BY[1]*ZX2 + BY[2]*ZX3 + CY[0]*ZY1 + CY[1]*ZY2 +
      CY[2]*ZY3)/2.0;
end;


procedure INTRC1 (const N: longint; const PX,PY: Real; const X,Y,Z: TRealArray;
  const IADJ: TIntArray; const IEND: TIntArray; const IFLAG: longint;
  const ZXZY: TRealArray; var IST: longint; var PZ: Real; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
// THIS ROUTINE DETERMINES A PIECEWISE CUBIC FUNCTION F(X,Y)
// WHICH INTERPOLATES A SET OF DATA VALUES AND PARTIAL
// DERIVATIVES AT THE VERTICES.  F HAS CONTINUOUS FIRST
// DERIVATIVES OVER THE MESH AND EXTENDS BEYOND THE MESH
// BOUNDARY ALLOWING EXTRAPOLATION.  INTERPOLATION IS EXACT
// FOR QUADRATIC DATA.  THE VALUE OF F AT (PX,PY) IS
// RETURNED.  INTRC1 IS PART OF AN INTERPOLATION PACKAGE
// WHICH PROVIDES ROUTINES TO GENERATE, UPDATE AND PLOT THE
// MESH.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//                            N >= 3.
//
//                    PX,PY - COORDINATES OF A POINT AT WHICH
//                            F IS TO BE EVALUATED.
//
//                      X,Y - VECTORS OF COORDINATES OF THE
//                            NODES IN THE MESH.
//
//                        Z - VECTOR OF DATA VALUES AT THE
//                            NODES.
//
//                     IADJ - SET OF ADJACENCY LISTS OF NODES
//                            IN THE MESH.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS IN IADJ FOR
//                            EACH NODE IN THE MESH.
//
//                    IFLAG - OPTION INDICATOR
//                            IFLAG = 0 IF INTRC1 IS TO
//                                      PROVIDE DERIVATIVE
//                                      ESTIMATES (FROM
//                                      GRADL).
//                            IFLAG = 1 IF DERIVATIVES ARE
//                                      USER PROVIDED.
//
//                     ZXZY - 2 BY N ARRAY WHOSE COLUMNS
//                            CONTAIN ESTIMATED PARTIAL DER-
//                            IVATIVES AT THE NODES (X PAR-
//                            TIALS IN THE FIRST ROW) IF
//                            IFLAG = 1, NOT USED IF IFLAG
//                            = 0.
//
//                      IST - INDEX OF THE STARTING NODE IN
//                            THE SEARCH FOR A TRIANGLE CON-
//                            TAINING (PX,PY).  1 .LE. IST
//                            .LE. N.  THE OUTPUT VALUE OF
//                            IST FROM A PREVIOUS CALL MAY
//                            BE A GOOD CHOICE.
//
// IADJ AND IEND MAY BE CREATED BY TRMESH AND DERIVATIVE
//   ESTIMATES MAY BE COMPUTED BY GRADL OR GRADG.
//
// INPUT PARAMETERS OTHER THAN IST ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS - IST - INDEX OF ONE OF THE VERTICES OF
//                           THE TRIANGLE CONTAINING (PX,PY)
//                           UNLESS IER .LT. 0.
//
//                      PZ - VALUE OF F AT (PX,PY), OR 0 IF
//                           IER .LT. 0.
//
//                     IER - ERROR INDICATOR
//                           IER = 0 IF NO ERRORS WERE
//                                   ENCOUNTERED.
//                           IER = 1 IF NO ERRORS WERE EN-
//                                   COUNTERED AND EXTRAPOLA-
//                                   TION WAS PERFORMED.
//                           IER = -1 IF N, IFLAG, OR IST IS
//                                    OUT OF RANGE.
//                           IER = -2 IF THE NODES ARE COL-
//                                    LINEAR.
//
// MODULES REFERENCED BY INTRC1 - TRFIND, TVAL,
//             (AND OPTIONALLY)   GRADL, GETNP, SETUP,
//                                GIVENS, ROTATE
//
//***********************************************************
var
  NN, I1, I2, I3, IERR, N1, N2, INDX: longint;
  XP, YP, ZX1, ZY1, ZX2, ZY2, ZX3, ZY3, X1, Y1,
  X2, Y2, X3, Y3, Z1, Z2, Z3, DUM, DP, U, V, XQ,
  YQ, R1, R2, A1, A2, B1, B2, C1, C2, F1, F2: Real;
// LOCAL PARAMETERS -
//
// NN =                      LOCAL COPY OF N
// I1,I2,I3 =                VERTICES DETERMINED BY TRFIND
// IERR =                    ERROR FLAG FOR CALLS TO GRADL
//                             AND TVAL
// N1,N2 =                   ENDPOINTS OF THE CLOSEST BOUND-
//                             ARY EDGE TO P WHEN P IS OUT-
//                             SIDE OF THE MESH BOUNDARY
// INDX =                    IADJ INDEX OF N1 AS A NEIGHBOR
//                             OF N2
// XP,YP =                   LOCAL COPIES OF THE COORDINATES
//                             OF P=(PX,PY)
// ZX1,ZY1,ZX2,ZY2,ZX3,ZY3 = X AND Y DERIVATIVES AT THE
//                             VERTICES OF A TRIANGLE T WHICH
//                             CONTAINS P OR AT N1 AND N2
// X1,Y1,X2,Y2,X3,Y3 =       X,Y COORDINATES OF THE VERTICES
//                             OF T OR OF N1 AND N2
// Z1,Z2,Z3 =                DATA VALUES AT THE VERTICES OF T
// DUM =                     DUMMY VARIABLE FOR CALL TO TVAL
// DP =                      INNER PRODUCT OF N1-N2 AND P-N2
// U,V =                     X,Y COORDINATES OF THE VECTOR
//                             N2-N1
// XQ,YQ =                   X,Y COORDINATES OF THE CLOSEST
//                             BOUNDARY POINT TO P WHEN P IS
//                             OUTSIDE OF THE MESH BOUNDARY
// R1,R2 =                   BARYCENTRIC COORDINATES OF Q
//                             WITH RESPECT TO THE LINE SEG-
//                             MENT N2-N1 CONTAINING Q
// A1,A2,B1,B2,C1,C2 =       CARDINAL FUNCTIONS FOR EVALUAT-
//                             ING THE INTERPOLATORY SURFACE
//                             AT Q
// F1,F2 =                   CUBIC FACTORS USED TO COMPUTE
//                             THE CARDINAL FUNCTIONS
//
begin
  NN := N;
  PZ := 0.;
  IF (NN < 3)  OR  (IFLAG < 0)  OR  (IFLAG > 1)
    OR  (IST < 1)  OR  (IST > NN) then
  begin
// N, IFLAG, OR IST OUT OF RANGE
    IER := -1;
    Exit;
  end;
  XP := PX;
  YP := PY;

// FIND A TRIANGLE CONTAINING P IF P IS WITHIN THE MESH
//   BOUNDARY

  TRFIND(IST,XP,YP,X,Y,IADJ,IEND, I1,I2,I3);
  IF (I1 = 0) then
  begin
    IER := -2;
    Exit;
  end;
  IST := I1;
  IF (I3 <> 0) then
  begin
    IF (IFLAG = 1) then
    begin

// DERIVATIVES ARE USER PROVIDED

      ZX1 := ZXZY[(I1-1)*2];
      ZX2 := ZXZY[(I2-1)*2];
      ZX3 := ZXZY[(I3-1)*2];
      ZY1 := ZXZY[(I1-1)*2+1];
      ZY2 := ZXZY[(I2-1)*2+1];
      ZY3 := ZXZY[(I3-1)*2+1];
    end
    else
    begin

// COMPUTE DERIVATIVE ESTIMATES AT THE VERTICES

      GRADL(NN,I1,X,Y,Z,IADJ,IEND, ZX1,ZY1,IERR);
      GRADL(NN,I2,X,Y,Z,IADJ,IEND, ZX2,ZY2,IERR);
      GRADL(NN,I3,X,Y,Z,IADJ,IEND, ZX3,ZY3,IERR);
    end;

// SET LOCAL PARAMETERS FOR CALL TO TVAL

    X1 := X[I1-1];
    Y1 := Y[I1-1];
    X2 := X[I2-1];
    Y2 := Y[I2-1];
    X3 := X[I3-1];
    Y3 := Y[I3-1];
    Z1 := Z[I1-1];
    Z2 := Z[I2-1];
    Z3 := Z[I3-1];
    TVAL(XP,YP,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,ZX1,ZX2,
             ZX3,ZY1,ZY2,ZY3,0, PZ,DUM,DUM,IERR);
    IF (IERR <> 0) then
    begin

// NODES ARE COLLINEAR

      IER := -2;
      Exit;
    end;

    IER := 0;
    Exit;
  end;
// P IS OUTSIDE OF THE MESH BOUNDARY.  EXTRAPOLATE TO P BY
//   PASSING A LINEAR FUNCTION OF ONE VARIABLE THROUGH THE
//   VALUE AND DIRECTIONAL DERIVATIVE (IN THE DIRECTION
//   P-Q) OF THE INTERPOLATORY SURFACE (TVAL) AT Q WHERE
//   Q IS THE CLOSEST BOUNDARY POINT TO P.
//
// DETERMINE Q BY TRAVERSING THE BOUNDARY STARTING FROM
//   THE RIGHTMOST VISIBLE NODE I1.
//

  N2 := I1;

// SET N1 TO THE LAST NONZERO NEIGHBOR OF N2 AND COMPUTE DP
  repeat
    INDX := IEND[N2-1] - 1;
    N1 := IADJ[INDX-1];
    X1 := X[N1-1];
    Y1 := Y[N1-1];
    X2 := X[N2-1];
    Y2 := Y[N2-1];
    DP := (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2);
    IF (DP <= 0.0) then
    begin
// N2 IS THE CLOSEST BOUNDARY POINT TO P.  COMPUTE PARTIAL
//   DERIVATIVES AT N2.
      IF (IFLAG = 1) then
      begin
        ZX2 := ZXZY[(N2-1)*2];
        ZY2 := ZXZY[(N2-1)*2+1];
      end
      else
      begin
        GRADL(NN,N2,X,Y,Z,IADJ,IEND, ZX2,ZY2,IERR)
      end;

// COMPUTE EXTRAPOLATED VALUE AT P

      PZ := Z[N2-1] + ZX2*(XP-X2) + ZY2*(YP-Y2);
      IER := 1;
      Exit;
    end;
    IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) > 0.0) then
    begin
      break;
    end;
    N2 := N1
  until False;

// THE CLOSEST BOUNDARY POINT Q LIES ON N2-N1.  COMPUTE
//   PARTIALS AT N1 AND N2.

  IF (IFLAG = 1) then
  begin
    ZX1 := ZXZY[(N1-1)*2];
    ZY1 := ZXZY[(N1-1)*2+1];
    ZX2 := ZXZY[(N2-1)*2];
    ZY2 := ZXZY[(N2-1)*2+1];
  end
  else
  begin
    GRADL(NN,N1,X,Y,Z,IADJ,IEND, ZX1,ZY1,IERR);
    GRADL(NN,N2,X,Y,Z,IADJ,IEND, ZX2,ZY2,IERR);
  end;

// COMPUTE Q, ITS BARYCENTRIC COORDINATES, AND THE CARDINAL
//   FUNCTIONS FOR EXTRAPOLATION

  U := X2-X1;
  V := Y2-Y1;
  R1 := DP/(Sqr(U) + Sqr(V));
  R2 := 1.0 - R1;
  XQ := R1*X1 + R2*X2;
  YQ := R1*Y1 + R2*Y2;
  F1 := R1*R1*R2;
  F2 := R1*R2*R2;
  A1 := R1 + (F1-F2);
  A2 := R2 - (F1-F2);
  B1 := U*F1;
  B2 := -U*F2;
  C1 := V*F1;
  C2 := -V*F2;

// COMPUTE THE VALUE OF THE INTERPOLATORY SURFACE (TVAL)
//   AT Q

  PZ := A1*Z[N1-1] + A2*Z[N2-1] + B1*ZX1 + B2*ZX2 +
      C1*ZY1 + C2*ZY2;

// COMPUTE THE EXTRAPOLATED VALUE AT P

  PZ := PZ + (R1*ZX1 + R2*ZX2)*(XP-XQ) +
           (R1*ZY1 + R2*ZY2)*(YP-YQ);
  IER := 1;
end;

procedure UNIF (const N: longint; const X,Y,Z: TRealArray; const IADJ: TIntArray;
  const IEND: TIntArray; const NROW,NX,NY: longint; const PX: TRealArray;
  const PY: TRealArray; const IFLAG: longint; var ZXZY: TRealArray; var ZZ: TRealArray;
  var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A THIESSEN TRIANGULATION OF A SET OF POINTS IN THE
// PLANE WITH CORRESPONDING DATA VALUES, THIS ROUTINE INTERP-
// OLATES THE DATA VALUES TO A SET OF RECTANGULAR GRID POINTS
// FOR SUCH APPLICATIONS AS CONTOURING.  THE INTERPOLANT IS
// ONCE CONTINUOUSLY DIFFERENTIABLE.  EXTRAPOLATION IS PER-
// FORMED AT GRID POINTS EXTERIOR TO THE TRIANGULATION.
//
// INPUT PARAMETERS - N - NUMBER OF NODES IN THE TRIANGULA-
//                        TION.  N >= 3
//
//                X,Y,Z - N-VECTORS OF NODAL COORDINATES AND
//                        DATA VALUES.
//
//            IADJ,IEND - TRIANGULATION DATA STRUCTURE -- MAY
//                        BE CREATED BY TRMESH.
//
//                 NROW - NUMBER OF ROWS IN THE DIMENSION
//                        STATEMENT OF ZZ.
//
//                NX,NY - NUMBER OF ROWS AND COLUMNS, RESPEC-
//                        TIVELY, IN THE RECTANGULAR GRID.
//                        1 .LE. NX .LE. NROW AND 1 .LE. NY.
//
//                PX,PY - VECTORS OF LENGTH NX AND NY, RE-
//                        SPECTIVELY, CONTAINING THE COORDI-
//                        NATES OF THE GRID LINES.
//
//                IFLAG - OPTION INDICATOR
//                        IFLAG = 0 IF DERIVATIVE ESTIMATES
//                                  AT THE VERTICES OF A
//                                  TRIANGLE ARE TO BE RECOM-
//                                  PUTED FOR EACH GRID POINT
//                                  IN THE TRIANGLE AND NOT
//                                  SAVED.
//                        IFLAG = 1 IF DERIVATIVE ESTIMATES
//                                  ARE INPUT IN ZXZY.
//                        IFLAG = 2 IF DERIVATIVE ESTIMATES
//                                  ARE TO BE COMPUTED ONCE
//                                  FOR EACH NODE (BY GRADL)
//                                  AND SAVED IN ZXZY.
//
//                 ZXZY - NOT USED IF IFLAG = 0, 2 BY N ARRAY
//                        WHOSE COLUMNS CONTAIN THE X AND Y
//                        PARTIAL DERIVATIVE ESTIMATES (X
//                        PARTIALS IN THE FIRST ROW) IF
//                        IFLAG = 1, OR 2 BY N ARRAY (OR WORK
//                        SPACE OF LENGTH >= 2*N) IF IFLAG
//                        = 2.
//
// DERIVATIVE ESTIMATES MAY BE COMPUTED BY GRADL OR GRADG.
//
//                   ZZ - NROW BY NCOL ARRAY WITH NROW >=
//                        NX AND NCOL >= NY.
//
// NONE OF THE INPUT PARAMETERS ARE ALTERED EXCEPT AS
//   NOTED BELOW.
//
// OUTPUT PARAMETERS - ZXZY - 2 BY N ARRAY WHOSE COLUMNS CON-
//                            TAIN X AND Y PARTIAL DERIVATIVE
//                            ESTIMATES AT THE NODES IF IFLAG
//                            = 2 AND IER >= 0, NOT ALTERED
//                            IF IFLAG .NE. 2.
//
//                       ZZ - INTERPOLATED VALUES AT THE GRID
//                            POINTS IF IER >= 0.
//                            ZZ(I,J) = F(PX(I),PY(J)) FOR
//                            I = 1,...,NX AND J = 1,...,NY.
//
//                      IER - ERROR INDICATOR
//                            IER >= 0 IF NO ERRORS WERE
//                                       ENCOUNTERED.  IER
//                                       CONTAINS THE NUMBER
//                                       OF GRID POINTS EXT-
//                                       ERIOR TO THE TRIAN-
//                                       GULATION BOUNDARY.
//                            IER  =  -1 IF N, NX, NY, OR
//                                       IFLAG IS OUT OF
//                                       RANGE.
//                            IER  =  -2 IF THE NODES ARE
//                                       COLLINEAR.
//
// MODULES REFERENCED BY UNIF - INTRC1, TRFIND, TVAL,
//           (AND OPTIONALLY)   GRADL, GETNP, SETUP, GIVENS,
//                                AND ROTATE
//
//***********************************************************
const
  NST = 1;
var
  IST, NN, NI, NJ, IFL, I, J, IERR,
  NEX: longint;
// LOCAL PARAMETERS -
//
// IST =   PARAMETER FOR INTRC1
// NST =   INITIAL VALUE FOR IST
// NN =    LOCAL COPY OF N
// NI,NJ = LOCAL COPIES OF NX AND NY
// IFL =   LOCAL COPY OF IFLAG FOR INTRC1
// I,J =   DO-LOOP INDICES
// IERR =  ERROR FLAG FOR CALLS TO GRADL AND INTRC1
// NEX =   NUMBER OF GRID POINTS EXTERIOR TO THE TRIANGULA-
//           TION BOUNDARY (NUMBER OF EXTRAPOLATED VALUES)
begin
  NN := N;
  NI := NX;
  NJ := NY;
  IFL := IFLAG;
  IF (NN < 3)  OR  (NI < 1)  OR  (NI > NROW)
    OR  (NJ < 1)  OR  (IFL < 0)  OR
    (IFL > 2) then
  begin
    //N, NX, NY, OR IFLAG IS OUT OF RANGE
    IER := -1;
    Exit;
  end;
  IST := NST;
  IF (IFL = 2) then
  begin

  // COMPUTE DERIVATIVE ESTIMATES AT THE NODES.

    IFL := 1;
    for I := 1 to NN do
    begin
      GRADL(NN,I,X,Y,Z,IADJ,IEND,
        ZXZY[(I-1) * 2], ZXZY[(I-1) * 2 + 1],
        IERR);
      IF (IERR < 0) then
      begin
        // TRIANGULATION NODES ARE COLLINEAR
        IER := -2;
        Exit;
      end;
    end;
  end;

  // COMPUTE INTERPOLATED VALUES
  NEX := 0;
  for J := 1 to NJ do
  begin
    for I := 1 to NI do
    begin
      INTRC1(NN,PX[I-1],PY[J-1],X,Y,Z,IADJ,IEND,IFL,
                 ZXZY, IST, ZZ[(J-1)*NI+I-1],IERR);
      IF (IERR < 0) then
      begin
  // TRIANGULATION NODES ARE COLLINEAR

        IER := -2;
        Exit;
      end;

      NEX := NEX + IERR;
    end;
  end;
  IER := NEX;
end;

procedure CIRCUM (const X1,X2,X3,Y1,Y2,Y3: Real; var CX,CY: Real;
  var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS SUBROUTINE COMPUTES THE COORDINATES OF THE CENTER
// OF A CIRCLE DEFINED BY THREE POINTS IN THE PLANE.
//
// INPUT PARAMETERS -
//
//       X1,...,Y3 - X AND Y COORDINATES OF THREE POINTS IN
//                   THE PLANE.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS -
//
//       CX,CY - COORDINATES OF THE CENTER OF THE CIRCLE
//               UNLESS IER = 1.
//
//       IER - ERROR INDICATOR
//             IER = 0 IF NO ERRORS WERE ENCOUNTERED.
//             IER = 1 IF THE POINTS ARE COLLINEAR.
//
// MODULES REQUIRED BY CIRCUM - NONE
//
//***********************************************************
var
  U, V, DS: array[0..2] of Real;
  A, FX, FY: Real;
  I: longint;
begin
// SET U(K) AND V(K) TO THE X AND Y COMPONENTS OF THE EDGE
//   OPPOSITE VERTEX K, TREATING THE POINTS AS VERTICES OF
//   A TRIANGLE.

  U[0] := X3 - X2;
  U[1] := X1 - X3;
  U[2] := X2 - X1;
  V[0] := Y3 - Y2;
  V[1] := Y1 - Y3;
  V[2] := Y2 - Y1;

// SET A TO TWICE THE SIGNED AREA OF THE TRIANGLE.  A .GT. 0
//   IFF (X3,Y3) IS STRICTLY TO THE LEFT OF THE EDGE FROM
//   (X1,Y1) TO (X2,Y2).

  A := U[0]*V[1] - U[1]*V[0];
  IF (A = 0.0) then
  begin
// COLLINEAR POINTS
    IER := 1;
    Exit;
  end;

// SET DS(K) TO THE SQUARED DISTANCE FROM THE ORIGIN TO
//   VERTEX K.

  DS[0] := Sqr(X1) + Sqr(Y1);
  DS[1] := Sqr(X2) + Sqr(Y2);
  DS[2] := Sqr(X3) + Sqr(Y3);

// COMPUTE FACTORS OF CX AND CY.

  FX := 0.0;
  FY := 0.0;
  for I := 0 to 2 do
  begin
    FX := FX - DS[I]*V[I];
    FY := FY + DS[I]*U[I];
  end;
  CX := FX/2.0/A;
  CY := FY/2.0/A;
  IER := 0;
end;


procedure TRMTST(const N: longint; const X,Y: TRealArray;
    const IADJ: TIntArray; const IEND: TIntArray; const TOL: Real;
    const LUN: longint; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE TESTS THE VALIDITY OF THE DATA STRUCTURE
// REPRESENTING A THIESSEN TRIANGULATION CREATED BY SUBROU-
// TINE TRMESH.  THE FOLLOWING PROPERTIES ARE TESTED --
//   1)  IEND(1) >= 3 AND IEND(K) >= IEND(K-1)+3 FOR K =
//       2,...,N (EACH NODE HAS AT LEAST THREE NEIGHBORS).
//   2)  0 .LE. IADJ(K) .LE. N FOR K = 1,...,IEND(N) (IADJ
//       ENTRIES ARE NODAL INDICES OR ZEROS REPRESENTING THE
//       BOUNDARY).
//   3)  NB >= 3, NT = 2N-NB-2, AND NA = 3N-NB-3 WHERE NB,
//       NT, AND NA ARE THE NUMBERS OF BOUNDARY NODES, TRI-
//       ANGLES, AND ARCS, RESPECTIVELY.
//   4)  EACH CIRCUMCIRCLE DEFINED BY THE VERTICES OF A TRI-
//       ANGLE CONTAINS NO NODES IN ITS INTERIOR.  THIS PROP-
//       ERTY DISTINGUISHES A THIESSEN TRIANGULATION FROM AN
//       ARBITRARY TRIANGULATION OF THE NODES.
// NOTE THAT NO TEST IS MADE FOR THE PROPERTY THAT A TRIANGU-
// LATION COVERS THE CONVEX HULL OF THE NODES, AND THUS A
// TEST ON A DATA STRUCTURE ALTERED BY SUBROUTINE DELETE
// SHOULD NOT RESULT IN AN ERROR.
//
// INPUT PARAMETERS -
//
//       N - NUMBER OF NODES.  N >= 3.
//
//       X,Y - NODAL COORDINATES.
//
//       IADJ,IEND - TRIANGULATION DATA STRUCTURE.  SEE SUB-
//                   ROUTINE TRMESH.
//
//       TOL - NONNEGATIVE TOLERANCE TO ALLOW FOR FLOATING-
//             POINT ERRORS IN THE CIRCUMCIRCLE TEST.  AN
//             ERROR SITUATION IS DEFINED AS R**2 - D**2 .GT.
//             TOL WHERE R IS THE RADIUS OF A CIRCUMCIRCLE
//             AND D IS THE DISTANCE FROM THE CIRCUMCENTER
//             TO THE NEAREST NODE.  A REASONABLE VALUE
//             FOR TOL IS 10*EPS WHERE EPS IS THE MACHINE
//             PRECISION.  THE TEST IS EFFECTIVELY BYPASSED
//             BY MAKING TOL LARGER THAN THE DIAMETER OF THE
//             CONVEX HULL OF THE NODES.
//
//       LUN - LOGICAL UNIT NUMBER FOR PRINTING ERROR MES-
//             SAGES.  IF LUN .LT. 1 OR LUN .GT. 99, NO MES-
//             SAGES ARE PRINTED.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETER -
//
//       IER - ERROR INDICATOR
//             IER = -1 IF ONE OR MORE NULL TRIANGLES (AREA =
//                      0) ARE PRESENT BUT NO (OTHER) ERRORS
//                      WERE ENCOUNTERED.  A NULL TRIANGLE IS
//                      AN ERROR ONLY IF IT OCCURS IN THE
//                      THE INTERIOR.
//             IER = 0 IF NO ERRORS OR NULL TRIANGLES WERE
//                     ENCOUNTERED.
//             IER = 1 IF N .LT. 3 OR TOL .LT. 0.
//             IER = 2 IF AN IEND OR IADJ ENTRY IS OUT OF
//                     RANGE.
//             IER = 3 IF THE TRIANGULATION PARAMETERS (NB,
//                     NT, AND NA) ARE INCONSISTENT.
//             IER = 4 IF A TRIANGLE CONTAINS A NODE INTERIOR
//                     TO ITS CIRCUMCIRCLE.
//             THE ERROR SITUATIONS ARE TESTED IN THE ORDER
//               DEFINED BY THE (POSITIVE) IER VALUES.
//
// MODULE REQUIRED BY TRMTST - CIRCUM
//
//***********************************************************
var
  RITE: boolean;
  IFORTRAN: longint;
  NN: Integer;
  RTOL: Real;
  NB: Integer;
  NT: Integer;
  NULL: Integer;
  NFAIL: Integer;
  INDF: Integer;
  N1: Integer;
  INDL: Integer;
  INDX: Integer;
  N2: Integer;
  N3: Integer;
  CX: Real;
  CY: Real;
  IERR: Integer;
  R: Real;
  I: Integer;
  NA: Integer;
begin
  // SET LOCAL VARIABLES, TEST FOR ERRORS IN INPUT, AND
  //   INITIALIZE COUNTS.

  IFORTRAN := 2;
  NN := N;
  RTOL := TOL;
  RITE := (LUN >= 1)  AND  (LUN <= 99);
  IF (NN < 3)  OR  (RTOL < 0.0) then
  begin
  // N OR TOL IS OUT OF RANGE.
    IER := 1;
    IF (RITE) then INVALID_INPUT_PARAMETER(IFORTRAN, N, RTOL);
    Exit;
  end;
  NB := 0;
  NT := 0;
  NULL := 0;
  NFAIL := 0;

  // LOOP ON TRIANGLES (N1,N2,N3) SUCH THAT N2 AND N3 INDEX
  //   ADJACENT NEIGHBORS OF N1 AND ARE BOTH LARGER THAN N1
  //   (TRIANGLES ARE ASSOCIATED WITH THEIR SMALLEST INDEX).

  INDF := 1;
  for N1 := 1 to NN do
  begin
    INDL := IEND[N1-1];
    IF (INDL < INDF+2) then
    begin
  // IEND ENTRY OUT OF RANGE
      IER := 2;
      IF (RITE) then LESS_THAN_3_NEIGHBORS(IFORTRAN, N1);
    end;
    IF (IADJ[INDL-1] = 0) then NB := NB + 1;

  //   LOOP ON NEIGHBORS OF N1

    for INDX := INDF to INDL do
    begin
      N2 := IADJ[INDX-1];
      IF (N2 < 0)  OR  (N2 > NN)  OR
         ((INDX < INDL)  AND  (N2 = 0)) then
      begin
  // IADJ ENTRY OUT OF RANGE
        IER := 2;
        IF (RITE) then ENTRY_OUT_OF_RANGE(IFORTRAN, INDX);
      end;
      IF (INDX < INDL) then N3 := IADJ[INDX-1+1];
      IF (INDX = INDL) then N3 := IADJ[INDF-1];
      IF (N2 < N1)  OR  (N3 < N1) then
      begin
        Continue;
      end;
      NT := NT + 1;

  //   COMPUTE THE COORDINATES OF THE CIRCUMCENTER OF
  //     (N1,N2,N3).

      CIRCUM (X[N1-1],X[N2-1],X[N3-1],Y[N1-1],Y[N2-1],Y[N3-1],
                  CX,CY,IERR);
      IF (IERR <> 0) then NULL := NULL + 1;
      IF (IERR <> 0) then Continue;

  //   TEST FOR NODES WITHIN THE CIRCUMCIRCLE.

      R := Sqr(CX-X[N1-1]) + Sqr(CY-Y[N1-1]) - RTOL;
      IF (R < 0.0) then Continue;
      for I := 1 to NN do
      begin
        IF (I = N1)  OR  (I = N2)  OR
           (I = N3) then
        begin
          Continue;
        end;
        IF (Sqr(CX-X[I-1]) + Sqr(CY-Y[I-1]) < R) then
        begin
  //   NODE I IS INTERIOR TO THE CIRCUMCIRCLE OF (N1,N2,N3).
          NFAIL := NFAIL + 1;
          break;
        end;
      end;
    end;
    INDF := INDL + 1;
  end;

  // CHECK PARAMETERS FOR CONSISTENCY AND TEST FOR NFAIL = 0.

  NA := (IEND[NN-1]-NB) div 2;
  IF (NB < 3)  OR  (NT <> 2*NN-NB-2)  OR
     (NA <> 3*NN-NB-3) then
  begin
  // INCONSISTENT TRIANGULATION PARAMETERS
    IER := 3;
    IF (RITE) then INCONSISTENT_PARAMETERS(IFORTRAN, NB, NT, NA);
    Exit;
  end;
  IF (NFAIL <> 0) then
  begin
  // CIRCUMCIRCLE TEST FAILURE
    IER := 4;
    IF (RITE) then CIRCUMCIRCLE_TEST_FAILURE(IFORTRAN, NFAIL);
    Exit;
  end;

  // NO ERRORS WERE ENCOUNTERED.

  IER := 0;
  IF (NULL = 0) then
  begin
    Exit;
  end;
  IER := -1;
  IF (RITE) then NULL_TRIANGLES(IFORTRAN, NULL);
end;


procedure TRPRNT (const N, LUNIT: longint;
   const X,Y: TRealArray; const IADJ: TIntArray;
  const IEND: TIntArray; const IFLAG: longint);
//      SUBROUTINE TRPRNT (N,LUNIT,X,Y,IADJ,IEND,IFLAG,IFORTRAN)
//      DLL_IMPORT ADJACENCY_SETS, NEIGHBORS, X_NODE, ADJACENCY,
//     &  Positions, BlankLine, NewPage, BOUNDARY_NODES,
//     &  OUT_OF_RANGE_TRPRNT
//      INTEGER N, LUNIT, IADJ(1), IEND(N), IFLAG
//      REAL    X(N), Y(N)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
//C THIS ROUTINE PRINTS THE ADJACENCY LISTS AND, OPTIONALLY,
//C THE NODAL COORDINATES.  THE NUMBERS OF BOUNDARY NODES,
//C TRIANGLES, AND ARCS ARE ALSO PRINTED.
//C
//C INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//C                            3 .LE. N .LE. 9999.
//C
//C                    LUNIT - LOGICAL UNIT FOR OUTPUT.  1
//C                            .LE. LUNIT .LE. 99.  OUTPUT IS
//C                            PRINTED ON UNIT 6 IF LUNIT IS
//C                            OUT OF RANGE.
//C
//C                      X,Y - VECTORS OF COORDINATES OF THE
//C                            NODES IN THE MESH.  NOT USED
//C                            UNLESS IFLAG = 0.
//C
//C                     IADJ - SET OF ADJACENCY LISTS OF NODES
//C                            IN THE MESH.
//C
//C                     IEND - POINTERS TO THE ENDS OF
//C                            ADJACENCY LISTS IN IADJ FOR
//C                            EACH NODE IN THE MESH.
//C
//C                    IFLAG - OPTION INDICATOR
//C                            IFLAG = 0 IF X AND Y ARE TO BE
//C                                      PRINTED (TO 6 DECIMAL
//C                                      PLACES).
//C                            IFLAG = 1 IF X AND Y ARE NOT
//C                                      TO BE PRINTED.
//C
//C IADJ AND IEND MAY BE CREATED BY TRMESH.
//C
//C NONE OF THE PARAMETERS ARE ALTERED BY THIS ROUTINE.
//C
//C MODULES REFERENCED BY TRPRNT - NONE
//C
//C***********************************************************
//C
//      INTEGER NN, NMAX, LUN, NODE, INDF, INDL, NL, NLMAX,
//     .        INC, I, NB, NT, NA
//      INTEGER IFORTRAN
//      DATA    NMAX/9999/, NLMAX/60/
//C
//C LOCAL PARAMETERS -
//C
//C NN =        LOCAL COPY OF N
//C NMAX =      UPPER BOUND ON N
//C LUN =       LOCAL COPY OF LUNIT
//C NODE =      INDEX OF A NODE
//C INDF,INDL = IADJ INDICES OF THE FIRST AND LAST NEIGHBORS
//C               OF NODE
//C NL =        NUMBER OF LINES PRINTED ON A PAGE
//C NLMAX =     MAXIMUM NUMBER OF PRINT LINES PER PAGE EXCEPT
//C               FOR THE LAST PAGE WHICH HAS 3 ADDITIONAL
//C               LINES
//C INC =       INCREMENT FOR NL
//C I =         IADJ INDEX FOR IMPLIED DO-LOOP
//C NB =        NUMBER OF BOUNDARY NODES
//C NT =        NUMBER OF TRIANGLES
//C NA =        NUMBER OF ARCS (UNDIRECTED EDGES)
//C
//!      IFORTRAN = 1
var
  NN: integer;
  IFORTRAN: Integer;
  NB: integer;
  INDF: integer;
  NODE: integer;
  INDL: integer;
  i: integer;
  NT: integer;
  NA: integer;
  LocalNode: Integer;
  label _2;
  label _4;
  label _5;
begin

      IFORTRAN := 2;
      NN := N;
//      LUN := LUNIT;
//      IF (LUN < 1)  or  (LUN > 99) then LUN := 6;

//      NN = N
//      LUN = LUNIT
//      IF (LUN .LT. 1  .OR.  LUN .GT. 99) LUN = 6
//C
//C PRINT HEADING AND INITIALIZE NL
//C

      ADJACENCY_SETS(IFORTRAN, NN);

//      call ADJACENCY_SETS(IFORTRAN, NN)
//!      WRITE (LUN,100) NN

      IF (NN < 3)  {or  (NN > NMAX)} then GOTO _5;
//      NL := 6;
      IF (IFLAG = 0) then GOTO _2;

//      IF (NN .LT. 3  .OR.  NN .GT. NMAX) GO TO 5
//      NL = 6
//      IF (IFLAG .EQ. 0) GO TO 2
//C
//C PRINT IADJ ONLY
//C
      NEIGHBORS(IFORTRAN);

//      call NEIGHBORS(IFORTRAN)
//!      WRITE (LUN,101)

      NB := 0;
      INDF := 1;
      for NODE := 1 to NN do
      begin
        INDL := IEND[NODE-1];
        IF (IADJ[INDL-1] = 0) then NB := NB + 1;
//        INC := (INDL - INDF) div 14 + 2;
//        NL := NL + INC;
//        IF (NL > NLMAX) then NewPage(IFORTRAN);
//        IF (NL .GT. NLMAX) NL := INC
        for i := INDF to INDL do
        begin
          ADJACENCY(IFORTRAN, NODE, IADJ[I-1]);
        end;
        IF (INDL-INDF <> 13) then BlankLine(IFORTRAN);
        INDF := INDL + 1;
      end;
      GOTO _4;

//      NB = 0
//      INDF = 1
//      DO 1 NODE = 1,NN
//        INDL = IEND(NODE)
//        IF (IADJ(INDL) .EQ. 0) NB = NB + 1
//        INC = (INDL - INDF)/14 + 2
//        NL = NL + INC
//!        IF (NL .GT. NLMAX) WRITE (LUN,106)
//        IF (NL .GT. NLMAX) call NewPage(IFORTRAN)
//        IF (NL .GT. NLMAX) NL = INC
//        DO i = INDF,INDL
//          call ADJACENCY(IFORTRAN, NODE, IADJ(I))
//        END DO
//!        WRITE (LUN,103) NODE, (IADJ(I), I = INDF,INDL)
//!        IF (INDL-INDF .NE. 13) WRITE (LUN,105)
//        IF (INDL-INDF .NE. 13) CALL BlankLine(IFORTRAN)
//        INDF = INDL + 1
//    1   CONTINUE
//      GO TO 4
//C
//C PRINT X, Y, AND IADJ
//C
//!    2 WRITE (LUN,102)
  _2:
      X_NODE(IFORTRAN);
      NB := 0;
      INDF := 1;
      for NODE := 1 to NN do
      begin
        INDL := IEND[NODE-1];
        IF (IADJ[INDL-1] = 0) then NB := NB + 1;
//        INC := (INDL - INDF) div 8 + 2;
//        NL := NL + INC;
//        IF (NL > NLMAX) then NewPage(IFORTRAN);
//        IF (NL .GT. NLMAX) NL := INC
        for I := INDF to INDL do
        begin
          LocalNode := NODE;
          Positions(IFORTRAN, LocalNode, X[NODE-1], Y[NODE-1], IADJ[I-1]);
        end;
        INDF := INDL + 1;
      end;


//    2 call X_NODE(IFORTRAN)
//      NB = 0
//      INDF = 1
//      DO 3 NODE = 1,NN
//        INDL = IEND(NODE)
//        IF (IADJ(INDL) .EQ. 0) NB = NB + 1
//        INC = (INDL - INDF)/8 + 2
//        NL = NL + INC
//!        IF (NL .GT. NLMAX) WRITE (LUN,106)
//        IF (NL .GT. NLMAX) call NewPage(IFORTRAN)
//        IF (NL .GT. NLMAX) NL = INC
//        do I = INDF,INDL
//          call Positions(IFORTRAN, NODE, X(NODE), Y(NODE), IADJ(I))
//        end do
//!        WRITE (LUN,104) NODE, X(NODE), Y(NODE),
//!     .                  (IADJ(I), I = INDF,INDL)
//!        IF (INDL-INDF .NE. 7) WRITE (LUN,105)
//!        IF (INDL-INDF .NE. 7) call BlankLine(IFORTRAN)
//        INDF = INDL + 1
//    3   CONTINUE
//C
//C PRINT NB, NA, AND NT
//C
  _4:
      NT := 2*NN - NB - 2;
      NA := NT + NN - 1;
      BOUNDARY_NODES(IFORTRAN, NB, NA, NT);
      Exit;


//    4 NT = 2*NN - NB - 2
//      NA = NT + NN - 1
//!      WRITE (LUN,107) NB, NA, NT
//      call BOUNDARY_NODES(IFORTRAN, NB, NA, NT)
//      RETURN
//C
//C N IS OUT OF RANGE
//C
//!    5 WRITE (LUN,108)
  _5:
    OUT_OF_RANGE_TRPRNT(IFORTRAN);
//    5 call OUT_OF_RANGE_TRPRNT(IFORTRAN)
//      RETURN
//C
//C PRINT FORMATS
//C
//!  100 FORMAT (1H1,26X,23HADJACENCY SETS,    N = ,I5//)
//!  101 FORMAT (1H ,4HNODE,32X,17HNEIGHBORS OF NODE//)
//!  102 FORMAT (1H ,4HNODE,5X,7HX(NODE),8X,7HY(NODE),
//!     .        20X,17HNEIGHBORS OF NODE//)
//!  103 FORMAT (1H ,I4,5X,14I5/(1H ,9X,14I5))
//!  104 FORMAT (1H ,I4,2E15.6,5X,8I5/(1H ,39X,8I5))
//!  105 FORMAT (1H )
//!  106 FORMAT (1H1)
//!  107 FORMAT (/1H ,5HNB = ,I4,15H BOUNDARY NODES,10X,
//!     .        5HNA = ,I5,5H ARCS,10X,5HNT = ,I5,
//!     .        10H TRIANGLES)
//!  108 FORMAT (1H ,10X,25H*** N IS OUT OF RANGE ***)
//      END

end;

FUNCTION LEFT (const X1,Y1,X2,Y2,X0,Y0:Real): longbool;
//      LOGICAL FUNCTION LEFT (X1,Y1,X2,Y2,X0,Y0)
//      REAL    X1, Y1, X2, Y2, X0, Y0
//C
//C***********************************************************
//C
//C                                               From TRIPACK
//C                                            Robert J. Renka
//C                                  Dept. of Computer Science
//C                                       Univ. of North Texas
//C                                           renka@cs.unt.edu
//C                                                   09/01/88
//C
//C   This function determines whether node N0 is to the left
//C or to the right of the line through N1-N2 as viewed by an
//C observer at N1 facing N2.
//C
//C
//C On input:
//C
//C       X1,Y1 = Coordinates of N1.
//C
//C       X2,Y2 = Coordinates of N2.
//C
//C       X0,Y0 = Coordinates of N0.
//C
//C Input parameters are not altered by this function.
//C
//C On output:
//C
//C       LEFT = .TRUE. if and only if (X0,Y0) is on or to the
//C              left of the directed line N1->N2.
//C
//C Modules required by LEFT:  None
//C
//C***********************************************************
//C
var
  DX1, DY1, DX2, DY2: Real;
//      REAL DX1, DY1, DX2, DY2
//C
//C Local parameters:
//C
//C DX1,DY1 = X,Y components of the vector N1->N2
//C DX2,DY2 = X,Y components of the vector N1->N0
//C
begin
      DX1 := X2-X1;
      DY1 := Y2-Y1;
      DX2 := X0-X1;
      DY2 := Y0-Y1;
//      DX1 = X2-X1
//      DY1 = Y2-Y1
//      DX2 = X0-X1
//      DY2 = Y0-Y1
//C
//C If the sign of the vector cross product of N1->N2 and
//C   N1->N0 is positive, then sin(A) > 0, where A is the
//C   angle between the vectors, and thus A is in the range
//C   (0,180) degrees.
//C
      result := DX1*DY2 >= DX2*DY1;
//      LEFT = DX1*DY2 .GE. DX2*DY1
//      RETURN
//      END
end;

procedure EDGE (const IN1,IN2: longint; const X,Y: TRealArray;
    var LWK: longint; var IWK: TIntArray;
    var IADJ: TIntArray; var IEND: TIntArray; var IER: longint);
//      SUBROUTINE EDGE (IN1,IN2,X,Y, LWK,IWK,IADJ,IEND, IER)
//      LOGICAL SWPTST
//      INTEGER IN1, IN2, LWK, IWK(2,1), IADJ(1), IEND(1), IER
//      REAL    X(1), Y(1)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A TRIANGULATION OF N NODES AND A PAIR OF NODAL
//C INDICES IN1 AND IN2, THIS ROUTINE SWAPS ARCS AS NECESSARY
//C TO FORCE IN1 AND IN2 TO BE ADJACENT.  ONLY ARCS WHICH
//C INTERSECT IN1-IN2 ARE SWAPPED OUT.  IF A THIESSEN TRIANGU-
//C LATION IS INPUT, THE RESULTING TRIANGULATION IS AS CLOSE
//C AS POSSIBLE TO A THIESSEN TRIANGULATION IN THE SENSE THAT
//C ALL ARCS OTHER THAN IN1-IN2 ARE LOCALLY OPTIMAL.
//C   A SEQUENCE OF CALLS TO EDGE MAY BE USED TO FORCE THE
//C PRESENCE OF A SET OF EDGES DEFINING THE BOUNDARY OF A NON-
//C CONVEX REGION.  SUBSEQUENT DELETION OF EDGES OUTSIDE THIS
//C REGION (BY SUBROUTINE DELETE) RESULTS IN A NONCONVEX TRI-
//C ANGULATION WHICH MAY SERVE AS A FINITE ELEMENT GRID.
//C (EDGE SHOULD NOT BE CALLED AFTER A CALL TO DELETE.)  IF,
//C ON THE OTHER HAND, INTERPOLATION IS TO BE PERFORMED IN THE
//C NONCONVEX REGION, EDGES MUST NOT BE DELETED, BUT IT IS
//C STILL ADVANTAGEOUS TO HAVE THE NONCONVEX BOUNDARY PRESENT
//C IF IT IS DESIRABLE THAT INTERPOLATED VALUES BE INFLUENCED
//C BY THE GEOMETRY.  NOTE THAT SUBROUTINE GETNP WHICH IS USED
//C TO SELECT THE NODES ENTERING INTO LOCAL DERIVATIVE ESTI-
//C MATES WILL NOT NECESSARILY RETURN CLOSEST NODES IF THE
//C TRIANGULATION HAS BEEN RENDERED NONOPTIMAL BY A CALL TO
//C EDGE.  HOWEVER, THE EFFECT WILL BE MERELY TO FURTHER EN-
//C HANCE THE INFLUENCE OF THE NONCONVEX GEOMETRY ON INTERPO-
//C LATED VALUES.
//C
//C INPUT PARAMETERS - IN1,IN2 - INDICES (OF X AND Y) IN THE
//C                              RANGE 1,...,N DEFINING A PAIR
//C                              OF NODES TO BE CONNECTED BY
//C                              AN ARC.
//C
//C                        X,Y - N-VECTORS CONTAINING CARTE-
//C                              SIAN COORDINATES OF THE
//C                              NODES.
//C
//C THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//C
//C                        LWK - NUMBER OF COLUMNS RESERVED
//C                              FOR IWK.  THIS MUST BE AT
//C                              LEAST NI -- THE NUMBER OF
//C                              ARCS WHICH INTERSECT IN1-IN2.
//C                              (NI IS BOUNDED BY N-3).
//C
//C                        IWK - INTEGER WORK ARRAY DIMENSION-
//C                              ED 2 BY LWK (OR VECTOR OF
//C                              LENGTH .GE. 2*LWK).
//C
//C                  IADJ,IEND - DATA STRUCTURE DEFINING THE
//C                              TRIANGULATION.  SEE SUBROU-
//C                              TINE TRMESH.
//C
//C OUTPUT PARAMETERS - LWK - NUMBER OF IWK COLUMNS REQUIRED
//C                           IF IER = 0 OR IER = 2.  LWK = 0
//C                           IFF IN1 AND IN2 WERE ADJACENT
//C                           ON INPUT.
//C
//C                     IWK - CONTAINS THE INDICES OF THE END-
//C                           POINTS OF THE NEW ARCS OTHER
//C                           THAN IN1-IN2 UNLESS IER .GT. 0
//C                           OR LWK = 0.  NEW ARCS TO THE
//C                           LEFT OF IN1->IN2 ARE STORED IN
//C                           THE FIRST K-1 COLUMNS (LEFT POR-
//C                           TION OF IWK), COLUMN K CONTAINS
//C                           ZEROS, AND NEW ARCS TO THE RIGHT
//C                           OF IN1->IN2 OCCUPY COLUMNS K+1,
//C                           ...,LWK.  (K CAN BE DETERMINED
//C                           BY SEARCHING IWK FOR THE ZEROS.)
//C
//C               IADJ,IEND - UPDATED IF NECESSARY TO REFLECT
//C                           THE PRESENCE OF AN ARC CONNECT-
//C                           ING IN1 AND IN2, UNALTERED IF
//C                           IER .NE. 0.
//C
//C                     IER - ERROR INDICATOR
//C                           IER = 0 IF NO ERRORS WERE EN-
//C                                   COUNTERED.
//C                           IER = 1 IF IN1 .LT. 1, IN2 .LT.
//C                                   1, IN1 = IN2, OR LWK
//C                                   .LT. 0 ON INPUT.
//C                           IER = 2 IF MORE SPACE IS REQUIR-
//C                                   ED IN IWK.  SEE LWK.
//C                           IER = 3 IF IN1 AND IN2 COULD NOT
//C                                   BE CONNECTED DUE TO AN
//C                                   INVALID DATA STRUCTURE.
//C
//C MODULES REFERENCED BY EDGE - SWAP, INDEX, SHIFTD, SWPTST
//C
//C***********************************************************
//C
var
  N1, N2, IWEND, IWL, INDF, INDX, N1LST, NL, NR,
  NEXT, IWF, LFT, N0, IWC, IWCP1, IWCM1, I, IO1,
  IO2, INDL: longint;
//      INTEGER N1, N2, IWEND, IWL, INDF, INDX, N1LST, NL, NR,
//     .        NEXT, IWF, LFT, N0, IWC, IWCP1, IWCM1, I, IO1,
//     .        IO2, INDL
  X1, Y1, X2, Y2, X0, Y0: Real;
  SWP: boolean;
  label _1;
  label _2;
  label _3;
  label _4;
  label _5;
  label _6;
  label _7;
  label _8;
  label _9;
  label _10;
  label _11;
  label _13;
  label _14;
  label _15;
  label _16;
  label _17;
  label _18;
  label _19;
  label _20;
  label _21;
  label _22;
  label _23;
  label _24;
  label _25;
  label _26;
  label _27;
  label _29;
  label _30;
  label _31;
  label _32;
  label _34;
  label _35;
  label _36;
  label _37;
begin
//      INTEGER N1, N2, IWEND, IWL, INDF, INDX, N1LST, NL, NR,
//     .        NEXT, IWF, LFT, N0, IWC, IWCP1, IWCM1, I, IO1,
//     .        IO2, INDL
//      REAL    X1, Y1, X2, Y2, X0, Y0
//      LOGICAL SWP, LEFT
//C
//C LOCAL PARAMETERS -
//C
//C N1,N2 =   LOCAL COPIES OF IN1 AND IN2 OR NODES OPPOSITE AN
//C             ARC IO1-IO2 TO BE TESTED FOR A SWAP IN THE
//C             OPTIMIZATION LOOPS
//C IWEND =   INPUT OR OUTPUT VALUE OF LWK
//C IWL =     IWK (COLUMN) INDEX OF THE LAST (RIGHTMOST) ARC
//C             WHICH INTERSECTS IN1->IN2
//C INDF =    IADJ INDEX OF THE FIRST NEIGHBOR OF IN1 OR IO1
//C INDX =    IADJ INDEX OF A NEIGHBOR OF IN1, NL, OR IO1
//C N1LST =   LAST NEIGHBOR OF IN1
//C NL,NR =   ENDPOINTS OF AN ARC WHICH INTERSECTS IN1-IN2
//C             WITH NL LEFT IN1->IN2
//C NEXT =    NODE OPPOSITE NL->NR
//C IWF =     IWK (COLUMN) INDEX OF THE FIRST (LEFTMOST) ARC
//C             WHICH INTERSECTS IN1->IN2
//C LFT =     FLAG USED TO DETERMINE IF A SWAP RESULTS IN THE
//C             NEW ARC INTERSECTING IN1-IN2 -- LFT = 0 IFF
//C             N0 = IN1, LFT = -1 IMPLIES N0 LEFT IN1->IN2,
//C             AND LFT = 1 IMPLIES N0 LEFT IN2->IN1
//C N0 =      NODE OPPOSITE NR->NL
//C IWC =     IWK INDEX BETWEEN IWF AND IWL -- NL->NR IS
//C             STORED IN IWK(1,IWC)->IWK(2,IWC)
//C IWCP1 =   IWC + 1
//C IWCM1 =   IWC - 1
//C I =       DO-LOOP INDEX AND COLUMN INDEX FOR IWK
//C IO1,IO2 = ENDPOINTS OF AN ARC TO BE TESTED FOR A SWAP IN
//C             THE OPTIMIZATION LOOPS
//C INDL =    IADJ INDEX OF THE LAST NEIGHBOR OF IO1
//C X1,Y1 =   COORDINATES OF IN1
//C X2,Y2 =   COORDINATES OF IN2
//C X0,Y0 =   COORDINATES OF N0
//C SWP =     FLAG SET TO .TRUE. IFF A SWAP OCCURS IN AN OPTI-
//C             MIZATION LOOP
//C LEFT =    STATEMENT FUNCTION WHICH RETURNS THE VALUE
//C             .TRUE. IFF (XP,YP) IS ON OR TO THE LEFT OF THE
//C             VECTOR (XA,YA)->(XB,YB)
//C
//      LEFT(XA,YA,XB,YB,XP,YP) = (XB-XA)*(YP-YA) .GE.
//     .                          (XP-XA)*(YB-YA)
//C
//C STORE IN1, IN2, AND LWK IN LOCAL VARIABLES AND CHECK FOR
//C   ERRORS.
//C

      N1 := IN1;
      N2 := IN2;
      IWEND := LWK;
      IF (N1 < 1)  or  (N2 < 1)  or  (N1 = N2)  or
         (IWEND < 0) then GOTO _35;


//      N1 = IN1
//      N2 = IN2
//      IWEND = LWK
//      IF (N1 .LT. 1  .OR.  N2 .LT. 1  .OR.  N1 .EQ. N2  .OR.
//     .    IWEND .LT. 0) GO TO 35
//C
//C STORE THE COORDINATES OF N1 AND N2 AND INITIALIZE IWL.
//C

      X1 := X[N1-1];
      Y1 := Y[N1-1];
      X2 := X[N2-1];
      Y2 := Y[N2-1];
      IWL := 0;


//      X1 = X(N1)
//      Y1 = Y(N1)
//      X2 = X(N2)
//      Y2 = Y(N2)
//      IWL = 0
//C
//C SET NR AND NL TO ADJACENT NEIGHBORS OF N1 SUCH THAT
//C   NR LEFT N2->N1 AND NL LEFT N1->N2.
//C
//C   SET INDF AND INDX TO THE INDICES OF THE FIRST AND LAST
//C     NEIGHBORS OF N1 AND SET N1LST TO THE LAST NEIGHBOR.
//C

      INDF := 1;
      IF (N1 > 1) then INDF := IEND[N1-2] + 1;
      INDX := IEND[N1-1];
      N1LST := IADJ[INDX-1];
      IF (N1LST = 0) then INDX := INDX - 1;
      IF (N1LST = 0) then GOTO _2;

//      INDF = 1
//      IF (N1 .GT. 1) INDF = IEND(N1-1) + 1
//      INDX = IEND(N1)
//      N1LST = IADJ(INDX)
//      IF (N1LST .EQ. 0) INDX = INDX - 1
//      IF (N1LST .EQ. 0) GO TO 2
//C
//C   N1 IS AN INTERIOR NODE.  LOOP THROUGH THE NEIGHBORS NL
//C     IN REVERSE ORDER UNTIL NL LEFT N1->N2.
//C

      NL := N1LST;
  _1: IF ( LEFT(X1,Y1,X2,Y2,X[NL-1],Y[NL-1]) ) then GOTO _2;
      INDX := INDX - 1;
      NL := IADJ[INDX-1];
      IF (INDX > INDF) then GOTO _1;


//      NL = N1LST
//    1 IF ( LEFT(X1,Y1,X2,Y2,X(NL),Y(NL)) ) GO TO 2
//      INDX = INDX - 1
//      NL = IADJ(INDX)
//      IF (INDX .GT. INDF) GO TO 1
//C
//C   NL IS THE FIRST NEIGHBOR OF N1.  SET NR TO THE LAST
//C     NEIGHBOR AND TEST FOR AN ARC N1-N2.
//C

      NR := N1LST;
      IF (NL = N2) then GOTO _34;
      GOTO _4;

//      NR = N1LST
//      IF (NL .EQ. N2) GO TO 34
//      GO TO 4
//C
//C   NL = IADJ(INDX) LEFT N1->N2 AND INDX .GT. INDF.  SET
//C     NR TO THE PRECEDING NEIGHBOR OF N1.
//C
     _2:
      INDX := INDX - 1;
      NR := IADJ[INDX-1];
      IF ( LEFT(X2,Y2,X1,Y1,X[NR-1],Y[NR-1]) ) then GOTO _3;
      IF (INDX > INDF) then GOTO _2;

//    2 INDX = INDX - 1
//      NR = IADJ(INDX)
//      IF ( LEFT(X2,Y2,X1,Y1,X(NR),Y(NR)) ) GO TO 3
//      IF (INDX .GT. INDF) GO TO 2
//C
//C   SET NL AND NR TO THE FIRST AND LAST NEIGHBORS OF N1 AND
//C     TEST FOR AN INVALID DATA STRUCTURE (N1 CANNOT BE A
//C     BOUNDARY NODE AND CANNOT BE ADJACENT TO N2).
//C

      NL := NR;
      NR := N1LST;
      IF (NR = 0)  or  (NR = N2) then GOTO _37;
      GOTO _4;

//      NL = NR
//      NR = N1LST
//      IF (NR .EQ. 0  .OR.  NR .EQ. N2) GO TO 37
//      GO TO 4
//C
//C   SET NL TO THE NEIGHBOR FOLLOWING NR AND TEST FOR AN ARC
//C     N1-N2.
//C
     _3:
      NL := IADJ[INDX+1-1];
      IF (NL = N2)  or  (NR = N2) then GOTO _34;

//    3 NL = IADJ(INDX+1)
//      IF (NL .EQ. N2  .OR.  NR .EQ. N2) GO TO 34
//C
//C STORE THE ORDERED SEQUENCE OF INTERSECTING EDGES NL->NR IN
//C   IWK(1,IWL)->IWK(2,IWL).
//C
     _4:
      IWL := IWL + 1;
      IF (IWL <= IWEND) then IWK[(IWL-1)*2  ] := NL;
      IF (IWL <= IWEND) then IWK[(IWL-1)*2+1] := NR;

//    4 IWL = IWL + 1
//      IF (IWL .LE. IWEND) IWK(1,IWL) = NL
//      IF (IWL .LE. IWEND) IWK(2,IWL) = NR
//C
//C   SET NEXT TO THE NEIGHBOR OF NL WHICH FOLLOWS NR.
//C

      INDX := IEND[NL-1];
      IF (IADJ[INDX-1] <> NR) then GOTO _5;

//      INDX = IEND(NL)
//      IF (IADJ(INDX) .NE. NR) GO TO 5
//C
//C   NR IS THE LAST NEIGHBOR OF NL.  SET NEXT TO THE FIRST
//C     NEIGHBOR.
//C

      INDX := 0;
      IF (NL <> 1) then INDX := IEND[NL-2];
      GOTO _6;

//      INDX = 0
//      IF (NL .NE. 1) INDX = IEND(NL-1)
//      GO TO 6
//C
//C   NR IS NOT THE LAST NEIGHBOR OF NL.  LOOP THROUGH THE
//C     NEIGHBORS IN REVERSE ORDER.
//C
     _5:
      INDX := INDX - 1;
      IF (IADJ[INDX-1] <> NR) then GOTO _5;

//    5 INDX = INDX - 1
//      IF (IADJ(INDX) .NE. NR) GO TO 5
//C
//C   STORE NEXT, TEST FOR AN INVALID TRIANGULATION (NL->NR
//C     CANNOT BE A BOUNDARY EDGE), AND TEST FOR TERMINATION
//C     OF THE LOOP.
//C
     _6:
      NEXT := IADJ[INDX+1-1];
      IF (NEXT = 0) then GOTO _37;
      IF (NEXT = N2) then GOTO _8;

//    6 NEXT = IADJ(INDX+1)
//      IF (NEXT .EQ. 0) GO TO 37
//      IF (NEXT .EQ. N2) GO TO 8
//C
//C   SET NL OR NR TO NEXT.
//C

      IF ( LEFT(X1,Y1,X2,Y2,X[NEXT-1],Y[NEXT-1]) ) then GOTO _7;
      NR := NEXT;
      GOTO _4;
  _7: NL := NEXT;
      GOTO _4;

//      IF ( LEFT(X1,Y1,X2,Y2,X(NEXT),Y(NEXT)) ) GO TO 7
//      NR = NEXT
//      GO TO 4
//    7 NL = NEXT
//      GO TO 4
//C
//C IWL IS THE NUMBER OF ARCS WHICH INTERSECT N1-N2.  STORE
//C   LWK AND TEST FOR SUFFICIENT SPACE.
//C
     _8:
      LWK := IWL;
      IF (IWL > IWEND) then GOTO _36;
      IWEND := IWL;

//    8 LWK = IWL
//      IF (IWL .GT. IWEND) GO TO 36
//      IWEND = IWL
//C
//C INITIALIZE FOR EDGE SWAPPING LOOP -- ALL POSSIBLE SWAPS
//C   ARE APPLIED (EVEN IF THE NEW ARC AGAIN INTERSECTS
//C   N1-N2), ARCS TO THE LEFT OF N1->N2 ARE STORED IN THE
//C   LEFT PORTION OF IWK, AND ARCS TO THE RIGHT ARE STORED IN
//C   THE RIGHT PORTION.  IWF AND IWL INDEX THE FIRST AND LAST
//C   INTERSECTING ARCS.
//C

      IER := 0;
      IWF := 1;

//      IER = 0
//      IWF = 1
//C
//C TOP OF LOOP -- SET N0 TO N1 AND NL->NR TO THE FIRST EDGE.
//C   IWC POINTS TO THE ARC CURRENTLY BEING PROCESSED.  LFT
//C   .LE. 0 IFF N0 LEFT N1->N2.
//C

  _9: LFT := 0;
      N0 := N1;
      X0 := X1;
      Y0 := Y1;
      NL := IWK[(IWF-1)*2  ];
      NR := IWK[(IWF-1)*2+1];
      IWC := IWF;

//    9 LFT = 0
//      N0 = N1
//      X0 = X1
//      Y0 = Y1
//      NL = IWK(1,IWF)
//      NR = IWK(2,IWF)
//      IWC = IWF
//C
//C   SET NEXT TO THE NODE OPPOSITE NL->NR UNLESS IWC IS THE
//C     LAST ARC.
//C

 _10: IF (IWC = IWL) then GOTO _21;
      IWCP1 := IWC + 1;
      NEXT := IWK[(IWCP1-1)*2  ];
      IF (NEXT <> NL) then GOTO _15;
      NEXT := IWK[(IWCP1-1)*2+1];

//   10 IF (IWC .EQ. IWL) GO TO 21
//      IWCP1 = IWC + 1
//      NEXT = IWK(1,IWCP1)
//      IF (NEXT .NE. NL) GO TO 15
//      NEXT = IWK(2,IWCP1)
//C
//C   NEXT RIGHT N1->N2 AND IWC .LT. IWL.  TEST FOR A POSSIBLE
//C     SWAP.
//C

      IF  not LEFT(X0,Y0,X[NR-1],Y[NR-1],X[NEXT-1],Y[NEXT-1])
        then GOTO _13;
      IF (LFT >= 0) then GOTO _11;
      IF not LEFT(X[NL-1],Y[NL-1],X0,Y0,X[NEXT-1],Y[NEXT-1])
        then GOTO _13;

//      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X(NEXT),Y(NEXT)) )
//     .   GO TO 13
//      IF (LFT .GE. 0) GO TO 11
//      IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X(NEXT),Y(NEXT)) )
//     .   GO TO 13
//C
//C   REPLACE NL->NR WITH N0->NEXT.
//C

      SWAP(NEXT,N0,NL,NR, IADJ,IEND );
      IWK[(IWC-1)*2  ] := N0;
      IWK[(IWC-1)*2+1] := NEXT;
      GOTO _14;

//      CALL SWAP(NEXT,N0,NL,NR, IADJ,IEND )
//      IWK(1,IWC) = N0
//      IWK(2,IWC) = NEXT
//      GO TO 14
//C
//C   SWAP NL-NR FOR N0-NEXT, SHIFT COLUMNS IWC+1,...,IWL TO
//C     THE LEFT, AND STORE N0-NEXT IN THE RIGHT PORTION OF
//C     IWK.
//C
  _11:
      SWAP(NEXT,N0,NL,NR, IADJ,IEND );
      for I := IWCP1 to IWL do
      begin
        IWK[(I-2)*2  ] := IWK[(I-1)*2  ];
        IWK[(I-2)*2+1] := IWK[(I-1)*2+1];
      end;
      IWK[(IWL-1)*2  ] := N0;
      IWK[(IWL-1)*2+1] := NEXT;
      IWL := IWL - 1;
      NR := NEXT;
      GOTO _10;

//   11 CALL SWAP(NEXT,N0,NL,NR, IADJ,IEND )
//      DO 12 I = IWCP1,IWL
//        IWK(1,I-1) = IWK(1,I)
//   12   IWK(2,I-1) = IWK(2,I)
//      IWK(1,IWL) = N0
//      IWK(2,IWL) = NEXT
//      IWL = IWL - 1
//      NR = NEXT
//      GO TO 10
//C
//C   A SWAP IS NOT POSSIBLE.  SET N0 TO NR.
//C
   _13:
      N0 := NR;
      X0 := X[N0-1];
      Y0 := Y[N0-1];
      LFT := 1;

//   13 N0 = NR
//      X0 = X(N0)
//      Y0 = Y(N0)
//      LFT = 1
//C
//C   ADVANCE TO THE NEXT ARC.
//C
 _14:
      NR := NEXT;
      IWC := IWC + 1;
      GOTO _10;

//   14 NR = NEXT
//      IWC = IWC + 1
//      GO TO 10
//C
//C   NEXT LEFT N1->N2, NEXT .NE. N2, AND IWC .LT. IWL.
//C     TEST FOR A POSSIBLE SWAP.
//C
 _15:
      IF  not LEFT(X[NL-1],Y[NL-1],X0,Y0,X[NEXT-1],Y[NEXT-1])
        then GOTO _19;
      IF (LFT <= 0) then GOTO _16;
      IF  not LEFT(X0,Y0,X[NR-1],Y[NR-1],X[NEXT-1],Y[NEXT-1])
        then GOTO _19;

//   15 IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X(NEXT),Y(NEXT)) )
//     .   GO TO 19
//      IF (LFT .LE. 0) GO TO 16
//      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X(NEXT),Y(NEXT)) )
//     .   GO TO 19
//C
//C   REPLACE NL->NR WITH NEXT->N0.
//C

      SWAP(NEXT,N0,NL,NR, IADJ,IEND );
      IWK[(IWC-1)*2  ] := NEXT;
      IWK[(IWC-1)*2+1] := N0;
      GOTO _20;

//      CALL SWAP(NEXT,N0,NL,NR, IADJ,IEND )
//      IWK(1,IWC) = NEXT
//      IWK(2,IWC) = N0
//      GO TO 20
//C
//C   SWAP NL-NR FOR N0-NEXT, SHIFT COLUMNS IWF,...,IWC-1 TO
//C     THE RIGHT, AND STORE N0-NEXT IN THE LEFT PORTION OF
//C     IWK.
//C
 _16:
      SWAP(NEXT,N0,NL,NR, IADJ,IEND );
      I := IWC;
 _17: IF (I = IWF) then GOTO _18;
      IWK[(I-1)*2  ] := IWK[(I-2)*2  ];
      IWK[(I-1)*2+1] := IWK[(I-2)*2+1];
      I := I - 1;
      GOTO _17;
 _18: IWK[(IWF-1)*2  ] := N0;
      IWK[(IWF-1)*2+1] := NEXT;
      IWF := IWF + 1;
      GOTO _20;


//   16 CALL SWAP(NEXT,N0,NL,NR, IADJ,IEND )
//      I = IWC
//   17 IF (I .EQ. IWF) GO TO 18
//      IWK(1,I) = IWK(1,I-1)
//      IWK(2,I) = IWK(2,I-1)
//      I = I - 1
//      GO TO 17
//   18 IWK(1,IWF) = N0
//      IWK(2,IWF) = NEXT
//      IWF = IWF + 1
//      GO TO 20
//C
//C   A SWAP IS NOT POSSIBLE.  SET N0 TO NL.
//C
 _19:
      N0 := NL;
      X0 := X[N0-1];
      Y0 := Y[N0-1];
      LFT := -1;

//   19 N0 = NL
//      X0 = X(N0)
//      Y0 = Y(N0)
//      LFT = -1
//C
//C   ADVANCE TO THE NEXT ARC.
//C
  _20:
      NL := NEXT;
      IWC := IWC + 1;
      GOTO _10;

//   20 NL = NEXT
//      IWC = IWC + 1
//      GO TO 10
//C
//C   N2 IS OPPOSITE NL->NR (IWC = IWL).
//C
 _21:
      IF (N0 = N1) then GOTO _24;
      IF (LFT < 0) then GOTO _22;

//   21 IF (N0 .EQ. N1) GO TO 24
//      IF (LFT .LT. 0) GO TO 22
//C
//C   N0 RIGHT N1->N2.  TEST FOR A POSSIBLE SWAP.
//C

      IF ( not LEFT(X0,Y0,X[NR-1],Y[NR-1],X2,Y2) ) then GOTO _9;

//      IF ( .NOT. LEFT(X0,Y0,X(NR),Y(NR),X2,Y2) ) GO TO 9
//C
//C   SWAP NL-NR FOR N0-N2 AND STORE N0-N2 IN THE RIGHT
//C     PORTION OF IWK.
//C

      SWAP(N2,N0,NL,NR, IADJ,IEND );
      IWK[(IWL-1)*2  ] := N0;
      IWK[(IWL-1)*2+1] := N2;
      IWL := IWL - 1;
      GOTO _9;

//      CALL SWAP(N2,N0,NL,NR, IADJ,IEND )
//      IWK(1,IWL) = N0
//      IWK(2,IWL) = N2
//      IWL = IWL - 1
//      GO TO 9
//C
//C   N0 LEFT N1->N2.  TEST FOR A POSSIBLE SWAP.
//C
  _22:
    IF ( not LEFT(X[NL-1],Y[NL-1],X0,Y0,X2,Y2) ) then GOTO _9;

//   22 IF ( .NOT. LEFT(X(NL),Y(NL),X0,Y0,X2,Y2) ) GO TO 9
//C
//C   SWAP NL-NR FOR N0-N2, SHIFT COLUMNS IWF,...,IWL-1 TO THE
//C     RIGHT, AND STORE N0-N2 IN THE LEFT PORTION OF IWK.
//C

      SWAP(N2,N0,NL,NR, IADJ,IEND );
      I := IWL;
 _23: IWK[(I-1)*2  ] := IWK[(I-2)*2  ];
      IWK[(I-1)*2+1] := IWK[(I-2)*2+1];
      I := I - 1;
      IF (I > IWF) then GOTO _23;
      IWK[(IWF-1)*2  ] := N0;
      IWK[(IWF-1)*2+1] := N2;
      IWF := IWF + 1;
      GOTO _9;

//      CALL SWAP(N2,N0,NL,NR, IADJ,IEND )
//      I = IWL
//   23 IWK(1,I) = IWK(1,I-1)
//      IWK(2,I) = IWK(2,I-1)
//      I = I - 1
//      IF (I .GT. IWF) GO TO 23
//      IWK(1,IWF) = N0
//      IWK(2,IWF) = N2
//      IWF = IWF + 1
//      GO TO 9
//C
//C IWF = IWC = IWL.  SWAP OUT THE LAST ARC FOR N1-N2 AND
//C   STORE ZEROS IN IWK.
//C
  _24:
      SWAP(N2,N1,NL,NR, IADJ,IEND );
      IWK[(IWC-1)*2  ] := 0;
      IWK[(IWC-1)*2+1] := 0;
      IF (IWC = 1) then GOTO _29;

//   24 CALL SWAP(N2,N1,NL,NR, IADJ,IEND )
//      IWK(1,IWC) = 0
//      IWK(2,IWC) = 0
//      IF (IWC .EQ. 1) GO TO 29
//C
//C OPTIMIZATION LOOPS -- OPTIMIZE THE SET OF NEW ARCS TO THE
//C   LEFT OF IN1->IN2.  THE LOOP IS REPEATED UNTIL NO SWAPS
//C   ARE PERFORMED.
//C

      IWCM1 := IWC - 1;
 _25: SWP := FALSE;
      for  I := 1 to IWCM1 do
      begin
        IO1 := IWK[(I-1)*2  ];
        IO2 := IWK[(I-1)*2+1];

//      IWCM1 = IWC - 1
//   25 SWP = .FALSE.
//      DO 28 I = 1,IWCM1
//        IO1 = IWK(1,I)
//        IO2 = IWK(2,I)
//C
//C   SET N1 TO THE NEIGHBOR OF IO1 WHICH FOLLOWS IO2 AND SET
//C     N2 TO THE NEIGHBOR OF IO1 WHICH PRECEDES IO2.
//C

        INDF := 1;
        IF (IO1 > 1) then INDF := IEND[IO1-2] + 1;
        INDL := IEND[IO1-1];
        INDX := INDL;
        IF (IADJ[INDX-1] <> IO2) then GOTO _26;

//        INDF = 1
//        IF (IO1 .GT. 1) INDF = IEND(IO1-1) + 1
//        INDL = IEND(IO1)
//        INDX = INDL
//        IF (IADJ(INDX) .NE. IO2) GO TO 26
//C
//C   IO2 IS THE LAST NEIGHBOR OF IO1.
//C

        N1 := IADJ[INDF-1];
        N2 := IADJ[INDX-2];
        GOTO _27;

//        N1 = IADJ(INDF)
//        N2 = IADJ(INDX-1)
//        GO TO 27
//C
//C   IO2 IS NOT THE LAST NEIGHBOR OF IO1.  LOOP THROUGH THE
//C     NEIGHBORS IN REVERSE ORDER.
//C
  _26:
        INDX := INDX - 1;
        IF (IADJ[INDX-1] <> IO2) then GOTO _26;
        N1 := IADJ[INDX+1-1];
        IF (INDX <> INDF) then N2 := IADJ[INDX-2];
        IF (INDX =  INDF) then N2 := IADJ[INDL-1];

//   26   INDX = INDX - 1
//        IF (IADJ(INDX) .NE. IO2) GO TO 26
//        N1 = IADJ(INDX+1)
//        IF (INDX .NE. INDF) N2 = IADJ(INDX-1)
//        IF (INDX .EQ. INDF) N2 = IADJ(INDL)
//C
//C   TEST IO1-IO2 FOR A SWAP.
//C

  _27:
       IF ( not SWPTST(N1,N2,IO1,IO2,X,Y) ) then Continue;
        SWP := TRUE;
        SWAP(N1,N2,IO1,IO2, IADJ,IEND );
        IWK[(I-1)*2  ] := N1;
        IWK[(I-1)*2+1] := N2;

//   27   IF ( .NOT. SWPTST(N1,N2,IO1,IO2,X,Y) ) GO TO 28
//        SWP = .TRUE.
//        CALL SWAP(N1,N2,IO1,IO2, IADJ,IEND )
//        IWK(1,I) = N1
//        IWK(2,I) = N2
//   28   CONTINUE
      end;

      IF (SWP) then GOTO _25;

//      IF (SWP) GO TO 25
//C
//C TEST FOR TERMINATION.
//C
 _29:
      IF (IWC = IWEND) then Exit;
      IWCP1 := IWC + 1;

//   29 IF (IWC .EQ. IWEND) RETURN
//      IWCP1 = IWC + 1
//C
//C OPTIMIZE THE SET OF NEW ARCS TO THE RIGHT OF IN1->IN2.
//C
 _30: SWP := FALSE;
      for I := IWCP1 to IWEND do
      begin
        IO1 := IWK[(I-1)*2  ];
        IO2 := IWK[(I-1)*2+1];

//   30 SWP = .FALSE.
//      DO 33 I = IWCP1,IWEND
//        IO1 = IWK(1,I)
//        IO2 = IWK(2,I)
//C
//C   SET N1 AND N2 TO THE NODES OPPOSITE IO1->IO2 AND
//C     IO2->IO1, RESPECTIVELY.
//C

        INDF := 1;
        IF (IO1 > 1) then INDF := IEND[IO1-2] + 1;
        INDL := IEND[IO1-1];
        INDX := INDL;
        IF (IADJ[INDX-1] <> IO2) then GOTO _31;

//        INDF = 1
//        IF (IO1 .GT. 1) INDF = IEND(IO1-1) + 1
//        INDL = IEND(IO1)
//        INDX = INDL
//        IF (IADJ(INDX) .NE. IO2) GO TO 31
//C

        N1 := IADJ[INDF-1];
        N2 := IADJ[INDX-2];
        GOTO _32;

//        N1 = IADJ(INDF)
//        N2 = IADJ(INDX-1)
//        GO TO 32
//C
  _31:
        INDX := INDX - 1;
        IF (IADJ[INDX-1] <> IO2) then GOTO _31;
        N1 := IADJ[INDX+1-1];
        IF (INDX <> INDF) then N2 := IADJ[INDX-2];
        IF (INDX = INDF) then N2 := IADJ[INDL-1];

//   31   INDX = INDX - 1
//        IF (IADJ(INDX) .NE. IO2) GO TO 31
//        N1 = IADJ(INDX+1)
//        IF (INDX .NE. INDF) N2 = IADJ(INDX-1)
//        IF (INDX .EQ. INDF) N2 = IADJ(INDL)
//C
  _32:
        IF ( not SWPTST(N1,N2,IO1,IO2,X,Y) ) then Continue;
        SWP := TRUE;
        SWAP(N1,N2,IO1,IO2, IADJ,IEND );
        IWK[(I-1)*2  ] := N1;
        IWK[(I-1)*2+1] := N2

//   32   IF ( .NOT. SWPTST(N1,N2,IO1,IO2,X,Y) ) GO TO 33
//        SWP = .TRUE.
//        CALL SWAP(N1,N2,IO1,IO2, IADJ,IEND )
//        IWK(1,I) = N1
//        IWK(2,I) = N2
//   33   CONTINUE
     end;

      IF (SWP) then GOTO _30;
      Exit;

//      IF (SWP) GO TO 30
//      RETURN
//C
//C IN1 AND IN2 WERE ADJACENT ON INPUT.
//C
 _34:
      IER := 0;
      LWK := 0;
      Exit;

//   34 IER = 0
//      LWK = 0
//      RETURN
//C
//C PARAMETER OUT OF RANGE
//C
    _35:
      IER := 1;
      Exit;
//   35 IER = 1
//      RETURN
//C
//C INSUFFICIENT SPACE IN IWK
//C
    _36:
      IER := 2;
      Exit;
//   36 IER = 2
//      RETURN
//C
//C INVALID TRIANGULATION DATA STRUCTURE
//C
   _37:
      IER := 3;
      Exit;
//   37 IER = 3
//      RETURN
//      END
end;

procedure GRADG (const N: longint; const X,Y,Z: TRealArray;
    const IADJ: TIntArray; const IEND: TIntArray; const EPS: Real;
    var NIT: longint; var ZXZY: TRealArray; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF N NODES IN THE PLANE WITH
// ASSOCIATED DATA VALUES, THIS ROUTINE USES A GLOBAL METHOD
// TO COMPUTE ESTIMATED GRADIENTS AT THE NODES.  THE METHOD
// CONSISTS OF MINIMIZING A QUADRATIC FUNCTIONAL Q(G) OVER
// THE N-VECTOR G OF GRADIENTS WHERE Q APPROXIMATES THE LIN-
// EARIZED CURVATURE OF AN INTERPOLANT F OVER THE TRIANGULA-
// TION.  THE RESTRICTION OF F TO AN ARC OF THE TRIANGULATION
// IS TAKEN TO BE THE HERMITE CUBIC INTERPOLANT OF THE DATA
// VALUES AND TANGENTIAL GRADIENT COMPONENTS AT THE END-
// POINTS OF THE ARC, AND Q IS THE SUM OF THE LINEARIZED
// CURVATURES OF F ALONG THE ARCS -- THE INTEGRALS OVER THE
// ARCS OF D2F(T)**2 WHERE D2F(T) IS THE SECOND DERIVATIVE
// OF F WITH RESPECT TO DISTANCE T ALONG THE ARC.  THIS MIN-
// IMIZATION PROBLEM CORRESPONDS TO AN ORDER 2N SYMMETRIC
// POSITIVE-DEFINITE SPARSE LINEAR SYSTEM WHICH IS SOLVED FOR
// THE X AND Y PARTIAL DERIVATIVES BY THE BLOCK GAUSS-SEIDEL
// METHOD WITH 2 BY 2 BLOCKS.
//   AN ALTERNATIVE METHOD, SUBROUTINE GRADL, COMPUTES A
// LOCAL APPROXIMATION TO THE PARTIALS AT A SINGLE NODE AND
// MAY BE MORE ACCURATE, DEPENDING ON THE DATA VALUES AND
// DISTRIBUTION OF NODES (NEITHER METHOD EMERGED AS SUPERIOR
// IN TESTS FOR ACCURACY).  HOWEVER, IN TESTS RUN ON AN IBM
// 370, GRADG WAS FOUND TO BE ABOUT 3.6 TIMES AS FAST FOR
// NIT = 4.
//
// INPUT PARAMETERS - N - NUMBER OF NODES.  N .GE. 3.
//
//                  X,Y - CARTESIAN COORDINATES OF THE NODES.
//
//                    Z - DATA VALUES AT THE NODES.  Z(I) IS
//                        ASSOCIATED WITH (X(I),Y(I)).
//
//            IADJ,IEND - DATA STRUCTURE DEFINING THE TRIAN-
//                        GULATION.  SEE SUBROUTINE TRMESH.
//
//                  EPS - NONNEGATIVE CONVERGENCE CRITERION.
//                        THE METHOD IS TERMINATED WHEN THE
//                        MAXIMUM CHANGE IN A GRADIENT COMPO-
//                        NENT BETWEEN ITERATIONS IS AT MOST
//                        EPS.  EPS = 1.E-2 IS SUFFICIENT FOR
//                        EFFECTIVE CONVERGENCE.
//
// THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
//                  NIT - MAXIMUM NUMBER OF GAUSS-SEIDEL
//                        ITERATIONS TO BE APPLIED.  THIS
//                        MAXIMUM WILL LIKELY BE ACHIEVED IF
//                        EPS IS SMALLER THAN THE MACHINE
//                        PRECISION.  OPTIMAL EFFICIENCY WAS
//                        ACHIEVED IN TESTING WITH EPS = 0
//                        AND NIT = 3 OR 4.
//
//                 ZXZY - 2 BY N ARRAY WHOSE COLUMNS CONTAIN
//                        INITIAL ESTIMATES OF THE PARTIAL
//                        DERIVATIVES (ZERO VECTORS ARE
//                        SUFFICIENT).
//
// OUTPUT PARAMETERS - NIT - NUMBER OF GAUSS-SEIDEL ITERA-
//                           TIONS EMPLOYED.
//
//                    ZXZY - ESTIMATED X AND Y PARTIAL DERIV-
//                           ATIVES AT THE NODES WITH X PAR-
//                           TIALS IN THE FIRST ROW.  ZXZY IS
//                           NOT CHANGED IF IER = 2.
//
//                     IER - ERROR INDICATOR
//                           IER = 0 IF THE CONVERGENCE CRI-
//                                   TERION WAS ACHIEVED.
//                           IER = 1 IF CONVERGENCE WAS NOT
//                                   ACHIEVED WITHIN NIT
//                                   ITERATIONS.
//                           IER = 2 IF N OR EPS IS OUT OF
//                                   RANGE OR NIT .LT. 0 ON
//                                   INPUT.
//
// MODULES REFERENCED BY GRADG - NONE
//
// INTRINSIC FUNCTIONS CALLED BY GRADG - SQRT, AMAX1, ABS
//
//***********************************************************
var
  NN, MAXIT, ITER, K, INDF, INDL, INDX, NB: longint;
  TOL, DGMAX, XK, YK, ZK, ZXK, ZYK, A11, A12,
  A22, R1, R2, DELX, DELY, DELXS, DELYS, DSQ,
  DCUB, T, DZX, DZY: Real;
//
// LOCAL PARAMETERS -
//
// NN =          LOCAL COPY OF N
// MAXIT =       INPUT VALUE OF NIT
// ITER =        NUMBER OF ITERATIONS USED
// K =           DO-LOOP AND NODE INDEX
// INDF,INDL =   IADJ INDICES OF THE FIRST AND LAST NEIGHBORS
//                 OF K
// INDX =        IADJ INDEX IN THE RANGE INDF,...,INDL
// NB =          NEIGHBOR OF K
// TOL =         LOCAL COPY OF EPS
// DGMAX =       MAXIMUM CHANGE IN A GRADIENT COMPONENT BE-
//                 TWEEN ITERATIONS
// XK,YK,ZK =    X(K), Y(K), Z(K)
// ZXK,ZYK =     INITIAL VALUES OF ZXZY(1,K) AND ZXZY(2,K)
// A11,A12,A22 = MATRIX COMPONENTS OF THE 2 BY 2 BLOCK A*DG
//                 = R WHERE A IS SYMMETRIC, DG = (DZX,DZY)
//                 IS THE CHANGE IN THE GRADIENT AT K, AND R
//                 IS THE RESIDUAL
// R1,R2 =       COMPONENTS OF THE RESIDUAL -- DERIVATIVES OF
//                 Q WITH RESPECT TO THE COMPONENTS OF THE
//                 GRADIENT AT NODE K
// DELX,DELY =   COMPONENTS OF THE ARC NB-K
// DELXS,DELYS = DELX**2, DELY**2
// DSQ =         SQUARE OF THE DISTANCE D BETWEEN K AND NB
// DCUB =        D**3
// T =           FACTOR OF R1 AND R2
// DZX,DZY =     SOLUTION OF THE 2 BY 2 SYSTEM -- CHANGE IN
//                 DERIVATIVES AT K FROM THE PREVIOUS ITERATE
//
begin
      NN := N;
      TOL := EPS;
      MAXIT := NIT;

// ERROR CHECKS AND INITIALIZATION

      IF (NN < 3)  OR  (TOL < 0.0)  OR  (MAXIT < 0) then
      begin
// PARAMETER OUT OF RANGE

        NIT := 0;
        IER := 2;
        Exit;
      end;

      ITER := 0;

// TOP OF ITERATION LOOP

      while ITER < MAXIT do
      begin
      DGMAX := 0.0;
      INDL := 0;
      for K := 1 to NN do
      begin
        XK := X[K-1];
        YK := Y[K-1];
        ZK := Z[K-1];
        ZXK := ZXZY[(K-1)*2];
        ZYK := ZXZY[(K-1)*2+1];

//   INITIALIZE COMPONENTS OF THE 2 BY 2 SYSTEM

        A11 := 0.0;
        A12 := 0.0;
        A22 := 0.0;
        R1 := 0.0;
        R2 := 0.0;

//   LOOP ON NEIGHBORS NB OF K

        INDF := INDL + 1;
        INDL := IEND[K-1];
        for INDX := INDF to INDL do
        begin
          NB := IADJ[INDX-1];
          IF (NB = 0) then
          begin
            continue;
          end;

//   COMPUTE THE COMPONENTS OF ARC NB-K

          DELX := X[NB-1] - XK;
          DELY := Y[NB-1] - YK;
          DELXS := DELX*DELX;
          DELYS := DELY*DELY;
          DSQ := DELXS + DELYS;
          DCUB := DSQ*SQRT(DSQ);

//   UPDATE THE SYSTEM COMPONENTS FOR NODE NB

          A11 := A11 + DELXS/DCUB;
          A12 := A12 + DELX*DELY/DCUB;
          A22 := A22 + DELYS/DCUB;
          T := ( 1.5*(Z[NB-1]-ZK) - ((ZXZY[(NB-1)*2]/2.0+ZXK)*DELX +
               (ZXZY[(NB-1)*2+1]/2.0+ZYK)*DELY) )/DCUB;
          R1 := R1 + T*DELX;
          R2 := R2 + T*DELY;
        end;

//   SOLVE THE 2 BY 2 SYSTEM AND UPDATE DGMAX

        DZY := (A11*R2 - A12*R1)/(A11*A22 - A12*A12);
        DZX := (R1 - A12*DZY)/A11;
        DGMAX := MAX(DGMAX,ABS(DZX));
        DGMAX := MAX(DGMAX,ABS(DZY));

//   UPDATE THE PARTIALS AT NODE K

        ZXZY[(K-1)*2] := ZXK + DZX;
        ZXZY[(K-1)*2+1] := ZYK + DZY;
      end;

//   INCREMENT ITER AND TEST FOR CONVERGENCE

      ITER := ITER + 1;
      IF (DGMAX > TOL) then
      begin
        Continue;
      end;

// METHOD CONVERGED

      NIT := ITER;
      IER := 0;
      Exit;
      end;

// METHOD FAILED TO CONVERGE WITHIN NIT ITERATIONS

      IER := 1;
end;

procedure COORDS (const X,Y,X1,X2,X3,Y1,Y2,Y3: Real;
    var R: TReal3; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   THIS ROUTINE COMPUTES THE THREE BARYCENTRIC COORDINATES
// OF A POINT IN THE PLANE FOR A GIVEN TRIANGLE.
//
// INPUT PARAMETERS - X,Y - X AND Y COORDINATES OF THE POINT
//                          WHOSE BARYCENTRIC COORDINATES ARE
//                          DESIRED.
//
//      X1,X2,X3,Y1,Y2,Y3 - COORDINATES OF THE VERTICES OF
//                          THE TRIANGLE.
//
// INPUT PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//
// OUTPUT PARAMETERS -  R - 3-VECTOR OF BARYCENTRIC COORDI-
//                          NATES UNLESS IER = 1.  NOTE THAT
//                          R(I) .LT. 0. IFF (X,Y) IS TO THE
//                          RIGHT OF THE VECTOR FROM VERTEX
//                          I+1 TO VERTEX I+2 (CYCLICAL
//                          ARITHMETIC).
//
//                    IER - ERROR INDICATOR
//                          IER = 0 IF NO ERRORS WERE
//                                  ENCOUNTERED.
//                          IER = 1 IF THE VERTICES OF THE
//                                  TRIANGLE ARE COLLINEAR.
//
// MODULES REFERENCED BY COORDS - NONE
//
//***********************************************************
var
          AREA, XP, YP: Real;
          U, V: TReal3;
// LOCAL PARAMETERS -
//
// U(K),V(K) = X AND Y COMPONENTS OF THE VECTOR REPRESENTING
//               THE SIDE OPPOSITE VERTEX K FOR K = 1,2,3.
// AREA =      TWICE THE AREA OF THE TRIANGLE.
// XP,YP =     X-X1, Y-Y1
begin
  U[0] := X3-X2;
  U[1] := X1-X3;
  U[2] := X2-X1;

  V[0] := Y3-Y2;
  V[1] := Y1-Y3;
  V[2] := Y2-Y1;

// AREA = 3-1 X 3-2

  AREA := U[0]*V[1] - U[1]*V[0];
  IF (AREA = 0.0) then
  begin
// VERTICES ARE COLLINEAR

    IER := 1;
    Exit;

  end;

// R[0] = (2-3 X 2-(X,Y))/AREA, R[1] = (1-(X,Y) X 1-3)/AREA,
//   R[2] = (1-2 X 1-(X,Y))/AREA

  R[0] := (U[0]*(Y-Y2) - V[0]*(X-X2))/AREA;
  XP := X - X1;
  YP := Y - Y1;
  R[1] := (U[1]*YP - V[1]*XP)/AREA;
  R[2] := (U[2]*YP - V[2]*XP)/AREA;
  IER := 0;
end;

procedure INTRC0 (const N: longint; const PX,PY: Real;
  const X,Y,Z: TRealArray; const IADJ: TIntArray; const IEND: TIntArray;
  var IST: longint; var PZ: Real; var IER: longint);
//***********************************************************
//
//                                               ROBERT RENKA
//                                       OAK RIDGE NATL. LAB.
//                                             (615) 576-5139
//
//   GIVEN A TRIANGULATION OF A SET OF POINTS IN THE PLANE,
// THIS ROUTINE COMPUTES THE VALUE AT (PX,PY) OF A PIECEWISE
// LINEAR SURFACE WHICH INTERPOLATES DATA VALUES AT THE
// VERTICES OF THE TRIANGLES.  THE SURFACE IS EXTENDED IN A
// CONTINUOUS FASHION BEYOND THE BOUNDARY OF THE TRIANGULAR
// MESH, ALLOWING EXTRAPOLATION.  INTRC0 IS PART OF AN
// INTERPOLATION PACKAGE WHICH PROVIDES ROUTINES TO GENERATE,
// UPDATE, AND PLOT THE MESH.
//
// INPUT PARAMETERS -     N - NUMBER OF NODES IN THE MESH.
//                            N .GE. 3.
//
//                    PX,PY - POINT AT WHICH THE INTERPOLATED
//                            VALUE IS DESIRED.
//
//                      X,Y - VECTORS OF COORDINATES OF THE
//                            NODES IN THE MESH.
//
//                        Z - VECTOR OF DATA VALUES AT THE
//                            NODES.
//
//                     IADJ - SET OF ADJACENCY LISTS OF NODES
//                            IN THE MESH.
//
//                     IEND - POINTERS TO THE ENDS OF
//                            ADJACENCY LISTS IN IADJ FOR
//                            EACH NODE IN THE MESH.
//
//                      IST - INDEX OF THE STARTING NODE IN
//                            THE SEARCH FOR A TRIANGLE CON-
//                            TAINING (PX,PY).  1 .LE. IST
//                            .LE. N.  THE OUTPUT VALUE OF
//                            IST FROM A PREVIOUS CALL MAY
//                            BE A GOOD CHOICE.
//
// IADJ AND IEND MAY BE CREATED BY TRMESH.
//
// INPUT PARAMETERS OTHER THAN IST ARE NOT ALTERED BY THIS
//   ROUTINE.
//
// OUTPUT PARAMETERS -  IST - INDEX OF ONE OF THE VERTICES OF
//                            THE TRIANGLE CONTAINING (PX,PY)
//                            UNLESS IER .LT. 0.
//
//                       PZ - VALUE OF THE INTERPOLATORY
//                            SURFACE AT (PX,PY) OR ZERO
//                            IF IER .LT. 0.
//
//                      IER - ERROR INDICATOR
//                            IER = 0 IF NO ERRORS WERE
//                                    ENCOUNTERED.
//                            IER = 1 IF NO ERRORS WERE EN-
//                                    COUNTERED AND EXTRAPO-
//                                    LATION WAS PERFORMED.
//                            IER = -1 IF N OR IST IS OUT OF
//                                     RANGE.
//                            IER = -2 IF THE NODES ARE COL-
//                                     LINEAR.
//
// MODULES REFERENCED BY INTRC0 - TRFIND, COORDS
//
//***********************************************************
var
  I1, I2, I3, N1, N2, INDX: longint;
  XP, YP, X1, Y1, X2, Y2, DP: Real;
  R: TReal3;
// LOCAL PARAMETERS -
//
// I1,I2,I3 = VERTEX INDICES RETURNED BY TRFIND
// N1,N2 =    ENDPOINTS OF THE CLOSEST BOUNDARY EDGE TO P
//              WHEN P IS OUTSIDE OF THE MESH BOUNDARY
// INDX =     IADJ INDEX OF N1 AS A NEIGHBOR OF N2
// XP,YP =    LOCAL COPIES OF THE COORDINATES OF P=(PX,PY)
// R =        BARYCENTRIC COORDINATES
// X1,Y1 =    X,Y COORDINATES OF N1
// X2,Y2 =    X,Y COORDINATES OF N2
// DP =       INNER PRODUCT OF N1-N2 AND P-N2
begin
  IF (N < 3)  OR  (IST < 1)  OR  (IST > N) then
  begin
  // N < 3 OR IST IS OUT OF RANGE

    PZ := 0.0;
    IER := -1;
    Exit;
  end;

  XP := PX;
  YP := PY;

  // FIND A TRIANGLE CONTAINING P IF P IS WITHIN THE MESH
  //   BOUNDARY

  TRFIND(IST,XP,YP,X,Y,IADJ,IEND, I1,I2,I3);
  IF (I1 = 0) then
  begin
  // NODES ARE COLLINEAR

    PZ := 0.0;
    IER := -2;
    Exit;
  end;
  IST := I1;
  IF (I3 <> 0) then
  begin

  // COMPUTE BARYCENTRIC COORDINATES

  COORDS(XP,YP,X[I1-1],X[I2-1],X[I3-1],Y[I1-1],Y[I2-1],
             Y[I3-1], R,IER);
  IF (IER <> 0) then
  begin
  // NODES ARE COLLINEAR

    PZ := 0.0;
    IER := -2;
    Exit;
  end;
  PZ := R[0]*Z[I1-1] + R[1]*Z[I2-1] + R[2]*Z[I3-1];
  Exit;

  // P IS OUTSIDE OF THE MESH BOUNDARY.  EXTRAPOLATE TO P BY
  //   EXTENDING THE INTERPOLATORY SURFACE AS A CONSTANT
  //   BEYOND THE BOUNDARY.  THUS PZ IS THE SURFACE FUNCTION
  //   VALUE AT Q WHERE Q IS THE CLOSEST BOUNDARY POINT TO P.

  // DETERMINE Q BY TRAVERSING THE BOUNDARY STARTING FROM THE
  //   RIGHTMOST VISIBLE NODE I1.

  end;
  N2 := I1;

  // SET N1 TO THE LAST NONZERO NEIGHBOR OF N2 AND COMPUTE DP

  repeat
    INDX := IEND[N2-1] - 1;
    N1 := IADJ[INDX-1];
    X1 := X[N1-1];
    Y1 := Y[N1-1];
    X2 := X[N2-1];
    Y2 := Y[N2-1];
    DP := (X1-X2)*(XP-X2) + (Y1-Y2)*(YP-Y2);
    IF (DP <= 0.0) then
    begin
  // N2 IS THE CLOSEST BOUNDARY POINT TO P

      PZ := Z[N2-1];
      IER := 1;
      Exit;
    end;
    IF ((XP-X1)*(X2-X1) + (YP-Y1)*(Y2-Y1) > 0.0) then
    begin
  // THE CLOSEST BOUNDARY POINT TO P LIES ON N2-N1.  COMPUTE
  //   ITS COORDINATES WITH RESPECT TO N2-N1.

      R[0] := DP/( Sqr(X2-X1) + Sqr(Y2-Y1) );
      R[1] := 1.0 - R[0];
      PZ := R[0]*Z[N1-1] + R[1]*Z[N2-1];
      IER := 1;
      Exit;
    end;
    N2 := N1
  until (False);
end;

procedure ARCTST (const N: longint; const X,Y: TRealArray;
  var IADJ, IEND: TIntArray; var LL: longint;
  var  LIST: TIntArray; var IER: longint); stdcall;

//      SUBROUTINE ARCTST (N,X,Y,IADJ,IEND, LL, LIST,IER)
//      INTEGER N, IADJ(1), IEND(N), LL, LIST(2,1), IER
//      REAL    X(N), Y(N)
var
  NM1: Integer;
  LMAX: Integer;
  INDF: Integer;
  L: Integer;
  IO1: Integer;
  INDL: Integer;
  IN2: Integer;
  INDX: Integer;
  IO2: Integer;
  IN1: integer;
//      LOGICAL SWPTST
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   THIS ROUTINE DETERMINES THE NUMBER OF ARCS IN A TRIANGU-
//C LATION WHICH ARE NOT LOCALLY OPTIMAL AS DEFINED BY LOGICAL
//C FUNCTION SWPTST.  A THIESSEN TRIANGULATION (SEE SUBROUTINE
//C TRMESH) IS CHARACTERIZED BY THE PROPERTY THAT ALL ARCS ARE
//C LOCALLY OPTIMAL.  THE ALGORITHM CONSISTS OF APPLYING THE
//C SWAP TEST TO ALL INTERIOR ARCS.
//C
//C INPUT PARAMETERS -
//C
//C       N - NUMBER OF NODES IN THE TRIANGULATION.  N .GE. 3.
//C
//C       X,Y - COORDINATES OF THE NODES.
//C
//C       IADJ,IEND - DATA STRUCTURE CONTAINING THE TRIANGULA-
//C                   TION.  SEE SUBROUTINE TRMESH.
//C
//C THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//C
//C       LL - NUMBER OF COLUMNS RESERVED FOR LIST (SEE THE
//C            OUTPUT VALUE OF LIST).  LL .GE. 0.
//C
//C       LIST - INTEGER ARRAY DIMENSIONED 2 BY LL (OR VECTOR
//C              OF LENGTH .GE. 2*LL) IF LL .GT. 0.
//C
//C OUTPUT PARAMETERS -
//C
//C       LL - NUMBER OF ARCS WHICH ARE NOT LOCALLY OPTIMAL
//C            UNLESS IER = 1.
//C
//C       LIST - COLUMNS CONTAIN THE ENDPOINT NODAL INDICES
//C              (SMALLER INDEX IN THE FIRST ROW) OF THE FIRST
//C              K NONOPTIMAL ARCS ENCOUNTERED, WHERE K IS THE
//C              MINIMUM OF THE INPUT AND OUTPUT VALUES OF LL,
//C              IF IER = 0 AND K .GT. 0.  THE NUMBER OF INTE-
//C              RIOR ARCS IS 3N-2NB-3 .LE. 3(N-3) WHERE NB IS
//C              THE NUMBER OF BOUNDARY NODES.  BOUNDARY ARCS
//C              ARE OPTIMAL BY DEFINITION.
//C
//C       IER - ERROR INDICATOR
//C             IER = 0 IF NO ERRORS WERE ENCOUNTERED.
//C             IER = 1 IF N .LT. 3 OR LL .LT. 0.
//C
//C MODULES REQUIRED BY ARCTST - SWPTST
//C
//C***********************************************************
//C
begin
      NM1 := N - 1;
      LMAX := LL;

//      NM1 = N - 1
//      LMAX = LL
//C
//C TEST FOR ERRORS AND INITIALIZE FOR LOOP ON INTERIOR ARCS.
//C
//      IF (NM1 .LT. 2  .OR.  LMAX .LT. 0) GO TO 3
      if (NM1 < 2)  or  (LMAX < 0) then
      begin
        IER := 1;
      end
      else
      begin

        IER := 0;
        L := 0;
        INDF := 1;

//      IER = 0
//      L = 0
//      INDF = 1
//C
//C OUTER LOOP ON NODES IO1
//C
//      DO 2 IO1 = 1,NM1
        for IO1 := 0 to NM1-1 do
        begin
          INDL := IEND[IO1];
          IN2 := IADJ[INDL-1];
//        INDL = IEND(IO1)
//        IN2 = IADJ(INDL)
//C
//C INNER LOOP ON NEIGHBORS IO2 OF IO1 SUCH THAT IO2 .GT. IO1
//C   AND IO1-IO2 IS AN INTERIOR ARC -- (IO1,IO2,IN1) AND
//C   (IO2,IO1,IN2) ARE TRIANGLES.
//C
          for INDX := INDF to INDL do
          begin
  //        DO 1 INDX = INDF,INDL

            try
            IO2 := IADJ[INDX-1];
            IN1 := IADJ[INDX+1-1];
            IF (INDX = INDL) then IN1 := IADJ[INDF-1];
            IF (IO2-1 < IO1)  or  (IN1 = 0)  or
               (IN2 = 0) then
            begin
              Continue;
            end;


  //          IO2 = IADJ(INDX)
  //          IN1 = IADJ(INDX+1)
  //          IF (INDX .EQ. INDL) IN1 = IADJ(INDF)
  //          IF (IO2 .LT. IO1  .OR.  IN1 .EQ. 0  .OR.
  //     .        IN2 .EQ. 0) GO TO 1
  //C
  //C TEST FOR A SWAP.
  //C
  //          IF (.NOT. SWPTST(IN1,IN2,IO1,IO2,X,Y)) GO TO 1
            IF (NOT SWPTST(IN1,IN2,IO1+1,IO2,X,Y)) then
            begin
  //            IN2 := IO2;
              Continue;
            end;

            L := L + 1;
            IF (L > LMAX) then
            begin
              Continue;
            end;
            LIST[2*(L-1)]   := IO1+1;
            LIST[2*(L-1)+1] := IO2;
  //          LIST[1,L] := IO1;
  //          LIST[2,L] := IO2;

  //          L = L + 1
  //          IF (L .GT. LMAX) GO TO 1
  //          LIST(1,L) = IO1
  //          LIST(2,L) = IO2
            finally
                IN2 := IO2;
            end;
  //    1     IN2 = IO2
          end;
          INDF := INDL + 1;
        end;
//    2   INDF = INDL + 1
//      LL = L
//      RETURN
      end;
//C
//C N OR LL OUT OF RANGE
//C
//    3 IER = 1
//      RETURN
//      END
end;

FUNCTION LOPTST (const N1,N2: longint; const X,Y: TRealArray;
  const IADJ,IEND: TIntArray): INTEGER;
//      INTEGER FUNCTION LOPTST (N1,N2,X,Y,IADJ,IEND)
//      INTEGER N1, N2, IADJ(1), IEND(1)
//      REAL    X(1), Y(1)
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   GIVEN A PAIR OF INDICES DEFINING A TRIANGULATION ARC,
//C THIS FUNCTION DETERMINES WHETHER OR NOT THE ARC IS LOCALLY
//C OPTIMAL AS DEFINED BY LOGICAL FUNCTION SWPTST.
//C
//C INPUT PARAMETERS -
//C
//C       N1,N2 - X,Y INDICES OF A PAIR OF ADJACENT NODES.
//C
//C       X,Y - NODAL COORDINATES.
//C
//C       IADJ,IEND - DATA STRUCTURE CONTAINING THE TRIANGULA-
//C                   TION.  SEE SUBROUTINE TRMESH.
//C
//C INPUT PARAMETERS ARE NOT ALTERED BY THIS FUNCTION.
//C
//C OUTPUT PARAMETER -
//C
//C       LOPTST = -2 IF N1 AND N2 ARE NOT ADJACENT,
//C              = -1 IF N1-N2 IS AN INTERIOR ARC WHICH IS
//C                   NOT LOCALLY OPTIMAL,
//C              =  0 IF N1-N2 SATISFIES THE NEUTRAL CASE (THE
//C                   VERTICES OF THE CORRESPONDING QUADRILAT-
//C                   ERAL LIE ON A COMMON CIRCLE),
//C              =  1 IF N1-N2 IS A LOCALLY OPTIMAL INTERIOR
//C                   ARC,
//C              =  2 IF N1-N2 IS A BOUNDARY ARC.
//C      NOTE THAT N1-N2 IS LOCALLY OPTIMAL IFF LOPTST .GE. 0.
//C
//C MODULES REQUIRED BY LOPTST - NONE
//C
//C***********************************************************
//C
var
  IO1: integer;
  IO2: Integer;
  INDF: Integer;
  INDL: Integer;
  INDX: Integer;
  IN1: integer;
  IN2: integer;
  DX11: Real;
  DX12: Real;
  DX22: Real;
  DX21: Real;
  DY11: Real;
  DY12: Real;
  DY22: Real;
  DY21: Real;
  COS1: Real;
  COS2: Real;
  SIN12: Real;
  label _1;
  label _2;
  label _3;
  label _4;
  label _5;
begin

      IO1 := N1;
      IO2 := N2;

//      IO1 = N1
//      IO2 = N2
//C
//C FIND THE INDEX OF IO2 AS A NEIGHBOR OF IO1
//C

      INDF := 1;
      IF (IO1 > 1) then INDF := IEND[IO1-2] + 1;
      INDL := IEND[IO1-1];
      INDX := INDL;
  _1: INDX := INDX - 1;
      IF (IADJ[INDX-1] = IO2) then GOTO _2;
      IF (INDX <> INDF) then GOTO _1;

//      INDF = 1
//      IF (IO1 .GT. 1) INDF = IEND(IO1-1) + 1
//      INDL = IEND(IO1)
//    1 INDX = INDX - 1
//      IF (IADJ(INDX) .EQ. IO2) GO TO 2
//      IF (INDX .NE. INDF) GO TO 1
//C
//C N1 AND N2 ARE NOT ADJACENT
//C

      result := -2;
      Exit;

//      LOPTST = -2
//      RETURN
//C
//C DETERMINE IN1 AND IN2 SUCH THAT (IO1,IO2,IN1) AND
//C   (IO2,IO1,IN2) ARE TRIANGLES.
//C
  _2:
      IF (INDX <> INDL) then IN1 := IADJ[INDX+1-1];
      IF (INDX = INDL) then IN1 := IADJ[INDF-1];
      IF (INDX <> INDF) then IN2 := IADJ[INDX-2];
      IF (INDX = INDF) then IN2 := IADJ[INDL-1];
      IF (IN1 <> 0)  and  (IN2 <> 0) then GOTO _3;

//    2 IF (INDX .NE. INDL) IN1 = IADJ(INDX+1)
//      IF (INDX .EQ. INDL) IN1 = IADJ(INDF)
//      IF (INDX .NE. INDF) IN2 = IADJ(INDX-1)
//      IF (INDX .EQ. INDF) IN2 = IADJ(INDL)
//      IF (IN1 .NE. 0  .AND.  IN2 .NE. 0) GO TO 3
//C
//C N1-N2 IS A BOUNDARY ARC
//C

      result := 2;
      Exit;

//      LOPTST = 2
//      RETURN
//C
//C COMPUTE COMPONENTS OF THE QUADRILATERAL SIDES.
//C
  _3:
      DX11 := X[IO1-1] - X[IN1-1];
      DX12 := X[IO2-1] - X[IN1-1];
      DX22 := X[IO2-1] - X[IN2-1];
      DX21 := X[IO1-1] - X[IN2-1];

      DY11 := Y[IO1-1] - Y[IN1-1];
      DY12 := Y[IO2-1] - Y[IN1-1];
      DY22 := Y[IO2-1] - Y[IN2-1];
      DY21 := Y[IO1-1] - Y[IN2-1];


//    3 DX11 = X(IO1) - X(IN1)
//      DX12 = X(IO2) - X(IN1)
//      DX22 = X(IO2) - X(IN2)
//      DX21 = X(IO1) - X(IN2)
//C
//      DY11 = Y(IO1) - Y(IN1)
//      DY12 = Y(IO2) - Y(IN1)
//      DY22 = Y(IO2) - Y(IN2)
//      DY21 = Y(IO1) - Y(IN2)
//C
//C COMPUTE INNER PRODUCTS.
//C

      COS1 := DX11*DX12 + DY11*DY12;
      COS2 := DX22*DX21 + DY22*DY21;

//      COS1 = DX11*DX12 + DY11*DY12
//      COS2 = DX22*DX21 + DY22*DY21
//C
//C IO1-IO2 IS LOCALLY OPTIMAL IFF A1+A2 .LE. 180 DEGREES
//C   WHERE A1 AND A2 DENOTE THE ANGLES AT IN1 AND IN2.
//C

      IF (COS1 < 0)  and  (COS2 < 0) then GOTO _4;
      IF (COS1 > 0)  and  (COS2 > 0) then GOTO _5;

//      IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 4
//      IF (COS1 .GT. 0.  .AND.  COS2 .GT. 0.) GO TO 5
//C
//C COMPUTE A QUANTITY WITH THE SIGN OF SIN(A1+A2).
//C

      SIN12 := (DX11*DY12 - DX12*DY11)*COS2 +
             (DX22*DY21 - DX21*DY22)*COS1;
      IF (SIN12 < 0) then GOTO _4;
      IF (SIN12 > 0) then GOTO _5;

//      SIN12 = (DX11*DY12 - DX12*DY11)*COS2 +
//     .        (DX22*DY21 - DX21*DY22)*COS1
//      IF (SIN12 .LT. 0.) GO TO 4
//      IF (SIN12 .GT. 0.) GO TO 5
//C
//C NEUTRAL CASE
//C

      result := 0;
      Exit;

//      LOPTST = 0
//      RETURN
//C
//C N1-N2 NOT LOCALLY OPTIMAL
//C
  _4:
      result := -1;
      Exit;
//    4 LOPTST = -1
//      RETURN
//C
//C N1-N2 LOCALLY OPTIMAL
//C
  _5:
      result := 1;
      Exit;

//    5 LOPTST = 1
//      RETURN
//      END
end;

procedure DELETE2 (const NN,NOUT1,NOUT2: longint;
  var IADJ: TIntArray; var IEND: TIntArray; var IER: longint);
var
  AdjacenyList: array of TIntArray;
  NodeIndex: Integer;
  Start: Integer;
  NodeCount: Integer;
  InnerNodeIndex: Integer;
  AdjustIndex: Integer;
//  Count: Integer;
  N1: Integer;
  N2: Integer;
  Found1: Boolean;
  Found2: Boolean;
  Found3: Boolean;
  Temp: Integer;
begin
  SetLength(AdjacenyList, NN);
  Start:= 0;
  for NodeIndex := 0 to NN   - 1 do
  begin
    NodeCount := IEND[NodeIndex]-Start;
    SetLength(AdjacenyList[NodeIndex], NodeCount);
    for InnerNodeIndex := 0 to NodeCount - 1 do
    begin
      AdjacenyList[NodeIndex, InnerNodeIndex] := IADJ[Start+InnerNodeIndex];
    end;
    Start := IEND[NodeIndex];
  end;

  Found1 := False;
  Found2 := False;
  Found3 := False;
  for NodeIndex := 0 to NN - 1 do
  begin
    if NodeIndex = NOUT1-1 then
    begin
      NodeCount := Length(AdjacenyList[NodeIndex]);
      for InnerNodeIndex := 0 to NodeCount - 1 do
      begin
        if AdjacenyList[NodeIndex, InnerNodeIndex] = NOUT2 then
        begin
          Found1 := True;
          AdjacenyList[NodeIndex, InnerNodeIndex] := 0;
          While AdjacenyList[NodeIndex, NodeCount-1] <> 0 do
          begin
            Temp := AdjacenyList[NodeIndex, NodeCount-1];
            for AdjustIndex := NodeCount - 1 downto 1 do
            begin
              AdjacenyList[NodeIndex, AdjustIndex] :=
                AdjacenyList[NodeIndex, AdjustIndex-1];
            end;
            AdjacenyList[NodeIndex, AdjustIndex] := Temp;
          end;

          if AdjacenyList[NodeIndex, NodeCount - 2] = 0 then
          begin
            SetLength(AdjacenyList[NodeIndex], NodeCount-1);
          end;
          break;
        end;
      end;
    end
    else if NodeIndex = NOUT2-1 then
    begin
      NodeCount := Length(AdjacenyList[NodeIndex]);
      for InnerNodeIndex := 0 to NodeCount - 1 do
      begin
        if AdjacenyList[NodeIndex, InnerNodeIndex] = NOUT1 then
        begin
          Found2 := True;
          AdjacenyList[NodeIndex, InnerNodeIndex] := 0;
          While AdjacenyList[NodeIndex, NodeCount-1] <> 0 do
          begin
            Temp := AdjacenyList[NodeIndex, NodeCount-1];
            for AdjustIndex := NodeCount - 1 downto 1 do
            begin
              AdjacenyList[NodeIndex, AdjustIndex] :=
                AdjacenyList[NodeIndex, AdjustIndex-1];
            end;
            AdjacenyList[NodeIndex, AdjustIndex] := Temp;
          end;

          if AdjacenyList[NodeIndex, NodeCount - 2] = 0 then
          begin
            SetLength(AdjacenyList[NodeIndex], NodeCount-1);
          end;
          break;
        end;
      end;
    end
    else
    begin
      NodeCount := Length(AdjacenyList[NodeIndex]);
      N1 := AdjacenyList[NodeIndex, NodeCount - 1];
      for InnerNodeIndex := 0 to NodeCount - 1 do
      begin
        N2 := AdjacenyList[NodeIndex, InnerNodeIndex];
        if ((N1 = NOUT1) and (N2 = NOUT2))
          or ((N1 = NOUT2) and (N2 = NOUT1)) then
        begin
          Found3 := True;
          Inc(NodeCount);
          SetLength(AdjacenyList[NodeIndex], NodeCount);
          for AdjustIndex := NodeCount - 1 downto InnerNodeIndex+1 do
          begin
            AdjacenyList[NodeIndex, AdjustIndex] :=
              AdjacenyList[NodeIndex, AdjustIndex-1];
          end;
          AdjacenyList[NodeIndex, InnerNodeIndex] := 0;

          While AdjacenyList[NodeIndex, NodeCount-1] <> 0 do
          begin
            Temp := AdjacenyList[NodeIndex, NodeCount-1];
            for AdjustIndex := NodeCount - 1 downto 1 do
            begin
              AdjacenyList[NodeIndex, AdjustIndex] :=
                AdjacenyList[NodeIndex, AdjustIndex-1];
            end;
            AdjacenyList[NodeIndex, AdjustIndex] := Temp;
          end;
          if AdjacenyList[NodeIndex, NodeCount - 2] = 0 then
          begin
            SetLength(AdjacenyList[NodeIndex], NodeCount-1);
          end;

          Break;
        end;
        N1 := N2;
      end;
    end;
         
  end;

  Start:= 0;
  for NodeIndex := 0 to NN   - 1 do
  begin
    NodeCount := Length(AdjacenyList[NodeIndex]);

    IEND[NodeIndex] := NodeCount+Start;
    for InnerNodeIndex := 0 to NodeCount - 1 do
    begin
      IADJ[Start+InnerNodeIndex] := AdjacenyList[NodeIndex, InnerNodeIndex];
    end;
    Start := IEND[NodeIndex];

  end;

  if Found1 and Found2 and Found3 then
  begin
    IER := 0
  end
  else
  begin
    IER := 1;
  end;

end;

procedure DELETE (const NN,NOUT1,NOUT2: longint;
  var IADJ: TIntArray; var IEND: TIntArray; var IER: longint);
//      SUBROUTINE DELETE (NN,NOUT1,NOUT2, IADJ,IEND, IER)
//      INTEGER NN, NOUT1, NOUT2, IADJ(1), IEND(NN), IER
//      EXTERNAL INDEX
//C
//C***********************************************************
//C
//C                                               ROBERT RENKA
//C                                       OAK RIDGE NATL. LAB.
//C                                             (615) 576-5139
//C
//C   THIS ROUTINE DELETES A BOUNDARY EDGE FROM A TRIANGU-
//C LATION OF A SET OF POINTS IN THE PLANE.  IT MAY BE NEC-
//C ESSARY TO FORCE CERTAIN EDGES TO BE PRESENT BEFORE CALL-
//C ING DELETE (SEE SUBROUTINE EDGE).  NOTE THAT SUBROUTINES
//C EDGE, TRFIND, AND THE ROUTINES WHICH CALL TRFIND (ADNODE,
//C UNIF, INTRC1, AND INTRC0) SHOULD NOT BE CALLED FOLLOWING
//C A DELETION.
//C
//C INPUT PARAMETERS -    NN - NUMBER OF NODES IN THE TRIAN-
//C                            GULATION.
//C
//C              NOUT1,NOUT2 - PAIR OF ADJACENT NODES ON THE
//C                            BOUNDARY DEFINING THE ARC TO
//C                            BE REMOVED.  NOUT2 MUST BE THE
//C                            LAST NONZERO NEIGHBOR OF NOUT1.
//C
//C THE ABOVE PARAMETERS ARE NOT ALTERED BY THIS ROUTINE.
//C
//C                IADJ,IEND - DATA STRUCTURE DEFINING THE
//C                            TRIANGULATION (SEE SUBROUTINE
//C                            TRMESH).
//C
//C OUTPUT PARAMETERS - IADJ,IEND - UPDATED WITH THE REMOVAL
//C                                 OF THE ARC NOUT1-NOUT2
//C                                 IF IER .EQ. 0.
//C
//C                           IER - ERROR INDICATOR
//C                                 IER = 0 IF NO ERRORS WERE
//C                                         ENCOUNTERED.
//C                                 IER = 1 IF NOUT1 OR NOUT2
//C                                         IS NOT ON THE
//C                                         BOUNDARY.
//C                                 IER = 2 IF NOUT1 OR NOUT2
//C                                         HAS ONLY 2 NONZERO
//C                                         NEIGHBORS.
//C                                 IER = 3 IF NOUT2 IS NOT
//C                                         THE LAST NEIGHBOR
//C                                         OF NOUT1.
//C                                 IER = 4 IF A DELETION
//C                                         WOULD DIVIDE THE
//C                                         MESH INTO TWO
//C                                         REGIONS.
//C
//C MODULES REFERENCED BY DELETE - SHIFTD, INDEX
//C
//C***********************************************************
//C
//      INTEGER N, IOUT1, IOUT2, IO1, IO2, IND12, IND21,
//     .        ITEMP, IND1F, IND1L, IND2F, IND2L, NEWBD,
//     .        INDNF, INDNL, INDN0, INDFP2, INDLM3, NF, NL,
//     .        I, IMAX
//C
//C LOCAL PARAMETERS -
//C
//C N =           LOCAL COPY OF NN
//C IOUT1,IOUT2 = LOCAL COPIES OF NOUT1 AND NOUT2
//C IO1,IO2 =     NOUT1,NOUT2 IN ORDER OF INCREASING MAGNITUDE
//C IND12 =       INDEX OF IO2 IN THE ADJACENCY LIST FOR IO1
//C IND21 =       INDEX OF IO1 IN THE ADJACENCY LIST FOR IO2
//C ITEMP =       TEMPORARY STORAGE LOCATION FOR PERMUTATIONS
//C IND1F =       IADJ INDEX OF THE FIRST NEIGHBOR OF IO1
//C IND1L =       IADJ INDEX OF THE LAST NEIGHBOR OF IO1
//C IND2F =       IADJ INDEX OF THE FIRST NEIGHBOR OF IO2
//C IND2L =       IADJ INDEX OF THE LAST NEIGHBOR OF IO2
//C NEWBD =       THE NEIGHBOR COMMON TO NOUT1 AND NOUT2
//C INDNF =       IADJ INDEX OF THE FIRST NEIGHBOR OF NEWBD
//C INDNL =       IADJ INDEX OF THE LAST NEIGHBOR OF NEWBD
//C INDN0 =       INDEX OF 0 IN THE ADJACENCY LIST FOR NEWBD
//C                 BEFORE PERMUTING THE NEIGHBORS
//C INDFP2 =      INDNF + 2
//C INDLM3 =      INDNL - 3
//C NF,NL =       BOUNDS ON THE PORTION OF IADJ TO BE SHIFTED
//C I =           DO-LOOP INDEX
//C IMAX =        UPPER BOUND ON DO-LOOP FOR SHIFTING IEND
//C
var
  N: integer;
  IOUT1: Integer;
  IOUT2: Integer;
  IND1F: Integer;
  IND1L: Integer;
  IND2F: Integer;
  IND2L: Integer;
  NEWBD: Integer;
  INDN0: Integer;
  INDNL: Integer;
  IO1: Integer;
  IO2: Integer;
  IND12: Integer;
  IND21: Integer;
  NF: Integer;
  NL: Integer;
  IMAX: Integer;
  I: Integer;
  INDNF: Integer;
  INDFP2: Integer;
  ITEMP: Integer;
  INDLM3: Integer;
  label _1;
  label _2;
  label _6;
  label _8;
  label _10;
  label _12;
  label _13;
  label _15;
  label _16;
  label _17;
  label _19;
  label _20;
  label _21;
  label _22;
  label _23;
  label _24;
begin
      N := NN;
      IOUT1 := NOUT1;
      IOUT2 := NOUT2;

//      N = NN
//      IOUT1 = NOUT1
//      IOUT2 = NOUT2
//C
//C INITIALIZE INDICES
//C

      IND1F := 1;
      IF (IOUT1 > 1) then IND1F := IEND[IOUT1-2] + 1;
      IND1L := IEND[IOUT1-1];
      IND2F := 1;
      IF (IOUT2 > 1) then IND2F := IEND[IOUT2-2] + 1;
      IND2L := IEND[IOUT2-1];
      NEWBD := IADJ[IND1L-3];
      INDN0 := INDEX_Pascal(NEWBD,IOUT2,IADJ,IEND);
      INDNL := IEND[NEWBD-1];

//      IND1F = 1
//      IF (IOUT1 .GT. 1) IND1F = IEND(IOUT1-1) + 1
//      IND1L = IEND(IOUT1)
//      IND2F = 1
//      IF (IOUT2 .GT. 1) IND2F = IEND(IOUT2-1) + 1
//      IND2L = IEND(IOUT2)
//      NEWBD = IADJ(IND1L-2)
//      INDN0 = INDEX(NEWBD,IOUT2,IADJ,IEND)
//      INDNL = IEND(NEWBD)
//C
//C ORDER VERTICES SUCH THAT THE NEIGHBORS OF IO1 PRECEDE
//C   THOSE OF IO2
//C

      IF (IOUT1 > IOUT2) then GOTO _1;
      IO1 := IOUT1;
      IO2 := IOUT2;
      IND12 := IND1L - 1;
      IND21 := IND2F;
      GOTO _2;
  _1: IO1 := IOUT2;
      IO2 := IOUT1;
      IND12 := IND2F;
      IND21 := IND1L - 1;

//      IF (IOUT1 .GT. IOUT2) GO TO 1
//      IO1 = IOUT1
//      IO2 = IOUT2
//      IND12 = IND1L - 1
//      IND21 = IND2F
//      GO TO 2
//    1 IO1 = IOUT2
//      IO2 = IOUT1
//      IND12 = IND2F
//      IND21 = IND1L - 1
//C
//C CHECK FOR ERRORS
//C
  _2:
     IF ( (IADJ[IND1L-1] <> 0) or (IADJ[IND2L-1] <> 0) ) then
        GOTO _21;
      IF ( (IND1L-IND1F < 2) or (IND2L-IND2F <= 2) ) then
        GOTO _22;
      IF (IADJ[IND1L-2] <> IOUT2) then GOTO _23;
      IF (IADJ[INDNL-1] = 0) then GOTO _24;

//    2 IF ( (IADJ(IND1L) .NE. 0) .OR. (IADJ(IND2L) .NE. 0) )
//     .   GO TO 21
//      IF ( (IND1L-IND1F .LE. 2) .OR. (IND2L-IND2F .LE. 2) )
//     .   GO TO 22
//      IF (IADJ(IND1L-1) .NE. IOUT2) GO TO 23
//      IF (IADJ(INDNL) .EQ. 0) GO TO 24
//C
//C DELETE THE EDGE IO1-IO2 AND MAKE NEWBD A BOUNDARY NODE
//C

      IF (NEWBD < IO1) then GOTO _8;
      IF (NEWBD < IO2) then GOTO _6;

//      IF (NEWBD .LT. IO1) GO TO 8
//      IF (NEWBD .LT. IO2) GO TO 6
//C
//C THE VERTICES ARE ORDERED IO1, IO2, NEWBD.
//C DELETE IO2 AS A NEIGHBOR OF IO1.
//C

      NF := IND12 + 1;
      NL := IND21 - 1;
      SHIFTD(NF,NL,-1, IADJ );
      IMAX := IO2 - 1;
      for I := IO1 to IMAX do
      begin
        IEND[I-1] := IEND[I-1] - 1;
      end;

//      NF = IND12 + 1
//      NL = IND21 - 1
//      CALL SHIFTD(NF,NL,-1, IADJ )
//      IMAX = IO2 - 1
//      DO 3 I = IO1,IMAX
//        IEND(I) = IEND(I) - 1
//    3   CONTINUE
//C
//C DELETE IO1 AS A NEIGHBOR OF IO2
//C

      NF := NL + 2;
      NL := INDN0;
      SHIFTD(NF,NL,-2, IADJ );
      IMAX := NEWBD - 1;
      for I := IO2 to IMAX do
      begin
        IEND[I-1] := IEND[I-1] - 2;
      end;

//      NF = NL + 2
//      NL = INDN0
//      CALL SHIFTD(NF,NL,-2, IADJ )
//      IMAX = NEWBD - 1
//      DO 4 I = IO2,IMAX
//        IEND(I) = IEND(I) - 2
//    4   CONTINUE
//C
//C SHIFT THE BOTTOM OF IADJ UP 1 LEAVING ROOM FOR 0 AS A
//C   NEIGHBOR OF NEWBD
//C

      INDN0 := INDN0 - 1;
      NF := NL + 1;
      NL := IEND[N-1];
      IF (NF <= NL) then SHIFTD(NF,NL,-1, IADJ );
      for I := NEWBD to N do
      begin
        IEND[I-1] := IEND[I-1] - 1
      end;
      GOTO _12;

//      INDN0 = INDN0 - 1
//      NF = NL + 1
//      NL = IEND(N)
//      IF (NF .LE. NL) CALL SHIFTD(NF,NL,-1, IADJ )
//      DO 5 I = NEWBD,N
//        IEND(I) = IEND(I) - 1
//    5   CONTINUE
//      GO TO 12
//C
//C THE VERTICES ARE ORDERED IO1, NEWBD, IO2.
//C DELETE IO2 AS A NEIGHBOR OF IO1 LEAVING ROOM FOR 0 AS A
//C   NEIGHBOR OF NEWBD.
//C
  _6:
      NF := IND12 + 1;
      NL := INDN0;
      SHIFTD(NF,NL,-1, IADJ );
      IMAX := NEWBD - 1;
      for I := IO1 to IMAX do
      begin
        IEND[I-1] := IEND[I-1] - 1
      end;
      GOTO _10;

//    6 NF = IND12 + 1
//      NL = INDN0
//      CALL SHIFTD(NF,NL,-1, IADJ )
//      IMAX = NEWBD - 1
//      DO 7 I = IO1,IMAX
//        IEND(I) = IEND(I) - 1
//    7   CONTINUE
//      GO TO 10
//C
//C THE VERTICES ARE ORDERED NEWBD, IO1, IO2.
//C DELETE IO2 AS A NEIGHBOR OF IO1 LEAVING ROOM FOR 0 AS A
//C   NEIGHBOR OF NEWBD.
//C
  _8:
      INDN0 := INDN0 + 1;
      NF := INDN0;
      NL := IND12 - 1;
      IF (NF <= NL) then SHIFTD(NF,NL,1, IADJ );
      IMAX := IO1 - 1;
      for I := NEWBD to IMAX do
      begin
        IEND[I-1] := IEND[I-1] + 1;
      end;


//    8 INDN0 = INDN0 + 1
//      NF = INDN0
//      NL = IND12 - 1
//      IF (NF .LE. NL) CALL SHIFTD(NF,NL,1, IADJ )
//      IMAX = IO1 - 1
//      DO 9 I = NEWBD,IMAX
//        IEND(I) = IEND(I) + 1
//    9   CONTINUE
//C
//C DELETE IO1 AS A NEIGHBOR OF IO2
//C
  _10:
      NF := IND21 + 1;
      NL := IEND[N-1];
      SHIFTD(NF,NL,-1, IADJ );
      for I := IO2 to N do
      begin
        IEND[I-1] := IEND[I-1] - 1;
      end;

//   10 NF = IND21 + 1
//      NL = IEND(N)
//      CALL SHIFTD(NF,NL,-1, IADJ )
//      DO 11 I = IO2,N
//        IEND(I) = IEND(I) - 1
//   11   CONTINUE
//C
//C PERMUTE THE NEIGHBORS OF NEWBD WITH END-AROUND SHIFTS SO
//C   THAT 0 IS THE LAST NEIGHBOR
//C
  _12:
      INDNF := 1;
      IF (NEWBD > 1) then INDNF := IEND[NEWBD-2] + 1;
      INDNL := IEND[NEWBD-1];
      IF (INDN0-INDNF > INDNL-INDN0) then GOTO _16;

//   12 INDNF = 1
//      IF (NEWBD .GT. 1) INDNF = IEND(NEWBD-1) + 1
//      INDNL = IEND(NEWBD)
//      IF (INDN0-INDNF .GE. INDNL-INDN0) GO TO 16
//C
//C SHIFT UPWARD
//C

      IF (INDN0 > INDNF) then GOTO _13;
      SHIFTD(INDNF+1,INDNL,-1, IADJ );
      GOTO _20;
 _13: INDFP2 := INDNF + 2;
      IF (INDN0 < INDFP2) then GOTO _15;
      for I := INDFP2 to INDN0 do
      begin
        ITEMP := IADJ[INDNF-1];
        SHIFTD(INDNF+1,INDNL,-1, IADJ );
        IADJ[INDNL-1] := ITEMP;
      end;

//      IF (INDN0 .GT. INDNF) GO TO 13
//      CALL SHIFTD(INDNF+1,INDNL,-1, IADJ )
//      GO TO 20
//   13 INDFP2 = INDNF + 2
//      IF (INDN0 .LT. INDFP2) GO TO 15
//      DO 14 I = INDFP2,INDN0
//        ITEMP = IADJ(INDNF)
//        CALL SHIFTD(INDNF+1,INDNL,-1, IADJ )
//        IADJ(INDNL) = ITEMP
//   14   CONTINUE
//C
//C THE LAST SHIFT IS BY 2
//C
  _15:
      ITEMP := IADJ[INDNF-1];
      SHIFTD(INDFP2,INDNL,-2, IADJ );
      IADJ[INDNL-2] := ITEMP;
      GOTO _20;

//   15 ITEMP = IADJ(INDNF)
//      CALL SHIFTD(INDFP2,INDNL,-2, IADJ )
//      IADJ(INDNL-1) = ITEMP
//      GO TO 20
//C
//C SHIFT DOWNWARD
//C
  _16:
      IF (INDN0 = INDNL) then GOTO _20;
      IF (INDN0 < INDNL-1) then GOTO _17;
      SHIFTD(INDNF,INDNL-2,1, IADJ );
      IADJ[INDNF-1] := IADJ[INDNL-1];
      GOTO _20;
 _17: INDLM3 := INDNL - 3;
      IF (INDN0 > INDLM3) then GOTO _19;
      for I := INDN0 to INDLM3 do
      begin
        ITEMP := IADJ[INDNL-1];
        SHIFTD(INDNF,INDNL-1,1, IADJ );
        IADJ[INDNF-1] := ITEMP
      end;

//   16 IF (INDN0 .EQ. INDNL) GO TO 20
//      IF (INDN0 .LT. INDNL-1) GO TO 17
//      CALL SHIFTD(INDNF,INDNL-2,1, IADJ )
//      IADJ(INDNF) = IADJ(INDNL)
//      GO TO 20
//   17 INDLM3 = INDNL - 3
//      IF (INDN0 .GT. INDLM3) GO TO 19
//      DO 18 I = INDN0,INDLM3
//        ITEMP = IADJ(INDNL)
//        CALL SHIFTD(INDNF,INDNL-1,1, IADJ )
//        IADJ(INDNF) = ITEMP
//   18   CONTINUE
//C
//C THE LAST SHIFT IS BY 2
//C
  _19:
      ITEMP := IADJ[INDNL-2];
      SHIFTD(INDNF,INDLM3,2, IADJ );
      IADJ[INDNF+1-1] := IADJ[INDNL-1];
      IADJ[INDNF-1] := ITEMP;

//   19 ITEMP = IADJ(INDNL-1)
//      CALL SHIFTD(INDNF,INDLM3,2, IADJ )
//      IADJ(INDNF+1) = IADJ(INDNL)
//      IADJ(INDNF) = ITEMP
//C
//C INSERT 0 AS THE LAST NEIGHBOR OF NEWBD
//C
  _20:
      IADJ[INDNL-1] := 0;
      IER := 0;
      Exit;

//   20 IADJ(INDNL) = 0
//      IER = 0
//      RETURN
//C
//C ONE OF THE VERTICES IS NOT ON THE BOUNDARY
//C
 _21: IER := 1;
      Exit;
//   21 IER = 1
//      RETURN
//C
//C ONE OF THE VERTICES HAS ONLY TWO NONZERO NEIGHBORS.  THE
//C   TRIANGULATION WOULD BE DESTROYED BY A DELETION
//C
 _22: IER := 2;
      Exit;
//   22 IER = 2
//      RETURN
//C
//C NOUT2 IS NOT THE LAST NONZERO NEIGHBOR OF NOUT1
//C
 _23: IER := 3;
      Exit;
//   23 IER = 3
//      RETURN
//C
//C A DELETION WOULD DIVIDE THE MESH INTO TWO REGIONS
//C   CONNECTED AT A SINGLE NODE
//C
 _24: IER := 4;
      Exit;
//   24 IER = 4
//      RETURN
//      END
end;

end.
