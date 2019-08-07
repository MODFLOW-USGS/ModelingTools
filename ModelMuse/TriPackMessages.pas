{
  These procedures replace the WRITE statements in Tripack.for.

 @author(Richard B. Winston <rbwinst@usgs.gov>)
 This file is in the public domain.
}
unit TriPackMessages;

interface

uses SysUtils, StdCtrls;


// TRPRNT Messages
//100

//
procedure ADJACENCY_SETS(var Fortran: Longint; var N: longint); stdcall;

//101

//
procedure NEIGHBORS(const Fortran: Longint); stdcall;

//102

//
procedure X_NODE(var Fortran: Longint); stdcall;

//103

//
procedure ADJACENCY(const Fortran: Longint; const Node, AdjacentNode: longint); stdcall;

//104

//
procedure Positions(var Fortran: Longint; var Node: longint; var X, Y: Real;
  var AdjacentNode: longint); stdcall;

//105

//
Procedure BlankLine(const Fortran: Longint);

//106

//
Procedure NewPage(const Fortran: Longint);

//107

//
procedure BOUNDARY_NODES(var Fortran: Longint; var NB, NA, NT: longint); stdcall;

//108

//
Procedure OUT_OF_RANGE_TRPRNT(const Fortran: Longint);


// TRMTST Messages
// 100

//
procedure NULL_TRIANGLES(const Fortran: Longint; const NullTriangles: longint); stdcall;

// 110

//
procedure INVALID_INPUT_PARAMETER(const Fortran: Longint; const N: longint; const Tol: Real); stdcall;

// 120

//
procedure LESS_THAN_3_NEIGHBORS(const Fortran: Longint; const Node: longint); stdcall;

// 130

//
procedure ENTRY_OUT_OF_RANGE(const Fortran: Longint; const INDX: longint); stdcall;

// 140

//
procedure INCONSISTENT_PARAMETERS(const Fortran: Longint;
  const BoundaryNodes, Triangles, Arcs: longint); stdcall;

// 150

//
procedure CIRCUMCIRCLE_TEST_FAILURE(const Fortran: Longint;
  const CIRCUMCIRCLES: longint); stdcall;

// Demo 1 messages
// TRIANGULATION ERROR MESSAGES
// 200

//
procedure OUT_OF_RANGE_Demo1(const Fortran: Longint; const N: longint); stdcall;

// 210

//
procedure ALL_NODES_ARE_COLLINEAR(const Fortran: Longint); stdcall;

// 220

//
procedure ERROR_IN_REORDR(const Fortran: Longint);stdcall;

// 230

//
procedure ERROR_IN_GETNP(const Fortran: Longint); stdcall;

// 240

//
procedure ERROR_IN_EDGE(const Fortran: Longint; const IER: longint);stdcall;

// 250

//
procedure ARCS_NOT_OPTIMAL(const Fortran: Longint; const I: longint); stdcall;

// 260

//
procedure ERROR_IN_DELETE_INVALID_FLAG(const Fortran: Longint; const IER: longint); stdcall;

// 270

//
procedure ERROR_IN_EDGE_TRIANGULATION(const Fortran: Longint; const IER: longint); stdcall;

//
procedure ERROR_IN_DELETE_TRIANGULATION(const Fortran: Longint; const IER: longint); stdcall;

// INTERPOLATION TEST MESSAGES
// 300

//
procedure OUTPUT_FROM_BNODES(var Fortran: Longint;
  var NodeCount, TriangleCount, ArcCount: longint; var Area, Volume: Real); stdcall;

// 310

//
procedure GRADG_TEST(var Fortran: Longint; var MaxError: Real); stdcall;

// 320

//
procedure INTRC0_TEST(var Fortran: Longint; var MaxError: Real); stdcall;

// 330

//
procedure GRADL_TEST(var Fortran: Longint; var MaxError: Real); stdcall;

// 340

//
procedure UNIF_TEST(var Fortran: Longint; var MaxError: Real; var PointCount: longint); stdcall;

// INFORMATIVE MESSAGES
// 400

//
procedure TRIPAK_SRFPAK_TEST(var Fortran: Longint; var Test: longint); stdcall;

// 410

//
procedure EDGE_NOT_TESTED(const Fortran: Longint); stdcall;

// 420

//
Procedure DELETE_NOT_TESTED(const Fortran: Longint); stdcall;

// 430

//
Procedure TRIANGULATION_ERRORS(var Fortran: Longint); stdcall;

// Demo 2 messages
procedure MAXIMUM_INTERPOLATION_ERROR(var Fortran: Longint; var IER: Real);stdcall;

implementation

//uses frmMainUnit;

{
exports ADJACENCY_SETS, NEIGHBORS, X_NODE, ADJACENCY, Positions, BlankLine,
  NewPage, BOUNDARY_NODES, OUT_OF_RANGE_TRPRNT, NULL_TRIANGLES,
  INVALID_INPUT_PARAMETER, LESS_THAN_3_NEIGHBORS, ENTRY_OUT_OF_RANGE,
  INCONSISTENT_PARAMETERS, CIRCUMCIRCLE_TEST_FAILURE, OUT_OF_RANGE_Demo1,
  ALL_NODES_ARE_COLLINEAR, ERROR_IN_REORDR, ERROR_IN_GETNP, ERROR_IN_EDGE,
  ARCS_NOT_OPTIMAL, ERROR_IN_DELETE_INVALID_FLAG, ERROR_IN_DELETE_TRIANGULATION,
  OUTPUT_FROM_BNODES, GRADG_TEST, INTRC0_TEST, GRADL_TEST, UNIF_TEST,
  TRIPAK_SRFPAK_TEST, EDGE_NOT_TESTED, DELETE_NOT_TESTED, TRIANGULATION_ERRORS,
  MAXIMUM_INTERPOLATION_ERROR;
}

{
Function GetMemo(const Fortran: Longint): TMemo;
begin
  result := nil;
  case Fortran of
    1:
      begin
        result := frmMain.memoFortran;
      end;
    2:
      begin
        result := frmMain.memoPascal;
      end;
    else
      Assert(False);
  end;
end;
}

// TRPRNT Messages
procedure ADJACENCY_SETS(var Fortran: Longint; var N: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ADJACENCY SETS,    N = ' + IntToStr(N));
end;

procedure NEIGHBORS(const Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('NODE                                NEIGHBORS OF NODE');
end;

procedure X_NODE(var Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('NODE    X(NODE)     Y(NODE)                       NEIGHBORS OF NODE');
end;

procedure ADJACENCY(const Fortran: Longint; const Node, AdjacentNode: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add(IntToStr(Node) + ' ' + IntToStr(AdjacentNode));
end;

procedure Positions(var Fortran: Longint; var Node: longint; var X, Y: Real;
  var AdjacentNode: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add(IntToStr(Node) + ' '
//    + FloatToStr(X)+ ' '
//    + FloatToStr(Y)+ ' '
//    + IntToStr(AdjacentNode));
end;

Procedure BlankLine(const Fortran: Longint);
begin
//  GetMemo(Fortran).Lines.Add('');
end;

Procedure NewPage(const Fortran: Longint);
begin
//  GetMemo(Fortran).Lines.Add('1');
end;

procedure BOUNDARY_NODES(var Fortran: Longint; var NB, NA, NT: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('NB = ' +  IntToStr(NB)
//    + ' BOUNDARY NODES NA = ' + IntToStr(NA)+ ' '
//    + ' ARCS NT = ' + IntToStr(NT)+ ' '
//    + ' TRIANGLES');
end;

Procedure OUT_OF_RANGE_TRPRNT(const Fortran: Longint);
begin
//  GetMemo(Fortran).Lines.Add('*** N IS OUT OF RANGE ***');
end;

// TRMTST Messages
procedure NULL_TRIANGLES(const Fortran: Longint; const NullTriangles: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('RMTST -- ' + IntToStr(NullTriangles)
//    + ' ARE PRESENT (NULL TRIANGLES ON THE BOUNDARY ARE UNAVOIDABLE)');
end;

procedure INVALID_INPUT_PARAMETER(const Fortran: Longint; const N: longint; const Tol: Real); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('TRMTST -- INVALID INPUT PARAMETER N = ' + IntToStr(N)
//    + ', TOL = ' + FloatToStr(Tol));
end;

procedure LESS_THAN_3_NEIGHBORS(const Fortran: Longint; const Node: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('TRMTST -- NODE ' + IntToStr(Node)
//    + ' HAS LESS THAN 3 NEIGHBORS');
end;

procedure ENTRY_OUT_OF_RANGE(const Fortran: Longint; const INDX: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('TRMTST -- IADJ(K) IS NOT A VALID INDEX FOR K = ' + IntToStr(INDX)
//    + ' HAS LESS THAN 3 NEIGHBORS');
end;

procedure INCONSISTENT_PARAMETERS(const Fortran: Longint;
  const BoundaryNodes, Triangles, Arcs: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('TRMTST -- INCONSISTENT PARAMETERS '
//    + IntToStr(BoundaryNodes) + ' BOUNDARY NODES'
//    + IntToStr(Triangles) + ' TRIANGLES'
//    + IntToStr(Arcs) + ' ARCS');
end;

procedure CIRCUMCIRCLE_TEST_FAILURE(const Fortran: Longint;
  const CIRCUMCIRCLES: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('TRMTST -- '
//    + IntToStr(CIRCUMCIRCLES) + ' CIRCUMCIRCLES CONTAIN NODES IN THEIR INTERIORS');
end;

//C
//C CIRCUMCIRCLE TEST FAILURE
//C
//    9 IER = 4
//      IF (RITE) WRITE (LUN,150) NFAIL
//  150 FORMAT (1H ,10HTRMTST -- ,I5,15H CIRCUMCIRCLES ,
//     .        32HCONTAIN NODES IN THEIR INTERIORS)


// TRIANGULATION ERROR MESSAGES

procedure OUT_OF_RANGE_Demo1(const Fortran: Longint; const N: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('N OUT OF RANGE -- N =' + IntToStr(N));
end;

procedure ALL_NODES_ARE_COLLINEAR(const Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ALL NODES ARE COLLINEAR');
end;

procedure ERROR_IN_REORDR(const Fortran: Longint);stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN REORDR');
end;

procedure ERROR_IN_GETNP(const Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN GETNP');
end;

procedure ERROR_IN_EDGE(const Fortran: Longint; const IER: longint);stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN EDGE -- IER = ' + IntToStr(IER));
end;

procedure ARCS_NOT_OPTIMAL(const Fortran: Longint; const I: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN EDGE -- = ' + IntToStr(I)
//    + ' ARCS ARE NOT LOCALLY OPTIMAL');
end;

procedure ERROR_IN_DELETE_INVALID_FLAG(const Fortran: Longint; const IER: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN DELETE (INVALID FLAG RETURNED) -- IER = '
//    + IntToStr(IER));
end;

procedure ERROR_IN_EDGE_TRIANGULATION(const Fortran: Longint; const IER: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN EDGE (TRIANGULATION DESTROYED; ERROR FLAG FROM TRMTST = '
//    + IntToStr(IER));
end;

procedure ERROR_IN_DELETE_TRIANGULATION(const Fortran: Longint; const IER: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('ERROR IN DELETE (TRIANGULATION DESTROYED; ERROR FLAG FROM TRMTST = '
//    + IntToStr(IER));
end;

// INTERPOLATION TEST MESSAGES

procedure OUTPUT_FROM_BNODES(var Fortran: Longint;
  var NodeCount, TriangleCount, ArcCount: longint; var Area, Volume: Real); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('OUTPUT FROM BNODES, AREA, AND VOLUME; BNODES --'
//    + IntToStr(NodeCount) + ' BOUNDARY NODES, '
//    + IntToStr(TriangleCount) + ' TRIANGLES, '
//    + IntToStr(ArcCount) + ' ARCS;'
//    + 'AREA   -- AREA OF CONVEX HULL = ' +  FloatToStr(Area)
//    + 'VOLUME   -- AREA OF CONVEX HULL = ' +  FloatToStr(Volume)
//    );
end;

procedure GRADG_TEST(var Fortran: Longint; var MaxError: Real); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('GRADG TEST -- MAXIMUM ERROR = '
//    + FloatToStr(MaxError));
end;

procedure INTRC0_TEST(var Fortran: Longint; var MaxError: Real); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('INTRC0 TEST -- MAXIMUM ERROR = '
//    + FloatToStr(MaxError));
end;

procedure GRADL_TEST(var Fortran: Longint; var MaxError: Real); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('GRADL TEST -- MAXIMUM ERROR = '
//    + FloatToStr(MaxError));
end;

procedure UNIF_TEST(var Fortran: Longint; var MaxError: Real; var PointCount: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('UNIF (INTRC1) TEST -- MAXIMUM ERROR = '
//    + FloatToStr(MaxError) + ' EXTRAPOLATION OCCURRED AT '
//    + IntToStr(PointCount) + ' POINTS');
end;

// INFORMATIVE MESSAGES

procedure TRIPAK_SRFPAK_TEST(var Fortran: Longint; var Test: longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('TRIPAK/SRFPAK TEST -- N = ' + IntToStr(Test));
end;

procedure EDGE_NOT_TESTED(const Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('SUBROUTINE EDGE NOT TESTED -- '
//    + ' THE LEFTMOST NODE IS ADJACENT TO ALL OTHER NODES');
end;

Procedure DELETE_NOT_TESTED(const Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('SUBROUTINE DELETE NOT TESTED -- '
//    + ' NO BOUNDARY ARC CAN BE REMOVED WITHOUT  DESTROYING THE TRIANGULATION');
end;

Procedure TRIANGULATION_ERRORS(var Fortran: Longint); stdcall;
begin
//  GetMemo(Fortran).Lines.Add('NO TRIANGULATION ERRORS ENCOUNTERED');
end;

// Demo 2 messages
procedure MAXIMUM_INTERPOLATION_ERROR(var Fortran: Longint; var IER: Real);stdcall;
begin
//  GetMemo(Fortran).Lines.Add('MAXIMUM INTERPOLATION ERROR = ' + FloatToStr(IER));
end;

end.
