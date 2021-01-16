// This file contains routines for plotting contour lines based on data points
// at arbitrary locations.
//
// Output from the procedures in this file are sent to @link(CalCompRoutines).
// You can alter the procedures in @link(CalCompRoutines) if you wish to
// capture the output in a different way.
//
// Original source code
// http://www.netlib.org/toms/626
//
// Conditions of use
// http://www.netlib.org/toms/
//
// Translation from Fortran by Richard B. Winston.
// @author(Richard B. Winston <rbwinst@usgs.gov>).
//
// Although this file is in the public domain, the algorithm it contains
// is not.
// For conditions of use see
// http://www.netlib.org/toms/


unit TriCP_Routines;

interface

uses SysUtils, TriPackRoutines, GoPhastTypes;

Type
  TIntArray3 = array[0..2] of longint;
  TIntArray25 = array[0..24] of longint;

  TRealArray25 = array[0..24] of Real;
  TRealArrayX_5 = array of array[0..4] of Real;
  TRealArray3 = array[0..2] of Real;
  TRealArray3_3 = array[0..2,0..2] of Real;
  TRealArray6_3 = array[0..5, 0..2] of Real;
  TRealArray6 = array[0..5] of Real;
  TRealArray5_3 = array[0..4, 0..2] of Real;

{
@preformatted(
     T R I ANGLE    C  ONTOUR    P  LOTTING

     USER INTERFACE   (VERSION 0.1)

     COPYRIGHT(C): A. PREUSSER, 1984
                   SEN. F. WISS. U. FORSCH.
                   PROJEKTGRUPPE PARALLELRECHNEN
                   HEILBRONNER STR. 10
                   D-1000 BERLIN 31

   XD,YD,ZD   COORDINATES OF DATA POINTS
              XD AND YD MUST BE GIVEN IN CM.
              FOR USING INCH,  SET INSTALLATION PARAMETER
              CMSCAL= 2.54 .
   ND         NUMBER OF DATA POINTS
   C          CONTOUR LEVELS
   NC         NUMBER OF CONTOUR LEVELS
   NT         NUMBER OF TRIANGLES
   WK         REAL WORK ARRAY, DIMENSION 5*ND

              ON EXIT, WK CONTAINS THE PARTIAL DERIVATIVES
              (ZX(I),ZY(I),ZXX(I),ZXY(I),ZYY(I),I=1,ND)

   IPT           INTEGER ARRAY. THE DIMENSION IS 3*NT.
                 POINT NUMBERS FOR THE TRIANGLES.
                 THE FIRST 3 NUMBERS DETERMINE THE VERTICES OF THE
                 FIRST TRIANGLE, THE NEXT 3 FOR THE SECOND AND SO ON.
                 THE NUMBERS CORRESPOND TO THE INDICES OF THE
                 XD,YD,ZD ARRAYS. THEY ARE ARRANGED COUNTER-
                 CLOCKWISE WITHIN A TRIANGLE.

   IPL           INTEGER ARRAY. THE DIMENSION IS NCP*ND
                 A LIST OF NCP*ND POINT NUMBERS, REPRESENTING
                 NCP POINTS FOR EACH DATA POINT THAT ARE
                 THE CLOSEST TO THIS POINT. THE FIRST NCP
                 NUMBERS ARE FOR THE FIRST DATA POINT, THE NEXT
                 NCP FOR THE SECOND AND SO ON. THESE NUMBERS
                 WERE USED FOR THE COMPUTATION OF THE PARTIAL
                 DERIVATIVES.

   NCP           NCP IS AN INSTALLATION PARAMETER AND WILL BE SET
                 TO 4.

   MODE       MODE OF USAGE
              0, TRIANGULATION REQUESTED.
                 NT, IPT AND IPL WILL HAVE THE INFORMATION DESCRIBED
                 ABOVE ON EXIT.

              1, NO TRIANGULATION REQUESTED.
                 IPT MUST CONTAIN THE INFORMATION ABOUT
                 THE TRIANGLES AS DESCRIBED ABOVE
                 ON ENTRY AND NT MUST BE SPECIFIED ON ENTRY.
                 IPT WILL NOT BE CHANGED
                 AND IN ADDITION, THE INFORMATION DESCRIBED
                 IN IPL WILL BE AVAILABLE ON EXIT.

              2, NO TRIANGULATION AND NO DETERMINATION OF THE
                 NCP CLOSEST POINTS FOR EACH DATA POINT.
                 IPT AND IPL MUST CONTAIN THE INFORMATION AS DESCRIBED
                 AS INPUT INFORMATION AND NT MUST BE SPECIFIED ON ENTRY.
                 THE CONTENTS OF IPT AND IPL WILL NOT BE CHANGED.

              3, NO TRIANGULATION AND NO COMPUTATION OF THE
                 PARTIAL DERIVATIVES.
                 IPT MUST CONTAIN THE INFORMATION ABOUT
                 THE TRIANGLES AS DESCRIBED ABOVE
                 ON ENTRY AND NT MUST BE SPECIFIED ON ENTRY
                 AND
                 WK MUST CONTAIN THE PARTIAL DERIVATIVES
                 (ZX(I),ZY(I),ZXX(I),ZXY(I),ZYY(I),I=1,ND)
                 ON ENTRY.
                 IPL IS IGNORED.
                 THE CONTENTS OF WK AND IPT WILL NOT BE CHANGED.
                 THIS MODE IS ESPECIALLY USEFUL WHEN TRICP IS
                 CALLED AGAIN AFTER A PREVIOUS CALL. FOR INSTANCE,
                 ONLY THE CONTOUR LEVELS MAY HAVE CHANGED.
                 WHEN DESIGNING A SURFACE, IT MAY BE APPROPRIATE
                 TO CHANGE THE XD,YD,ZD PARAMETERS AND THE PARTIAL
                 DERIVATIVES INTERACTIVELY AND TO CALL TRICP AGAIN
                 USING THIS MODE.
)

@seealso(TMultipleContourCreator.CreateAndDrawContours)
@seealso(TCustomContourCreator.PerformAlg626)

}

procedure TRICP_Pascal (const XD,YD,ZD, C: TRealArray; var WK: TRealArray;
  const ND, NCP, NC, MODE: longint;
  var NT: longint; var IPT, IPL: TIntArray);


{
@preformatted(
 THE IDTANG SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y
 PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
 POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
 BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
 CORRESPONDING TO THE BORDER LINE SEGMENTS.
 AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
 ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
 OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
 LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
 THIS SUBROUTINE CALLS THE IDXCHG FUNCTION.

 THE INPUT PARAMETERS ARE
     NDP = NUMBER OF DATA POINTS,
     XD  = ARRAY OF DIMENSION NDP CONTAINING THE
           X COORDINATES OF THE DATA POINTS,
     YD  = ARRAY OF DIMENSION NDP CONTAINING THE
           Y COORDINATES OF THE DATA POINTS.
 THE OUTPUT PARAMETERS ARE
     NT  = NUMBER OF TRIANGLES,
     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE
           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH
           TRIANGLE ARE TO BE STORED AS THE (3*IT-3)ND,
           (3*IT-2)ST, AND (3*IT-1)TH ELEMENTS,
           IT=1,2,...,NT,
     NL  = NUMBER OF BORDER LINE SEGMENTS,
     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE
           POINT NUMBERS OF THE END POINTS OF THE (IL)TH
           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE
           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND,
           (3*IL-1)ST, AND (3*IL)TH ELEMENTS,
           IL=1,2,..., NL.

 THE OTHER PARAMETERS ARE
     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
           INTERNALLY AS A WORK AREA,
     IWP = INTEGER ARRAY OF DIMENSION NDP USED
           INTERNALLY AS A WORK AREA,
     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
           WORK AREA.
}
procedure  IDTANG_Pascal(const NDP: longint; const XD,YD: TRealArray;
  var NT, NL: longint; var IPT, IPL: TIntArray);

// @name converts the triangulation data structure used by @link(TRMESH)
// to that used by @link(IDTANG_Pascal).
procedure Trmesh_2_Idtang(var NT: Integer;
  const IEND, IADJ: TIntArray;
  var IPL, IPT: TIntArray; const ND: Integer);

// @name is used to convert a @link(TRealArray) to a @link(TRealArrayX_5).
procedure Convert_1DArrayTo2DArray(const InArray: TRealArray;
  out OutArray: TRealArrayX_5);

// @name is used to convert a @link(TRealArrayX_5) to a @link(TRealArray).
procedure Convert_2DArrayTo1DArray(const InArray: TRealArrayX_5;
  out OutArray: TRealArray);

const
//     NCP = NUMBER OF DATA POINTS CLOSEST TO EACH DATA
//           POINTS.
  NCP = 4;

implementation

uses Math, CalCompRoutines, Classes, Contnrs, QuadTreeClass, FastGEO,
  Generics.Collections, IntListUnit, Generics.Defaults, Dialogs;


Type

//     X,Y,Z      COORDINATES AT THE THREE VERTICES OF A TRIANGLE
//     DX,DY      DIFFERENCES OF X,Y
//     DX2,DY2    (DIFFERENCES OF X,Y)**2
//     SL         SIDE LENGTHS
//     SL2        (SIDE LENGTHS)**2
//     ZT(IV,K)   FIRST PARTIAL DERIVATIVE WITH RESPECT TO VARIABLE IV
//                (U,V,W FOR IV=1,2,3) AT POINT K
//     ZTT        SECOND PARTIAL DERIVATIVES
//     ZUV        MIXED PARTIAL DERIVATIVES
//     AP,BP,CP,DP  CONSTANTS DERIVED FROM DX,DY
//                NAMES DUE TO ALG. 526 CACM
  TTRPCOT = record
    X: TRealArray3;
    Y: TRealArray3;
    Z: TRealArray3;
    DX: TRealArray3;
    DY: TRealArray3;
    DX2: TRealArray3;
    DY2: TRealArray3;
    SL: TRealArray3;
    SL2: TRealArray3;
    ZT: TRealArray3_3;
    ZTT: TRealArray3_3;
    ZUV: TRealArray3;
    AP: Real;
    BP: Real;
    CP: Real;
    DP: Real;
  end;

// *** DSPMIN     RESOLUTION OF PLOTTING DEVICE IN USE
//                (MINIMAL DISTANCE OF TWO POINTS TO BE PLOTTED)
// *** CMSCAL     VARIABLE FOR SWITCHING BETWEEN CM AND INCH
//     NPMAX      MAXIMUM NUMBER OF POINTS PER LINE FOR A TRIANGLE
  TTRPCOC = record
    DSPMIN: Real;
    CMSCAL: Real;
    NPMAX: longint;
  end;

{
     /TRPCOF/ CONTAINS VARIABLES WHICH ARE PASSED TO FUNCTION
              TRICPF AS PARAMETERS

     KK          NUMBER OF FUNCTION TO BE EVALUATED BY TRICPF
     KSE         ACTUAL SIDE NUMBER
     XX4F,YY4F   COORDINATES FOR POINT P4F (PRELEMINARY POSITION
                 OF POINT P4)
     SIR,COR     COSINUS OF DIRECTION NORMAL TO CURVE DIRECTION
     CL          ACTUAL SCALED CONTOUR LEVEL
     ContourLevel Unscaled contour level.
}
  TTRPCOF =record
    KK: longint;
    KSE: longint;
    XX4F:Real;
    YY4F:Real;
    SIR:Real;
    COR:Real;
    CL:Real;
    ContourLevel: Real;
    TriangleNumber: longint;
  end;

{
     P0...P5     COEFFICIENTS OF POLYNOMIALS ALONG THE SIDES
     Q0...Q4     COEFFICIENTS OF 1ST DERIVATIVES ALONG THE SIDES
     R0...R3     COEFFICIENTS OF 2ND DERIVATIVES ALONG THE SIDES
     S0...S2     COEFFICIENTS OF 3RD DERIVATIVES ALONG THE SIDES
     T0...T1     COEFFICIENTS OF 4TH DERIVATIVES ALONG THE SIDES
     P11...P41   COEFFICIENTS FOR BIVARIATE POLYNOMIAL INSIDE TRIANGLE
}
  TTRPCOP = record
    P0 : TRealArray3;
    P1 : TRealArray3;
    P2 : TRealArray3;
    P3 : TRealArray3;
    P4 : TRealArray3;
    P5 : TRealArray3;
    Q0 : TRealArray3;
    Q1 : TRealArray3;
    Q2 : TRealArray3;
    Q3 : TRealArray3;
    Q4 : TRealArray3;
    R0 : TRealArray3;
    R1 : TRealArray3;
    R2 : TRealArray3;
    R3 : TRealArray3;
    S0 : TRealArray3;
    S1 : TRealArray3;
    S2 : TRealArray3;
    T0 : TRealArray3;
    T1 : TRealArray3;
    P11: Real;
    P12: Real;
    P13: Real;
    P14: Real;
    P21: Real;
    P22: Real;
    P23: Real;
    P31: Real;
    P32: Real;
    P41: Real;
  end;

var
  TRPCOT: TTRPCOT;
  TRPCOC: TTRPCOC;
  TRPCOF: TTRPCOF;
  TRPCOP: TTRPCOP;

//function FortranSign(A, B: longint): longint; overload;
//begin
//  if B > 0 then
//  begin
//    result := Abs(A);
//  end
//  else
//  begin
//    result := -Abs(A);
//  end;
//end;

// FortranSign replicates the action of the SIGN function in
// the Fortran language.
function FortranSign(A, B: Real): Real; //overload;
begin
  if B > 0 then
  begin
    result := Abs(A);
  end
  else
  begin
    result := -Abs(A);
  end;
end;

procedure TRP002_Pascal;
//
//        SUBROUTINE TRP002
//  C
//  C     T R I ANGLE    C  ONTOUR    P  LOTTING
//  C
//  C     COMPUTATION OF POLYNOMIAL COEFFICIENTS P11...P41 FOR
//  C     BIVARIATE POLYNOMIAL INSIDE TRIANGLE
//  C
//  C     COPYRIGHT(C): A. PREUSSER, 1984
//  C                   SEN. F. WISS. U. FORSCH.
//  C                   PROJEKTGRUPPE PARALLELRECHNEN
//  C                   HEILBRONNER STR. 10
//  C                   D-1000 BERLIN 31
//  C
//  C     FOR EXPLANATION OF VARIABLES IN /TRPCOP/ SEE  TTRPCOP
//  C
//  C     FOR EXPLANATION OF VARIABLES IN /TRPCOT/ SEE  SUBROUTINE TRP00
//  C
//  C
var
  H1: Real;
  H2: Real;
  E1: Real;
  G1: Real;
  G2: Real;
  G3: Real;
begin
  TRPCOP.P14 := TRPCOP.P5[0] *
    (2.5*(TRPCOT.SL2[1]-TRPCOT.SL2[2])/TRPCOT.SL2[0]+2.5);
  TRPCOP.P41 := TRPCOP.P5[1] *
    (2.5*(TRPCOT.SL2[0]-TRPCOT.SL2[2])/TRPCOT.SL2[1]+2.5);
  TRPCOP.P11 := TRPCOT.ZUV[2];
  H1 := TRPCOT.ZT[1,0]-TRPCOP.P1[0]-TRPCOP.P11-TRPCOP.P41;
  H2 :=  TRPCOT.ZUV[0]-TRPCOP.P11-4.0*TRPCOP.P41;
  TRPCOP.P21 :=  3.0*H1-H2;
  TRPCOP.P31 := -2.0*H1+H2;
  H1 :=  TRPCOT.ZT[0,1]-TRPCOP.P1[1]-TRPCOP.P11-TRPCOP.P14;
  H2 :=  TRPCOT.ZUV[1]-TRPCOP.P11-4.0*TRPCOP.P14;
  TRPCOP.P12 :=  3.0*H1-H2;
  TRPCOP.P13 := -2.0*H1+H2;
  H1 :=  0.5*TRPCOT.ZTT[1,0]-TRPCOP.P2[0]-TRPCOP.P12;
  H2 :=  0.5*TRPCOT.ZTT[0,1]-TRPCOP.P2[1]-TRPCOP.P21;
  E1 :=  2.5*(TRPCOT.SL2[1]-TRPCOT.SL2[0])/TRPCOT.SL2[2]+2.5;
  G1 :=  3.-E1;
  G2 := -2.+E1;
  G3 :=  E1*(TRPCOP.P5[0]-TRPCOP.P5[1]+TRPCOP.P41 -TRPCOP.P14)
    +TRPCOP.P14-4.*TRPCOP.P41+5.*TRPCOP.P5[1];
  TRPCOP.P22 :=  G1*H1+G2*H2+G3;
  TRPCOP.P32 :=  H1-TRPCOP.P22;
  TRPCOP.P23 :=  H2-TRPCOP.P22;
end;

procedure IDCLDP_Pascal(const NDP: longint;
  const XD,YD: TRealArray; const NCP: longint; var IPL: TIntArray);
{ SUBROUTINE  IDCLDP
 SELECTS SEVERAL DATA POINTS THAT ARE CLOSEST
 TO EACH OF THE DATA POINT.
 THE INPUT PARAMETERS ARE
     NDP = NUMBER OF DATA POINTS,
     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y
           COORDINATES OF THE DATA POINTS,
     NCP = NUMBER OF DATA POINTS CLOSEST TO EACH DATA
           POINTS.
 THE OUTPUT PARAMETER IS
     IPL = INTEGER ARRAY OF DIMENSION NCP*NDP, WHERE THE
           POINT NUMBERS OF NCP DATA POINTS CLOSEST TO
           EACH OF THE NDP DATA POINTS ARE TO BE STORED.
 THIS SUBROUTINE ARBITRARILY SETS A RESTRICTION THAT NCP MUST
 NOT EXCEED 25.
}
const
  NCPMX = 25;
var
  DSQ0: TRealArray25;
  IPC0: TIntArray25;
  IP1: Integer;
  X1: Real;
  Y1: Real;
  J1: Integer;
  DSQMX: Real;
  IP2: Integer;
  DSQI: Real;
  JMX: Integer;
  IP2MN: Integer;
  DX12: Real;
  DY12: Real;
  J3: Integer;
  GoTo50: boolean;
  GoTo43: boolean;
  IP3: Integer;
  DX13: Real;
  DY13: Real;
  J4: Integer;
  DSQMN: Real;
  IP3MN: Integer;
  J2: Integer;
  IP2Stored: integer;
  PointFound: boolean;
  Sin1Sqr: Extended;
  function DSQF(const U1,V1,U2,V2: Real): Real;
  begin
    result :=Sqr(U2-U1)+Sqr(V2-V1);
  end;
begin
  Sin1Sqr := Sqr(Sin(1/180*Pi));
  JMX := -1;

// PRELIMINARY PROCESSING
  {10}
  IF(NDP<2) or ((NCP<1) OR (NCP>NCPMX) OR (NCP>=NDP)) then
  begin
    raise Exception.Create('IMPROPER INPUT PARAMETER VALUE(S). '
     +'NDP = '+IntToStr(NDP)+'; NCP = '+IntToStr(NCP)
     +#13#10+'ERROR DETECTED IN ROUTINE   IDCLDP');
  end;
// CALCULATION
  for IP1 := 0 to NDP-1 do
  begin
    //-SELECTS NCP POINTS.
    X1 :=XD[IP1];
    Y1 :=YD[IP1];
    J1 :=-1;
    DSQMX :=0.0;
    IP2Stored := -1;
    for IP2 := 0 to NDP-1 do
    begin
      IP2Stored := IP2+1;
      IF(IP2=IP1) then
      begin
        Continue;
      end;
      DSQI :=DSQF(X1,Y1,XD[IP2],YD[IP2]);
      J1 :=J1+1;
      DSQ0[J1]:=DSQI;
      IPC0[J1]:=IP2;
      IF (DSQI>DSQMX)  then
      begin
        DSQMX :=DSQI;
        JMX :=J1;
      end;
      IF (J1+1>=NCP) then
      begin
        break;
      end;
    end;
    IP2MN :=IP2Stored;
    IF(IP2MN<=NDP) then
    begin
      for IP2 := IP2MN to NDP-1 do
      begin
        IF(IP2=IP1) then
        begin
          Continue;
        end;
        DSQI :=DSQF(X1,Y1,XD[IP2],YD[IP2]);
        IF(DSQI>=DSQMX) then
        begin
          Continue;
        end;
        DSQ0[JMX] := DSQI;
        IPC0[JMX] := IP2;
        DSQMX := 0.0;
        for J1 := 0 to NCP-1 do
        begin
          IF(DSQ0[J1]<=DSQMX) then
          begin
            Continue;
          end;
          DSQMX :=DSQ0[J1];
          JMX := J1;
        end;
      end;
    end;
 // CHECKS IF ALL THE NCP+1 POINTS ARE COLLINEAR.
    IP2 :=IPC0[0];
    DX12 := XD[IP2]-X1;
    DY12 := YD[IP2]-Y1;
    GoTo50 := False;
    for J3 := 1 to NCP-1 do
    begin
      IP3 :=IPC0[J3];
      DX13 :=XD[IP3]-X1;
      DY13 :=YD[IP3]-Y1;
      IF(Sqr(DY13*DX12-DX13*DY12)/(DSQ0[0]*DSQ0[J3])
        > Sin1Sqr { 0.06698}) then
      begin
        GoTo50 := True;
        break;
      end;
 // ----- 0.06698 CORRESPONDS TO AN ANGLE OF ABOUT 15. DEGREES(=SIN**2)
    end;
    if not GoTo50 then
    begin
 //-SEARCHES FOR THE CLOSEST NONCOLLINEAR POINT.
      PointFound := False;
      for IP3 := 0 to NDP-1 do
      begin
        IF(IP3=IP1) then
        begin
          Continue;
        end;
        GoTo43 := false;
        for J4 := 0 to NCP-1 do
        begin
          IF(IP3=IPC0[J4]) then
          begin
            GoTo43 := True;
            break;
          end;
        end;
        if GoTo43 then
        begin
          Continue;
        end;
        DX13 := XD[IP3]-X1;
        DY13 := YD[IP3]-Y1;
        DSQI := DSQF(X1,Y1,XD[IP3],YD[IP3]);
        IF(Sqr(DY13*DX12-DX13*DY12)/(DSQ0[0]*DSQI) <= Sin1Sqr {0.06698}) then
        begin
          Continue;
        end;
        IF not PointFound or (DSQI<=DSQMN) then
        begin
          PointFound := true;
          DSQMN :=DSQI;
          IP3MN :=IP3;
        end;
      end;
      IF not PointFound  then
      begin
        raise Exception.Create('ALL COLLINEAR DATA POINTS.'
         +#13#10+'ERROR DETECTED IN ROUTINE   IDCLDP');
      end;
//      DSQMX :=DSQMN;
      IPC0[JMX]:=IP3MN;
    end;
    // REPLACES THE LOCAL ARRAY FOR THE OUTPUT ARRAY.
    J1 :=IP1*NCP-1;
    for J2 := 0 to NCP-1 do
    begin
      J1 :=J1+1;
      IPL[J1] :=IPC0[J2];
    end;
  end;
end;

type
  TSortNode = class(TObject)
    NodeNumber: integer;
    Distance: double;
  end;
  TSortNodeComparer = TComparer<TSortNode>;

procedure IDCLDP_New(const NDP: longint;
  const XD,YD: TRealArray; const NCP, NT: longint; const IPT: TIntArray;
  var IPL: TIntArray);
var
  QuadTree: TRbwQuadTree;
  XMax: Real;
  XMin: Real;
  YMax: Real;
  YMin: Real;
  index: NativeInt;
  PointIndex: Integer;
//  Data: TQuadPointArray;
  DataIndex: Integer;
//  NodeNumber: NativeInt;
//  CandidatePoints: array of NativeInt;
//  CanditatePointLength: integer;
//  AllColinear: Boolean;
//  CandidateIndex: integer;
  Triangle: NativeInt;
  Data2: TPointerArray;
  NodeIndex: Integer;
  SortList: TObjectList<TSortNode>;
  SortObject: TSortNode;
  IntList: TIntegerList;
  Node: Integer;
  X: double;
  Y: double;
  SortIndex: Integer;
  PointsToGet: Integer;
  Data3: TQuadPointArray;
  QuadPointIndex: Integer;
  TriangleList: TIntegerList;
begin
  Assert(NCP=4);
//  CanditatePointLength := NCP;
//  SetLength(CandidatePoints, CanditatePointLength);
  TriangleList := TIntegerList.Create;
  QuadTree := TRbwQuadTree.Create(nil);
  try
    QuadTree.MaxPoints := 5;
    Assert(Length(XD)=Length(YD));
    Assert(Length(XD) >= 5);



    XMax := XD[0];
    XMin := XMax;
    YMax := YD[0];
    YMin := YMax;
    for index := 1 to Length(XD) - 1 do
    begin
      XMax := Max(XMax, XD[index]);
      XMin := Min(XMin, XD[index]);
      YMax := Max(YMax, YD[index]);
      YMin := Min(YMin, YD[index]);
    end;
    QuadTree.XMax := XMax;
    QuadTree.XMin := XMin;
    QuadTree.YMax := YMax;
    QuadTree.YMin := YMin;

    for Index := 0 to NT*3 - 1 do
    begin
      Triangle := index div 3;
      Node := IPT[index];
      QuadTree.AddPoint(XD[Node], YD[Node], Pointer(Triangle));
    end;

    PointIndex := 0;

    for index := 0 to Length(XD) - 1 do
    begin
      X := XD[index];
      Y := YD[index];
      QuadTree.FindClosestPointsData(X, Y, Data2);
      IntList := TIntegerList.Create;
      SortList := TObjectList<TSortNode>.Create;
      try
//      CanditatePointLength := Length(Data2)*2;
//      SetLength(CandidatePoints, CanditatePointLength);
//      CandidateIndex := 0;
        TriangleList.Clear;
        for DataIndex := 0 to Length(Data2) - 1 do
        begin
          Triangle := NativeInt(Data2[DataIndex]);
          if TriangleList.IndexOf(Triangle) >= 0 then
          begin
            Continue;
          end
          else
          begin
            TriangleList.Add(Triangle);
          end;
          for NodeIndex := 0 to 2 do
          begin
            Node := IPT[Triangle*3+NodeIndex];
            if Node <> index then
            begin
              if IntList.IndexOf(Node) < 0 then
              begin
                IntList.Add(Node);
                SortObject := TSortNode.Create;
                SortList.Add(SortObject);
                SortObject.NodeNumber := Node;
                SortObject.Distance := Distance(XD[index], YD[index],
                  XD[Node], YD[Node])
              end;
            end;
          end;
        end;

//        for DataIndex := 0 to CanditatePointLength - 1 do
//        begin
//          if IntList.IndexOf(CandidatePoints[DataIndex]) < 0 then
//          begin
//            IntList.Add(CandidatePoints[DataIndex]);
//            SortObject := TSortNode.Create;
//            SortList.Add(SortObject);
//            SortObject.NodeNumber := CandidatePoints[DataIndex];
//            SortObject.Distance := Distance(XD[index], YD[index],
//              XD[SortObject.NodeNumber], YD[SortObject.NodeNumber])
//          end;
//        end;

        PointsToGet := 0;
        While SortList.Count < NCP do
        begin
          Inc(PointsToGet);
          for SortIndex := 0 to SortList.Count - 1 do
          begin
            SortObject := SortList[SortIndex];
            X := XD[SortObject.NodeNumber];
            Y := YD[SortObject.NodeNumber];
            QuadTree.FindNearestPoints(X, Y, PointsToGet, Data3);
            for QuadPointIndex := 0 to Length(Data3) - 1 do
            begin
              Data2 := Data3[QuadPointIndex].Data;
//            QuadTree.FindClosestPointsData(X, Y, Data2);
//            CanditatePointLength := Length(Data2)*2;
//            SetLength(CandidatePoints, CanditatePointLength);
//            CandidateIndex := 0;
              for DataIndex := 0 to Length(Data2) - 1 do
              begin
                Triangle := NativeInt(Data2[DataIndex]);
                if TriangleList.IndexOf(Triangle) >= 0 then
                begin
                  Continue;
                end
                else
                begin
                  TriangleList.Add(Triangle);
                end;
                for NodeIndex := 0 to 2 do
                begin
                  Node := IPT[Triangle*3+NodeIndex];
                  if Node <> index then
                  begin
                    if IntList.IndexOf(Node) < 0 then
                    begin
                      IntList.Add(Node);
                      SortObject := TSortNode.Create;
                      SortList.Add(SortObject);
                      SortObject.NodeNumber := Node;
                      SortObject.Distance := Distance(XD[index], YD[index],
                        XD[Node], YD[Node])
                    end;
                  end;
                end;
              end;
            end;
          end;

//          for DataIndex := 0 to CanditatePointLength - 1 do
//          begin
//            if IntList.IndexOf(CandidatePoints[DataIndex]) < 0 then
//            begin
//              IntList.Add(CandidatePoints[DataIndex]);
//              SortObject := TSortNode.Create;
//              SortList.Add(SortObject);
//              SortObject.NodeNumber := CandidatePoints[DataIndex];
//              SortObject.Distance := Distance(XD[index], YD[index],
//                XD[SortObject.NodeNumber], YD[SortObject.NodeNumber])
//            end;
//          end;

        end;

        SortList.Sort(TSortNodeComparer.Construct(
          function (const L, R: TSortNode): integer
          begin
            result := Sign(L.Distance - R.Distance);
          end
          )) ;
        for DataIndex := 0 to NCP - 1 do
        begin
          SortObject := SortList[DataIndex];
          IPL[PointIndex] := SortObject.NodeNumber;
          Inc(PointIndex);
        end;
      finally
        SortList.Free;
        IntList.Free;
      end;

//      AllColinear := True;
//      While AllColinear do
//      begin
//        QuadTree.FindNearestPoints(XD[index], YD[index], CanditatePointLength+1, Data);
//        Assert(Length(Data) >= CanditatePointLength+1);
//        CandidateIndex := 0;
//        for DataIndex := 0 to CanditatePointLength do
//        begin
//          NodeNumber := NativeInt(Data[DataIndex].Data[0]);
//          if NodeNumber <> index then
//          begin
//            CandidatePoints[CandidateIndex] := NodeNumber;
//            Inc(CandidateIndex);
//          end;
//        end;
//
//
//
//        for DataIndex := 2 to CanditatePointLength - 1 do
//        begin
//          AllColinear := Collinear(XD[0], YD[0], XD[1], YD[1],
//            XD[DataIndex], YD[DataIndex]);
//          if not AllColinear then
//          begin
//            Break;
//          end;
//        end;
//        if AllColinear then
//        begin
//          if CanditatePointLength = Length(XD) then
//          begin
//            IDCLDP_Pascal(NDP, XD,YD, NCP, IPL);
//            Exit;
//          end;
//          CanditatePointLength := Min(CanditatePointLength*2, Length(XD));
//          SetLength(CandidatePoints, CanditatePointLength);
//        end
//        else
//        begin
//          IPL[PointIndex] := CandidatePoints[0];
//          Inc(PointIndex);
//          IPL[PointIndex] := CandidatePoints[1];
//          Inc(PointIndex);
//          IPL[PointIndex] := CandidatePoints[2];
//          Inc(PointIndex);
//          if Collinear(XD[0], YD[0], XD[1], YD[1], XD[2], YD[2]) then
//          begin
//            for DataIndex := 3 to CanditatePointLength - 1 do
//            begin
//              if not Collinear(XD[0], YD[0], XD[1], YD[1],
//                XD[DataIndex], YD[DataIndex]) then
//              begin
//                IPL[PointIndex] := CandidatePoints[DataIndex];
//                Inc(PointIndex);
//                Break;
//              end;
//            end;
//          end
//          else
//          begin
//            IPL[PointIndex] := CandidatePoints[3];
//            Inc(PointIndex);
//          end;
//        end;
//      end;
    end;
  finally
    QuadTree.Free;
    TriangleList.Free;
  end;
end;

procedure TRP001_Pascal (const IT0: longint; const IPT: TIntArray;
  const PDD: TRealArray);
{
       SUBROUTINE TRP001

     T R I ANGLE    C  ONTOUR    P  LOTTING

     COMPUTATION OF COEFFICIENTS FOR POLYNOMIALS ALONG SIDES

     COPYRIGHT(C): A. PREUSSER, 1984
                   SEN. F. WISS. U. FORSCH.
                   PROJEKTGRUPPE PARALLELRECHNEN
                   HEILBRONNER STR. 10
                   D-1000 BERLIN 31

     IT0     NUMBER OF TRIANGLE IN USE
     IPT     POINT NUMBERS STORED AS 3*I-2, 3*I-1, 3*I TH
             ELEMENT FOR TRIANGLE I
     PDD     PARTIAL DERIVATIVES AT DATA POINTS
             (ZX,ZY,ZXX,ZXY,ZYY)


C     FOR EXPLANATION OF VARIABLES IN /TRPCOT/ SEE SUBROUTINE TRP00

     PD(5,3)  PARTIAL DERIVATIVES IN X-Y DIRECTIONS AT THE THREE VERTICES
}
var
  PD: TRealArray5_3;
  JIPT: Integer;
  K: Integer;
  IDP: Integer;
  JPDD: Integer;
  KPD: Integer;
  AD: Real;
  BC: Real;
  DLT: Real;
  AB: Real;
  ADBC: Real;
  CD: Real;
  DXDY1: Real;
  DXDY2: Real;
  DXDY3: Real;
  JSE: Integer;
  NP1: Integer;
  NP2: Integer;
  IV: Integer;
  H1: Real;
  H2: Real;
  H3: Real;
begin
//     LOADS PARTIAL DERIVATIVES AT THE VERTICES
  JIPT :=3*IT0-1;
  for K := 0 to 2 do
  begin
    JIPT :=JIPT+1;
    IDP :=IPT[JIPT];
    JPDD := 5*IDP-1;
    for KPD := 0 to 4 do
    begin
      JPDD :=JPDD+1;
      PD[KPD,K] :=PDD[JPDD];
    end;
  end;
//C     DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM
//C     TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM
  AD :=  TRPCOT.DX[1]*TRPCOT.DY[0];
  BC :=  TRPCOT.DX[0]*TRPCOT.DY[1];
  DLT := AD-BC;

  if DLT <> 0 then
  begin
    TRPCOT.AP :=  TRPCOT.DY[0]/DLT;
    TRPCOT.BP := -TRPCOT.DX[0]/DLT;
    TRPCOT.CP := -TRPCOT.DY[1]/DLT;
    TRPCOT.DP :=  TRPCOT.DX[1]/DLT;
  end
  else
  begin
    // RBW
    TRPCOT.AP := 0;
    TRPCOT.BP := 0;
    TRPCOT.CP := 0;
    TRPCOT.DP := 0;
  end;
//C     CONVERTS THE PARTIAL DERIVATIVES AT THE VERTEXES OF THE
//C     TRIANGLE FOR THE U-V COORDINATE SYSTEM.
  AB :=  TRPCOT.DX[1]*TRPCOT.DX[0];
  ADBC := AD+BC;
  CD :=  TRPCOT.DY[0]*TRPCOT.DY[1];
  DXDY1 := 2.0*TRPCOT.DX[0]*TRPCOT.DY[0];
  DXDY2 := 2.0*TRPCOT.DX[1]*TRPCOT.DY[1];
  DXDY3 := 2.0*TRPCOT.DX[2]*TRPCOT.DY[2];
  for K := 0 to 2 do
  begin
    TRPCOT.ZT[0,K] := TRPCOT.DX[1]*PD[0,K] +TRPCOT.DY[1]*PD[1,K];
    TRPCOT.ZT[1,K] := TRPCOT.DX[0]*PD[0,K] +TRPCOT.DY[0]*PD[1,K];
    TRPCOT.ZTT[0,K]:= TRPCOT.DX2[1]*PD[2,K]+DXDY2*PD[3,K]+TRPCOT.DY2[1]*PD[4,K];
    TRPCOT.ZUV[K]  :=   AB  *PD[2,K]+ ADBC*PD[3,K]+  CD  *PD[4,K];
    TRPCOT.ZTT[1,K]:= TRPCOT.DX2[0]*PD[2,K]+DXDY1*PD[3,K]+TRPCOT.DY2[0]*PD[4,K];
  end;
  for K := 0 to 1 do
  begin
    TRPCOT.ZT[2,K] := TRPCOT.DX[2]*PD[0,K] +TRPCOT.DY[2]*PD[1,K];
    TRPCOT.ZTT[2,K]:= TRPCOT.DX2[2]*PD[2,K]+DXDY3*PD[3,K]+TRPCOT.DY2[2]*PD[4,K];
  end;
//C     CALCULATES THE COEFFICIENTS OF THE POLYNOMIALS ALONG
//C     THE THREE SIDES OF THE TRIANGLE
  for JSE := 0 to 2 do
  begin
    NP1 := 2;
    NP2 := 1;
    IF (JSE=2) then
    begin
      NP1 := 0;
    end;
    IF (JSE=1) then
    begin
      NP2:= 0;
    end;
    IV := NP2;
    IF (JSE=2) then
    begin
      IV:= 2;
    end;
    TRPCOP.P0[JSE] := TRPCOT.Z[NP1];
    TRPCOP.P1[JSE] := TRPCOT.ZT[IV,NP1];
    TRPCOP.P2[JSE] := 0.5*TRPCOT.ZTT[IV,NP1];
    H1 := TRPCOT.Z[NP2]-TRPCOP.P0[JSE]-TRPCOP.P1[JSE]-TRPCOP.P2[JSE];
    H2 := TRPCOT.ZT[IV,NP2]-TRPCOP.P1[JSE]-TRPCOT.ZTT[IV,NP1];
    H3 := TRPCOT.ZTT[IV,NP2]-TRPCOT.ZTT[IV,NP1];
    TRPCOP.P3[JSE] := 10.0*H1-4.0*H2+0.5*H3;
    TRPCOP.P4[JSE] :=-15.0*H1+7.0*H2    -H3;
    TRPCOP.P5[JSE] :=  6.0*H1-3.0*H2+0.5*H3;
//C       CALCULATES COEFFICIENTS FOR DERIVATIVES ALONG SIDES
    TRPCOP.Q0[JSE] :=     TRPCOP.P1[JSE];
    TRPCOP.Q1[JSE] := 2. *TRPCOP.P2[JSE];
    TRPCOP.Q2[JSE] := 3. *TRPCOP.P3[JSE];
    TRPCOP.Q3[JSE] := 4. *TRPCOP.P4[JSE];
    TRPCOP.Q4[JSE] := 5. *TRPCOP.P5[JSE];
    TRPCOP.R0[JSE] :=     TRPCOP.Q1[JSE];
    TRPCOP.R1[JSE] := 2. *TRPCOP.Q2[JSE];
    TRPCOP.R2[JSE] := 3. *TRPCOP.Q3[JSE];
    TRPCOP.R3[JSE] := 4. *TRPCOP.Q4[JSE];
    TRPCOP.S0[JSE] :=     TRPCOP.R1[JSE];
    TRPCOP.S1[JSE] := 2. *TRPCOP.R2[JSE];
    TRPCOP.S2[JSE] := 3. *TRPCOP.R3[JSE];
    TRPCOP.T0[JSE] :=     TRPCOP.S1[JSE];
    TRPCOP.T1[JSE] := 2. *TRPCOP.S2[JSE];
  end;
end;

procedure InitializeTTRPCOP;
var
  Index: longint;
begin
  for Index := 0 to 2 do
  begin
    TRPCOP.P0[Index] := 0;
    TRPCOP.P1[Index] := 0;
    TRPCOP.P2[Index] := 0;
    TRPCOP.P3[Index] := 0;
    TRPCOP.P4[Index] := 0;
    TRPCOP.P5[Index] := 0;
    TRPCOP.Q0[Index] := 0;
    TRPCOP.Q1[Index] := 0;
    TRPCOP.Q2[Index] := 0;
    TRPCOP.Q3[Index] := 0;
    TRPCOP.Q4[Index] := 0;
    TRPCOP.R0[Index] := 0;
    TRPCOP.R1[Index] := 0;
    TRPCOP.R2[Index] := 0;
    TRPCOP.R3[Index] := 0;
    TRPCOP.S0[Index] := 0;
    TRPCOP.S1[Index] := 0;
    TRPCOP.S2[Index] := 0;
    TRPCOP.T0[Index] := 0;
    TRPCOP.T1[Index] := 0;
  end;
  TRPCOP.P11 := 0;
  TRPCOP.P12 := 0;
  TRPCOP.P13 := 0;
  TRPCOP.P14 := 0;
  TRPCOP.P21 := 0;
  TRPCOP.P22 := 0;
  TRPCOP.P23 := 0;
  TRPCOP.P31 := 0;
  TRPCOP.P32 := 0;
  TRPCOP.P41 := 0;
end;

function TRICPF_Pascal(const T: Real): Real;
//      FUNCTION TRICPF(T)
//
//     T R I ANGLE    C  ONTOUR    P  LOTTING
//
//     FUNCTION EVALUATION
//
//     COPYRIGHT(C): A. PREUSSER, 1984
//                   SEN. F. WISS. U. FORSCH.
//                   PROJEKTGRUPPE PARALLELRECHNEN
//                   HEILBRONNER STR. 10
//                   D-1000 BERLIN 31
//
//     VARIABLES IN /TRPCOF/ ARE USED AS ARGUMENTS
//     FOR AN EXPLANATION SEE SUBROUTINE TRP00
//
//     KK      NUMBER OF FUNCTION TO BE EVALUATED
//     KK=1    4TH DERIVATIVE ALONG SIDE KSE
//     KK=2    3RD DERIVATIVE ALONG SIDE KSE
//     KK=3    2ND DERIVATIVE ALONG SIDE KSE
//     KK=4    1ST DERIVATIVE ALONG SIDE KSE
//     KK=5    ORIGINAL POLYNOMIAL ALONG SIDE KSE
//     KK=6    BIVARIATE POLYNOMIAL INSIDE TRIANGLE
//
//     FOR EXPLANATION OF VARIABLES IN /TRPCOP/ SEE SUBROUTINE TRP001
//
//     FOR AN EXPLANATION OF VARIABLES IN /TRPCOT/ SEE SUBROUTINE TRP00
//
var
  XX4: Real;
  YY4: Real;
  U: Real;
  V: Real;
  H0: Real;
  H1: Real;
  H2: Real;
  H3: Real;
  H4: Real;
begin
  result := 0;
  case TRPCOF.KK of
    1:
      begin
        result := TRPCOP.T0[TRPCOF.KSE]+T*TRPCOP.T1[TRPCOF.KSE];
      end;
    2:
      begin
        result := TRPCOP.S0[TRPCOF.KSE]
         +T*(TRPCOP.S1[TRPCOF.KSE]
          +T*TRPCOP.S2[TRPCOF.KSE]);
      end;
    3:
      begin
        result := TRPCOP.R0[TRPCOF.KSE]
         +T*(TRPCOP.R1[TRPCOF.KSE]
          +T*(TRPCOP.R2[TRPCOF.KSE]
          +T*TRPCOP.R3[TRPCOF.KSE]));
      end;
    4:
      begin
        result := TRPCOP.Q0[TRPCOF.KSE]
          +T*(TRPCOP.Q1[TRPCOF.KSE]
          +T*(TRPCOP.Q2[TRPCOF.KSE]
          +T*(TRPCOP.Q3[TRPCOF.KSE]
          +T*TRPCOP.Q4[TRPCOF.KSE])));
      end;
    5:
      begin
        result := TRPCOP.P0[TRPCOF.KSE]
          +T*(TRPCOP.P1[TRPCOF.KSE]
          +T*(TRPCOP.P2[TRPCOF.KSE]
          +T*(TRPCOP.P3[TRPCOF.KSE]
          +T*(TRPCOP.P4[TRPCOF.KSE]
          +T*TRPCOP.P5[TRPCOF.KSE]))))-TRPCOF.CL;
      end;
    6:
      begin
        XX4 := TRPCOF.XX4F+TRPCOF.COR*T;
        YY4 := TRPCOF.YY4F+TRPCOF.SIR*T;
        U := TRPCOT.AP*XX4+TRPCOT.BP*YY4;
        V := TRPCOT.CP*XX4+TRPCOT.DP*YY4;
        H0 :=TRPCOP.P0[0]
          +V*(TRPCOP.P1[0]
          +V*(TRPCOP.P2[0]
          +V*(TRPCOP.P3[0]
          +V*(TRPCOP.P4[0]
          +V*TRPCOP.P5[0]))));
        H1 :=TRPCOP.P1[1]
          +V*(TRPCOP.P11
          +V*(TRPCOP.P12
          +V*(TRPCOP.P13
          +V*TRPCOP.P14)));
        H2 :=TRPCOP.P2[1]
          +V*(TRPCOP.P21
          +V*(TRPCOP.P22
          +V*TRPCOP.P23));
        H3 :=TRPCOP.P3[1]
          +V*(TRPCOP.P31
          +V*TRPCOP.P32);
        H4 :=TRPCOP.P4[1]
          +V*TRPCOP.P41;
        result :=H0+U*(H1+U*(H2+U*(H3+U*(H4+U*TRPCOP.P5[1]))))-TRPCOF.CL;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure InitializeTRPCOF;
begin
  TRPCOF.KK := 0;
  TRPCOF.KSE := -1;
  TRPCOF.XX4F := 0;
  TRPCOF.YY4F := 0;
  TRPCOF.SIR := 0;
  TRPCOF.COR := 0;
  TRPCOF.CL := 0;
  TRPCOF.ContourLevel := 0;
end;

procedure InitializeTrpcot;
var
  Index1, Index2: longint;
begin
  for Index1 := 0 to 2 do
  begin
    TRPCOT.X[Index1] := 0;
    TRPCOT.Y[Index1] := 0;
    TRPCOT.Z[Index1] := 0;
    TRPCOT.DX[Index1] := 0;
    TRPCOT.DY[Index1] := 0;
    TRPCOT.DX2[Index1] := 0;
    TRPCOT.DY2[Index1] := 0;
    TRPCOT.SL[Index1] := 0;
    TRPCOT.SL2[Index1] := 0;
    TRPCOT.ZUV[Index1] := 0;
    for Index2 := 0 to 2 do
    begin
      TRPCOT.ZT[Index1,Index2] := 0;
      TRPCOT.ZTT[Index1,Index2] := 0;
    end;
  end;
  TRPCOT.AP := 0;
  TRPCOT.BP := 0;
  TRPCOT.CP := 0;
  TRPCOT.DP := 0;
end;

procedure IDPDRV_Pascal(const NDP: longint; const XD,YD,ZD: TRealArray;
  const NCP: longint; const IPL: TIntArray; var PD: TRealArray);
// SUBROUTINE  IDPDRV
// ESTIMATES PARTIAL DERIVATIVES OF THE FIRST AND
// SECOND ORDER AT THE DATA POINTS.
// THE INPUT PARAMETERS ARE
//     NDP = NUMBER OF DATA POINTS,
//     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X,
//           Y, AND Z COORDINATES OF THE DATA POINTS,
//     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI-
//           MATING PARTIAL DERIVATIVES AT EACH DATA POINT,
//     IPL = INTEGER ARRAY OF DIMENSION NCP*NDP CONTAINING
//           THE POINT NUMBERS OF NCP DATA POINTS CLOSEST TO
//           EACH OF THE NDP DATA POINTS.
// THE OUTPUT PARAMETER IS
//     PD  = ARRAY OF DIMENSION 5*NDP, WHERE THE ESTIMATED
//           ZX, ZY, ZXX, ZXY, AND ZYY VALUES AT THE DATA
//           POINTS ARE TO BE STORED.
var
  NMX: Real;
  NMY: Real;
  NMZ: Real;
  NMXX: Real;
  NMXY: Real;
  NMYX: Real;
  NMYY: Real;
  NDP0: Integer;
  NCP0: Integer;
  NCPM1: Integer;
  IP0: Integer;
  X0: Real;
  Y0: Real;
  Z0: Real;
  JIPC0: Integer;
  IC1: Integer;
  JIPC: Integer;
  IPI: Integer;
  DX1: Real;
  DY1: Real;
  DZ1: Real;
  IC2MN: Integer;
  IC2: Integer;
  DX2: Real;
  DY2: Real;
  DNMZ: Real;
  DZ2: Real;
  DNMX: Real;
  DNMY: Real;
  JPD0: Integer;
  ZX0: Real;
  ZY0: Real;
  JPD: Integer;
  DZX1: Real;
  DZY1: Real;
  DZX2: Real;
  DZY2: Real;
  DNMXX: Real;
  DNMXY: Real;
  DNMYX: Real;
  DNMYY: Real;
begin
//C PRELIMINARY PROCESSING
  {10}
  NDP0:=NDP;
  NCP0 :=NCP;
  NCPM1 :=NCP0-1;
//C ESTIMATION OF ZX AND ZY
  for IP0 := 0 to NDP0-1 do
  begin
    X0 :=XD[IP0];
    Y0 :=YD[IP0];
    Z0 :=ZD[IP0];
    NMX :=0.0;
    NMY :=0.0;
    NMZ :=0.0;
    JIPC0 :=NCP0*IP0-1;
    for IC1 := 1 to NCPM1 do
    begin
      JIPC :=JIPC0+IC1;
      IPI :=IPL[JIPC];
      DX1 :=XD[IPI]-X0;
      DY1 :=YD[IPI]-Y0;
      DZ1 :=ZD[IPI]-Z0;
      IC2MN :=IC1+1;
      for IC2 := IC2MN to NCP0 do
      begin
        JIPC :=JIPC0+IC2;
        IPI :=IPL[JIPC];
        DX2 :=XD[IPI]-X0;
        DY2 :=YD[IPI]-Y0;
        DNMZ :=DX1*DY2-DY1*DX2;
        IF(DNMZ=0.0) then
        begin
          Continue;
        end;
        DZ2 :=ZD[IPI]-Z0;
        DNMX :=DY1*DZ2-DZ1*DY2;
        DNMY :=DZ1*DX2-DX1*DZ2;
        IF(DNMZ<0.0) then
        begin
          DNMX :=-DNMX;
          DNMY :=-DNMY;
          DNMZ :=-DNMZ;
        end;
        NMX :=NMX+DNMX;
        NMY :=NMY+DNMY;
        NMZ :=NMZ+DNMZ;
      end;
    end;
    JPD0 :=5*IP0;
//    if NMZ = 0 then
//    begin
//      ShowMessage(IntToStr(IP0));
//    end;
    if NMX = 0 then
    begin
      PD[JPD0] := 0;
    end
    else
    begin
      PD[JPD0] :=-NMX/NMZ;
    end;
    if NMY = 0 then
    begin
      PD[JPD0+1] := 0;
    end
    else
    begin
      PD[JPD0+1] :=-NMY/NMZ;
    end;
 end;
 for IP0 := 0 to NDP0-1 do
 begin
//C ESTIMATION OF ZXX, ZXY, AND ZYY
    JPD0 :=JPD0+5;
    X0 :=XD[IP0];
    JPD0 :=5*IP0;
    Y0 :=YD[IP0];
    ZX0 :=PD[JPD0];
    ZY0 :=PD[JPD0+1];
    NMXX :=0.0;
    NMXY :=0.0;
    NMYX :=0.0;
    NMYY :=0.0;
    NMZ :=0.0;
    JIPC0 :=NCP0*(IP0);
    for IC1 := 0 to NCPM1-1 do
    begin
      JIPC :=JIPC0+IC1;
      IPI :=IPL[JIPC];
      DX1 :=XD[IPI]-X0;
      DY1 :=YD[IPI]-Y0;
      JPD :=5*IPI;
      DZX1 :=PD[JPD]-ZX0;
      DZY1 :=PD[JPD+1]-ZY0;
      IC2MN :=IC1+1+1;
      for IC2 := IC2MN-1 to NCP0-1 do
      begin
        JIPC :=JIPC0+IC2;
        IPI :=IPL[JIPC];
        DX2 :=XD[IPI]-X0;
        DY2 :=YD[IPI]-Y0;
        DNMZ :=DX1*DY2 -DY1*DX2;
        IF(DNMZ=0.0) then
        begin
          Continue;
        end;
        JPD :=5*IPI;
        DZX2 :=PD[JPD]-ZX0;
        DZY2 :=PD[JPD+1]-ZY0;
        DNMXX :=DY1*DZX2-DZX1*DY2;
        DNMXY :=DZX1*DX2-DX1*DZX2;
        DNMYX :=DY1*DZY2-DZY1*DY2;
        DNMYY :=DZY1*DX2-DX1*DZY2;
        IF(DNMZ<0.0) then
        begin
          DNMXX :=-DNMXX;
          DNMXY :=-DNMXY;
          DNMYX :=-DNMYX;
          DNMYY :=-DNMYY;
          DNMZ  :=-DNMZ;
        end;
        NMXX :=NMXX+DNMXX;
        NMXY :=NMXY+DNMXY;
        NMYX :=NMYX+DNMYX;
        NMYY :=NMYY+DNMYY;
        NMZ  :=NMZ +DNMZ;
      end;
    end;
    if NMXX = 0 then
    begin
      PD[JPD0+2] := 0;
    end
    else
    begin
      PD[JPD0+2] :=-NMXX/NMZ;
    end;

    if (NMXY+NMYX) = 0 then
    begin
      PD[JPD0+3] := 0
    end
    else
    begin
      PD[JPD0+3] :=-(NMXY+NMYX)/(2.0*NMZ);
    end;

    if NMYY = 0 then
    begin
      PD[JPD0+4]  := 0;
    end
    else
    begin
      PD[JPD0+4]  :=-NMYY/NMZ;
    end;
  end;
end;


procedure BasicTriangleGeometry(out SI, CO: TRealArray3; const ZS: Real);
var
  NP2: longint;
  NP1: longint;
  J: longint;
begin
  //     SOME BASIC GEOMETRY FOR THE TRIANGLE
  for J := 0 to 2 do
  begin
    TRPCOT.Z[J] := TRPCOT.Z[J]-ZS;
    NP1 := 2;
    NP2 := 1;
    if (J = 2) then
    begin
      NP1 := 0;
    end;
    if (J = 1) then
    begin
      NP2 := 0;
    end;
    TRPCOT.DX[J] := TRPCOT.X[NP2]-TRPCOT.X[NP1];
    TRPCOT.DY[J] := TRPCOT.Y[NP2]-TRPCOT.Y[NP1];
    TRPCOT.DX2[J] := TRPCOT.DX[J] * TRPCOT.DX[J];
    TRPCOT.DY2[J] := TRPCOT.DY[J] * TRPCOT.DY[J];
    TRPCOT.SL2[J] := TRPCOT.DX2[J]+TRPCOT.DY2[J];
    TRPCOT.SL[J] := SQRT(TRPCOT.SL2[J]);
    if TRPCOT.SL[J] <> 0 then
    begin
      CO[J] := TRPCOT.DX[J] / TRPCOT.SL[J];
      SI[J] := TRPCOT.DY[J] / TRPCOT.SL[J];
    end
    else
    begin
      CO[J] := 0;
      SI[J] := 0;
    end;
  end;
  CO[0] := -CO[0];
  SI[0] := -SI[0];
end;

procedure MoreTriangleGeometry(const SI, CO: TRealArray3;
  out SLMAX, HMIN: Real);
var
  DI1: Real;
  DI2: Real;
  DI3: Real;
begin
  SLMAX := Max(TRPCOT.SL[0], Max(TRPCOT.SL[1], TRPCOT.SL[2]));
  DI1 := ABS(TRPCOT.DY[2] * CO[0]-TRPCOT.DX[2] * SI[0]);
  DI2 := ABS(TRPCOT.DY[0] * CO[1]-TRPCOT.DX[0] * SI[1]);
  DI3 := ABS(TRPCOT.DY[1] * CO[2]-TRPCOT.DX[1] * SI[2]);
  HMIN := Min(DI1, Min(DI2, DI3));
//C         = SHORTEST DISTANCE BETWEEN A VERTEX AND
//C           ITS OPPOSITE SIDE (MINIMUM HEIGHT)
//C
//C *** ISSUE A WARNING MESSAGE HERE, IF HMIN IS LESS THAN,
//C     SAY 0.01/
end;

procedure DefineConstants(const SLMAX, HMIN: Real; var RMAX, RMIN, POSERR,
  DSMAX, FSTEP, EPS, RMAX2: Real);
const
  EPSFactor: Real = 0.01;
begin
  //
  //    DEFINE CONSTANTS DEPENDING ON HMIN

  RMAX := Min(0.02 / TRPCOC.CMSCAL, HMIN * 0.01);
  //           = DISTANCE NORMAL TO CURVE DIRECTION WITHIN WHICH
  //             A ZERO MUST BE FOUND

  RMIN := RMAX * 0.2;
  //             IF ZERO HAS BEEN FOUND WITHIN A DISTANCE SMALLER
  //             THAN RMIN, THEN STEPSIZE DS IS MULTIPLIED BY 2.

  POSERR := Min(0.001 / TRPCOC.CMSCAL, 0.001 * HMIN * HMIN / SLMAX);
  //           = PERMITTED POSITION ERROR

  DSMAX := HMIN * 0.2;
  //           = MAXIMUM STEP SIZE

  FSTEP := RMAX * 10;
  //           = STARTING STEP SIZE

  EPS := HMIN * EPSFactor;
  //           = DIFFERENCE FOR ESTIMATING STARTING DIRECTION

  RMAX2 := RMAX * 2;
  //           = DISTANCE NORMAL TO CURVE DIRECTION. A ZERO MUST BE
  //             FOUND WITHIN WHEN CROSSING TRIANGLE BORDER.
end;

FUNCTION TRICPZ_Pascal (const TA,TB,F1,F2,ER: Real): Real;
//      FUNCTION TRICPZ (TA,TB,F1,F2,ER)
//
//     T R I ANGLE    C  ONTOUR    P  LOTTING
//
//     COMPUTE ZERO BETWEEN TA AND TB
//
//     F1= FUNCTION VALUE AT TA
//     F2= FUNCTION VALUE AT TB
//         F1 AND F2 MUST HAVE OPPOSITE SIGN
//         THIS MUST BE CHECKED BEFORE ENTRY
//     ER= PERMITTED ERROR FOR SOLUTION TRICPZ
//     NAME OF FUNCTION = TRICPF
//
//     THE METHOD IS A COMBINATION OF THE REGULA FALSI
//     AND THE MIDPOINT METHOD
//
//     IT IS A MODIFIED VERSION OF THE VIM- (CONTROL DATA
//     USER GROUP) ROUTINE WITH CATALOG IDENTIFICATION
//                C2BKYZERO
//     WRITTEN BY LOREN P. MEISSNER, 1965
//
var
  A: Real;
  B: Real;
  FA: Real;
  FB: Real;
  C: Real;
  FC: Real;
  S: Real;
  FS: Real;
  H: Real;
  Y: Real;
  FY: Real;
  G: Real;
  FG: Real;
  E: Real;
begin
  A :=TA;
  B :=TB;
  FA :=F1;
  FB :=F2;
  C :=A;
  FC :=FA;
  S :=C;
  FS :=FC;
  H := 0;
  while True do
  begin
    H :=0.5*(B+C);
    IF(ABS(H-B) <ER) then
    begin
      Break;
    end;
    IF (ABS(FB) > ABS(FC)) then
    begin
      Y := B;
      FY := FB;
      G := B;
      FG := FB;
      S := C;
      FS := FC;
    end
    else
    begin
      Y := S;
      FY := FS;
      G := C;
      FG := FC;
      S := B;
      FS := FB;
    end;
    IF (FY = FS) then
    begin
      B :=H;
    end
    else
    begin
      E :=(S*FY-Y*FS)/(FY-FS);
      IF (ABS(E-S) <= ER) then
      begin
        E :=S+FortranSign(ER,G-S);
      end;
      IF ((E-H)*(S-E) >= 0.0) then
      begin
        B:=E;
      end
      else
      begin
        B :=H;
      end;
    end;
//C
//C *** FUNCTION CALL
    FB := TRICPF_Pascal(B);

    IF (FG*FB >= 0.0) then
    begin
      C :=S;
      FC :=FS;
      Continue;
    end;
    C :=G;
    FC :=FG;
  end;
  result := H;
end;

procedure ComputeEndpointsOfIntervals(out I: longint; var TS1: TRealArray6_3;
  const TPER: TRealArray3);
var
  F1: Real;
  F2: Real;
  K: longint;
  J: longint;
  II: longint;
  TS2: TRealArray6;
  TA: Real;
  TB: Real;
begin
  //C
  //C       COMPUTE ENDPOINTS OF INTERVALS
  TS1[0, TRPCOF.KSE] := 0;
  TS1[1, TRPCOF.KSE] := 1;
  I := 1;
  for K := 0 to 3 do
  begin
    TRPCOF.KK := K+1;
    TS2[0] := 0;
    II := 1;
    TB := 0;
    F2 := TRICPF_Pascal(TB);
    for J := 1 to I do
    begin
      TA := TB;
      F1 := F2;
      TB := TS1[J, TRPCOF.KSE];
      F2 := TRICPF_Pascal(TB);
      if ((F1 * F2) > 0) then
      begin
        Continue;
      end;
      if ((F1 = 0) and (F2 = 0)) then
      begin
        Continue;
      end;
      TS2[II] := TRICPZ_Pascal(TA, TB, F1, F2, TPER[TRPCOF.KSE]);
      II := II+1;
    end;

    TS2[II] := 1;
    for J := 0 to II do
    begin
      TS1[J, TRPCOF.KSE] := TS2[J];
    end;
    I := II;
  end;
end;

procedure ComputeMaximaAndMinima(const TS1: TRealArray6_3;
  var Z1: TRealArray6_3; var ZMIN, ZMAX: TRealArray3;
  const I: longint);
var
  J: longint;
  NP2: longint;
  NP1: longint;
begin
  //       COMPUTE MAXIMA AND MINIMA FOR EACH SIDE
  NP1 := 2;
  NP2 := 1;
  if (TRPCOF.KSE+1 = 3) then
  begin
    NP1 := 0;
  end;
  if (TRPCOF.KSE+1 = 2) then
  begin
    NP2 := 0;
  end;
  ZMAX[TRPCOF.KSE] := Max(TRPCOT.Z[NP1], TRPCOT.Z[NP2]);
  ZMIN[TRPCOF.KSE] := Min(TRPCOT.Z[NP1], TRPCOT.Z[NP2]);
  Z1[0, TRPCOF.KSE] := TRPCOT.Z[NP1];
  Z1[I, TRPCOF.KSE] := TRPCOT.Z[NP2];
  if (I <> 1) then
  begin
    TRPCOF.KK := 5;
    for J := 1 to I-1 do
    begin
      Z1[J, TRPCOF.KSE] := TRICPF_Pascal(TS1[J, TRPCOF.KSE]);
      if (Z1[J, TRPCOF.KSE] > ZMAX[TRPCOF.KSE]) then
      begin
        ZMAX[TRPCOF.KSE] := Z1[J, TRPCOF.KSE];
      end;
      if (Z1[J, TRPCOF.KSE] < ZMIN[TRPCOF.KSE]) then
      begin
        ZMIN[TRPCOF.KSE] := Z1[J, TRPCOF.KSE];
      end;
    end;
  end;
end;

procedure ComputeZeros(const TS1: TRealArray6_3;
  var TZR: TRealArray5_3; const Z1: TRealArray6_3;
  var NZ: TIntArray3; const ZMAX, ZMIN: TRealArray3;
  const IN_Array: TIntArray3; const TPER: TRealArray3);
var
  JSE: longint;
  J: longint;
  Pen: longint;
  F1F2: Real;
  NI: longint;
  JN: longint;
  NP1: longint;
  NP2: longint;
  F1, F2: Real;
  Procedure UpdateTZR;
  begin
        JN := JN+1;
        TZR[JN, TRPCOF.KSE] :=
          TRICPZ_Pascal(TS1[J, TRPCOF.KSE],
          TS1[J+1, TRPCOF.KSE], F1, F2, TPER[TRPCOF.KSE]);
  end;
begin
 // COMPUTE ZEROS (IF ANY) ON THE THREE SIDES FOR LEVEL CL
 TRPCOF.KK := 5;
  for JSE := 0 to 2 do
  begin
    NZ[JSE] := 0;
    if ((TRPCOF.CL < ZMIN[JSE]) or (TRPCOF.CL > ZMAX[JSE])) then
    begin
      continue;
    end;
    TRPCOF.KSE := JSE;
    JN := -1;
    NI := IN_Array[TRPCOF.KSE];
    // CHECK INTERVALS FOR ZEROS
    for J := 0 to NI-1 do
    begin
      F1 := Z1[J, TRPCOF.KSE]-TRPCOF.CL;
      F2 := Z1[J+1, TRPCOF.KSE]-TRPCOF.CL;
      F1F2 := F1 * F2;
      if (F1F2 > 0) then
      begin
        Continue;
      end;
      if (F1F2 < 0) then
      begin
        UpdateTZR;
        Continue;
      end;
      //  SPECIAL SITUATIONS
      if ((NI = 1) and (F1 = 0) and (F2 = 0)) then
      begin
        // CONTOURLINE = SIDE KSE
        NP1 := 2;
        NP2 := 1;
        if (TRPCOF.KSE+1 = 3) then
        begin
          NP1 := 0;
        end;
        if (TRPCOF.KSE+1 = 2) then
        begin
          NP2 := 0;
        end;
        Pen := 3;
        PLOT(TRPCOT.X[NP1], TRPCOT.Y[NP1], Pen);
        SpecifyContourLevel(TRPCOF.ContourLevel, TRPCOF.TriangleNumber, TRPCOF.KSE);
        Pen := 2;
        PLOT(TRPCOT.X[NP2], TRPCOT.Y[NP2], Pen);
        Continue;
      end
      else
      if (((J = 0) and (F1 = 0)) or ((J = NI-1) and (F2 = 0))) then
      begin
        //  LINE PASSES THROUGH DATA POINT
        //  ONLY ONE ZERO FOR THIS TRIANGLE, SKIP EITHER SIDE
        if ((TRPCOF.KSE+1 = 3) or ((F1 = 0) and (TRPCOF.KSE+1 = 2))) then
        begin
          continue;
        end;
        JN := JN+1;
        if (F2 = 0) then
        begin
          TZR[JN, TRPCOF.KSE] := 1-TPER[TRPCOF.KSE] * 0.5;
        end;
        if (F1 = 0) then
        begin
          TZR[JN, TRPCOF.KSE] := TPER[TRPCOF.KSE] * 0.5;
        end;
        Continue;
      end
      else
      begin
        UpdateTZR;
      end;
    end;
    NZ[TRPCOF.KSE] := JN+1;
  end;
end;

procedure ComputeX0andY0(
  var Y0, X0: TRealArray5_3; const TZR: TRealArray5_3;
  const NZ: TIntArray3);
var
  JSE: longint;
  JZR: longint;
  JN: longint;
  T: Real;
begin
  // COMPUTE X0,Y0 FOR EACH ZERO (RELATIVE TO X(3),Y(3))
  for JSE := 0 to 2 do
  begin
    JN := NZ[JSE];
    if (JN <> 0) then
    begin
      for JZR := 0 to JN-1 do
      begin
        T := TZR[JZR, JSE];
        if (JSE <> 2) then
        begin
          X0[JZR, JSE] := TRPCOT.DX[JSE] * T;
          Y0[JZR, JSE] := TRPCOT.DY[JSE] * T;
        end
        else
        begin
          X0[JZR, JSE] := TRPCOT.DX[1]+TRPCOT.DX[2] * T;
          Y0[JZR, JSE] := TRPCOT.DY[1]+TRPCOT.DY[2] * T;
        end;
      end;
    end;
  end;
end;

procedure ComputeTriangleFactors(var DT, THETAS: TRealArray3;
  var KC: longint; const EPS: Real);
begin
  //C
  KC := KC+1;
  if (KC = 1) then
  begin
    // COMPUTE DIRECTION OF SIDES
    THETAS[0] := ArcTAN2(-TRPCOT.DY[0], -TRPCOT.DX[0]);
    THETAS[1] := ArcTAN2(TRPCOT.DY[1], TRPCOT.DX[1]);
    THETAS[2] := ArcTAN2(TRPCOT.DY[2], TRPCOT.DX[2]);
    // COMPUTE DIFFERENCES FOR ESTIMATING START DIRECTION
    DT[0] := -EPS / TRPCOT.SL[0];
    DT[1] := EPS / TRPCOT.SL[1];
    DT[2] := EPS / TRPCOT.SL[2];
    // COMPUTE COEFFICIENTS FOR POLYNOMIAL INSIDE TRIANGLE
    TRP002_Pascal;
  end;
end;

procedure ComputeF1andF2(const Y0, X0, TZR: TRealArray5_3;
  const JZR: longint; out FD1, F1, F2: Real; const EPS: Real;
  const DT, CO, SI: TRealArray3);
begin
  // COMPUTE F1 ON SIDE USING FIRST DERIVATIVE FD1
  TRPCOF.KK := 4;
  FD1 := TRICPF_Pascal(TZR[JZR, TRPCOF.KSE]);
  F1 := FD1 * DT[TRPCOF.KSE];

  // COMPUTE F2 NORMAL TO SIDE
  TRPCOF.KK := 6;
  TRPCOF.XX4F := X0[JZR, TRPCOF.KSE];
  TRPCOF.YY4F := Y0[JZR, TRPCOF.KSE];
  TRPCOF.SIR := CO[TRPCOF.KSE];
  TRPCOF.COR := -SI[TRPCOF.KSE];
  F2 := TRICPF_Pascal(EPS);
end;

procedure PlotLastPointAndUpdateQueue(
  var XX1, YY1, XX2, YY2, XX3, YY3, DS, DS01, DS12, DS23, DSP: Real;
  const R, DS34, XX4, YY4, RMIN, DSMAX: Real;
  var NPQ: longint);
var
  Pen: longint;
  PlotX, PlotY : Real;
begin
  // POINT IS INSIDE
  // PLOT LAST POINT OF QUEUE AND UPDATE QUEUE
  if (NPQ = 4) then
  begin
    //      C ***       INSERT CALL FOR LABELING ROUTINE HERE
    //      C ***       USING XX1...XX4, YY1...YY4, DS12...DS34 AS PARAMETERS
    //      C ***       AND REDUCE NPQ BY THE NUMBER OF POINTS WHICH HAVE BEEN
    //      C ***       USED FROM THE QUEUE FOR LABELING
    NPQ := NPQ-1;
    DSP := DSP+DS01;
    if (DSP >= TRPCOC.DSPMIN) then
    begin
      Pen := 2;
      PlotX := XX1+TRPCOT.X[2];
      PlotY := YY1+TRPCOT.Y[2];
      PLOT(PlotX, PlotY, Pen);
      DSP := 0;
    end;
  end;

  DS01 := DS12;
  DS12 := DS23;
  DS23 := DS34;
  XX1 := XX2;
  YY1 := YY2;
  XX2 := XX3;
  YY2 := YY3;
  XX3 := XX4;
  YY3 := YY4;
  //  SET NEW STEP SIZE
  if (ABS(R) < RMIN) then
  begin
    DS := Min(DSMAX, DS * 2);
  end;
end;

procedure SearchForCorrespondingZero(const JZR: longint;
  var KE, J1: longint;
  var FlagZeroFound: Boolean;
  const RMAX2, DYDS, DXDS, YY3, XX3: Real;
  const Y0, X0: TRealArray5_3; var TZR: TRealArray5_3;
  const NZ: TIntArray3); 
var
  N: longint;
  JESE: longint;
  J: longint;
  DIST: Real;
  CHECK: Real;
  DI1: Real;
  JJ: longint;
begin
  // FLAG ZERO WHERE LINE STARTED
  TZR[JZR, TRPCOF.KSE] := TZR[JZR, TRPCOF.KSE]+10;
  // SEARCH FOR CORRESPONDING ZERO ON SIDE KE
  CHECK := 5;
  DIST := 99999;
  FlagZeroFound := False;
  for N := 0 to 1 do
  begin
    FlagZeroFound := False;
    for JESE := 0 to 2 do
    begin
      JJ := NZ[KE];
      if (JJ <> 0) then
      begin
        for J := 0 to JJ-1 do
        begin
          if (TZR[J, KE] > CHECK) then
          begin
            continue;
          end;
          DI1 := ABS((Y0[J, KE]-YY3) *
            DXDS-(X0[J, KE]-XX3) * DYDS);
          if (DI1 >= DIST) then
          begin
            Continue;
          end;
          J1 := J+1;
          DIST := DI1;
        end;
      end;
      if (DIST < RMAX2) then
      begin
        FlagZeroFound := true;
        break;
      end;
      KE := ((KE+1) mod 3);
    end;
    if FlagZeroFound then
    begin
      break;
    end;
    // FOR N=1, DISREGARD FLAGS
    CHECK := 15;
  end;
end;

procedure PlotPoints(const J1, NFOUND, NSTOP, NPQ: longint;
  var DSP : Real;
  const DS01, DS12, DS23, YY1, XX1, YY2, XX2, YY3, XX3 : Real;
  const Y0, X0: TRealArray5_3;
  const KE: longint);
var
  PlotX: Real;
  PlotY: Real;
  Pen: longint;
begin
  if (NPQ > 1) then
  begin
    if (NPQ > 3) then
    begin
      DSP := DSP+DS01;
      if (DSP >= TRPCOC.DSPMIN) then
      begin
        Pen := 2;
        PlotX := XX1+TRPCOT.X[2];
        PlotY := YY1+TRPCOT.Y[2];
        PLOT(PlotX, PlotY, Pen);
        DSP := 0;
      end;
    end;

    if (NPQ > 2) then
    begin
      DSP := DSP+DS12;
      if (DSP >= TRPCOC.DSPMIN) then
      begin
        Pen := 2;
        PlotX := XX2+TRPCOT.X[2];
        PlotY := YY2+TRPCOT.Y[2];
        PLOT(PlotX, PlotY, Pen);
        DSP := 0;
      end;
    end;

    DSP := DSP+DS23;
    if (DSP >= TRPCOC.DSPMIN) then
    begin
      Pen := 2;
      PlotX := XX3+TRPCOT.X[2];
      PlotY := YY3+TRPCOT.Y[2];
      PLOT(PlotX, PlotY, Pen);
    end;
  end;
  if (NFOUND+NSTOP = 0) then
  begin
    Pen := 2;
    PlotX := X0[J1, KE]+TRPCOT.X[2];
    PlotY := Y0[J1, KE]+TRPCOT.Y[2];
    PLOT(PlotX, PlotY, Pen);
  end;
end;

procedure ComputeAngle(var THETAC: Real; const FD1, F1, F2: Real;
  out ContinueLoopOverZeros: Boolean; const THETAS: TRealArray3);
const
  PI_Real: Real = PI;
  Half: Real = 0.5;
var
  THETSC: Real;
  procedure SetTHETAC;
  begin
    THETAC := THETAS[TRPCOF.KSE]+THETSC;
  end;
begin
  // COMPUTE ANGLE
  ContinueLoopOverZeros := False;
  if (F2 = 0) then
  begin
    if (FD1 = 0) then
    begin
      ContinueLoopOverZeros := True;
    end
    else
    begin
      THETSC := PI_Real * Half;
      SetTHETAC;
    end;
  end
  else
  begin
    THETSC := ArcTAN2(-F1, F2);
    if (THETSC <= 0) then
    begin
      THETSC := THETSC+PI_Real;
    end;
    SetTHETAC;
  end;
end;

procedure MovePenToStart(const JZR: longint; const Y0, X0: TRealArray5_3);
var
  PlotY: Real;
  PlotX: Real;
  Pen: longint;
begin
  // MOVE PEN TO START
  PlotX := X0[JZR, TRPCOF.KSE]+TRPCOT.X[2];
  PlotY := Y0[JZR, TRPCOF.KSE]+TRPCOT.Y[2];
  Pen := 3;
  PLOT(PlotX, PlotY, Pen);
  SpecifyContourLevel(TRPCOF.ContourLevel, TRPCOF.TriangleNumber, TRPCOF.KSE);
end;

procedure InitializeTracing(var NOST, NP, NPQ, NFOUND, NSTOP: longint;
  const FSTEP: Real;
  var DSP, DS12, DS23, YY1, XX1, YY2, XX2, YY3, XX3, DS: Real;
  const  THETAC: Real);
var
  DY12: Real;
  DX12: Real;
begin
  // INITIALIZE TRACING
  DS := FSTEP;
  DX12 := DS * COS(THETAC);
  DY12 := DS * SIN(THETAC);
  XX3 := TRPCOF.XX4F;
  YY3 := TRPCOF.YY4F;
  XX2 := XX3-DX12;
  YY2 := YY3-DY12;
  XX1 := XX2-DX12;
  YY1 := YY2-DY12;
  DS23 := DS;
  DS12 := DS;

  // POINTS P1,P2,P3,P4 WITH COORDINATES XX1...XX4, YY1...YY4
  // ARE REFERRED TO AS *QUEUE*. DS12...DS34 ARE THE DISTANCES
  // BETWEEN POINTS IN THE QUEUE.
  // EVERY POINT NORMALLY MUST PASS THROUGH THE QUEUE
  // BEFORE IT IS PLOTTED.
  // FIRST (OLDEST) POINT IN THE QUEUE IS P1, WHICH IS NORMALLY
  // (NPQ = 4) THE FIRST CANDIDATE TO BE PLOTTED.
  // P4 IS THE NEXT POINT TO BE COMPUTED.

  NPQ := 0;
  // NPQ= NUMBER OF POINTS IN THE QUEUE COUNTED FROM THE END
  //      WHICH CAN BE PLOTTED.
  //      E.G. NPQ=1, P4 IS STILL TO BE PLOTTED

  NP := 0;
  // = NUMBER OF POINTS COMPUTED FOR THIS CONTOUR LINE
  DSP := 0;
  // = DISTANCE OF POINT TO BE PLOTTED TO LAST POINT PLOTTED

  NSTOP := 0;
  NFOUND := 0;
  NOST := 0;
end;

procedure InitializeDerivatives(var DYDS, DXDS: Real;
  const XX3, YY3, XX2, YY2, XX1, YY1, DS23, DS12: Real;
  out SkipPlottingPoints: boolean);
var
  SQ: Real;
  PL2: Real;
  PL1: Real;
  PL0: Real;
  DS13: Real;
begin
  DS13 := DS23+DS12;
  PL0 := DS23 / (DS12 * DS13);
  PL1 := -DS13 / (DS12 * DS23);
  PL2 := (DS13+DS23) / (DS13 * DS23);
  DXDS := PL0 * XX1+PL1 * XX2+PL2 * XX3;
  DYDS := PL0 * YY1+PL1 * YY2+PL2 * YY3;
  SQ := SQRT(DXDS * DXDS+DYDS * DYDS);
  SkipPlottingPoints := SQ = 0;
  if SkipPlottingPoints then
  begin
    Exit;
  end;
  DXDS := DXDS / SQ;
  DYDS := DYDS / SQ;
  TRPCOF.COR := -DYDS;
  TRPCOF.SIR := DXDS;
end;

procedure DivideStepsizeNormalToCurveByTwo(var F1TimesF2LessThanOrEqualToZero: Boolean;
  var DS, RMA: Real; const F1: Real; var F2: Real;
  const POSERR: Real; var NOST: longint);
begin
  // DIVIDE STEPSIZE NORMAL TO CURVE BY 2.
  NOST := NOST+1;
  DS := DS * 2;
  RMA := RMA * 0.5;
  F1TimesF2LessThanOrEqualToZero := false;
  while True do
  begin
    F2 := TRICPF_Pascal(RMA);
    if (F1 * F2 <= 0) then
    begin
      F1TimesF2LessThanOrEqualToZero := true;
      Break;
    end;
    RMA := -RMA;
    F2 := TRICPF_Pascal(RMA);
    if (F1 * F2 <= 0) then
    begin
      F1TimesF2LessThanOrEqualToZero := true;
      Break;
    end;
    RMA := RMA * 0.5;
    if (ABS(RMA) > POSERR) then
    begin
      continue;
    end;
    break;
  end;
end;

procedure SearchForTwoPointsWithOppositeSign(out TwoPointsWithOppositeSignFound: Boolean;
  const DXDS, DYDS: Real; var DS: Real; const XX3, YY3: Real;
  var RMA: Real; out F1, F2: Real; const POSERR, RMAX: Real;
  var NSTOP, NOST: longint);
var
  F1TimesF2LessThanOrEqualToZero: Boolean;
begin
  // SEARCH FOR TWO POINTS WITH OPPOSITE SIGN
  RMA := FortranSIGN(RMAX, RMA);
  TwoPointsWithOppositeSignFound := True;
  while True do
  begin
    TRPCOF.XX4F := XX3+DXDS * DS;
    TRPCOF.YY4F := YY3+DYDS * DS;
    F1 := TRICPF_Pascal(0);
    F2 := TRICPF_Pascal(RMA);
    if (F1 * F2 < 0) then
    begin
      break;
    end;
    if (ABS(F2) >= ABS(F1)) then
    begin
      RMA := -RMA;
      F2 := TRICPF_Pascal(RMA);
      if (F1 * F2 < 0) then
      begin
        Break;
      end;
    end;

    // DIVIDE STEPSIZE IN CURVE DIRECTION BY 2.
    DS := DS * 0.5;
    if (DS > POSERR) then
    begin
      Continue;
    end;
    DivideStepsizeNormalToCurveByTwo(
      F1TimesF2LessThanOrEqualToZero, DS, RMA, F1, F2, POSERR, NOST);
    if F1TimesF2LessThanOrEqualToZero then
    begin
      break;
    end;
    NSTOP := 1;
    TwoPointsWithOppositeSignFound := False;
    break;
  end;
end;

procedure CheckIfPointIsOutsideTriangle(var ContinueFromCheckPoint: Boolean;
  var NPQ, KE: longint;
  const V, U, YY4, XX4, DS34, R, DSMAX, RMIN: Real;
  var DS01, DS, XX3, YY3, XX2, YY2, XX1, YY1,
  DS23, DS12, DSP: Real);
var
  PointIsOutsideTriangle: Boolean;
begin
  // CHECK IF POINT IS OUTSIDE THE TRIANGLE
  ContinueFromCheckPoint := False;
  KE := 0;
  PointIsOutsideTriangle := False;
  if (U < 0) then
  begin
    PointIsOutsideTriangle := true;
  end
  else
  begin
    KE := 1;
    if (V < 0) then
    begin
      PointIsOutsideTriangle := true;
    end
    else
    begin
      KE := 2;
      if (V > 1-U) then
      begin
        PointIsOutsideTriangle := true;
      end;
    end;
  end;
  if not PointIsOutsideTriangle then
  begin
    PlotLastPointAndUpdateQueue(XX1, YY1, XX2, YY2, XX3, YY3, DS,
      DS01, DS12, DS23, DSP, R, DS34, XX4, YY4, RMIN, DSMAX, NPQ);

    ContinueFromCheckPoint := True;
  end;
end;

procedure ComputeCurveDirection(const TPER: TRealArray3;
  const RMAX2, DSMAX, POSERR, RMIN, RMAX: Real;
  var DSP, DS12, DS23, YY1, XX1, YY2, XX2, DS, XX3, YY3, DS01: Real;
  var TZR: TRealArray5_3; const Y0, X0: TRealArray5_3;
  var KE, NOST, NP, NPQ, NFOUND, NSTOP, J1: longint; const JZR: longint;
  const NZ: TIntArray3;
  var SkipPlottingPoints, ContinueLoopOverZeros: Boolean;
  var RMA: Real);
var
  FlagZeroFound: Boolean;
  ContinueFromCheckPoint: Boolean;
  V: Real;
  U: Real;
  YY4: Real;
  XX4: Real;
  DS34: Real;
  R: Real;
//  DummyRMA: Real;
  TwoPointsWithOppositeSignFound: Boolean;
  DXDS: Real;
  DYDS: Real;
  F1: Real;
  F2: Real;
begin
  // COMPUTE CURVE DIRECTION
  ContinueLoopOverZeros := false;
  SkipPlottingPoints := false;
  while True do
  begin
    InitializeDerivatives(DYDS, DXDS, XX3, YY3, XX2, YY2, XX1, YY1, DS23, DS12,
      SkipPlottingPoints);
    if SkipPlottingPoints then
    begin
      Exit;
    end;

    SearchForTwoPointsWithOppositeSign(TwoPointsWithOppositeSignFound, DXDS, DYDS, DS, XX3, YY3,
      RMA, F1, F2, POSERR, RMAX, NSTOP, NOST);

    // FIND ZERO FOR NEW POINT
    SkipPlottingPoints := false;
    if not TwoPointsWithOppositeSignFound then
    begin
      Exit;
    end;
    NPQ := NPQ+1;
    NP := NP+1;
    if (NP > TRPCOC.NPMAX) then
    begin
      SkipPlottingPoints := True;
      Exit;
    end;
    R := TRICPZ_Pascal(0, RMA, F1, F2, POSERR);
    DS34 := SQRT(DS * DS+R * R);
    XX4 := TRPCOF.XX4F+TRPCOF.COR * R;
    YY4 := TRPCOF.YY4F+TRPCOF.SIR * R;
    U := TRPCOT.AP * XX4+TRPCOT.BP * YY4;
    V := TRPCOT.CP * XX4+TRPCOT.DP * YY4;
    CheckIfPointIsOutsideTriangle(ContinueFromCheckPoint, NPQ, KE,
      V, U, YY4, XX4, DS34, R, DSMAX, RMIN, DS01, DS, XX3, YY3, XX2, YY2, XX1, YY1, DS23,
      DS12, DSP);
    if ContinueFromCheckPoint then
    begin
      Continue;
    end;
    break;
  end;

  // NO SEARCH IF FIRST POINT WAS COMPUTED AND
  // START WAS AT DATA POINT
  if (NP = 1) then
  begin
    if ((TZR[JZR, TRPCOF.KSE] > 1-TPER[TRPCOF.KSE])
      or (TZR[JZR, TRPCOF.KSE] < TPER[TRPCOF.KSE])) then
    begin
      ContinueLoopOverZeros := true;
      Exit;
    end;
  end;
  SearchForCorrespondingZero(JZR, KE, J1, FlagZeroFound,
    RMAX2, DYDS, DXDS, YY3, XX3, Y0, X0, TZR, NZ);

  if FlagZeroFound then
  begin
    // FLAG ZERO FOUND
    TZR[J1-1, KE] := TZR[J1-1, KE]+10;
  end
  else
  begin
    // NO ACCEPTABLE ZERO ON ALL THREE SIDES
    NFOUND := 1;
    TwoPointsWithOppositeSignFound := False;
  end;
end;


procedure TRP00_Pascal (const XD,YD,ZD: TRealArray;
  const CN: TRealArray; const NC: longint; const IPT: TIntArray;
  const PDD: TRealArray; const NT: longint);
{
C
C     T R I ANGLE    C  ONTOUR    P  LOTTING
C
C     MAIN ROUTINE
C
C     COPYRIGHT(C): A. PREUSSER, 1984
C                   SEN. F. WISS. U. FORSCH.
C                   PROJEKTGRUPPE PARALLELRECHNEN
C                   HEILBRONNER STR. 10
C                   D-1000 BERLIN 31
C
      DLL_IMPORT PLOTS, PLOT, SYMBOL, NUMBER
      DIMENSION XD(*),YD(*),ZD(*),CN(NC),IPT(*),PDD(*)
C
C     XD,YD,ZD  COORDINATES OF DATA POINTS
C     CN        CONTOUR LEVELS
C     NC        NUMBER OF CONTOUR LEVELS
C     IPT       POINT NUMBERS STORED AS 3*I-2, 3*I-1, 3*I TH
C               ELEMENT FOR TRIANGLE I
C     PDD       PARTIAL DERIVATIVES
C               (ZX,ZY,ZXX,ZXY,ZYY)
C
C
C     FOR PLOTTING THE CALCOMP ROUTINE PLOT(X,Y,IPEN) IS USED
C     IPEN= 3 MEANS     MOVE TO X,Y WITH PEN IN UPWARD POSITION
C     IPEN= 2 MEANS     MOVE TO X,Y WITH PEN DOWN
C
}
var
  ZMAX: TRealArray3;
  ZMIN: TRealArray3;
  IN_Array: TIntArray3;
  NZ: TIntArray3;
  TPER: TRealArray3;
  DT: TRealArray3;
  THETAS: TRealArray3;
  SI: TRealArray3;
  CO: TRealArray3;
  IT: longint;
  JI: longint;
  J: longint;
  ID: longint;
  ZS: Real;
  SLMAX: Real;
  HMIN: Real;
  RMAX: Real;
  RMIN: Real;
  POSERR: Real;
  DSMAX: Real;
  FSTEP: Real;
  EPS: Real;
  RMAX2: Real;
  JSE: longint;
  I: longint;
  K: longint;
  F2: Real;
  F1: Real;
//  NPTRI: longint;
  KA: longint;
  KE: longint;
  KC: longint;
  JN: longint;
  JZR: longint;
  FD1: Real;
  THETAC: Real;
  DS: Real;
  XX3: Real;
  YY3: Real;
  XX2: Real;
  YY2: Real;
  XX1: Real;
  YY1: Real;
  DS23: Real;
  DS12: Real;
  DSP: Real;
  NPQ: longint;
  NP: longint;
  NSTOP: longint;
  NFOUND: longint;
  NOST: longint;
  SkipPlottingPoints: boolean;
  DS01: Real;
  J1: longint;
  ContinueLoopOverZeros: boolean;
//  Pen: longint;
  TS1: TRealArray6_3;
  Z1: TRealArray6_3;
  TZR: TRealArray5_3;
  Y0: TRealArray5_3;
  X0: TRealArray5_3;
  RMA: Real;
  procedure InitializeArrays;
  var
    Index1, Index2: integer;
  begin
    for Index1 := 0 to 2 do
    begin
      for Index2 := 0 to 4 do
      begin
        TZR[Index2, Index1] := 0;
        Y0[Index2, Index1] := 0;
        X0[Index2, Index1] := 0;
      end;
    end;
    for Index1 := 0 to 2 do
    begin
      for Index2 := 0 to 5 do
      begin
        TS1[Index2, Index1] := 0;
        Z1[Index2, Index1] := 0;
      end;
    end;
  end;
begin
  InitializeArrays;

  // START MAIN LOOP OVER TRIANGLES
  for IT := 0 to NT-1 do
  begin
    TRPCOF.TriangleNumber := IT;
    // LOAD COORDINATES AT VERTICES
    JI:= 3*IT-1;
    for J := 0 to 2 do
    begin
      JI:= JI+1;
      ID:= IPT[JI];
      TRPCOT.X[J]:= XD[ID];
      TRPCOT.Y[J]:= YD[ID];
      TRPCOT.Z[J]:= ZD[ID];
    end;
    ZS := (TRPCOT.Z[0]+TRPCOT.Z[1]+TRPCOT.Z[2])/3.;
    BasicTriangleGeometry(SI, CO, ZS);

    MoreTriangleGeometry(SI, CO, SLMAX, HMIN);

    DefineConstants(SLMAX, HMIN, RMAX, RMIN, POSERR, DSMAX, FSTEP, EPS, RMAX2);

    // COMPUTE COEFFICIENTS FOR POLYNOMIALS ALONG SIDES
    TRP001_Pascal (IT,IPT,PDD);

    TRPCOF.CL:= 0.;

    // LOOP OVER SIDES
    for JSE := 0 to 2 do
    begin
      TRPCOF.KSE:= JSE;
      TPER[TRPCOF.KSE]:= POSERR/TRPCOT.SL[TRPCOF.KSE];

      ComputeEndpointsOfIntervals(I, TS1, TPER);

      // IN(KSE)= NUMBER OF INTERVALS
      IN_Array[TRPCOF.KSE]:= I;

      //(E.G. IF IN(KSE)=0, THERE IS NO POINT FOR WHICH 1ST DER.=0)
      ComputeMaximaAndMinima(TS1, Z1, ZMIN, ZMAX, I);
    end;

//    NPTRI:= 0;
    KA:= 0;
    KE:= 0;
    KC:= 0;

    // START LOOP OVER CONTOUR LEVELS
    for K := 0 to NC-1 do
    begin
      TRPCOF.CL:= CN[K]-ZS;
      TRPCOF.ContourLevel := CN[K];

      ComputeZeros(TS1, TZR, Z1,
        NZ, ZMAX, ZMIN, IN_Array, TPER);

      IF (NZ[0]+NZ[1]+NZ[2]<2) then
      begin
        continue;
      end;
      // IF (.TRUE.) THEN STOP PROCESSING THIS CONTOUR LEVEL

      ComputeX0andY0(Y0, X0, TZR, NZ);

      ComputeTriangleFactors(DT, THETAS, KC, EPS);
      //
      // FOLLOW CONTOURS

      RMA := RMAX;

      // LOOP OVER SIDES, KSE= SIDE NUMBER
      for JSE := 0 to 2 do
      begin
        TRPCOF.KSE:= KA;
        JN:= NZ[TRPCOF.KSE];
        IF (JN<>0) then
        begin
          RMA := FortranSign(RMAX, -RMA);
          // LOOP OVER ZEROS
          for JZR := 0 to JN-1 do
          begin
            IF (TZR[JZR,TRPCOF.KSE]>5) then 
            begin
              // THIS ZERO HAS ALREADY BEEN PROCESSED
              Continue;
            end;

            // ESTIMATE STARTING DIRECTION
            ComputeF1andF2(Y0, X0, TZR,
              JZR, FD1, F1, F2, EPS, DT, CO, SI);

            ComputeAngle(THETAC, FD1, F1, F2, ContinueLoopOverZeros, THETAS);

            if ContinueLoopOverZeros then
            begin
              Continue
            end;

            MovePenToStart(JZR, Y0, X0);

            InitializeTracing(NOST, NP, NPQ, NFOUND, NSTOP, FSTEP,DSP, DS12,
              DS23, YY1, XX1, YY2, XX2, YY3, XX3, DS, THETAC);

            // COMPUTE NEW POINT FOR CONTOUR LINE
            ComputeCurveDirection(TPER, RMAX2, DSMAX, POSERR, RMIN, RMAX,
              DSP, DS12, DS23, YY1, XX1, YY2, XX2, DS, XX3, YY3, DS01,
              TZR, Y0, X0,
              KE, NOST, NP, NPQ, NFOUND, NSTOP, J1, JZR, NZ,
              SkipPlottingPoints, ContinueLoopOverZeros, RMA);

            if ContinueLoopOverZeros then
            begin
              Continue;
            end;

            if not SkipPlottingPoints then
            begin
              PlotPoints(J1-1, NFOUND, NSTOP, NPQ, DSP, DS01, DS12, DS23,
                YY1, XX1, YY2, XX2, YY3, XX3, Y0, X0, KE);
            end;
//            NPTRI:= NPTRI+NP;
          end;
          // END OF LOOP OVER ZEROS
        end;
        KA := ((KA+1) mod 3);
      end;
      // END OF LOOP OVER SIDES

      KA:= KE;
    end;
    // END OF LOOP OVER CONTOUR LEVELS

   end;
   // END OF LOOP OVER TRIANGLES
end;


FUNCTION  IDXCHG_Pascal(const X,Y: TRealArray;
  const I1,I2,I3,I4: longint): longint;
{
 THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
 TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
 BY C. L. LAWSON.
 THE INPUT PARAMETERS ARE
     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA
           POINTS,
     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,
           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3
           AND P4 CONNECTED DIAGONALLY.
 THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX-
 CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE.

}
var
  C2SQC1SQ: Real;
  A3SQB2SQ: Real;
  B3SQA1SQ: Real;
  A4SQB1SQ: Real;
  B4SQA2SQ: Real;
  C4SQC3SQ: Real;
  X1: Real;
  Y1: Real;
  X2: Real;
  Y2: Real;
  X3: Real;
  Y3: Real;
  X4: Real;
  Y4: Real;
  IDX: longint;
  U1: Real;
  U2: Real;
  U3: Real;
  U4: Real;
  S1SQ: Real;
  S2SQ: Real;
  S3SQ: Real;
  S4SQ: Real;
begin
  // PRELIMINARY PROCESSING
  X1:=X[I1];
  Y1:=Y[I1];
  X2:=X[I2];
  Y2:=Y[I2];
  X3:=X[I3];
  Y3:=Y[I3];
  X4:=X[I4];
  Y4:=Y[I4];
  // CALCULATION
  IDX:=0;
  U3:=(Y2-Y3)*(X1-X3)-(X2-X3)*(Y1-Y3);
  U4:=(Y1-Y4)*(X2-X4)-(X1-X4)*(Y2-Y4);
  IF (U3*U4>0.0) then
  begin
    U1:=(Y3-Y1)*(X4-X1)-(X3-X1)*(Y4-Y1);
    U2:=(Y4-Y2)*(X3-X2)-(X4-X2)*(Y3-Y2);
    B3SQA1SQ:=Sqr(X1-X3)+Sqr(Y1-Y3);
    A4SQB1SQ:=Sqr(X4-X1)+Sqr(Y4-Y1);
    C2SQC1SQ:=Sqr(X3-X4)+Sqr(Y3-Y4);
    B4SQA2SQ:=Sqr(X2-X4)+Sqr(Y2-Y4);
    A3SQB2SQ:=Sqr(X3-X2)+Sqr(Y3-Y2);
    C4SQC3SQ:=Sqr(X2-X1)+Sqr(Y2-Y1);
    S1SQ:=U1*U1/(C2SQC1SQ*Max(B3SQA1SQ,A4SQB1SQ));
    S2SQ:=U2*U2/(C2SQC1SQ*Max(B4SQA2SQ,A3SQB2SQ));
    S3SQ:=U3*U3/(C4SQC3SQ*Max(A3SQB2SQ,B3SQA1SQ));
    S4SQ:=U4*U4/(C4SQC3SQ*Max(A4SQB1SQ,B4SQA2SQ));
    IF(Min(S1SQ,S2SQ)<Min(S3SQ,S4SQ)) then
    begin
      IDX:=1
    end;
  end;
  result:=IDX
end;

procedure  IDTANG_Pascal(const NDP: longint; const XD,YD: TRealArray;
  var NT, NL: longint; var IPT, IPL: TIntArray);
{
      SUBROUTINE  IDTANG(NDP,XD,YD,NT,IPT,NL,IPL,IWL,IWP,WK)
C THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y
C PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
C POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
C BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
C CORRESPONDING TO THE BORDER LINE SEGMENTS.
C AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
C OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
C LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
C THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE
C LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,
C THEREFORE, SYSTEM DEPENDENT.
C THIS SUBROUTINE CALLS THE IDXCHG FUNCTION.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           X COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           Y COORDINATES OF THE DATA POINTS.
C THE OUTPUT PARAMETERS ARE
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE
C           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH
C           TRIANGLE ARE TO BE STORED AS THE (3*IT-2)ND,
C           (3*IT-1)ST, AND (3*IT)TH ELEMENTS,
C           IT=1,2,...,NT,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE
C           POINT NUMBERS OF THE END POINTS OF THE (IL)TH
C           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE
C           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND,
C           (3*IL-1)ST, AND (3*IL)TH ELEMENTS,
C           IL=1,2,..., NL.
C THE OTHER PARAMETERS ARE
C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
C           INTERNALLY AS A WORK AREA,
C     IWP = INTEGER ARRAY OF DIMENSION NDP USED
C           INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
C           WORK AREA.
}
const
  RATIO: Real = 1.0E-6;
  NREP = 100;
  function DSQF(const U1,V1,U2,V2: Real): Real;
  begin
    result :=Sqr(U2-U1)+Sqr(V2-V1);
  end;
  function SIDE(const U1,V1,U2,V2,U3,V3: Real): Real;
  begin
    result :=(V3-V1)*(U2-U1)-(U3-U1)*(V2-V1);
  end;
var
  ITF: array[0..1] of longint;
  NDPM1: longint;
  DSQMN: Real;
  IPMN1: longint;
  IPMN2: longint;
  IP1: longint;
  IP2: longint;
  IP3: longint;
  X1: Real;
  Y1: Real;
  IP1P1: longint;
  DSQI: Real;
  DSQ12: Real;
  XDMP: Real;
  YDMP: Real;
  JP1: longint;
  JP2: longint;
  JPMN: longint;
  ITS: longint;
  AR: Real;
  DX21: Real;
  DY21: Real;
  ErrorFound: boolean;
  IP: longint;
  JP: longint;
  JPMX: longint;
  JPC: longint;
  TriangleIndex: longint;
  NTT3: longint;
  NL0: longint;
  NLT3: longint;
  DXMN: Real;
  DYMN: Real;
  ARMN: Real;
  DXMX: Real;
  DYMX: Real;
  DSQMX: Real;
  ARMX: Real;
  DX: Real;
  DY: Real;
  NSH: longint;
  NSHT3: longint;
  JP2T3_index: longint;
  JP2T3: longint;
  JP3T3: longint;
  JWL: longint;
  IPL1: longint;
  IPL2: longint;
  IT: longint;
  NLN: longint;
  NLNT3: longint;
  ITT3: longint;
  IPTI: longint;
  NLF: longint;
  NTT3P3: longint;
  IREP: longint;
  ILF: longint;
  ILFT2: longint;
  NTF: longint;
  ITT3R_Index: longint;
  ITT3R: longint;
  IPT1: longint;
  IPT2: longint;
  IPT3: longint;
  TriangleFound: boolean;
  IT1T3: longint;
  IPTI1: longint;
  IT2T3: longint;
  IPTI2: longint;
  JLT3_Index: longint;
  JLT3: longint;
  IPLJ1: longint;
  IPLJ2: longint;
  NLFC: longint;
  JWL1MN: longint;
  NLFT2: longint;
  JWL1_Index: longint;
  JWL1: longint;
  ITT3_Index: longint;
  IWL: TIntArray;
  IWP: TIntArray;
  WK: TRealArray;
begin
  // PRELIMINARY PROCESSING
  SetLength(IWL, 18*NDP);
  SetLength(IWP, NDP);
  SetLength(WK, NDP);
  NDPM1 :=NDP-1;
  IF(NDP<4) then
  begin
    NT := 0;
    raise Exception.Create('NDP LESS THAN 4.  NDP = '+IntToStr(NDP));
  end;
  // DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT.
  DSQMN :=DSQF(XD[0],YD[0],XD[1],YD[1]);
  IPMN1 :=0;
  IPMN2 :=1;
  for IP1 := 0 to NDPM1-1 do
  begin
    X1 :=XD[IP1];
    Y1 :=YD[IP1];
    IP1P1 :=IP1+1;
    for IP2 := IP1P1 to NDP-1 do
    begin
      DSQI :=DSQF(X1,Y1,XD[IP2],YD[IP2]);

      IF(DSQI=0.0) then
      begin
        NT := 0;
        raise Exception.Create('IDENTICAL DATA POINTS: IP1+1 = '
         +IntToStr(IP1+1)+'; IP2+1 = '+IntToStr(IP2+1)
         +'; X = '+FloatToStr(X1)+ '; Y = '+FloatToStr(Y1));
      end;

      IF(DSQI<DSQMN) then
      begin
        DSQMN :=DSQI;
        IPMN1 :=IP1;
        IPMN2 :=IP2;
      end;
    end;
  end;
  DSQ12 :=DSQMN;
  XDMP :=(XD[IPMN1]+XD[IPMN2])/2.0;
  YDMP :=(YD[IPMN1]+YD[IPMN2])/2.0;
  // SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF
  // DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT
  // NUMBERS IN THE IWP ARRAY.}
  JP1 :=1;
  for IP1 := 0 to NDP-1 do
  begin
    IF((IP1=IPMN1) OR(IP1=IPMN2))  then continue;
    JP1 :=JP1+1;
    IWP[JP1]:=IP1+1;
    WK[JP1]:=DSQF(XDMP,YDMP,XD[IP1],YD[IP1]);
  end;
  for JP1 := 2 to NDPM1-1 do
  begin
    DSQMN :=WK[JP1];
    JPMN :=JP1;
    for JP2 := JP1 to NDP-1 do
    begin
      IF(WK[JP2]<DSQMN)     then
      begin
        DSQMN:=WK[JP2];
        JPMN:=JP2;
      end;
    end;
    ITS :=IWP[JP1];
    IWP[JP1]:=IWP[JPMN];
    IWP[JPMN]:=ITS;
    WK[JPMN]:=WK[JP1];
  end;
  // IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE
  // FIRST THREE DATA POINTS ARE NOT COLLINEAR.
  AR :=DSQ12*RATIO;
  X1:=XD[IPMN1];
  Y1:=YD[IPMN1];
  DX21:=XD[IPMN2]-X1;
  DY21:=YD[IPMN2]-Y1;
  ErrorFound := True;
  for JP := 2 to NDP-1 do
  begin
    IP:=IWP[JP]-1;
    IF(ABS((YD[IP]-Y1)*DX21-(XD[IP]-X1)*DY21)>AR) then
    begin
      ErrorFound := False;
      break;
    end;
  end;
  if ErrorFound then
  begin
    NT := 0;
    raise Exception.Create('ALL COLLINEAR DATA POINTS, '
   +'Number of data points = '+IntToStr(NDP));
  end;
  IF(JP<>3)   then
  begin
    JPMX :=JP;
    for JPC := 4 to JPMX do
    begin
      JP:=JP-1;
      IWP[JP]:=IWP[JP-1];
    end;
    IWP[2]:=IP+1;
  end;
  // FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER-
  // TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM-
  // BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN
  // THE IPL ARRAY.
  IP1 :=IPMN1;
  IP2 :=IPMN2;
  IP3 :=IWP[2]-1;
  IF (SIDE(XD[IP1],YD[IP1],XD[IP2],YD[IP2],XD[IP3],YD[IP3]) < 0) then
  begin
    IP1 :=IPMN2;
    IP2 :=IPMN1;
  end;
  TriangleIndex:=0;
  NTT3 :=3;
  IPT[0]:=IP1;
  IPT[1]:=IP2;
  IPT[2]:=IP3;
  NL0:=3;
  NLT3:=9;
  IPL[0]:=IP1;
  IPL[1]:=IP2;
  IPL[2]:=0;
  IPL[3]:=IP2;
  IPL[4]:=IP3;
  IPL[5]:=0;
  IPL[6]:=IP3;
  IPL[7]:=IP1;
  IPL[8]:=0;
  // ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE.
  for JP1 := 3 to NDP-1 do
  begin
    IP1:=IWP[JP1]-1;
    X1:=XD[IP1];
    Y1:=YD[IP1];
    // -DETERMINES THE VISIBLE BORDER LINE SEGMENTS.
    IP2:=IPL[0];
    JPMN:=1;
    DXMN:=XD[IP2]-X1;
    DYMN:=YD[IP2]-Y1;
    DSQMN:=Sqr(DXMN)+Sqr(DYMN);
    ARMN:=DSQMN*RATIO;
    JPMX:=1;
    DXMX:=DXMN;
    DYMX:=DYMN;
    DSQMX:=DSQMN;
    ARMX:=ARMN;
    for JP2 := 1 to NL0-1 do
    begin
      IP2:=IPL[3*JP2];
      DX:=XD[IP2]-X1;
      DY:=YD[IP2]-Y1;
      AR:=DY*DXMN-DX*DYMN;
      IF(AR<=ARMN) then
      begin
        DSQI:=Sqr(DX)+Sqr(DY);
        IF not ((AR>=-ARMN) AND (DSQI >= DSQMN)) then
        begin
          JPMN:=JP2+1;
          DXMN:=DX;
          DYMN:=DY;
          DSQMN:=DSQI;
          ARMN:=DSQMN*RATIO;
        end;
      end;
      AR:=DY*DXMX-DX*DYMX;
      IF(AR>=(-ARMX)) then
      begin
        DSQI:=Sqr(DX)+Sqr(DY);
        IF not ((AR<=ARMX) AND (DSQI>=DSQMX)) then
        begin
          JPMX:=JP2+1;
          DXMX:=DX;
          DYMX:=DY;
          DSQMX:=DSQI;
          ARMX:=DSQMX*RATIO;
        end;
      end;
    end;
    IF (JPMX<JPMN) then
    begin
      JPMX:=JPMX+NL0;
    end;
    NSH :=JPMN-1;
    IF (NSH>0) then
    begin
      //-SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER
      //-LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY.
      NSHT3:=NSH*3;
      for JP2T3_index := 0 to NSH-1 do
      begin
        JP2T3 := JP2T3_index*3;
        JP3T3 :=JP2T3+NLT3;
        IPL[JP3T3]:=IPL[JP2T3];
        IPL[JP3T3+1]:=IPL[JP2T3+1];
        IPL[JP3T3+2]:=IPL[JP2T3+2];
      end;
      for JP2T3_index := 0 to (NLT3 div 3)-1 do
      begin
        JP2T3 := JP2T3_index*3;
        JP3T3:=JP2T3+NSHT3;
        IPL[JP2T3]:=IPL[JP3T3];
        IPL[JP2T3+1]:=IPL[JP3T3+1];
        IPL[JP2T3+2]:=IPL[JP3T3+2];
      end;
      JPMX:=JPMX-NSH;
    end;
    //-ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE
    //-SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER
    //-LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY.
    JWL:=0;
    for JP2 := JPMX to NL0 do
    begin
      JP2T3:=(JP2)*3;
      IPL1:=IPL[JP2T3-3];
      IPL2:=IPL[JP2T3-2];
      IT  :=IPL[JP2T3-1];
      //-- ADDS A TRIANGLE TO THE IPT ARRAY.
      TriangleIndex:=TriangleIndex+1;
      NTT3:=NTT3+3;
      IPT[NTT3-3]:=IPL2;
      IPT[NTT3-2]:=IPL1;
      IPT[NTT3-1]:=IP1;
      //-- UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY.
      IF(JP2=JPMX) then
      begin
        IPL[JP2T3-2]:=IP1;
        IPL[JP2T3-1]:=TriangleIndex;
      end;
      IF(JP2=NL0) then
      begin
        NLN:=JPMX+1;
        NLNT3:=NLN*3;
        IPL[NLNT3-3]:=IP1;
        IPL[NLNT3-2]:=IPL[0];
        IPL[NLNT3-1]:=TriangleIndex;
      end;
      //-- DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER
      //-- LINE SEGMENTS.
      ITT3:=(IT+1)*3-1;
      IPTI:=IPT[ITT3-2];
      IF not ((IPTI<>IPL1) AND (IPTI<>IPL2)) then
      begin
        IPTI:=IPT[ITT3-1];
        IF not ((IPTI<>IPL1) AND (IPTI<>IPL2)) then
        begin
          IPTI:=IPT[ITT3];
        end;
      end;
      //-- CHECKS IF THE EXCHANGE IS NECESSARY.
      IF (IDXCHG_Pascal(XD,YD,IP1,IPTI,IPL1,IPL2)<>0) then
      begin
        //-- MODIFIES THE IPT ARRAY WHEN NECESSARY.
        IPT[ITT3-2]:=IPTI;
        IPT[ITT3-1]:=IPL1;
        IPT[ITT3]:=IP1;
        IPT[NTT3-2]:=IPTI;
        IF (JP2=JPMX) then
        begin
          IPL[JP2T3-1]:=IT;
        end;
        IF ((JP2=NL0) AND (IPL[2]=IT)) then
        begin
          IPL[2]:=TriangleIndex;
        end;
        //-- SETS FLAGS IN THE IWL ARRAY.
        JWL:=JWL+4;
        IWL[JWL-4]:=IPL1+1;
        IWL[JWL-3]:=IPTI+1;
        IWL[JWL-2]:=IPTI+1;
        IWL[JWL-1]:=IPL2+1;
      end;
    end;
    NL0:=NLN;
    NLT3:=NLNT3;
    NLF:=JWL div 2;
    IF(NLF<>0) then
    begin
      //-IMPROVES TRIANGULATION.
      NTT3P3:=NTT3+3;
      for IREP := 0 to NREP-1 do
      begin
        for ILF := 1 to NLF do
        begin
          ILFT2:=ILF*2;
          IPL1:=IWL[ILFT2-2]-1;
          IPL2:=IWL[ILFT2-1]-1;
          //-- LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF
          //-- THE FLAGGED LINE SEGMENT.
          NTF:=-1;
          TriangleFound := False;
          for ITT3R_Index := 1 to NTT3 div 3 do
          begin
            ITT3R := ITT3R_Index*3;
            ITT3:=NTT3P3-ITT3R-1;
            IPT1:=IPT[ITT3-2];
            IPT2:=IPT[ITT3-1];
            IPT3:=IPT[ITT3];
            IF not (((IPL1<>IPT1) AND (IPL1<>IPT2) AND
               (IPL1<>IPT3))) then
            begin
              IF not ((IPL2<>IPT1) AND (IPL2<>IPT2) AND
                 (IPL2<>IPT3)) then
              begin
                NTF:=NTF+1;
                ITF[NTF]:=(ITT3+1) div 3;
                IF(NTF=1) then
                begin
                   TriangleFound := True;
                   break;
                end;
              end;
            end;
          end;
          IF TriangleFound or (NTF>=1) then
          begin
            //-- DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LIE
            //-- ON THE LINE SEGMENT.
            IT1T3:=ITF[0]*3-1;
            IPTI1:=IPT[IT1T3-2];
            IF not ((IPTI1<>IPL1) AND (IPTI1<>IPL2)) then
            begin
              IPTI1:=IPT[IT1T3-1];
              IF not ((IPTI1<>IPL1) AND (IPTI1<> IPL2)) then
              begin
                IPTI1:=IPT[IT1T3];
              end;
            end;
            IT2T3:=ITF[1]*3;
            IPTI2:=IPT[IT2T3-3];
            IF not ((IPTI2<>IPL1) AND (IPTI2<>IPL2)) then
            begin
              IPTI2 :=IPT[IT2T3-2];
              IF not ((IPTI2<>IPL1) AND (IPTI2<>IPL2)) then
              begin
                IPTI2:=IPT[IT2T3-1];
              end;
            end;
            //-- CHECKS IF THE EXCHANGE IS NECESSARY.
            IF not ((IDXCHG_Pascal(XD,YD,IPTI1,IPTI2,IPL1,IPL2)=0)) then
            begin
              //-- MODIFIES THE IPT ARRAY WHEN NECESSARY.
              IPT[IT1T3-2]:=IPTI1;
              IPT[IT1T3-1]:=IPTI2;
              IPT[IT1T3]:=IPL1;
              IPT[IT2T3-3]:=IPTI2;
              IPT[IT2T3-2]:=IPTI1;
              IPT[IT2T3-1]:=IPL2;
              //-- SETS NEW FLAGS.
              JWL:=JWL+8;
              IWL[JWL-8]:=IPL1+1;
              IWL[JWL-7]:=IPTI1+1;
              IWL[JWL-6]:=IPTI1+1;
              IWL[JWL-5]:=IPL2+1;
              IWL[JWL-4]:=IPL2+1;
              IWL[JWL-3]:=IPTI2+1;
              IWL[JWL-2]:=IPTI2+1;
              IWL[JWL-1]:=IPL1+1;
              for JLT3_Index := 1 to NLT3  div 3 do
              begin
                JLT3 := JLT3_Index*3;
                IPLJ1:=IPL[JLT3-3];
                IPLJ2:=IPL[JLT3-2];
                IF (((IPLJ1=IPL1) AND (IPLJ2=IPTI2)) OR
                  ((IPLJ2=IPL1) AND (IPLJ1=IPTI2))) then
                begin
                  IPL[JLT3-1]:=ITF[0]-1;
                end;
                IF(((IPLJ1=IPL2) AND (IPLJ2=IPTI1)) OR
                  ((IPLJ2=IPL2) AND (IPLJ1=IPTI1))) then
                begin
                  IPL[JLT3-1]:=ITF[1]-1;
                end;
              end;
            end;
          end;
        end;
        NLFC :=NLF;
        NLF :=JWL div 2;
        IF(NLF<>NLFC) then
        begin
          //-- RESETS THE IWL ARRAY FOR THE NEXT ROUND.
          JWL:=0;
          JWL1MN:=(NLFC+1)*2;
          NLFT2:=NLF*2;
          for JWL1_Index := JWL1MN div 2 to NLFT2 div 2 do
          begin
            JWL1 := JWL1_Index*2;
            JWL:=JWL+2;
            IWL[JWL-2]:=IWL[JWL1-2];
            IWL[JWL-1]:=IWL[JWL1-1];
          end;
          NLF:=JWL div 2;
        end;
      end;
    end;
  end;
  // REARRANGES THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE
  // ARE LISTED COUNTER-CLOCKWISE.
  for ITT3_Index := 1 to NTT3 div 3 do
  begin
    ITT3 := ITT3_Index*3-1;
    IP1:=IPT[ITT3-2];
    IP2:=IPT[ITT3-1];
    IP3:=IPT[ITT3];
    IF (SIDE(XD[IP1],YD[IP1],XD[IP2],YD[IP2],XD[IP3],YD[IP3]) <0.0) then
    begin
      IPT[ITT3-2]:=IP2;
      IPT[ITT3-1]:=IP1;
    end;
  end;
  NT:=TriangleIndex+1;
  NL:=NL0+1;
end;

procedure TRICP_Pascal (const XD,YD,ZD, C: TRealArray; var WK: TRealArray;
  const ND, NCP, NC, MODE: longint;
  var NT: longint; var IPT, IPL: TIntArray);
{
      SUBROUTINE TRICP (XD,YD,ZD,ND,C,NC,WK,IWK,MODE)
C
C     T R I ANGLE    C  ONTOUR    P  LOTTING
C
C     USER INTERFACE   (VERSION 0.1)
C
C     COPYRIGHT(C): A. PREUSSER, 1984
C                   SEN. F. WISS. U. FORSCH.
C                   PROJEKTGRUPPE PARALLELRECHNEN
C                   HEILBRONNER STR. 10
C                   D-1000 BERLIN 31
C
      DIMENSION XD(ND),YD(ND),ZD(ND),C(NC),WK(*),IWK(*)
C
C     XD,YD,ZD   COORDINATES OF DATA POINTS
C                XD AND YD MUST BE GIVEN IN CM.
C                FOR USING INCH,  SET INSTALLATION PARAMETER
C                CMSCAL= 2.54 .
C     ND         NUMBER OF DATA POINTS
C     C          CONTOUR LEVELS
C     NC         NUMBER OF CONTOUR LEVELS
C     WK         REAL WORK ARRAY, DIMENSION 5*ND
C
C                ON EXIT, WK CONTAINS THE PARTIAL DERIVATIVES
C                (ZX(I),ZY(I),ZXX(I),ZXY(I),ZYY(I),I=1,ND)
C
C     IWK        INTEGER WORK ARRAY. FOR MODE=0, THE DIMENSION IS
C                                  31*ND  .
C                FOR OTHER MODES, SEE BELOW.
C
C                ON EXIT, IWK CONTAINS
C                A) IWK(1)= NT= NUMBER OF TRIANGLES
C                B) IWK(2...3*NT+1) POINT NUMBERS FOR THE TRIANGLES.
C                   THE FIRST 3 NUMBERS DETERMINE THE VERTICES OF THE
C                   FIRST TRIANGLE, THE NEXT 3 FOR THE SECOND AND SO ON.
C                   THE NUMBERS COORESPOND TO THE INDICES OF THE
C                   XD,YD,ZD ARRAYS. THEY ARE ARRANGED COUNTER-
C                   CLOCKWISE WITHIN A TRIANGLE.
C
C                C) IWK(3*NT+2  ...  3*NT+NCP*ND+1)
C                   A LIST OF NCP*ND POINT NUMBERS, REPRESENTING
C                   NCP POINTS FOR EACH DATA POINT THAT ARE
C                   THE CLOSEST TO THIS POINT. THE FIRST NCP
C                   NUMBERS ARE FOR THE FIRST DATA POINT, THE NEXT
C                   NCP FOR THE SECOND AND SO ON. THESE NUMBERS
C                   WERE USED FOR THE COMPUTATION OF THE PARTIAL
C                   DERIVATIVES.
C                   NCP IS AN INSTALLATION PARAMETER AND WILL BE SET
C                   TO 4.
C
C     MODE       MODE OF USAGE
C                0, NORMAL MODE, SEE ABOVE.
C                1, NO TRIANGULATION REQUESTED.
C                   IWK MUST CONTAIN THE INFORMATION ABOUT
C                   THE TRIANGLES AS DESCRIBED UNDER A) AND B)
C                   ON ENTRY.
C                   THESE LOCATIONS OF IWK WILL NOT BE CHANGED
C                   AND IN ADDITION, THE INFORMATION DESCRIBED
C                   UNDER C) WILL BE AVAILABLE ON EXIT.
C                2, NO TRIANGULATION AND NO DETERMINATION OF THE
C                   NCP CLOSEST POINTS FOR EACH DATA POINT.
C                   IWK MUST CONTAIN THE INFORMATION AS DESCRIBED
C                   UNDER A), B), AND C) AS INPUT INFORMATION.
C                   THE CONTENTS OF IWK WILL NOT BE CHANGED.
C                3, NO TRIANGULATION AND NO COMPUTATION OF THE
C                   PARTIAL DERIVATIVES.
C                   IWK MUST CONTAIN THE INFORMATION A) AND B)
C                   AND
C                   WK MUST CONTAIN THE PARTIAL DERIVATIVES
C                   (ZX(I),ZY(I),ZXX(I),ZXY(I),ZYY(I),I=1,ND)
C                   ON ENTRY.
C                   THE CONTENTS OF WK AND IWK WILL NOT BE CHANGED.
C                   THIS MODE IS ESPECIALLY USEFUL WHEN TRICP IS
C                   CALLED AGAIN AFTER A PREVIOUS CALL. FOR INSTANCE,
C                   ONLY THE CONTOUR LEVELS MAY HAVE CHANGED.
C                   WHEN DESIGNING A SURFACE, IT MAY BE APPROPRIATE
C                   TO CHANGE THE XD,YD,ZD PARAMETERS AND THE PARTIAL
C                   DERIVATIVES INTERACTIVELY AND TO CALL TRICP AGAIN
C                   USING THIS MODE.
C
C                   FOR MODES 1,2, AND 3 THE DIMENSION OF IWK CAN
C                   BE REDUCED TO
C                                 3*NT + NCP*ND + 1.
C
}
var
  NL: longint;
begin
  // SET INSTALLATION PARAMETERS
  TRPCOC.CMSCAL := 1.;
  //C *** SET  CMSCAL= 2.54  FOR INCH CALIBRATED PLOTTERS
  TRPCOC.DSPMIN := 0.02/TRPCOC.CMSCAL;
  TRPCOC.NPMAX := 500;

  Assert(Length(IPT) = 6*ND-15);
  if (Mode in [0, 1, 2]) then
  begin
    Assert(Length(IPL) = 6*ND);
  end;

  if (Mode = 0) then
  begin
    // TRIANGULATION
    IDTANG_Pascal (ND,XD,YD,NT,NL,IPT,IPL);
    // IDTANG IS PART OF ACM ALG. 526 BY H. AKIMA
  end;
  // FIND NCP POINTS CLOSEST TO EACH DATA POINT
  if (Mode in [0,1]) then
  begin
    IDCLDP_New(ND,XD,YD,NCP, NT,IPT,IPL);
//    IDCLDP_Pascal (ND,XD,YD,NCP,IPL);
  end;
  // IDCLDP IS PART OF ACM ALG. 526 BY H. AKIMA

  // COMPUTE PARTIAL DERIVATIVES
  if (Mode in [0, 1, 2]) then
  begin

    IDPDRV_Pascal (ND,XD,YD,ZD,NCP,IPL,WK);
  end;
  // IDPDRV IS PART OF ACM ALG. 526 BY H. AKIMA

  // COMPUTE CONTOURS BY SUCCESSIVE SOLUTION OF
  // QUINTIC POLYNOMIAL EQUATIONS
  TRP00_Pascal (XD,YD,ZD,C,NC,IPT,WK,NT);
end;

type
  TEdge = class(TObject)
    N1: integer;
    N2: Integer;
    Tri: Integer;
  end;

procedure Trmesh_2_Idtang(var NT: Integer;
  const IEND, IADJ: TIntArray;
  var IPL, IPT: TIntArray; const ND: Integer);
var
  Index: Integer;
  Next: Integer;
  EdgeIndex: Integer;
  AnEdge: TEdge;
  AnotherEdge: TEdge;
  Node3: Integer;
  Node2: Integer;
  Node1: Integer;
  INDL: Integer;
  NODE: Integer;
  INDF: Integer;
  TriIndex: Integer;
  IPL_Index: Integer;
  IPTIndex: Integer;
  Edges: TList;
  Local_I: Integer;
  Start: Integer;
  InnerIndex: Integer;
  TriIndexCreated: integer;
  TestEdge: TEdge;
  procedure CreateEdges;
    function MatchingEdge(N1, N2: Integer): TEdge;
    var
      LocalEdgeIndex: Integer;
      AnotherEdge: TEdge;
    begin
      result := nil;
      for LocalEdgeIndex := 0 to Edges.Count - 1 do
      begin
        AnotherEdge := Edges[LocalEdgeIndex];
        if (AnotherEdge.N1 = N1) and (AnotherEdge.N2 = N2) then
        begin
          result := AnotherEdge;
          Break;
        end;
      end;
    end;
  begin
    if (Node1 >= 0) and (Node2 >= 0) and (Node3 >= 0) then
    begin

      if (Node1 < Node2) and (Node1 < Node3) then
      begin
        IPT[IPTIndex] := Node1;
        Inc(IPTIndex);
        IPT[IPTIndex] := Node2;
        Inc(IPTIndex);
        IPT[IPTIndex] := Node3;
        Inc(IPTIndex);
        TriIndexCreated := TriIndex;
      end
      else
      begin
        TriIndexCreated := -1;
      end;
      if (IADJ[IEND[Node1] - 1] = 0) and (IADJ[IEND[Node2] - 1] = 0) then
      begin
        AnEdge := MatchingEdge(Node1, Node2);
        if AnEdge = nil then
        begin
          AnEdge := TEdge.Create;
          Edges.Add(AnEdge);
          AnEdge.N1 := Node1;
          AnEdge.N2 := Node2;
          AnEdge.Tri := TriIndexCreated;
        end;
      end;
      if (IADJ[IEND[Node2] - 1] = 0) and (IADJ[IEND[Node3] - 1] = 0) then
      begin
        AnEdge := MatchingEdge(Node2, Node3);
        if AnEdge = nil then
        begin
          AnEdge := TEdge.Create;
          Edges.Add(AnEdge);
          AnEdge.N1 := Node2;
          AnEdge.N2 := Node3;
          AnEdge.Tri := TriIndexCreated;
        end;
      end;
      if (IADJ[IEND[Node3] - 1] = 0) and (IADJ[IEND[Node1] - 1] = 0) then
      begin
        AnEdge := MatchingEdge(Node3, Node1);
        if AnEdge = nil then
        begin
          AnEdge := TEdge.Create;
          Edges.Add(AnEdge);
          AnEdge.N1 := Node3;
          AnEdge.N2 := Node1;
          AnEdge.Tri := TriIndexCreated;
        end;
      end;
      Inc(TriIndex);
    end;
  end;
begin
  Edges := TObjectList.Create;
  try
    IPTIndex := 0;
    IPL_Index := 0;
    TriIndex := 0;
    INDF := 1;
    for NODE := 1 to ND do
    begin
      INDL := IEND[NODE - 1];
      Node1 := NODE - 1;
      if (IADJ[IEND[Node1] - 1] <> 0) then
      begin
        Node2 := IADJ[INDL - 1] - 1;
        Node3 := IADJ[INDF - 1] - 1;
        CreateEdges;
      end;
      for Local_I := INDF + 1 to INDL do
      begin
        Node2 := IADJ[Local_I - 2] - 1;
        Node3 := IADJ[Local_I - 1] - 1;
        CreateEdges;
      end;
      INDF := INDL + 1;
    end;

    for EdgeIndex := Edges.Count - 1 downto 1 do
    begin
      AnEdge := Edges[EdgeIndex];
      if AnEdge <> nil then
      begin
        for InnerIndex := EdgeIndex-1 downto 0 do
        begin
          AnotherEdge := Edges[InnerIndex];
          if AnotherEdge <> nil then
          begin
            if ((AnEdge.N1 = AnotherEdge.N2) and (AnEdge.N2 = AnotherEdge.N1))
              then
            begin
              Edges[EdgeIndex] := nil;
              Edges[InnerIndex] := nil;
              AnEdge := nil;
              break;
            end;
          end;
        end;
      end;
    end;

    Edges.Pack;

    if Edges.Count > 0 then
    begin
      AnEdge := Edges[0];
      EdgeIndex := 0;
      Start := AnEdge.N1;
      Next := -1;
      while AnEdge <> nil do
      begin
        IPL[IPL_Index] := AnEdge.N1;
        Inc(IPL_Index);
        IPL[IPL_Index] := AnEdge.N2;
        Inc(IPL_Index);
        IPL[IPL_Index] := AnEdge.Tri;
        Inc(IPL_Index);
        if AnEdge.N2 = Start then
        begin
          AnEdge := nil;
          Edges.Delete(EdgeIndex);
        end
        else
        begin
          Next := AnEdge.N2;
          Edges.Delete(EdgeIndex);
          AnEdge := nil;
          for Index := 0 to Edges.Count - 1 do
          begin
            TestEdge := Edges[Index];
            if TestEdge.N1 = Next then
            begin
              EdgeIndex := Index;
              AnEdge := TestEdge;
              break;
            end;
          end;
        end;
        if AnEdge <> nil then
        begin
          Assert(AnEdge.N1 = Next);
        end
        else
        begin
          if Edges.Count > 0 then
          begin
            AnEdge := Edges[0];
            EdgeIndex := 0;
            Start := AnEdge.N1;
          end;
        end;
      end;
    end;
    NT := TriIndex div 3;
  finally
    Edges.Free;
  end;
end;

procedure Convert_2DArrayTo1DArray(const InArray: TRealArrayX_5;
  out OutArray: TRealArray);
var
  i: longint;
  J: longint;
  OutIndex: longint;
begin
  SetLength(OutArray, Length(InArray)* 5);
  OutIndex := 0;
  for I := 0 to Length(InArray)-1 do
  begin
    for J := 0 to Length(InArray[0])-1 do
    begin
      OutArray[OutIndex] := InArray[I,J];
      Inc(OutIndex);
    end;
  end;
end;

procedure Convert_1DArrayTo2DArray(const InArray: TRealArray;
  out OutArray: TRealArrayX_5);
var
  i: longint;
  J: longint;
  OutIndex: longint;
begin
  SetLength(OutArray, Length(InArray) div 5);
  OutIndex := 0;
  for I := 0 to Length(OutArray)-1 do
  begin
    for J := 0 to Length(OutArray[0])-1 do
    begin
      OutArray[I,J] := InArray[OutIndex];
      Inc(OutIndex);
    end;
  end;
end;

initialization
  InitializeTrpcot;
  InitializeTRPCOF;
  InitializeTTRPCOP;

end.
