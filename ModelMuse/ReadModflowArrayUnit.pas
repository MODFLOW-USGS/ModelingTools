// This file is part of the source code of ModelMuse by
// @author(Richard B. Winston <rbwinst@usgs.gov>).
unit ReadModflowArrayUnit;

interface

uses SysUtils, Classes, Generics.Collections;

type
  TModflowFloat = single;
  TModflowDouble = double;
  TModflowDesc = Array[0..15] of AnsiChar;
  TModflowDesc2 = Array[0..17] of AnsiChar;
  TModflowSingleArray = array of array of TModflowFloat;
  TModflowDoubleArray = array of array of TModflowDouble;
  T3DTModflowArray = array of TModflowDoubleArray;

  TAuxArray = record
    Name: string;
    Values: T3DTModflowArray;
  end;
  TAuxArrays = array of TAuxArray;

  TStructCell = class(TObject)
    Layer: Integer;
    Row: integer;
    Column: Integer;
    Value: TModflowDouble;
  end;

  TStructuredCellList = TObjectList<TStructCell>;

  TModflowPrecision = (mpSingle, mpDouble);

  EPrecisionReadError = class(Exception);
  EEndOfModflowFileError = class(Exception);

  TFileVariable = class(TObject)
    AFile: TextFile;
  end;

  THydModData = class(TObject)
  private
    FTimes: Array of TModflowDouble;
    FLabels: TStringList;
    FValues: TModflowDoubleArray;
    FTimeUnit: integer;
    function GetValue(LabelIndex, TimeIndex: integer): TModflowDouble;
    function GetLabel(Index: integer): string;
    function GetLabelCount: integer;
    function GetTimeCount: integer;
    function GetTime(Index: Integer): double;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure ReadFile(const FileName: string);
    property TimeCount: integer Read GetTimeCount;
    property Labels[Index: integer]: string read GetLabel;
    property LabelCount: integer read GetLabelCount;
    property Values[LabelIndex, TimeIndex: integer]: TModflowDouble read GetValue;
    property TimeUnit: integer Read FTimeUnit;
    property Times[Index: Integer]: double read GetTime;
    function IndexOfLabel(const ALabel: string): integer;
  end;

procedure ReadSinglePrecisionMt3dmsBinaryRealArray(AFile: TFileStream;
  var NTRANS, KSTP, KPER: Integer;
  var TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckArrayPrecision to determine the precision before calling
// this procedure.
procedure ReadSinglePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, ILAY: Integer;
  var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);

procedure ReadDoublePrecisionMt3dmsBinaryRealArray(AFile: TFileStream;
  var NTRANS, KSTP, KPER: Integer;
  var TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckArrayPrecision to determine the precision before calling
// this procedure.
procedure ReadDoublePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);

// F.AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
procedure ReadModflowAsciiRealArray(F: TFileVariable;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc2; var NCOL, NROW, ILAY: Integer;
  var AnArray: TModflowDoubleArray; ReadArray: boolean = True);

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckBudgetPrecision to determine the precision before calling
// this procedure.
procedure ReadModflowSinglePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  var AnArray: T3DTModflowArray;
  var AuxArray: TAuxArrays;
  HufFormat: boolean;
  ReadArray: Boolean = True
  );

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckBudgetPrecision to determine the precision before calling
// this procedure.
procedure ReadModflowDoublePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  var AuxArray: TAuxArrays;
  HufFormat: boolean;
  ReadArray: Boolean = True
  );

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckBudgetPrecision to determine the precision before calling
// this procedure.
procedure ReadModflowSinglePrecFluxList(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  CellList: TStructuredCellList;
  HufFormat: boolean;
  ReadList: Boolean = True
  );

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckBudgetPrecision to determine the precision before calling
// this procedure.
procedure ReadModflowDoublePrecFluxList(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  CellList: TStructuredCellList;
  HufFormat: boolean;
  ReadList: Boolean = True
  );

function CheckMt3dmsArrayPrecision(AFile: TFileStream): TModflowPrecision;

// Use this function to check the precision of
// a binary head or drawdown file or other similar files.
// AFile should be at the beginning of the file when this
// function is called.  It will still be at the beginning
// when the function returns.
function CheckArrayPrecision(AFile: TFileStream): TModflowPrecision;

// Use this function to check the precision of a
// cell by cell flow file. AFile should be at the beginning of
// the file when this function is called.  It will still
// be at the beginning when the function returns.
function CheckBudgetPrecision(AFile: TFileStream; out HufFormat: boolean;
  IsModflow6: Boolean): TModflowPrecision;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnableToReadFile = 'Unable to read file. Check that the file is an unstructured, non-formatted file. In MODFLOW-2005, this is determined in OPENSPEC.inc';
  StrTheMODFLOWResults = 'The MODFLOW results file terminated prematurely. Y' +
  'ou may have run out of disk space or MODFLOW may have stopped prematurely' +
  ' for some other reason. If you ran out of disk space saving the heads and ' +
  'drawdowns in binary files would reduce the output file size.';
  StrTheMODFLOWResultsBinary = 'The MODFLOW results file terminated prematurely. Y' +
  'ou may have run out of disk space or MODFLOW may have stopped prematurely' +
  ' for some other reason.';

//var
//  Dummy: TModflowDouble;

function CheckArrayPrecision(AFile: TFileStream): TModflowPrecision;
var
  KSTP, KPER: Integer;
  PERTIM, TOTIM: TModflowFloat;
  DESC: TModflowDesc;
  Description : AnsiString;
  PERTIM_Double: TModflowDouble;
  TOTIM_Double: TModflowDouble;
  function ValidDescription: boolean;
  begin
    // If these values are changed, update the
    // MODFLOW Online Guide with the new values.
    result := (Description = '            HEAD')
           or (Description = '        DRAWDOWN')
           or (Description = '      SUBSIDENCE')
           or (Description = '      COMPACTION')
           or (Description = '   CRITICAL HEAD')
           or (Description = '     HEAD IN HGU')
           or (Description = 'NDSYS COMPACTION')
           or (Description = '  Z DISPLACEMENT')
           or (Description = ' D CRITICAL HEAD')
           or (Description = 'LAYER COMPACTION')
           or (Description = ' DSYS COMPACTION')
           or (Description = 'ND CRITICAL HEAD')
           or (Description = 'LAYER COMPACTION')
           or (Description = 'SYSTM COMPACTION')
           or (Description = 'PRECONSOL STRESS')
           or (Description = 'CHANGE IN PCSTRS')
           or (Description = 'EFFECTIVE STRESS')
           or (Description = 'CHANGE IN EFF-ST')
           or (Description = '      VOID RATIO')
           or (Description = '       THICKNESS')
           or (Description = 'CENTER ELEVATION')
           or (Description = 'GEOSTATIC STRESS')
           or (Description = 'CHANGE IN G-STRS')
           or (Description = 'CONCENTRATION   ')
           or (Description = '   EL LAYER CMPT')
           or (Description = ' INEL LAYER CMPT')
           or (Description = '   NDSYS EL CMPT')
           or (Description = '    DSYS EL CMPT')
           or (Description = ' NDSYS INEL CMPT')
           or (Description = '  DSYS INEL CMPT')
           or (Description = 'HEAD            ')
  end;
begin
  Assert(AFile.Position = 0);
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM, SizeOf(PERTIM));
  AFile.Read(TOTIM, SizeOf(TOTIM));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  if ValidDescription then
  begin
    result := mpSingle;
  end
  else
  begin
    result := mpDouble;
  end;
  AFile.Position := 0;
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM_Double, SizeOf(PERTIM_Double));
  AFile.Read(TOTIM_Double, SizeOf(TOTIM_Double));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  case result of
    mpSingle:
      begin
        if ValidDescription then
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
    mpDouble:
      begin
        if not ValidDescription then
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
  end;
  AFile.Position := 0;
end;

function CheckMt3dmsArrayPrecision(AFile: TFileStream): TModflowPrecision;
var
  NTRANS, KSTP, KPER: Integer;
  TOTIM: TModflowFloat;
  DESC: TModflowDesc;
  Description : AnsiString;
  TOTIM_Double: TModflowDouble;
  function ValidDescription: boolean;
  begin
    // If these values are changed, update the
    // MODFLOW Online Guide with the new values.
    result := (Description = 'CONCENTRATION   ');
  end;
begin
  Assert(AFile.Position = 0);
  AFile.Read(NTRANS, SizeOf(NTRANS));
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(TOTIM, SizeOf(TOTIM));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  if ValidDescription then
  begin
    result := mpSingle;
  end
  else
  begin
    result := mpDouble;
  end;
  AFile.Position := 0;
  AFile.Read(NTRANS, SizeOf(NTRANS));
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(TOTIM_Double, SizeOf(TOTIM_Double));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  case result of
    mpSingle:
      begin
        if ValidDescription then
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
    mpDouble:
      begin
        if not ValidDescription then
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
  end;
  AFile.Position := 0;
end;

function CheckBudgetPrecision(AFile: TFileStream; out HufFormat: boolean;
  IsModflow6: Boolean): TModflowPrecision;
var
  Description: AnsiString;
  FirstDescription: AnsiString;
  SecondDescription: AnsiString;
  MF2005BudgetTerms: TStringList;
  procedure ReadModflow6Name(var AName: string);
  var
    NameArray: TModflowDesc;
  begin
    AFile.Read(NameArray, SizeOf(NameArray));
    AName := string(NameArray);
  end;
  procedure ReadDescription(var KPER: Integer; var KSTP: Integer);
  var
    DESC: TModflowDesc;
  begin
    AFile.Read(KSTP, SizeOf(KSTP));
    AFile.Read(KPER, SizeOf(KPER));
    AFile.Read(DESC, SizeOf(DESC));
    Description := DESC;
  end;
  function ReadSingleArray: boolean;
  var
    KSTP, KPER: Integer;
    NCOL, NROW, NLAY: Integer;
    AnArray: T3DTModflowArray;
    RowIndex: Integer;
    LayerIndex: Integer;
    ColIndex: Integer;
    PERTIM, TOTIM: TModflowFloat;
    ITYPE: integer;
    DELT: TModflowFloat;
    NVAL: Integer;
    Index: Integer;
    NLIST: Integer;
    NRC: Integer;
    Values: array of TModflowFloat;
    ICELL: Integer;
    ValIndex: Integer;
    AValue: TModflowFloat;
    NewPosition: Int64;
    ModelName1: string;
    PackageName1: string;
    ModelName2: string;
    PackageName2: string;
    NumVariable: Integer;
    AuxVarIndex: Integer;
    AuxName: string;
    NodeIndex: Integer;
    N1: Integer;
    N2: Integer;
    Value: single;
    AuxValue: single;
  begin
    result := True;
    try
      ReadDescription(KPER, KSTP);
      AFile.Read(NCOL, SizeOf(NCOL));
      AFile.Read(NROW, SizeOf(NROW));
      AFile.Read(NLAY, SizeOf(NLAY));
      if (KSTP < 1) or (KPER < 1) or (NCOL < 1) or (NROW < 1) then
      begin
        result := False;
        Exit;
      end;
      try
        SetLength(AnArray, Abs(NLAY), NROW, NCOL);
      except on E: ERangeError do
        begin
          result := False;
          Exit;
        end;
      end;
      for LayerIndex := 0 to Abs(NLAY) - 1 do
      begin
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AnArray[LayerIndex, RowIndex, ColIndex] := 0;
          end;
        end;
      end;
      PERTIM := -1;
      TOTIM := -1;
      NVAL := -1;

      ITYPE := 0;
      if (NLAY < 0) or HufFormat then
      begin
        AFile.Read(ITYPE, SizeOf(ITYPE));
        AFile.Read(DELT, SizeOf(DELT));
        AFile.Read(PERTIM, SizeOf(PERTIM));
        AFile.Read(TOTIM, SizeOf(TOTIM));
        if (DELT < 0) or (PERTIM < 0) or (TOTIM < 0) then
        begin
          result := False;
          Exit;
        end;
        NVAL := 1;
    //C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
    //C  values because the first budget terms must be from the internal
    //C  flow package (BCF,LPF, or HUF).
        if (ITYPE = 2) then
        begin
          AFile.Read(NLIST, SizeOf(NLIST));
          if NLIST < 0 then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
      case ITYPE of
        0,1: // full 3D array
          begin
            if (AFile.Size-AFile.Position) < (Abs(NLAY)*NROW*NCOL*SizeOf(AValue)) then
            begin
              result := false;
              Exit;
            end;
            for LayerIndex := 0 to Abs(NLAY) - 1 do
            begin
              for RowIndex := 0 to NROW - 1 do
              begin
                for ColIndex := 0 to NCOL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  try
                    AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
                  except
                    result := False;
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        2:
          begin
            if NLIST > 0 then
            begin
              NewPosition := (SizeOf(ICELL) + NVAL*SizeOf(AValue));
              NewPosition := NLIST * NewPosition;
              if NewPosition > AFile.Size then
              begin
                result := False;
                Exit;
              end;
              NRC := NROW*NCOL;
              SetLength(Values, NVAL);
              for Index := 0 to NLIST - 1 do
              begin
                AFile.Read(ICELL, SizeOf(ICELL));
                for ValIndex := 0 to NVAL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  Values[ValIndex] := AValue;
                end;
                LayerIndex :=  (ICELL-1) div NRC;
                RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
                ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
                if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                  AND (ColIndex < ncol) AND (RowIndex < NROW)
                  AND (LayerIndex < Abs(NLAY))) then
                begin
                  AnArray[LayerIndex, RowIndex, ColIndex] :=
                    AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
                end
                else
                begin
                  result := False;
                  Exit;
                end;
              end;
            end;
          end;
        4:
          begin
            if (AFile.Size-AFile.Position) < (NROW*NCOL*SizeOf(AValue)) then
            begin
              result := false;
              Exit;
            end;
            LayerIndex := 0;
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AFile.Read(AValue, SizeOf(AValue));
                try
                  AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
                except
                  result := false;
                  Exit;
                end;
              end;
            end;
          end;
        6:
          begin
            Assert(IsModflow6);
            ReadModflow6Name(ModelName1);
            ReadModflow6Name(PackageName1);
            ReadModflow6Name(ModelName2);
            ReadModflow6Name(PackageName2);
            // NumVariable is the number of auxilliary variables + 1.
            AFile.Read(NumVariable, SizeOf(NumVariable));
            if (NumVariable < 0) or (NumVariable > 20) then
            begin
              result := False;
              Exit;
            end;
            for AuxVarIndex := 1 to NumVariable - 1 do
            begin
              ReadModflow6Name(AuxName);
            end;
            AFile.Read(NLIST, SizeOf(NLIST));

            NRC := NROW*NCOL;
            for NodeIndex := 0 to NLIST - 1 do
            begin
              AFile.Read(N1, SizeOf(N1));
              AFile.Read(N2, SizeOf(N2));
              AFile.Read(Value, SizeOf(Value));
              for AuxVarIndex := 1 to NumVariable - 1 do
              begin
                AFile.Read(AuxValue, SizeOf(AuxValue));
              end;
              LayerIndex :=  (N1-1) div NRC;
              RowIndex := ( (N1 - LayerIndex*NRC)-1 ) div NCOL;
              ColIndex := N1 - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
              if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                AND (ColIndex < ncol) AND (RowIndex < NROW)
                AND (LayerIndex < Abs(NLAY))) then
              begin
                AnArray[LayerIndex, RowIndex, ColIndex] :=
                  AnArray[LayerIndex, RowIndex, ColIndex] + Value;
              end
              else
              begin
                result := False;
                Exit;
              end;
            end;

          end
        else
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
    except
      result := false;
    end;
  end;
  function ReadDoubleArray: boolean;
  var
    KSTP, KPER: Integer;
    NCOL, NROW, NLAY: Integer;
    AnArray: T3DTModflowArray;
    RowIndex: Integer;
    LayerIndex: Integer;
    ColIndex: Integer;
    PERTIM, TOTIM: TModflowDouble;
    ITYPE: integer;
    DELT: TModflowDouble;
    NVAL: Integer;
    Index: Integer;
    NLIST: Integer;
    NRC: Integer;
    Values: array of TModflowDouble;
    ICELL: Integer;
    ValIndex: Integer;
    AValue: TModflowDouble;
    NewPosition: Int64;
    ModelName1: string;
    PackageName1: string;
    ModelName2: string;
    PackageName2: string;
    NumVariable: Integer;
    AuxVarIndex: Integer;
    AuxName: string;
    NodeIndex: Integer;
    N1: Integer;
    N2: Integer;
    Value: double;
    AuxValue: double;
  begin
    result := True;
    try
      ReadDescription(KPER, KSTP);
      AFile.Read(NCOL, SizeOf(NCOL));
      AFile.Read(NROW, SizeOf(NROW));
      AFile.Read(NLAY, SizeOf(NLAY));
      if (KSTP < 1) or (KPER < 1) or (NCOL < 1) or (NROW < 1) then
      begin
        result := False;
        Exit;
      end;
      try
        SetLength(AnArray, Abs(NLAY), NROW, NCOL);
      except on E: ERangeError do
        begin
          result := False;
          Exit;
        end;
      end;
      for LayerIndex := 0 to Abs(NLAY) - 1 do
      begin
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AnArray[LayerIndex, RowIndex, ColIndex] := 0;
          end;
        end;
      end;
      PERTIM := -1;
      TOTIM := -1;
      NVAL := -1;

      ITYPE := 0;
      if (NLAY < 0) or HufFormat then
      begin
        AFile.Read(ITYPE, SizeOf(ITYPE));
        AFile.Read(DELT, SizeOf(DELT));
        AFile.Read(PERTIM, SizeOf(PERTIM));
        AFile.Read(TOTIM, SizeOf(TOTIM));
        if (DELT < 0) or (PERTIM < 0) or (TOTIM < 0) then
        begin
          result := False;
          Exit;
        end;
        NVAL := 1;
    //C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
    //C  values because the first budget terms must be from the internal
    //C  flow package (BCF,LPF, or HUF).
        if (ITYPE = 2) then
        begin
          AFile.Read(NLIST, SizeOf(NLIST));
          if NLIST < 0 then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
      case ITYPE of
        0,1: // full 3D array
          begin
            if (AFile.Size-AFile.Position) < (Abs(NLAY)*NROW*NCOL*SizeOf(AValue)) then
            begin
              result := false;
              Exit;
            end;
            for LayerIndex := 0 to Abs(NLAY) - 1 do
            begin
              for RowIndex := 0 to NROW - 1 do
              begin
                for ColIndex := 0 to NCOL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  try
                    AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
                  except
                    result := false;
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        2:
          begin
            if NLIST > 0 then
            begin
              NewPosition := (SizeOf(ICELL) + NVAL*SizeOf(AValue));
              NewPosition := NLIST * NewPosition;
              if NewPosition > AFile.Size then
              begin
                result := False;
                Exit;
              end;
              NRC := NROW*NCOL;
              SetLength(Values, NVAL);
              for Index := 0 to NLIST - 1 do
              begin
                AFile.Read(ICELL, SizeOf(ICELL));
                for ValIndex := 0 to NVAL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  Values[ValIndex] := AValue;
                end;
                LayerIndex :=  (ICELL-1) div NRC;
                RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
                ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
                if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                  AND (ColIndex < ncol) AND (RowIndex < NROW)
                  AND (LayerIndex < Abs(NLAY))) then
                begin
                  AnArray[LayerIndex, RowIndex, ColIndex] :=
                    AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
                end
                else
                begin
                  result := False;
                  Exit;
                end;
              end;
//            end
//            else
//            begin
//              result := False;
//              Exit;
            end;
          end;
        4:
          begin
            if (AFile.Size-AFile.Position) < (NROW*NCOL*SizeOf(AValue)) then
            begin
              result := false;
              Exit;
            end;
            LayerIndex := 0;
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AFile.Read(AValue, SizeOf(AValue));
                try
                  AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
                except
                  result := false;
                  Exit;
                end;
              end;
            end;
          end;
        6:
          begin
            Assert(IsModflow6);
            ReadModflow6Name(ModelName1);
            ReadModflow6Name(PackageName1);
            ReadModflow6Name(ModelName2);
            ReadModflow6Name(PackageName2);
            // NumVariable is the number of auxilliary variables + 1.
            AFile.Read(NumVariable, SizeOf(NumVariable));
            if (NumVariable < 0) or (NumVariable > 20) then
            begin
              result := False;
              Exit;
            end;
            for AuxVarIndex := 1 to NumVariable - 1 do
            begin
              ReadModflow6Name(AuxName);
            end;
            AFile.Read(NLIST, SizeOf(NLIST));

            NRC := NROW*NCOL;
            for NodeIndex := 0 to NLIST - 1 do
            begin
              AFile.Read(N1, SizeOf(N1));
              AFile.Read(N2, SizeOf(N2));
              AFile.Read(Value, SizeOf(Value));
              for AuxVarIndex := 1 to NumVariable - 1 do
              begin
                AFile.Read(AuxValue, SizeOf(AuxValue));
              end;
              LayerIndex :=  (N1-1) div NRC;
              RowIndex := ( (N1 - LayerIndex*NRC)-1 ) div NCOL;
              ColIndex := N1 - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
              if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                AND (ColIndex < ncol) AND (RowIndex < NROW)
                AND (LayerIndex < Abs(NLAY))) then
              begin
                AnArray[LayerIndex, RowIndex, ColIndex] :=
                  AnArray[LayerIndex, RowIndex, ColIndex] + Value;
              end
              else
              begin
                result := False;
                Exit;
              end;
            end;
          end
        else
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
    except
      result := False;
    end;
  end;
begin
  MF2005BudgetTerms := TStringList.Create;
  try
    if IsModflow6 then
    begin
      MF2005BudgetTerms.Sorted := TRUE;
      MF2005BudgetTerms.Duplicates := dupIgnore;

      MF2005BudgetTerms.Add('    FLOW-JA-FACE');
      MF2005BudgetTerms.Add('      DATA-SPDIS');
      MF2005BudgetTerms.Add('             GWF');
      MF2005BudgetTerms.Add('        RAINFALL');
      MF2005BudgetTerms.Add('     EVAPORATION');
      MF2005BudgetTerms.Add('          RUNOFF');
      MF2005BudgetTerms.Add('      EXT-INFLOW');
      MF2005BudgetTerms.Add('      WITHDRAWAL');
      MF2005BudgetTerms.Add('     EXT-OUTFLOW');
      MF2005BudgetTerms.Add('         STORAGE');
      MF2005BudgetTerms.Add('        CONSTANT');
      MF2005BudgetTerms.Add('        FROM-MVR');
      MF2005BudgetTerms.Add('          TO-MVR');
      MF2005BudgetTerms.Add('             GWF');
      MF2005BudgetTerms.Add('            RATE');
      MF2005BudgetTerms.Add('         STORAGE');
      MF2005BudgetTerms.Add('        CONSTANT');
      MF2005BudgetTerms.Add('         FW-RATE');
      MF2005BudgetTerms.Add('        FROM-MVR');
      MF2005BudgetTerms.Add('     RATE-TO-MVR');
      MF2005BudgetTerms.Add('  FW-RATE-TO-MVR');
      MF2005BudgetTerms.Add('       AUXILIARY');
      MF2005BudgetTerms.Add('        RAINFALL');
      MF2005BudgetTerms.Add('     EVAPORATION');
      MF2005BudgetTerms.Add('          RUNOFF');
      MF2005BudgetTerms.Add('      EXT-INFLOW');
      MF2005BudgetTerms.Add('             GWF');
      MF2005BudgetTerms.Add('     EXT-OUTFLOW');
      MF2005BudgetTerms.Add('        FROM-MVR');
      MF2005BudgetTerms.Add('          TO-MVR');
      MF2005BudgetTerms.Add('    INFILTRATION');
      MF2005BudgetTerms.Add('             GWF');
      MF2005BudgetTerms.Add('         STORAGE');
      MF2005BudgetTerms.Add('            UZET');
      MF2005BudgetTerms.Add('        UZF-GWET');
      MF2005BudgetTerms.Add('         UZF-GWD');
      MF2005BudgetTerms.Add('SAT.-UNSAT. EXCH');
      MF2005BudgetTerms.Add('         REJ-INF');
      MF2005BudgetTerms.Add('  REJ-INF-TO-MVR');
      MF2005BudgetTerms.Add('        FROM-MVR');
      MF2005BudgetTerms.Add('             CHD');
      MF2005BudgetTerms.Add('             DRN');
      MF2005BudgetTerms.Add('             EVT');
      MF2005BudgetTerms.Add('             GHB');
      MF2005BudgetTerms.Add('             LAK');
      MF2005BudgetTerms.Add('             MAW');
      MF2005BudgetTerms.Add('             RCH');
      MF2005BudgetTerms.Add('             RIV');
      MF2005BudgetTerms.Add('             SFR');
      MF2005BudgetTerms.Add('       UZF CELLS');
      MF2005BudgetTerms.Add('         UZF-INF');
      MF2005BudgetTerms.Add('       UZF-GWRCH');
      MF2005BudgetTerms.Add('         UZF-GWD');
      MF2005BudgetTerms.Add('        UZF-GWET');
      MF2005BudgetTerms.Add('  UZF-GWD TO-MVR');
      MF2005BudgetTerms.Add('             WEL');
      MF2005BudgetTerms.Add('          STO-SS');
      MF2005BudgetTerms.Add('          STO-SY');

    end;

    HufFormat := False;
    result := mpSingle;
    Assert(AFile.Position = 0);
    if ReadDoubleArray then
    begin
      FirstDescription := Description;
      if AFile.Position = AFile.Size then
      begin
        result := mpDouble;
        Exit;
      end;
      if (AFile.Position < AFile.Size) and ReadDoubleArray then
      begin
        SecondDescription := Description;
        if IsModflow6 then
        begin
          if (MF2005BudgetTerms.IndexOf(string(FirstDescription)) >= 0)
            and (MF2005BudgetTerms.IndexOf(string(SecondDescription)) >= 0) then
          begin
            result := mpDouble;
          end
          else
          begin
            result := mpSingle;
          end;
        end
        else if (FirstDescription = '         STORAGE')
          and (SecondDescription = '   CONSTANT HEAD') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '   CONSTANT HEAD')
          and (SecondDescription = 'FLOW RIGHT FACE ') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '   CONSTANT HEAD')
          and (SecondDescription = 'FLOW FRONT FACE ') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '   CONSTANT HEAD')
          and (SecondDescription = 'FLOW LOWER FACE ') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = 'FLOW RIGHT FACE ')
          and (SecondDescription = 'FLOW FRONT FACE ') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = 'FLOW RIGHT FACE ')
          and (SecondDescription = 'FLOW LOWER FACE ') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = 'FLOW FRONT FACE ')
          and (SecondDescription = 'FLOW LOWER FACE ') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '     NETRECHARGE')
          and (SecondDescription = '     NETRECHARGE') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '    NETDISCHARGE')
          and (SecondDescription = '    NETDISCHARGE') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '      ZETASRF  1')
          and (SecondDescription = '      ZETASRF  1') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '      ZETASRF  1')
          and (SecondDescription = '      ZETASRF  2') then
        begin
          result := mpDouble;
        end
        else
        begin
          result := mpSingle;
        end;
      end;
    end;
    AFile.Position := 0;
    if ReadSingleArray then
    begin
      FirstDescription := Description;
      if AFile.Position = AFile.Size then
      begin
        result := mpSingle;
        Exit;
      end;
      if (AFile.Position < AFile.Size) and ReadSingleArray then
      begin
        SecondDescription := Description;
        if IsModflow6 then
        begin
          if (MF2005BudgetTerms.IndexOf(string(FirstDescription)) >= 0)
            and (MF2005BudgetTerms.IndexOf(string(SecondDescription)) >= 0) then
          begin
            if (result <> mpSingle) then
            begin
              raise EPrecisionReadError.Create(StrUnableToReadFile);
            end;
          end;
		    end
        else if (FirstDescription = '         STORAGE')
          and (SecondDescription = '   CONSTANT HEAD') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '   CONSTANT HEAD')
          and (SecondDescription = 'FLOW RIGHT FACE ') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '   CONSTANT HEAD')
          and (SecondDescription = 'FLOW FRONT FACE ') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '   CONSTANT HEAD')
          and (SecondDescription = 'FLOW LOWER FACE ') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = 'FLOW RIGHT FACE ')
          and (SecondDescription = 'FLOW FRONT FACE ') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = 'FLOW RIGHT FACE ')
          and (SecondDescription = 'FLOW LOWER FACE ') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = 'FLOW FRONT FACE ')
          and (SecondDescription = 'FLOW LOWER FACE ') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '     NETRECHARGE')
          and (SecondDescription = '     NETRECHARGE') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '    NETDISCHARGE')
          and (SecondDescription = '    NETDISCHARGE') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '      ZETASRF  1')
          and (SecondDescription = '      ZETASRF  1') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else if (FirstDescription = '      ZETASRF  1')
          and (SecondDescription = '      ZETASRF  2') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else
        begin
          if (result <> mpDouble) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end;
      end
      else
      begin
        if (result <> mpDouble) then
        begin
          AFile.Position := 0;
          HufFormat := True;
          result := mpSingle;
          if ReadDoubleArray then
          begin
            FirstDescription := Description;
            if (AFile.Position < AFile.Size) and ReadDoubleArray then
            begin
              SecondDescription := Description;
              if (FirstDescription = '         STORAGE')
                and (SecondDescription = '   CONSTANT HEAD') then
              begin
                result := mpDouble;
              end
              else if (FirstDescription = '   CONSTANT HEAD')
                and (SecondDescription = 'FLOW RIGHT FACE ') then
              begin
                if (result <> mpDouble) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = 'FLOW RIGHT FACE ')
                and (SecondDescription = 'FLOW FRONT FACE ') then
              begin
                if (result <> mpDouble) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = 'FLOW RIGHT FACE ')
                and (SecondDescription = 'FLOW LOWER FACE ') then
              begin
                if (result <> mpDouble) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = 'FLOW FRONT FACE ')
                and (SecondDescription = 'FLOW LOWER FACE ') then
              begin
                if (result <> mpDouble) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else
              begin
                result := mpSingle;
              end;
            end;
          end;
          AFile.Position := 0;
          if ReadSingleArray then
          begin
            FirstDescription := Description;
            if (AFile.Position < AFile.Size) and ReadSingleArray then
            begin
              SecondDescription := Description;
              if (FirstDescription = '         STORAGE')
                and (SecondDescription = '   CONSTANT HEAD') then
              begin
                if (result <> mpSingle) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = '   CONSTANT HEAD')
                and (SecondDescription = 'FLOW RIGHT FACE ') then
              begin
                if (result <> mpSingle) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = 'FLOW RIGHT FACE ')
                and (SecondDescription = 'FLOW FRONT FACE ') then
              begin
                if (result <> mpSingle) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = 'FLOW RIGHT FACE ')
                and (SecondDescription = 'FLOW LOWER FACE ') then
              begin
                if (result <> mpSingle) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else if (FirstDescription = 'FLOW FRONT FACE ')
                and (SecondDescription = 'FLOW LOWER FACE ') then
              begin
                if (result <> mpSingle) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end
              else
              begin
                if (result <> mpDouble) then
                begin
                  raise EPrecisionReadError.Create(StrUnableToReadFile);
                end;
              end;
            end
            else
            begin
              if (result <> mpDouble) then
              begin
                raise EPrecisionReadError.Create(StrUnableToReadFile);
              end;
            end;
          end
          else
          begin
            if (result <> mpDouble) then
            begin
              raise EPrecisionReadError.Create(StrUnableToReadFile);
            end;
          end;
        end;
      end;
    end
    else
    begin
      if (result <> mpDouble) then
      begin
        raise EPrecisionReadError.Create(StrUnableToReadFile);
      end;
    end;
  finally
    AFile.Position := 0;
    MF2005BudgetTerms.Free;
  end;
end;

procedure ReadSinglePrecisionMt3dmsBinaryRealArray(AFile: TFileStream;
  var NTRANS, KSTP, KPER: Integer;
  var TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);
var
  ColIndex: Integer;
  RowIndex: Integer;
  AValue: TModflowFloat;
begin
  AFile.Read(NTRANS, SizeOf(NTRANS));
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));

  AFile.Read(AValue, SizeOf(TModflowFloat));
  TOTIM := AValue;
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(ILAY, SizeOf(ILAY));
  if ReadArray then
  begin
    SetLength(AnArray, NROW, NCOL);
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        AFile.Read(AValue, SizeOf(TModflowFloat));
        AnArray[RowIndex, ColIndex] := AValue;
      end;
    end;
  end
  else
  begin
    AFile.Position := AFile.Position + SizeOf(TModflowFloat)*NROW*NCOL;
    if AFile.Position > AFile.Size then
    begin
      raise EEndOfModflowFileError.Create(StrTheMODFLOWResultsBinary);
    end;
  end;
end;

procedure ReadSinglePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);
var
  ColIndex: Integer;
  RowIndex: Integer;
  AValue: TModflowFloat;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));

  AFile.Read(AValue, SizeOf(TModflowFloat));
  PERTIM := AValue;
  AFile.Read(AValue, SizeOf(TModflowFloat));
  TOTIM := AValue;
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(ILAY, SizeOf(ILAY));
  if ReadArray then
  begin
    SetLength(AnArray, NROW, NCOL);
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        AFile.Read(AValue, SizeOf(TModflowFloat));
        AnArray[RowIndex, ColIndex] := AValue;
      end;
    end;
  end
  else
  begin
    AFile.Position := AFile.Position + SizeOf(TModflowFloat)*NROW*NCOL;
    if AFile.Position > AFile.Size then
    begin
      raise EEndOfModflowFileError.Create(StrTheMODFLOWResultsBinary);
    end;
  end;
end;

procedure ReadDoublePrecisionMt3dmsBinaryRealArray(AFile: TFileStream;
  var NTRANS, KSTP, KPER: Integer;
  var TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  AFile.Read(NTRANS, SizeOf(NTRANS));
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(TOTIM, SizeOf(TModflowDouble));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(ILAY, SizeOf(ILAY));
  SetLength(AnArray, NROW, NCOL);
  if ReadArray then
  begin
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        AFile.Read(AnArray[RowIndex, ColIndex], SizeOf(TModflowDouble));
      end;
    end;
  end
  else
  begin
    AFile.Position := AFile.Position + SizeOf(TModflowDouble)*NROW*NCOL;
    if AFile.Position > AFile.Size then
    begin
      raise EEndOfModflowFileError.Create(StrTheMODFLOWResultsBinary);
    end;
  end;
end;


procedure ReadDoublePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM, SizeOf(TModflowDouble));
  AFile.Read(TOTIM, SizeOf(TModflowDouble));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(ILAY, SizeOf(ILAY));
  SetLength(AnArray, NROW, NCOL);
  if ReadArray then
  begin
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        AFile.Read(AnArray[RowIndex, ColIndex], SizeOf(TModflowDouble));
      end;
    end;
  end
  else
  begin
    AFile.Position := AFile.Position + SizeOf(TModflowDouble)*NROW*NCOL;
    if AFile.Position > AFile.Size then
    begin
      raise EEndOfModflowFileError.Create(StrTheMODFLOWResultsBinary);
    end;
  end;
end;

procedure ReadModflowSinglePrecFluxList(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  CellList: TStructuredCellList;
  HufFormat: boolean;
  ReadList: Boolean = True
  );
var
  ITYPE: integer;
  DELT: TModflowFloat;
  NVAL: Integer;
  Index: Integer;
  CTMP: TModflowDesc;
  NLIST: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  FluxArray: TModflowSingleArray;
  LayerIndicatorArray: array of array of integer;
  Values: array of TModflowFloat;
  ICELL: Integer;
  ValIndex: Integer;
  NRC: Integer;
  AValue: TModflowFloat;
  ACell: TStructCell;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
  if ReadList then
  begin
    CellList.Clear;
  end;
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if (NLAY < 0) or HufFormat then
  begin
    AFile.Read(ITYPE, SizeOf(ITYPE));
    AFile.Read(DELT, SizeOf(DELT));
    AFile.Read(AValue, SizeOf(AValue));
    PERTIM := AValue;
    AFile.Read(AValue, SizeOf(AValue));
    TOTIM := AValue;
    NVAL := 1;
    if ITYPE = 5 then
    begin
      AFile.Read(NVAL, SizeOf(NVAL));
      if NVAL > 1 then
      begin
        for Index := 2 to NVAL do
        begin
          AFile.Read(CTMP, SizeOf(CTMP));
        end;
      end;
    end;
    if (ITYPE = 2) or (ITYPE = 5) then
    begin
      AFile.Read(NLIST, SizeOf(NLIST));
    end;
  end;
  case ITYPE of
    0,1: // full 3D array
      begin
        if ReadList then
        begin
          CellList.Capacity := Abs(NLAY)*NROW*NCOL;
          for LayerIndex := 0 to Abs(NLAY) - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AFile.Read(AValue, SizeOf(TModflowFloat));
                ACell := TStructCell.Create;
                CellList.Add(ACell);
                ACell.Layer := LayerIndex+1;
                ACell.Row := RowIndex+1;
                ACell.Column := ColIndex+1;
                ACell.Value := AValue;
              end;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position + SizeOf(TModflowFloat)
            * Abs(NLAY) * NROW * NCOL;
        end;
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
          if ReadList then
          begin
            CellList.Capacity := NLIST;
            SetLength(Values, NVAL);
            for Index := 0 to NLIST - 1 do
            begin
              AFile.Read(ICELL, SizeOf(ICELL));
              for ValIndex := 0 to NVAL - 1 do
              begin
                AFile.Read(Values[ValIndex], SizeOf(TModflowFloat));
              end;
              LayerIndex :=  (ICELL-1) div NRC;
              RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
              ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
              if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                AND (ColIndex < ncol) AND (RowIndex < NROW)
                AND (LayerIndex < Abs(NLAY))) then
              begin
                ACell := TStructCell.Create;
                CellList.Add(ACell);
                ACell.Layer := LayerIndex+1;
                ACell.Row := RowIndex+1;
                ACell.Column := ColIndex+1;
                ACell.Value := Values[0];
              end;
            end;
          end
          else
          begin
            AFile.Position := AFile.Position +
              NLIST * (SizeOf(ICELL) + NVAL*SizeOf(TModflowFloat))
          end;
        end;
      end;
    3: // 1 layer array with layer indicator array
      begin
        if ReadList then
        begin
          CellList.Capacity := NROW * NCOL;
          SetLength(FluxArray, NROW, NCOL);
          SetLength(LayerIndicatorArray, NROW, NCOL);
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(LayerIndicatorArray[RowIndex, ColIndex],
                SizeOf(integer));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(FluxArray[RowIndex, ColIndex],
                SizeOf(TModflowFloat));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              ACell := TStructCell.Create;
              CellList.Add(ACell);
              ACell.Layer := LayerIndicatorArray[RowIndex, ColIndex];
              ACell.Row := RowIndex+1;
              ACell.Column := ColIndex+1;
              ACell.Value := FluxArray[RowIndex, ColIndex];
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NROW * NCOL * (SizeOf(integer) + SizeOf(TModflowFloat))
        end;
      end;
    4: // 1-layer array that defines layer 1.
      begin
        if ReadList then
        begin
          CellList.Capacity := NROW*NCOL;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(AValue, SizeOf(TModflowFloat));

              ACell := TStructCell.Create;
              CellList.Add(ACell);
              ACell.Layer := 1;
              ACell.Row := RowIndex+1;
              ACell.Column := ColIndex+1;
              ACell.Value := AValue;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position + NROW * NCOL * SizeOf(TModflowFloat);
        end;
      end;
    else Assert(False);
  end;
end;


procedure ReadModflowSinglePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  var AuxArray: TAuxArrays;
  HufFormat: boolean;
  ReadArray: Boolean = True);
var
  ITYPE: integer;
  DELT: TModflowFloat;
  NVAL: Integer;
  Index: Integer;
  CTMP: TModflowDesc;
  NLIST: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  FluxArray: TModflowSingleArray;
  LayerIndicatorArray: array of array of integer;
  Values: array of TModflowFloat;
  ICELL: Integer;
  ValIndex: Integer;
  NRC: Integer;
  AValue: TModflowFloat;
  ModelName1: string;
  PackageName1: string;
  ModelName2: string;
  PackageName2: string;
  AuxName: string;
  Value: TModflowFloat;
  AuxValue: TModflowFloat;
  N1: Integer;
  N2: Integer;
  AuxVarIndex: Integer;
  NumVariable: Integer;
  NodeIndex: Integer;
  procedure ReadModflow6Name(var AName: string);
  var
    NameArray: TModflowDesc;
  begin
    AFile.Read(NameArray, SizeOf(NameArray));
    AName := string(NameArray);
  end;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
  if ReadArray then
  begin
    SetLength(AnArray, Abs(NLAY), NROW, NCOL);
    for LayerIndex := 0 to Abs(NLAY) - 1 do
    begin
      for RowIndex := 0 to NROW - 1 do
      begin
        for ColIndex := 0 to NCOL - 1 do
        begin
          AnArray[LayerIndex, RowIndex, ColIndex] := 0;
        end;
      end;
    end;
  end;
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if (NLAY < 0) or HufFormat then
  begin
    AFile.Read(ITYPE, SizeOf(ITYPE));
    AFile.Read(DELT, SizeOf(DELT));
    AFile.Read(AValue, SizeOf(AValue));
    PERTIM := AValue;
    AFile.Read(AValue, SizeOf(AValue));
    TOTIM := AValue;
    NVAL := 1;
    if ITYPE = 5 then
    begin
      AFile.Read(NVAL, SizeOf(NVAL));
      if NVAL > 1 then
      begin
        for Index := 2 to NVAL do
        begin
          AFile.Read(CTMP, SizeOf(CTMP));
        end;
      end;
    end;
    if (ITYPE = 2) or (ITYPE = 5) then
    begin
      AFile.Read(NLIST, SizeOf(NLIST));
    end;
  end;
  case ITYPE of
    0,1: // full 3D array
      begin
        if ReadArray then
        begin
          for LayerIndex := 0 to Abs(NLAY) - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AFile.Read(AValue, SizeOf(TModflowFloat));
                AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
              end;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position + SizeOf(TModflowFloat)
            * Abs(NLAY) * NROW * NCOL;
        end;
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
          if ReadArray then
          begin
            SetLength(Values, NVAL);
            for Index := 0 to NLIST - 1 do
            begin
              AFile.Read(ICELL, SizeOf(ICELL));
              for ValIndex := 0 to NVAL - 1 do
              begin
                AFile.Read(Values[ValIndex], SizeOf(TModflowFloat));
              end;
              LayerIndex :=  (ICELL-1) div NRC;
              RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
              ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
              if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                AND (ColIndex < ncol) AND (RowIndex < NROW)
                AND (LayerIndex < Abs(NLAY))) then
              begin
                AnArray[LayerIndex, RowIndex, ColIndex] :=
                  AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
              end;
            end;
          end
          else
          begin
            AFile.Position := AFile.Position +
              NLIST * (SizeOf(ICELL) + NVAL*SizeOf(TModflowFloat))
          end;
        end;
      end;
    3: // 1 layer array with layer indicator array
      begin
        if ReadArray then
        begin
          SetLength(FluxArray, NROW, NCOL);
          SetLength(LayerIndicatorArray, NROW, NCOL);
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(LayerIndicatorArray[RowIndex, ColIndex],
                SizeOf(integer));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(FluxArray[RowIndex, ColIndex],
                SizeOf(TModflowFloat));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AnArray[LayerIndicatorArray[RowIndex, ColIndex]-1,
                RowIndex, ColIndex] := FluxArray[RowIndex, ColIndex];
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NROW * NCOL * (SizeOf(integer) + SizeOf(TModflowFloat))
        end;
      end;
    4: // 1-layer array that defines layer 1.
      begin
        if ReadArray then
        begin
          for LayerIndex := 1 to Abs(NLAY) - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AnArray[LayerIndex, RowIndex, ColIndex] := 0;
              end;
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(AValue, SizeOf(TModflowFloat));
              AnArray[0, RowIndex, ColIndex] := AValue;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position + NROW * NCOL * SizeOf(TModflowFloat);
        end;
      end;
    6:
      begin
        ReadModflow6Name(ModelName1);
        ReadModflow6Name(PackageName1);
        ReadModflow6Name(ModelName2);
        ReadModflow6Name(PackageName2);
        // NumVariable is the number of auxilliary variables + 1.
        AFile.Read(NumVariable, SizeOf(NumVariable));
        Assert( (NumVariable >= 0) and (NumVariable <= 20));
        if ReadArray then
        begin
          SetLength(AuxArray, NumVariable - 1);
        end;
        for AuxVarIndex := 1 to NumVariable - 1 do
        begin
          ReadModflow6Name(AuxName);
          if ReadArray then
          begin
            AuxArray[AuxVarIndex-1].Name := AuxName;
          end;
        end;
        AFile.Read(NLIST, SizeOf(NLIST));

        if ReadArray then
        begin
          NRC := NROW*NCOL;
          for AuxVarIndex := 1 to NumVariable - 1 do
          begin
            SetLength(AuxArray[AuxVarIndex-1].Values, Abs(NLAY), NROW, NCOL);
            for LayerIndex := 0 to Abs(NLAY) - 1 do
            begin
              for RowIndex := 0 to NROW - 1 do
              begin
                for ColIndex := 0 to NCOL - 1 do
                begin
                  AuxArray[AuxVarIndex-1].Values[LayerIndex, RowIndex, ColIndex] := 0;
                end;
              end;
            end;
          end;
          for NodeIndex := 0 to NLIST - 1 do
          begin
            AFile.Read(N1, SizeOf(N1));
            AFile.Read(N2, SizeOf(N2));
            AFile.Read(Value, SizeOf(Value));
            LayerIndex :=  (N1-1) div NRC;
            RowIndex := ( (N1 - LayerIndex*NRC)-1 ) div NCOL;
            ColIndex := N1 - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
            if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
              AND (ColIndex < ncol) AND (RowIndex < NROW)
              AND (LayerIndex < Abs(NLAY))) then
            begin
              AnArray[LayerIndex, RowIndex, ColIndex] :=
                AnArray[LayerIndex, RowIndex, ColIndex] + Value;
            end
            else
            begin
              Assert(False);
              Exit;
            end;
            for AuxVarIndex := 1 to NumVariable - 1 do
            begin
              AFile.Read(AuxValue, SizeOf(AuxValue));
              AuxArray[AuxVarIndex-1].Values[LayerIndex, RowIndex, ColIndex] :=
                AuxArray[AuxVarIndex-1].Values[LayerIndex, RowIndex, ColIndex] + AuxValue;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NLIST * (SizeOf(N1) + SizeOf(2) + (NumVariable * SizeOf(TModflowFloat)));
        end;
      end
    else Assert(False);
  end;
end;

procedure ReadModflowDoublePrecFluxList(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  CellList: TStructuredCellList;
  HufFormat: boolean;
  ReadList: Boolean = True
  );
var
  ITYPE: integer;
  DELT: TModflowDouble;
  NVAL: Integer;
  Index: Integer;
  CTMP: TModflowDesc;
  NLIST: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  FluxArray: TModflowDoubleArray;
  LayerIndicatorArray: array of array of integer;
  Values: array of TModflowDouble;
  ICELL: Integer;
  ValIndex: Integer;
  NRC: Integer;
  AValue: TModflowDouble;
  ACell: TStructCell;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
  if ReadList then
  begin
    CellList.Clear;
  end;
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if (NLAY < 0) or HufFormat then
  begin
    AFile.Read(ITYPE, SizeOf(ITYPE));
    AFile.Read(DELT, SizeOf(DELT));
    AFile.Read(AValue, SizeOf(AValue));
    PERTIM := AValue;
    AFile.Read(AValue, SizeOf(AValue));
    TOTIM := AValue;
    NVAL := 1;
    if ITYPE = 5 then
    begin
      AFile.Read(NVAL, SizeOf(NVAL));
      if NVAL > 1 then
      begin
        for Index := 2 to NVAL do
        begin
          AFile.Read(CTMP, SizeOf(CTMP));
        end;
      end;
    end;
    if (ITYPE = 2) or (ITYPE = 5) then
    begin
      AFile.Read(NLIST, SizeOf(NLIST));
    end;
  end;
  case ITYPE of
    0,1: // full 3D array
      begin
        if ReadList then
        begin
          CellList.Capacity := Abs(NLAY) * NROW * NCOL;
          for LayerIndex := 0 to Abs(NLAY) - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AFile.Read(AValue, SizeOf(TModflowDouble));
                ACell := TStructCell.Create;
                CellList.Add(ACell);
                ACell.Layer := LayerIndex+1;
                ACell.Row := RowIndex+1;
                ACell.Column := ColIndex+1;
                ACell.Value := AValue;
              end;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            Abs(NLAY) * NROW * NCOL * SizeOf(TModflowDouble);
        end;
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
          if ReadList then
          begin
            CellList.Capacity := NLIST;
            SetLength(Values, NVAL);
            for Index := 0 to NLIST - 1 do
            begin
              AFile.Read(ICELL, SizeOf(ICELL));
              for ValIndex := 0 to NVAL - 1 do
              begin
                AFile.Read(Values[ValIndex], SizeOf(TModflowDouble));
              end;
              LayerIndex :=  (ICELL-1) div NRC;
              RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
              ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
              if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                AND (ColIndex < ncol) AND (RowIndex < NROW)
                AND (LayerIndex < Abs(NLAY))) then
              begin
                ACell := TStructCell.Create;
                CellList.Add(ACell);
                ACell.Layer := LayerIndex+1;
                ACell.Row := RowIndex+1;
                ACell.Column := ColIndex+1;
                ACell.Value := Values[0];
              end;
            end;
          end
          else
          begin
            AFile.Position := AFile.Position +
              NLIST * (SizeOf(ICELL) + NVAL * SizeOf(TModflowDouble));
          end;
        end;
      end;
    3: // 1 layer array with layer indicator array
      begin
        if ReadList then
        begin
          CellList.Capacity := NROW * NCOL;
          SetLength(FluxArray, NROW, NCOL);
          SetLength(LayerIndicatorArray, NROW, NCOL);
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(LayerIndicatorArray[RowIndex, ColIndex],
                SizeOf(integer));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(FluxArray[RowIndex, ColIndex],
                SizeOf(TModflowDouble));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              ACell := TStructCell.Create;
              CellList.Add(ACell);
              ACell.Layer := LayerIndicatorArray[RowIndex, ColIndex];
              ACell.Row := RowIndex+1;
              ACell.Column := ColIndex+1;
              ACell.Value := FluxArray[RowIndex, ColIndex];
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NROW * NCOL * (SizeOf(integer) + SizeOf(TModflowDouble));
        end;
      end;
    4: // 1-layer array that defines layer 1.
      begin
        if ReadList then
        begin
          CellList.Capacity := NROW * NCOL;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(AValue, SizeOf(TModflowDouble));

              ACell := TStructCell.Create;
              CellList.Add(ACell);
              ACell.Layer := 1;
              ACell.Row := RowIndex+1;
              ACell.Column := ColIndex+1;
              ACell.Value := AValue;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NROW * NCOL * SizeOf(TModflowDouble);
        end;
      end;
    else Assert(False);
  end;
end;

procedure ReadModflowDoublePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  var AuxArray: TAuxArrays;
  HufFormat: boolean;
  ReadArray: Boolean = True);
var
  ITYPE: integer;
  DELT: TModflowDouble;
  NVAL: Integer;
  Index: Integer;
  CTMP: TModflowDesc;
  NLIST: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  FluxArray: TModflowDoubleArray;
  LayerIndicatorArray: array of array of integer;
  Values: array of TModflowDouble;
  ICELL: Integer;
  ValIndex: Integer;
  NRC: Integer;
  AValue: TModflowDouble;
  ModelName1: string;
  ModelName2: string;
  PackageName1: string;
  PackageName2: string;
  AuxName: string;
  NumVariable: Integer;
  N1: integer;
  N2: integer;
  Value: TModflowDouble;
  AuxValue: TModflowDouble;
  AuxVarIndex: Integer;
  NodeIndex: Integer;
  procedure ReadModflow6Name(var AName: string);
  var
    NameArray: TModflowDesc;
  begin
    AFile.Read(NameArray, SizeOf(NameArray));
    AName := string(NameArray);
  end;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
  if ReadArray then
  begin
    SetLength(AnArray, Abs(NLAY), NROW, NCOL);
    for LayerIndex := 0 to Abs(NLAY) - 1 do
    begin
      for RowIndex := 0 to NROW - 1 do
      begin
        for ColIndex := 0 to NCOL - 1 do
        begin
          AnArray[LayerIndex, RowIndex, ColIndex] := 0;
        end;
      end;
    end;
  end;
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if (NLAY < 0) or HufFormat then
  begin
    AFile.Read(ITYPE, SizeOf(ITYPE));
    AFile.Read(DELT, SizeOf(DELT));
    AFile.Read(AValue, SizeOf(AValue));
    PERTIM := AValue;
    AFile.Read(AValue, SizeOf(AValue));
    TOTIM := AValue;
    NVAL := 1;
    if ITYPE = 5 then
    begin
      AFile.Read(NVAL, SizeOf(NVAL));
      if NVAL > 1 then
      begin
        for Index := 2 to NVAL do
        begin
          AFile.Read(CTMP, SizeOf(CTMP));
        end;
      end;
    end;
    if (ITYPE = 2) or (ITYPE = 5) then
    begin
      AFile.Read(NLIST, SizeOf(NLIST));
    end;
  end;
  case ITYPE of
    0,1: // full 3D array
      begin
        if ReadArray then
        begin
          for LayerIndex := 0 to Abs(NLAY) - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AFile.Read(AValue, SizeOf(TModflowDouble));
                AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
              end;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            Abs(NLAY) * NROW * NCOL * SizeOf(TModflowDouble);
        end;
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
          if ReadArray then
          begin
            SetLength(Values, NVAL);
            for Index := 0 to NLIST - 1 do
            begin
              AFile.Read(ICELL, SizeOf(ICELL));
              for ValIndex := 0 to NVAL - 1 do
              begin
                AFile.Read(Values[ValIndex], SizeOf(TModflowDouble));
              end;
              LayerIndex :=  (ICELL-1) div NRC;
              RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
              ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
              if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                AND (ColIndex < ncol) AND (RowIndex < NROW)
                AND (LayerIndex < Abs(NLAY))) then
              begin
                AnArray[LayerIndex, RowIndex, ColIndex] :=
                  AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
              end;
            end;
          end
          else
          begin
            AFile.Position := AFile.Position +
              NLIST * (SizeOf(ICELL) + NVAL * SizeOf(TModflowDouble));
          end;
        end;
      end;
    3: // 1 layer array with layer indicator array
      begin
        if ReadArray then
        begin
          SetLength(FluxArray, NROW, NCOL);
          SetLength(LayerIndicatorArray, NROW, NCOL);
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(LayerIndicatorArray[RowIndex, ColIndex],
                SizeOf(integer));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(FluxArray[RowIndex, ColIndex],
                SizeOf(TModflowDouble));
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AnArray[LayerIndicatorArray[RowIndex, ColIndex]-1,
                RowIndex, ColIndex] := FluxArray[RowIndex, ColIndex];
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NROW * NCOL * (SizeOf(integer) + SizeOf(TModflowDouble));
        end;
      end;
    4: // 1-layer array that defines layer 1.
      begin
        if ReadArray then
        begin
          for LayerIndex := 1 to Abs(NLAY) - 1 do
          begin
            for RowIndex := 0 to NROW - 1 do
            begin
              for ColIndex := 0 to NCOL - 1 do
              begin
                AnArray[LayerIndex, RowIndex, ColIndex] := 0;
              end;
            end;
          end;
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(AValue, SizeOf(TModflowDouble));
              AnArray[0, RowIndex, ColIndex] := AValue;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NROW * NCOL * SizeOf(TModflowDouble);
        end;
      end;
    6:
      begin
        ReadModflow6Name(ModelName1);
        ReadModflow6Name(PackageName1);
        ReadModflow6Name(ModelName2);
        ReadModflow6Name(PackageName2);
        // NumVariable is the number of auxilliary variables + 1.
        AFile.Read(NumVariable, SizeOf(NumVariable));
        Assert( (NumVariable >= 0) and (NumVariable <= 20));
        if ReadArray then
        begin
          SetLength(AuxArray, NumVariable - 1);
        end;
        for AuxVarIndex := 1 to NumVariable - 1 do
        begin
          ReadModflow6Name(AuxName);
          if ReadArray then
          begin
            AuxArray[AuxVarIndex-1].Name := AuxName;
          end;
        end;
        AFile.Read(NLIST, SizeOf(NLIST));

        if ReadArray then
        begin
          NRC := NROW*NCOL;
          for AuxVarIndex := 1 to NumVariable - 1 do
          begin
            SetLength(AuxArray[AuxVarIndex-1].Values, Abs(NLAY), NROW, NCOL);
            for LayerIndex := 0 to Abs(NLAY) - 1 do
            begin
              for RowIndex := 0 to NROW - 1 do
              begin
                for ColIndex := 0 to NCOL - 1 do
                begin
                  AuxArray[AuxVarIndex-1].Values[LayerIndex, RowIndex, ColIndex] := 0;
                end;
              end;
            end;
          end;
          for NodeIndex := 0 to NLIST - 1 do
          begin
            AFile.Read(N1, SizeOf(N1));
            AFile.Read(N2, SizeOf(N2));
            AFile.Read(Value, SizeOf(Value));
            LayerIndex :=  (N1-1) div NRC;
            RowIndex := ( (N1 - LayerIndex*NRC)-1 ) div NCOL;
            ColIndex := N1 - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
            if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
              AND (ColIndex < ncol) AND (RowIndex < NROW)
              AND (LayerIndex < Abs(NLAY))) then
            begin
              AnArray[LayerIndex, RowIndex, ColIndex] :=
                AnArray[LayerIndex, RowIndex, ColIndex] + Value;
            end
            else
            begin
              Assert(False);
              Exit;
            end;
            for AuxVarIndex := 1 to NumVariable - 1 do
            begin
              AFile.Read(AuxValue, SizeOf(AuxValue));
              AuxArray[AuxVarIndex-1].Values[LayerIndex, RowIndex, ColIndex] := 
                AuxArray[AuxVarIndex-1].Values[LayerIndex, RowIndex, ColIndex] + AuxValue;
            end;
          end;
        end
        else
        begin
          AFile.Position := AFile.Position +
            NLIST * (SizeOf(N1) + SizeOf(N2) + (NumVariable * SizeOf(TModflowDouble)));
        end;
      end
    else Assert(False);
  end;
end;

procedure ReadModflowAsciiRealArray(F: TFileVariable; var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc2;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: boolean = True);
var
  ColIndex: Integer;
  RowIndex: Integer;
  Splitter: TStringList;
  index: Integer;
  ALine: string;
begin
  Read(F.AFile, KSTP);
  Read(F.AFile, KPER);
  Read(F.AFile, PERTIM);
  Read(F.AFile, TOTIM);
  Read(F.AFile, DESC);
  Read(F.AFile, NCOL);
  Read(F.AFile, NROW);
  Read(F.AFile, ILAY);
  ReadLn(F.AFile);
  Splitter := TStringList.Create;
  try
    index := 0;
    Splitter.Clear;
    if ReadArray then
    begin
      SetLength(AnArray, NROW, NCOL);
    end
    else
    begin
      SetLength(AnArray,0,0)
    end;
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        if Eof(F.AFile) and (index >= Splitter.Count) then
        begin
          raise EEndOfModflowFileError.Create(StrTheMODFLOWResults);
        end;
        if (index >= Splitter.Count) then
        begin
          Readln(F.AFile, ALine);
          Splitter.CommaText := ALine;
          index := 0;
        end;
        if ReadArray then
        begin
          AnArray[RowIndex, ColIndex] := FortranStrToFloat(Splitter[index]);
        end;
        Inc(index);
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

{ THydModData }

constructor THydModData.Create;
begin
  FLabels := TStringList.Create;
end;

destructor THydModData.Destroy;
begin
  FLabels.Free;
  inherited;
end;

function THydModData.GetLabel(Index: integer): string;
begin
  result := FLabels[Index];
end;

function THydModData.GetLabelCount: integer;
begin
  result := FLabels.Count;
end;

function THydModData.GetTime(Index: Integer): double;
begin
  result := FTimes[Index]
end;

function THydModData.GetTimeCount: integer;
begin
  result := Length(FTimes);
end;

function THydModData.GetValue(LabelIndex, TimeIndex: integer): TModflowDouble;
begin
  result := FValues[LabelIndex, TimeIndex];
end;

function THydModData.IndexOfLabel(const ALabel: string): integer;
begin
  result := FLabels.IndexOf(ALabel);
end;

procedure THydModData.ReadFile(const FileName: string);
var
  FileStream: TFileStream;
  NHYDTOT: integer;
  Precision: TModflowPrecision;
  TimeLabel: Array[0..3] of AnsiChar;
  HydLabel:Array[0..19] of AnsiChar;
  Index: Integer;
  RecordLength: Integer;
  RecordCount: integer;
  TimeIndex: Integer;
  HydIndex: Integer;
  ASingle: TModflowFloat;
  ADouble: TModflowDouble;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite,
    fmShareDenyWrite);
  try
    FileStream.Read(NHYDTOT, SizeOf(NHYDTOT));
    if NHYDTOT < 0 then
    begin
      Precision := mpDouble;
      NHYDTOT := -NHYDTOT;
    end
    else
    begin
      Precision := mpSingle;
    end;
    FLabels.Capacity := NHYDTOT;
    FileStream.Read(FTimeUnit, SizeOf(FTimeUnit));
    FileStream.Read(TimeLabel, SizeOf(TimeLabel));
    for Index := 0 to NHYDTOT - 1 do
    begin
      FileStream.Read(HydLabel, SizeOf(HydLabel));
      FLabels.Add(Trim(string(HydLabel)));
    end;

    RecordLength := 0;
    case Precision of
      mpSingle: RecordLength := (1+FLabels.Count)*SizeOf(TModflowFloat);
      mpDouble: RecordLength := (1+FLabels.Count)*SizeOf(TModflowDouble);
      else Assert(False);
    end;
    RecordCount := (FileStream.Size - FileStream.Position) div RecordLength;
    SetLength(FValues, FLabels.Count, RecordCount);
    SetLength(FTimes, RecordCount);
    for TimeIndex := 0 to RecordCount - 1 do
    begin
      case Precision of
        mpSingle:
          begin
            FileStream.Read(ASingle, SizeOf(ASingle));
            FTimes[TimeIndex] := ASingle;
            for HydIndex := 0 to FLabels.Count - 1 do
            begin
              FileStream.Read(ASingle, SizeOf(ASingle));
              FValues[HydIndex, TimeIndex] := ASingle;
            end;
          end;
        mpDouble:
          begin
            FileStream.Read(ADouble, SizeOf(ADouble));
            FTimes[TimeIndex] := ADouble;
            for HydIndex := 0 to FLabels.Count - 1 do
            begin
              FileStream.Read(ADouble, SizeOf(ADouble));
              FValues[HydIndex, TimeIndex] := ADouble;
            end;
          end;
      end;
    end;

  finally
    FileStream.Free;
  end;
end;

end.
