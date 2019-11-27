unit ReadModflowArrayUnit;

interface

uses SysUtils, Classes;

type
  TModflowFloat = single;
  TModflowDouble = double;
  TModflowDesc = Array[0..15] of Char;
  TModflowDesc2 = Array[0..17] of Char;
  TModflowSingleArray = array of array of TModflowFloat;
  TModflowDoubleArray = array of array of TModflowDouble;
  T3DTModflowArray = array of TModflowDoubleArray;

  TModflowPrecision = (mpSingle, mpDouble);

  EPrecisionReadError = class(Exception);

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

  TAuxArray = record
    Name: string;
    Values: T3DTModflowArray;
  end;
  TAuxArrays = array of TAuxArray;


procedure ReadSinglePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, ILAY: Integer;
  var AnArray: TModflowDoubleArray);

procedure ReadDoublePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);

procedure ReadModflowAsciiRealArray(F: TFileVariable;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc2; var NCOL, NROW, ILAY: Integer;
  var AnArray: TModflowDoubleArray);

procedure ReadModflowSinglePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  var AnArray: T3DTModflowArray; var IRESULT: integer);

procedure ReadModflowDoublePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  var IRESULT: integer);

procedure ReadSinglePrecisionMt3dmsBinaryRealArray(AFile: TFileStream;
  var NTRANS, KSTP, KPER: Integer;
  var TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray;
  ReadArray: Boolean = True);

function CheckArrayPrecision(AFile: TFileStream): TModflowPrecision;
function CheckBudgetPrecision(AFile: TFileStream): TModflowPrecision;

implementation

resourcestring
  StrUnableToReadFile = 'Unable to read file. Check that the file is an unstructured, non-formatted file. In MODFLOW-2005, this is determined in OPENSPEC.inc';

function ValidDescription(const Description: string): boolean;
begin
  result := (Description = '            HEAD')
    or (Description = '        DRAWDOWN')
    or (Description = '      SUBSIDENCE')
    or (Description = '      COMPACTION')
    or (Description = '   CRITICAL HEAD')
    or (Description = '     HEAD IN HGU')
    or (Description = '      SUBSIDENCE')
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
    or (Description = 'HEAD            ');
end;

function CheckArrayPrecision(AFile: TFileStream): TModflowPrecision;
var
  KSTP, KPER: Integer;
  PERTIM, TOTIM: TModflowFloat;
  DESC: TModflowDesc;
  Description : string;
  PERTIM_Double: TModflowDouble;
  TOTIM_Double: TModflowDouble;
begin
  Assert(AFile.Position = 0);
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM, SizeOf(PERTIM));
  AFile.Read(TOTIM, SizeOf(TOTIM));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  if ValidDescription(Description) then
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
        if ValidDescription(Description) then
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
    mpDouble:
      begin
        if not ValidDescription(Description) then
        begin
          raise EPrecisionReadError.Create(StrUnableToReadFile);
        end;
      end;
  end;
  AFile.Position := 0;
end;

function CheckBudgetPrecision(AFile: TFileStream): TModflowPrecision;
var
  Description: string;
  FirstDescription: string;
  SecondDescription: string;
  function ReadSingleArray: boolean;
  var
    KSTP, KPER: Integer;
    DESC: TModflowDesc;
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
//    FluxArray: TModflowSingleArray;
//    LayerIndicatorArray: array of array of integer;
    AValue: TModflowFloat;
    NewPosition: Int64;
  begin
    result := True;
    try
      AFile.Read(KSTP, SizeOf(KSTP));
      AFile.Read(KPER, SizeOf(KPER));
      AFile.Read(DESC, SizeOf(DESC));
      Description := DESC;
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
      except on ERangeError do
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
      if NLAY < 0 then
      begin
        AFile.Read(ITYPE, SizeOf(ITYPE));
        AFile.Read(DELT, SizeOf(DELT));
        AFile.Read(PERTIM, SizeOf(PERTIM));
        AFile.Read(TOTIM, SizeOf(TOTIM));
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
                  AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
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
//                  result := False;
//                  Exit;
                end;
              end;
            end;
          end;
        else
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
      end;
    except 
      result := False;
    end;
  end;
  procedure ReadModflow6Name(var AName: string);
  var
    NameArray: TModflowDesc;
  begin
    AFile.Read(NameArray, SizeOf(NameArray));
    AName := string(NameArray);
  end;
  function ReadDoubleArray: boolean;
  var
    KSTP, KPER: Integer;
    DESC: TModflowDesc;
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
//    FluxArray: TModflowSingleArray;
//    LayerIndicatorArray: array of array of integer;
    AValue: TModflowDouble;
    NewPosition: Int64;
  ModelName1: string;
  ModelName2: string;
  PackageName1: string;
  PackageName2: string;
  AuxName: string;
  NumVariable: Integer;
  AuxVarIndex: Integer;
  AuxArray: TAuxArrays;
  NodeIndex: Integer;
  N1: integer;
  N2: integer;
  Value: TModflowDouble;
  AuxValue: TModflowDouble;
  begin
    result := True;
    try
      AFile.Read(KSTP, SizeOf(KSTP));
      AFile.Read(KPER, SizeOf(KPER));
      AFile.Read(DESC, SizeOf(DESC));
      Description := DESC;
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
      except on ERangeError do
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
      if NLAY < 0 then
      begin
        AFile.Read(ITYPE, SizeOf(ITYPE));
        AFile.Read(DELT, SizeOf(DELT));
        AFile.Read(PERTIM, SizeOf(PERTIM));
        AFile.Read(TOTIM, SizeOf(TOTIM));
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
                  AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
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
            end
            else
            begin
//              result := False;
//              Exit;
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
//        if ReadArray then
        begin
          SetLength(AuxArray, NumVariable - 1);
        end;
        for AuxVarIndex := 1 to NumVariable - 1 do
        begin
          ReadModflow6Name(AuxName);
//          if ReadArray then
          begin
            AuxArray[AuxVarIndex-1].Name := AuxName;
          end;
        end;
        AFile.Read(NLIST, SizeOf(NLIST));

//        if ReadArray then
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
//        end
//        else
//        begin
//          AFile.Position := AFile.Position +
//            NLIST * (SizeOf(N1) + SizeOf(N2) + (NumVariable * SizeOf(TModflowDouble)));
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
  const ErrorMessage = 'The file is not a valid unstructured, non-formatted, cell-by-cell flow file';
begin
  try
    result := mpSingle;
    Assert(AFile.Position= 0);
    if ReadDoubleArray then
    begin
      FirstDescription := Description;
      if AFile.Position = AFile.Size then
      begin
        result := mpDouble;
        Exit;
      end;
      if ReadDoubleArray then
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
        else if (FirstDescription = '          STO-SS')
          and (SecondDescription = '    FLOW-JA-FACE') then
        begin
          result := mpDouble;
        end
        else if (FirstDescription = '    FLOW-JA-FACE')
          {and (SecondDescription = '      DATA-SPDIS')} then
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
      if ReadSingleArray then
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
        else if (FirstDescription = '          STO-SS')
          and (SecondDescription = '    FLOW-JA-FACE') then
        begin
          if (result <> mpSingle) then
          begin
            raise EPrecisionReadError.Create(StrUnableToReadFile);
          end;
        end
        else
        begin
          if (result <> mpSingle) then
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
      Assert(result = mpDouble, ErrorMessage);
      end;
    end;
  finally
    AFile.Position := 0;
  end;
end;


procedure ReadSinglePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);
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
  SetLength(AnArray, NROW, NCOL);
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      AFile.Read(AValue, SizeOf(TModflowFloat));
      AnArray[RowIndex, ColIndex] := AValue;
    end;
  end;
end;

procedure ReadDoublePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);
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
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      AFile.Read(AnArray[RowIndex, ColIndex], SizeOf(TModflowDouble));
    end;
  end;
end;

procedure ReadModflowSinglePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  var IRESULT: integer);
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
begin
  IRESULT := 0;
  if AFile.Position = AFile.Size then
  begin
    IRESULT := 1;
    Exit;
  end;

  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
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
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if NLAY < 0 then
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
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
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
        end;
      end;
    3: // 1 layer array with layer indicator array
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
      end;
    4: // 1-layer array that defines layer 1.
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
      end;
    else Assert(False);
  end;
end;

procedure ReadModflowDoublePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  var IRESULT: integer);
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
  AuxVarIndex: Integer;
  AuxArray: TAuxArrays;
  NodeIndex: Integer;
  N1: integer;
  N2: integer;
  Value: TModflowDouble;
  AuxValue: TModflowDouble;
  procedure ReadModflow6Name(var AName: string);
  var
    NameArray: TModflowDesc;
  begin
    AFile.Read(NameArray, SizeOf(NameArray));
    AName := string(NameArray);
  end;
begin
  IRESULT := 0;
  if AFile.Position = AFile.Size then
  begin
    IRESULT := 1;
    Exit;
  end;
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
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
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if NLAY < 0 then
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
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
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
        end;
      end;
    3: // 1 layer array with layer indicator array
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
      end;
    4: // 1-layer array that defines layer 1.
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
//        if ReadArray then
        begin
          SetLength(AuxArray, NumVariable - 1);
        end;
        for AuxVarIndex := 1 to NumVariable - 1 do
        begin
          ReadModflow6Name(AuxName);
//          if ReadArray then
          begin
            AuxArray[AuxVarIndex-1].Name := AuxName;
          end;
        end;
        AFile.Read(NLIST, SizeOf(NLIST));

//        if ReadArray then
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
//        end
//        else
//        begin
//          AFile.Position := AFile.Position +
//            NLIST * (SizeOf(N1) + SizeOf(N2) + (NumVariable * SizeOf(TModflowDouble)));
        end;
      end
    else Assert(False);
  end;
end;

procedure ReadModflowAsciiRealArray(F: TFileVariable; var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc2;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);
var
  ColIndex: Integer;
  RowIndex: Integer;
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
  SetLength(AnArray, NROW, NCOL);
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      Read(F.AFile, AnArray[RowIndex, ColIndex]);
    end;
  end;
  ReadLn(F.AFile);
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

function THydModData.GetValue(LabelIndex,
  TimeIndex: integer): TModflowDouble;
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
  end;
end;

end.
