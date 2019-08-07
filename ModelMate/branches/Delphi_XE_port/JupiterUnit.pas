unit JupiterUnit;
interface

  uses
    Classes, Dialogs, SysUtils,
    GlobalBasicData, GlobalTypesUnit, Utilities, Windows;

  type

    { Enumerations }
    TBlockFormat = (bfKeywords, bfTable, bfFiles);
    TValueType = (vtBool, vtDbl, vtInt, vtStr);
    { Classes }
    TKeyVal = class(TPersistent)
    { TKeyVal is a class that stores one column (one array or one scalar value)
      of data (of any supported type) for an input block.
      Supported types include: Boolean, Double, LongInt, and String. }
      private
        fName: string; // Keyword or column name
        fNRow: LongInt; // Number of values and array dimension
        fVtype: TValueType;
        fVboolean: array of Boolean;
        fVdouble: array of Double;
        fVinteger: array of LongInt;
        fVstring: array of String;
        procedure setNRow(N: LongInt);
        procedure setName(Str: String);
      published
        property Name: string read fName write setName; // Keyword or Column name
        property NRow: LongInt read fNRow write setNRow;
        property Vtype: TValueType read fVtype write fVtype;
      public
        constructor Create;
        constructor CreateAndAllocate(Nam: string;
                        NR: LongInt; ValType: TValueType);
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure SetNameVal(Index: LongInt; NamStr: string; B: Boolean); overload;
        procedure SetNameVal(Index: LongInt; NamStr: string; D: Double); overload;
        procedure SetNameVal(Index: LongInt; NamStr: string; I: LongInt); overload;
        procedure SetNameVal(Index: LongInt; NamStr: string; S: String); overload;
        procedure SetVal(Index: LongInt; B: Boolean); overload;
        procedure SetVal(Index: LongInt; D: Double); overload;
        procedure SetVal(Index: LongInt; I: LongInt); overload;
        procedure SetVal(Index: LongInt; S: String); overload;
        function GetBool(Index: LongInt): Boolean;
        function GetDbl(Index: LongInt): Double;
        function GetInt(Index: LongInt): LongInt;
        function GetStr(Index: LongInt): String;
    end;

    TKeyValMat = array of TKeyVal;

    TBlockData = class(TPersistent)
      private
        fNCol: LongInt;
        fKeyValMatrix: TKeyValMat;
        procedure setNCol(NKV: LongInt);
      published
        property NCol: LongInt read fNCol write setNCol;
        property KeyValMatrix: TKeyValMat read fKeyValMatrix write fKeyValMatrix;
      public
        procedure Assign(Source: TPersistent); override;
        constructor Create;
        constructor CreateAndAllocate(NR: LongInt; NC: LongInt);
        destructor Destroy; override;
    end;

    TKeyItem = record
      BLabel: string40;  // Blocklabel
      KItem: string40;   // Keyitem
    end;

    TModelIOPair = class(TCollectionItem)
      // Class stores one pair of file names used by the Model_IO
      // module of JUPITER API.
      private
        fModelFile: TFileName;
        fAppFile: TFileName;
        procedure SetModelFile(const Value: TFileName);
        procedure SetAppFile(const Value: TFileName);
      public
        function AbsAppFile: string;
        function AbsModelFile: string;
        procedure Assign(Source: TPersistent); override;
      published
        property ModelFile: TFileName read fModelFile write SetModelFile;
        property AppFile: TFileName read fAppFile write SetAppFile;
    end;

    TModelIOPairs = class(TCollection)
      // Class stores data used to populate the Model_Input_Files and
      // Model_Output_Files input blocks.
      private
        function GetItem(Index: integer): TModelIOPair;
        procedure SetItem(Index: integer; const Value: TModelIOPair);
      public
        property Items[I: integer]: TModelIOPair
                     read GetItem write SetItem; default;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent);  override;
    end;

    TProjectFile = class(TCollectionItem)
      // Class stores one file name, stored as a relative path
      // but returned as an absolute path.
      private
        fFile: TFileName;
        procedure SetFile(const Value: TFileName);
        function GetFile: TFileName;
        function GetRelPath: TFileName;
      public
        procedure Assign(Source: TPersistent); override;
        function SameAs(Source: TPersistent): boolean;
        property FileName: TFileName read GetFile write SetFile;
      published
        property RelFileName: TFileName read GetRelPath write SetFile;
    end;

    TRunnerFiles = class(TCollection)
      // Class stores files to be copied into runner directories
      // before a parallel run is made.
      private
        function GetItem(Index: integer): TProjectFile;
        procedure SetItem(Index: integer; const Value: TProjectFile);
      public
        property Items[I: integer]: TProjectFile
                    read GetItem write SetItem; default;
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        function SameAs(Source: TPersistent): boolean;
    end;

    TModelInfo = class
      // If fields are added, modify function NumFields.
      fName: string;
      fLengthUnit: string;
      fMassUnit: string;
      fTimeUnit: string;
      fNumEstParams: integer;
      fOrigNumEstParams: integer;
      fTotalNumParams: integer;
      fNumObsIncluded: integer;
      fNumObsProvided: integer;
      fNumPrior: integer;
      fRegConv: boolean;
      fCalcErrVar: double;
      fStdErrReg: double;
      fMLOFD: double;
      fMLOFDP: double;
      fAICc: double;
      fBIC: double;
      fHQ: double;
      fKashyap: double;
      fLnDeterFisherInfoMx: double;
      fRN2Dep: double;
      fRN2DepPri: double;
      fNumIter: integer;
    public
      function NumFields: integer; virtual;
    end;

  // Public functions and procedures
  procedure J_BuildInputBlock(BlockLabel: string; BlockFormat: TBlockFormat;
                         BlockData: TBlockData; IBlock: TStringList);
  procedure J_BuildInputBlockExclDef(BlockLabel: string;
               BlockData: TBlockData; Defaults: TBlockData; IBlock: TStringList;
               Required: boolean);
  procedure J_Free;
  procedure J_Initialize;
  procedure J_KeyItem(const BlockLabel: string; var KeyItem: string; var Index: integer);
//  function J_Valid_Name(Name: string): boolean;

  var
    { KeyItems and NKeyItems are public to allow other units
      (e.g. UcodeUnit) to add to the array }
    KeyItems: array of TKeyItem;
    NKeyItems: LongInt = 17;
    { Parameter attribute types defined for JUPITER API }
    JupiterPATypes: set of TParamAttType;
    { Dependent attribute types defined for JUPITER API }
    JupiterOATypes: set of TDepAttType;

implementation

//###################################################################

{ TKeyVal }

procedure TKeyVal.Assign(Source: TPersistent);
var
  I: integer;
  KVSource: TKeyVal;
begin
  if Source is TKeyVal then
    begin
      KVSource := Source as TKeyVal;
      fName := KVSource.fName; // use field rather than property to avoid rechecking validity
      NRow := KVSource.NRow;
      VType := KVSource.Vtype;
      setlength(fVstring,NRow);
      for I := 0 to NRow - 1 do
        fVstring[I] := KVSource.fVstring[I];
      case VType of
        vtBool:
          begin
            setlength(fVboolean,NRow);
            for I := 0 to NRow - 1 do
              fVBoolean[I] := KVSource.fVboolean[I];
          end;
        vtDbl:
          begin
            setlength(fVdouble,NRow);
            for I := 0 to NRow - 1 do
              fVdouble[I] := KVSource.fVdouble[I];
          end;
        vtInt:
          begin
            setlength(fVinteger,NRow);
            for I := 0 to NRow - 1 do
              fVinteger[I] := KVSource.fVinteger[I];
          end;
        vtStr: ; // do nothing
      end;
    end
  else
    inherited;
end; // procedure TKeyVal.Assign

constructor TKeyVal.Create;
begin
  inherited Create;
  fName := '';
  fNRow := 0;
  fVtype := vtStr;
end;  // constructor TKeyVal.Create

constructor TKeyVal.CreateAndAllocate(Nam: string;
                     NR: Integer; ValType: TValueType);
var
  I: Integer;
begin
  Create;
  fName := Nam;
  fVtype := ValType;
  if NR>0 then
  begin
    case ValType of
      vtInt: begin
        SetLength(fVboolean, 0);
        SetLength(fVdouble, 0);
        SetLength(fVinteger, NR);
        SetLength(fVstring, NR);
      end;
      vtDbl: begin
        SetLength(fVboolean, 0);
        SetLength(fVdouble, NR);
        SetLength(fVinteger, 0);
        SetLength(fVstring, NR);
      end;
      vtBool: begin
        SetLength(fVboolean, NR);
        SetLength(fVdouble, 0);
        SetLength(fVinteger, 0);
        SetLength(fVstring, NR);
      end;
      vtStr: begin
        SetLength(fVboolean, 0);
        SetLength(fVdouble, 0);
        SetLength(fVinteger, 0);
        SetLength(fVstring, NR);
      end;
    end;
    fNRow := NR;
    for I := 0 to NR - 1 do
      begin
        fVstring[I] := '';
      end;
  end;
end; // constructor TKeyVal.CreateAndAllocate

destructor TKeyVal.Destroy;
begin
  if fNRow>0 then
  begin
    case fVtype of
      vtInt: begin
        fVinteger := nil;
        SetLength(fVinteger, 0);
        SetLength(fVstring, 0);
      end;
      vtDbl: begin
        SetLength(fVdouble, 0);
        SetLength(fVstring, 0);
      end;
      vtBool: begin
        SetLength(fVboolean, 0);
        SetLength(fVstring, 0);
      end;
      vtStr: begin
        SetLength(fVstring, 0);
      end;
    end;
    fNRow := 0;
  end;
  inherited;
end; // destructor TKeyVal.Destroy

function TKeyVal.GetBool(Index: Integer): Boolean;
begin
  result := fVboolean[Index];
end;

function TKeyVal.GetDbl(Index: Integer): Double;
begin
  result := fVdouble[Index];
end;

function TKeyVal.GetInt(Index: Integer): LongInt;
begin
  result := fVinteger[Index];
end;

function TKeyVal.GetStr(Index: Integer): string;
begin
  result := fVstring[Index];
end;  // function TKeyVal.GetStr

procedure TKeyVal.setName(Str: String);
//var
//  mess: string;
begin
  fName := Str
end;

procedure TKeyVal.SetNameVal(Index: Integer; NamStr: string; D: Double);
begin
  Name := NamStr;
  SetVal(Index, D);
end;

procedure TKeyVal.SetNameVal(Index: Integer; NamStr: string; B: Boolean);
begin
  Name := NamStr;
  SetVal(Index, B);
end;

procedure TKeyVal.SetNameVal(Index: Integer; NamStr, S: String);
begin
  Name := NamStr;
  SetVal(Index, S);
end;

procedure TKeyVal.SetNameVal(Index: Integer; NamStr: string; I: Integer);
begin
  Name := NamStr;
  SetVal(Index, I);
end;

// function TKeyVal.GetStr

procedure TKeyVal.setNRow(N: LongInt);
begin
  // May want to validate this operation in some way
  fNRow := N;
end;  // procedure TKeyVal.setNval

procedure TKeyVal.SetVal(Index: Integer; D: Double);
// Store a double value
var
  Imax: LongInt;
begin
  Imax := fNRow-1;
  if fVtype <> vtDbl then
  begin
    ShowMessage('Error in TKeyVal.SetVal(double): Wrong value type');
  end;
  if Index<0 then
  begin
    ShowMessage('Error in TKeyVal.SetVal(double): Index < 0');
  end;
  if Index>Imax then
  begin
    ShowMessage('Error in TKeyVal.Setval(double): Index too big');
  end;
  // If array is not allocated, allocate it
  if Length(fVdouble) = 0 then
    SetLength(fVdouble, fNrow);
  fVdouble[Index] := D;
  fVstring[Index] := FreeFormattedReal(D);
end;  // procedure TKeyVal.SetVal (double)

procedure TKeyVal.SetVal(Index, I: Integer);
// Store an integer value
var
  Imax: LongInt;
begin
  Imax := fNRow-1;
  if fVtype <> vtInt then
  begin
    ShowMessage('Error in TKeyVal.SetVal(integer): Wrong value type');
  end;
  if Index<0 then
  begin
    ShowMessage('Error in TKeyVal.SetVal(integer): Index < 0');
  end;
  if Index>Imax then
  begin
    ShowMessage('Error in TKeyVal.Setval(integer): Index too big');
  end;
  // If array is not allocated, allocate it
  if Length(fVinteger) = 0 then
    SetLength(fVinteger, fNrow);
  fVinteger[Index] := I;
  fvstring[Index] := IntToStr(I);
end;  // procedure TKeyVal.SetVal (integer)

procedure TKeyVal.SetVal(Index: Integer; S: String);
// Store a string value
var
  Imax: LongInt;
begin
  Imax := fNRow-1;
  if fVtype <> vtStr then
  begin
    ShowMessage('Error in TKeyVal.SetVal(string): Wrong value type');
  end;
  if Index<0 then
  begin
    ShowMessage('Error in TKeyVal.SetVal(string): Index < 0');
  end;
  if Index>Imax then
  begin
    ShowMessage('Error in TKeyVal.Setval(string): Index too big');
  end;
  fVstring[Index] := S;
end;  // procedure TKeyVal.SetVal (string)

procedure TKeyVal.SetVal(Index: Integer; B: Boolean);
// Store a boolean value
var
  Imax: LongInt;
begin
  Imax := fNRow-1;
  if fVtype <> vtBool then
  begin
    ShowMessage('Error in TKeyVal.SetVal(boolean): Wrong value type');
  end;
  if Index<0 then
  begin
    ShowMessage('Error in TKeyVal.SetVal(boolean): Index < 0');
  end;
  if Index>Imax then
  begin
    ShowMessage('Error in TKeyVal.Setval(boolean): Index too big');
  end;
  // If array is not allocated, allocate it
  if Length(fVboolean) = 0 then
    SetLength(fVboolean, fNrow);
  fVboolean[Index] := B;
  if B then
    fVstring[Index] := 'True'
  else
    fVstring[Index] := 'False';
end;  // procedure TKeyVal.SetVal (boolean)

//###################################################################

{ TBlockData }

procedure TBlockData.Assign(Source: TPersistent);
var
  I: integer;
  BDSource: TBlockData;
begin
  if Source is TBlockData then
    begin
      BDSource := Source as TBlockData;
      NCol := BDSource.NCol;
      if NCol > 0 then
        begin
          for I := 0 to NCol - 1 do
            fKeyValMatrix[I].Assign(BDSource.KeyValMatrix[I]);
        end;
    end;
end;

constructor TBlockData.Create;
begin
  inherited Create;
  fNCol := 0;
end;

constructor TBlockData.CreateAndAllocate(NR: LongInt; NC: LongInt);
var
  ICol: Integer;
begin
  if not Assigned(self) then
    begin
      Create;
    end;
  NCol := NC; // Invokes TBlockData.setNCol
  for ICol := 0 to NCol - 1 do
  begin
    KeyValMatrix[ICol] := TKeyVal.CreateAndAllocate(' ', NR, vtStr);
  end;
end;

destructor TBlockData.Destroy;
var
  I: integer;
begin
  for I := Length(fKeyValMatrix) - 1 downto 0 do
    begin
      fKeyValMatrix[I].Free;
    end;

  inherited;
end;

procedure TBlockData.setNCol(NKV: Integer);
begin
  SetLength(fKeyValMatrix, NKV);
  fNCol := NKV;
end;

//###################################################################

{ TModelIOPair }

function TModelIOPair.AbsAppFile: string;
// Return absolute path for application file
begin
  result := PathToAbsPath(ProjectDirectory, AppFile);
end;

function TModelIOPair.AbsModelFile: string;
// Return absolute path for model file
begin
  result := PathToAbsPath(ProjectDirectory, ModelFile);
end;

procedure TModelIOPair.Assign(Source: TPersistent);
var
  Item: TModelIOPair;
begin
  if Source is TModelIOPair then
    begin
      Item := Source as TModelIOPair;
      ModelFile := Item.ModelFile;
      AppFile := Item.AppFile;
    end
  else
    inherited;
end;

procedure TModelIOPair.SetAppFile(const Value: TFileName);
begin
  fAppFile := RelativePath(Value);
end;

procedure TModelIOPair.SetModelFile(const Value: TFileName);
begin
  fModelFile := RelativePath(Value);
end;

//###################################################################

{ TModelIOPairs }

procedure TModelIOPairs.Assign(Source: TPersistent);
var
  I: integer;
  Item: TModelIOPairs;
begin
  if Source is TModelIOPairs then
    begin
      Item := Source as TModelIOPairs;
      // Clear Items and dimension it to be the same size as Source.Items
      Clear;
      for I := 0 to Item.Count - 1 do
        begin
          Add;
          Items[I].Assign(Item.Items[I]); // Copy TModelIOPair objects
        end;
    end
  else
    inherited;
end;

constructor TModelIOPairs.Create;
begin
  inherited Create(TModelIOPair);
end;

destructor TModelIOPairs.Destroy;
begin
  self.Clear;
  inherited;
end;

function TModelIOPairs.GetItem(Index: integer): TModelIOPair;
begin
  result := TModelIOPair(inherited GetItem(Index));
end;

procedure TModelIOPairs.SetItem(Index: integer; const Value: TModelIOPair);
begin
  inherited SetItem(Index, Value);
end;

//###################################################################

{ Public functions and  procedures}

//###################################################################

procedure J_BuildInputBlock(BlockLabel: string; BlockFormat: TBlockFormat;
                         BlockData: TBlockData; IBlock: TStringList);
var
  NClocal, NRlocal, NRtemp: LongInt;
  I, IKeyItem, IKeyPos, IRow, J: Integer;
  BF, bl, BLlocal, KeyStr, KIlocal, Indent1, Indent2, Line, Str, ValStr: string;
  NCstr, NRstr: string;
begin
  BLlocal := UpperCase(BlockLabel);
  NClocal := BlockData.NCol;
  IKeyItem := -1; // By default, there is no keyitem
  IKeyPos := -1;
  KIlocal := '';
  Indent1 := '  ';
  Indent2 := '  ';
  if (BlockFormat = bfKeywords) or (BlockFormat = bfTable) then
  begin
    // Determine if a KeyItem is defined for BlockLabel
    I := -1;
    J_KeyItem(BLlocal, KIlocal, I);
    if KIlocal <> '' then
      begin
        IKeyItem := I;
        Indent2 := '    ';
      end;
    { If KeyItem is defined, find KeyValMatrix position that
      contains the KeyItem }
    if IKeyItem > -1 then
      begin
        I := 0;
        while (IKeyPos = -1) and (I < NClocal) do
          begin
            Str := UpperCase(BlockData.KeyValMatrix[I].Name);
            if Str = KIlocal then IKeyPos := I;
            I := I+1;
          end;
        if IKeyPos = -1 then
          // KeyItem is required but not in KeyValMatrix
          begin
            Line := 'Error in J_BuildInputBlock: KeyItem not found in KeyValMatrix';
            ShowMessage(Line);
          end;
      end;
  end;
  bl := ' ';
  IBlock.Clear;
  // Build BEGIN line
  case BlockFormat of
    bfKeywords: BF := 'Keywords';
    bfTable: BF := 'Table';
    bfFiles: BF := 'Files';
  end;
  Line := 'BEGIN ' + BlockLabel + bl + BF;
  IBlock.Add(Line);
  //
  // Build Blockbody
  case BlockFormat of
    bfKeywords:
      // Build Blockbody for Keywords format
      begin
        NClocal := BlockData.NCol;
        NRlocal := BlockData.KeyValMatrix[0].NRow;
        // loop through rows in KeyValMatrix
        for IRow := 0 to NRLocal - 1 do
          begin
            if IKeyPos>-1 then
            // Write Keyitem=value first, if there is a Keyitem
            begin
              ValStr := BlockData.KeyValMatrix[IKeyPos].GetStr(IRow);
              if ValStr <> '' then
              begin
                KeyStr := BlockData.KeyValMatrix[IKeyPos].Name;
                Line := Indent1 + KeyStr + ' = ' + QuoteIfNeeded(ValStr);
                IBlock.Add(Line);
              end;
            end;
            //
            for I := 0 to NClocal - 1 do
            // Write Keyword=value for keywords that are not Keyitems
            begin
              if I <> IKeyPos then
              begin
                ValStr := BlockData.KeyValMatrix[I].GetStr(IRow);
                if ValStr <> '' then
                begin
                  KeyStr := BlockData.KeyValMatrix[I].Name;
                  Line := Indent2 + KeyStr + ' = ' + QuoteIfNeeded(ValStr);
                  IBlock.Add(Line);
                end;
              end;
            end;
          end;
      end;
    bfTable:
      // Build Blockbody for Table format
      begin
        NClocal := BlockData.NCol;
        NRlocal := BlockData.KeyValMatrix[0].NRow;
        // Ensure that all columns have the same number of rows
        if NClocal > 1 then
        for I := 1 to NClocal - 1 do
          begin
            NRtemp := BlockData.KeyValMatrix[I].NRow;
            if NRtemp <> NRlocal then
              ShowMessage('Error in J_BuildInputBlock: NRow values differ');
          end;
        NRstr := IntToStr(NRlocal);
        NCstr := IntToStr(NClocal);
        // Build line to define table dimensions.
        Line := Indent1 + 'NROW=' + NRstr + ' NCOL=' + NCstr + ' COLUMNLABELS';
        IBlock.Add(Line);
        // Build line of column labels.
        Line := bl;
        for I := 0 to NClocal - 1 do
          begin
            Line := Line + bl + BlockData.KeyValMatrix[I].Name;
          end;
        IBlock.Add(Line);
        // Build table of values.
        for I := 0 to NRlocal - 1 do
          // Build line (row I)
          begin
            Line := ' ';
            for J := 0 to NClocal - 1 do
              // Append value for column J.
              begin
                Str := BlockData.KeyValMatrix[J].GetStr(I);
                if Str = '' then
                  ShowMessage('Error in J_BuildInputBlock: Table entry is blank');
                Line := Line + bl + QuoteIfNeeded(Str);
              end;
            IBlock.Add(Line);
          end;
      end;
    bfFiles:
      // Build Blockbody for Files format
      begin
        // Do nothing for now.
      end;
  end;
  //
  // Build END line.
  Line := 'END ' + BlockLabel;
  IBlock.Add(Line);
end; // procedure J_BuildInputBlock.

//###################################################################
procedure J_BuildInputBlockExclDef(BlockLabel: string;
               BlockData: TBlockData; Defaults: TBlockData; IBlock: TStringList;
               Required: boolean);
// Write Keyword input block in Keywords format, exluding default values
var
  NClocal, NRlocal: LongInt;
  I, IKeyItem, IKeyPos, IRow, K: Integer;
  BF, bl, BLlocal, DefStr, KeyStr, KIlocal: string;
  Indent1, Indent2, Line, Str, ValStr: string;
begin
  BLlocal := UpperCase(BlockLabel);
  NClocal := BlockData.NCol;
  IKeyItem := -1; // By default, there is no keyitem
  IKeyPos := -1;
  KIlocal := '';
  bl := ' ';
  Indent1 := '  ';
  Indent2 := '  ';
  K := 0;
  // Determine if a KeyItem is defined for BlockLabel
  I := -1;
  J_KeyItem(BLlocal, KIlocal, I);
  if KIlocal <> '' then
    begin
      IKeyItem := I;
      Indent2 := '    ';
    end;
  { If KeyItem is defined, find KeyValMatrix position that
    contains the KeyItem }
  if IKeyItem > -1 then
    begin
      I := 0;
      while (IKeyPos = -1) and (I < NClocal) do
        begin
          Str := UpperCase(BlockData.KeyValMatrix[I].Name);
          if Str = KIlocal then IKeyPos := I;
          I := I+1;
        end;
      if IKeyPos = -1 then
        // KeyItem is required but not in KeyValMatrix
        begin
          Line := 'Error in J_BuildInputBlockExclDef: KeyItem not found in KeyValMatrix';
          ShowMessage(Line);
        end;
    end;
  IBlock.Clear;
  // Build BEGIN line
  BF := 'Keywords';
  Line := 'BEGIN ' + BlockLabel + bl + BF;
  IBlock.Add(Line);
  //
  // Build Blockbody for Keywords format
  NClocal := BlockData.NCol;
  NRlocal := BlockData.KeyValMatrix[0].NRow;
  // loop through rows in KeyValMatrix
  for IRow := 0 to NRLocal - 1 do
    begin
      if IKeyPos>-1 then
        // Write Keyitem=value first, if there is a Keyitem
        begin
          ValStr := BlockData.KeyValMatrix[IKeyPos].GetStr(IRow);
          if ValStr <> '' then
          begin
            KeyStr := BlockData.KeyValMatrix[IKeyPos].Name;
            Line := Indent1 + KeyStr + ' = ' + QuoteIfNeeded(ValStr);
            IBlock.Add(Line);
            K := K + 1;
          end;
        end;
      //
      for I := 0 to NClocal - 1 do
        // Write Keyword=value for keywords that are not Keyitems
        begin
          if I <> IKeyPos then
            begin
              ValStr := BlockData.KeyValMatrix[I].GetStr(IRow);
              DefStr := Defaults.KeyValMatrix[I].GetStr(IRow);
              if (ValStr <> '') and (ValStr <> DefStr) then
                begin
                  KeyStr := BlockData.KeyValMatrix[I].Name;
                  Line := Indent2 + KeyStr + ' = ' + QuoteIfNeeded(ValStr);
                  IBlock.Add(Line);
                  K := K + 1;
                end;
            end;
        end;
    end;
  //
  if (K > 0) or (Required) then
    begin
      if K = 0 then
        begin
          // Insert a meaningless entry so that JUPITER does not fail
          Line := '  All entries = Default (this line is ignored)';
          IBlock.Add(Line);
        end;
      // Build END line
      Line := 'END ' + BlockLabel;
      IBlock.Add(Line);
    end
  else
    // Omit block
    begin
      IBlock.Clear;
      Line := '# Block ' + BlockLabel +
              ' is omitted because defaults are used for all entries';
      IBlock.Add(Line);
    end;
end; // procedure J_BuildInputBlockExclDef

//###################################################################

procedure J_Free;
begin
  SetLength(KeyItems, 0);
end; // procedure J_Free

//###################################################################

procedure J_Initialize;
begin
  JupiterPATypes := [patParamName, patGroupName, patDerived, patTransform,
         patAdjustable, patStartValue, patLowerBound, patUpperBound];
  // KeyItems is populated only for BlockLabels for which a KeyItem is defined.
  SetLength(KeyItems, NKeyItems);
  KeyItems[0].BLabel := 'MODEL_COMMAND_LINES';
  KeyItems[0].KItem := 'COMMAND';
  KeyItems[1].BLabel := 'PARAMETER_GROUPS';
  KeyItems[1].KItem := 'GROUPNAME';
  KeyItems[2].BLabel := 'PARAMETER_DATA';
  KeyItems[2].KItem := 'PARAMNAME';
  KeyItems[3].BLabel := 'PARAMETER_VALUES';
  KeyItems[3].KItem := 'PARAMNAME';
  KeyItems[4].BLabel := 'DERIVED_PARAMETERS';
//  KeyItems[4].KItem := 'PARAMNAME';
  KeyItems[4].KItem := 'DERPARNAME'; // To agree with UCODE documentation.
  KeyItems[5].BLabel := 'OBSERVATION_GROUPS';
  KeyItems[5].KItem := 'GROUPNAME';
  KeyItems[6].BLabel := 'OBSERVATION_DATA';
  KeyItems[6].KItem := 'OBSNAME';
  KeyItems[7].BLabel := 'DERIVED_OBSERVATIONS';
  KeyItems[7].KItem := 'OBSNAME';
  KeyItems[8].BLabel := 'PREDICTION_GROUPS';
  KeyItems[8].KItem := 'GROUPNAME';
  KeyItems[9].BLabel := 'PREDICTION_DATA';
  KeyItems[9].KItem := 'PREDNAME';
  KeyItems[10].BLabel := 'DERIVED_PREDICTIONS';
  KeyItems[10].KItem := 'PREDNAME';
  KeyItems[11].BLabel := 'MATRIX_FILES';
  KeyItems[11].KItem := 'MATRIXFILE';
  KeyItems[12].BLabel := 'PRIOR_INFORMATION_GROUPS';
  KeyItems[12].KItem := 'GROUPNAME';
  KeyItems[13].BLabel := 'LINEAR_PRIOR_INFORMATION';
  KeyItems[13].KItem := 'PRIORNAME';
  KeyItems[14].BLabel := 'MODEL_INPUT_FILES';
  KeyItems[14].KItem := 'MODINFILE';
  KeyItems[15].BLabel := 'MODEL_OUTPUT_FILES';
  KeyItems[15].KItem := 'MODOUTFILE';
  KeyItems[16].BLabel := 'PARALLEL_RUNNERS';
  KeyItems[16].KItem := 'RUNNERNAME';
end; // procedure J_Initialize.

//###################################################################

procedure J_KeyItem(const BlockLabel: string; var KeyItem: string; var Index: integer);
// Given Blocklabel, return KeyItem and Index in list of BlockLabels and KeyItems.
// If no KeyItem is defined for BlockLabel, return KeyItem = '' and Index = -1
var
  I: integer;
  BLupper: string;
begin
  Blupper := UpperCase(BlockLabel);
  KeyItem := '';
  Index := -1;
  I := 0;
  while (I < NKeyItems) and (KeyItem = '') do
    begin
      if KeyItems[I].BLabel = BLupper then
        begin
          KeyItem := KeyItems[I].KItem;
          Index := I;
        end;
      I := I+1;
    end;
end; // procedure J_KeyItem.

//###################################################################

//function J_Valid_Name(Name: string): boolean;
//// Check Name argument for conformance with JUPITER naming convention
//type
//  setChar = set of Char;
//var
//  Ch: Char;
//  I, LenName: integer;
//  Letters, Digits, Symbols, JChars: setChar;
//begin
//  // Initialize character sets
//  Letters := [ 'A' .. 'Z', 'a' .. 'z' ];
//  Digits := [ '0' .. '9' ];
//  Symbols := [ '_', '.', ':', '&', '#', '@' ];
//  JChars := Letters + Digits + Symbols;
//  //
//  result := True;
//  LenName := Length(Name);
//  if LenName > 0 then
//    begin
//      Ch := Name[1];  //bad when LenName=0
//      if Ch in Letters then
//        begin
//          for I := 2 to LenName do
//            begin
//              Ch := Name[I];
//              if not (Ch in JChars) then
//                result := False;
//            end;
//        end
//      else
//        result := False;
//    end
//  else
//    begin
//      result := False;
//    end;
//end; // function J_Valid_Name

//###################################################################

{ TModelInfo }

function TModelInfo.NumFields: integer;
begin
  result := 23;
end;

//###################################################################

{ TRunnerFiles }

procedure TRunnerFiles.Assign(Source: TPersistent);
var
  I: integer;
  Item: TRunnerFiles;
begin
  if Source is TRunnerFiles then
    begin
      Item := Source as TRunnerFiles;
      // Clear Items and dimension it to be the same size as Source.Items
      Clear;
      for I := 0 to Item.Count - 1 do
        begin
          Add;
          Items[I].Assign(Item.Items[I]); // Copy TProjectFile objects.
        end;
    end
  else
    inherited;
end;

constructor TRunnerFiles.Create;
begin
  inherited Create(TProjectFile);
end;

destructor TRunnerFiles.Destroy;
begin
  Clear;
  inherited;
end;

function TRunnerFiles.GetItem(Index: integer): TProjectFile;
begin
  result :=TProjectFile(inherited GetItem(Index));
end;

function TRunnerFiles.SameAs(Source: TPersistent): boolean;
var
  I: integer;
  Item: TRunnerFiles;
begin
  result := True;
  if Source is TRunnerFiles then
    begin
      Item := Source as TRunnerFiles;
      if Count = Item.Count then
        begin
          for I := 0 to Item.Count - 1 do
            begin
              if not Items[I].SameAs(Item.Items[I]) then
                begin
                  result := False;
                  Break;
                end;
            end
        end
      else
        result := False;
    end;
end;

procedure TRunnerFiles.SetItem(Index: integer; const Value: TProjectFile);
begin
  inherited SetItem(Index, Value);
end;

{ TProjectFile }

procedure TProjectFile.Assign(Source: TPersistent);
var
  Item: TProjectFile;
begin
  if Source is TProjectFile then
    begin
      Item := Source as TProjectFile;
      FileName := Item.FileName;
    end
  else
    inherited;
end;

function TProjectFile.GetFile: TFileName;
begin
  result := PathToAbsPath(ProjectDirectory, fFile);
end;

function TProjectFile.GetRelPath: TFileName;
begin
  result := RelativePath(fFile);
end;

function TProjectFile.SameAs(Source: TPersistent): boolean;
var
  PFSource: TProjectFile;
begin
  result := False;
  if Source is TProjectFile then
    begin
      result := True;
      PFSource := Source as TProjectFile;
      if not AnsiSameStr(FileName,PFSource.FileName) then result := False;
    end;
end;

procedure TProjectFile.SetFile(const Value: TFileName);
begin
  fFile := RelativePath(Value);
end;

initialization
  J_Initialize;

finalization
  J_Free;
  
end.
