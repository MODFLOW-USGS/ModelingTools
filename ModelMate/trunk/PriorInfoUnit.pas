unit PriorInfoUnit;
interface

  uses Classes, SysUtils, Dialogs, Windows,
       GlobalTypesUnit, GlobalBasicData, Utilities;

  type

    // Sets.
    TPriAttTypes = set of TPriAttType;

    // Prior information attribute data.
    TPriorAttribute = class(TCollectionItem)
      private
        fPriAttType: TPriAttType;
        fText: string;
      published
        property PriAttType: TPriAttType read fPriAttType
                               write fPriAttType;
        property Text: string read fText write fText;
      public
        destructor Destroy; override;
        function Caption: string;
        function DefaultText: string;
        function Hint: string;
        function ItemType: TItemType;
        procedure Assign(Source: TPersistent); override;
        procedure Initialize(PiAType: TPriAttType);
    end;

    TPriorAttributes = class(TCollection)
      protected
        function GetItem(Index: Integer): TPriorAttribute;
        procedure SetItem(Index: Integer;
                          aPiAttribute: TPriorAttribute);
      public
        property Items[Index: Integer]: TPriorAttribute
             read GetItem write SetItem; default;
        constructor Create;
        destructor Destroy; override;
        function Add: TPriorAttribute;
        procedure Assign(Source: TPersistent); override;
//        procedure Empty;
    end;

    // Prior setup attribute data.
    { A TPriorSetupAttribute is a TPriorAttribute with the addition of
      property ControlMethod, which is used to select the data grid
      (e.g. Prior Information Groups or Prior Information) used to
      control the prior information attribute }
    TPriorSetupAttribute = class(TPriorAttribute)
      private
        fControlMethod: TControlMethod;
      published
        property ControlMethod: TControlMethod read fControlMethod
                                write fControlMethod;
      public
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
    end; // end of TPriorSetupAttribute

    TPriorSetupAttributes = class(TCollection)
      protected
        function GetItem(Index: Integer): TPriorSetupAttribute;
        procedure SetItem(Index: Integer;
                          aPiSetupAttribute: TPriorSetupAttribute);
      public
        property Items[Index: Integer]: TPriorSetupAttribute read GetItem
             write SetItem; default;
        constructor Create;
        function Add: TPriorSetupAttribute;
    end;

    TPri = class(TCollectionItem)
      // One instance defines one Dependent.
      private
        // Fields
        fPriName: string20;
        fAllAtts: TPriorAttributes;
        procedure SetAllAtts(const Value: TPriorAttributes); // a TCollection
      public
        //  Methods
        constructor Create(aParent: TCollection); override;
        constructor CreateAndName(aParent: TCollection; aName: string20;
                         aGroupName: string12);
        procedure Assign(Source: TPersistent); override;
        destructor Destroy; override;
        procedure Initialize(aName: string20; aGroupName: string12);
        procedure DefinePri(aName: string20; aGroupName: string12;
                            aValue: double; aPlotSymbol: integer);
        procedure SetAttributeByType(piaType: TPriAttType; Text: string);
      published
        property Name: string20 read fPriName write fPriName;
        property AllAtts: TPriorAttributes read fAllAtts write SetAllAtts;
    end; // end of TPri.

    TPriSet = class(TCollection)
      private
        function GetItem(Index: integer): TPri;
        procedure SetItem(Index: integer; const Value: TPri);
      public
        property Items[I: integer]: TPri read GetItem write SetItem;
        constructor Create;
        destructor Destroy; override;
        function Add: TPri;
        procedure Append(Source: TPriSet);
        procedure Assign(Source: TPersistent); override;
        procedure ChangeGroupNames(OldGroup, NewGroup: string);
        function NumPriByGroup(GpName: string): integer;
        procedure SetGpDefault;
   end;  // end of TPriSet

    TPriorSetup = class(TComponent)
    { TPriorSetup is used to define and store the configuration of e.g. the
      Prior Information Groups and Prior Information data grids }
      private
        fNumAtt: integer;
        fAllPriAttributes: TPriorSetupAttributes;
      published
        property NumAtt: integer read fNumAtt write fNumAtt;
        property PriAttributes: TPriorSetupAttributes read fAllPriAttributes
             write fAllPriAttributes;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure SetControlMethod(priaType: TPriAttType; ControlMethod: TControlMethod);
    end; // end of TPriorSetup.

  procedure InitializePriorInfoMemoryObjects;
  function PriAttPos(PiAT: TPriAttType): integer;
  function PosPriCap(Cap: string): integer;
  procedure PI_Initialize(aOwner: TComponent);
  procedure PI_Free;

  var

    // Prior information attribute type sets.

    AttTypesPri: TPriAttTypes = [piatPriorName, piatPriValue,
      piatStatistic, piatStatFlag, piatGroupName, piatEquation,
      piatUseFlag, piatPlotSymbol, piatWtMultiplier, piatCovMatrix];

    PriAttributeTypes: array of TPriAttType;

    PriorSetupChanged: boolean;
    DefaultPriCtrlMethod: array of TControlMethod;
    PriorSetupDefault: TPriorSetup;
    PriorSetupCurrent: TPriorSetup;
    PriorSetupLastSaved: TPriorSetup;
    PriSetCurrent: TPriSet;
    PriGpsDefault: TPriSet;
    PriGpsCurrent: TPriSet;
    PriGpsLastSaved: TPriSet;
    PriSetTemp: TPriSet;
    PriGpsTemp: TPriSet;

    slPriStatFlag: TStringList;
    slCMPriDefault: TStringList;
    slCMPriGpsDefault: TStringList;
    slCMPriGps: TStringList;
    slCMPriAllOptions: TStringList;

implementation

//###################################################################

// Public procedures

procedure InitializePriorInfoMemoryObjects;
begin
  PriorSetupCurrent.Assign(PriorSetupDefault);
  PriSetCurrent.Clear;
  PriGpsCurrent.Assign(PriGpsDefault);
end;

function PriAttPos(PiAT: TPriAttType): integer;
// Assign prior information attribute index in arrays.
var
  I: Integer;
begin
  result := -9;
  I := 0;
  while (I < NumPriAttributes) and (result = -9) do
    begin
      if PriAttributeTypes[I] = PiAT then
        result := I;
      I := I+1;
    end;
  { This order determines order of attributes in data grids, and default
    assignment to Prior Information Groups or Prior Information table }
end;

function PosPriCap(Cap: string): integer;
// Find prior information attribute index that has Cap as caption.
var
  PriTemp: TPri;
  I: integer;
begin
  PriTemp := TPri.CreateAndName(nil, 'AnyName', 'AnyGroup');
  try
    result := -8;
    for I := 0 to NumPriAttributes - 1 do
      begin
        if PriTemp.AllAtts[I].Caption = Cap then
          result := I;
      end;
  finally
    PriTemp.Free;
  end;
end;

procedure PI_Initialize(aOwner: TComponent);
begin
  // Allocate and populate arrays needed below.
  SetLength(PriAttributeTypes, NumPriAttributes);
  SetLength(DefaultPriCtrlMethod, NumPriAttributes);
  { This order determines order of attributes in data grids, and default
    assignment to Prior Info Groups or Prior Info table }
  PriAttributeTypes[0] := piatGroupName;     DefaultPriCtrlMethod[0] := cmByDefault;
  PriAttributeTypes[1] := piatEquation;      DefaultPriCtrlMethod[1] := cmByItem;
  PriAttributeTypes[2] := piatPriValue;      DefaultPriCtrlMethod[2] := cmByItem;
  PriAttributeTypes[3] := piatStatistic;     DefaultPriCtrlMethod[3] := cmByItem;
  PriAttributeTypes[4] := piatStatFlag;      DefaultPriCtrlMethod[4] := cmByItem;
  PriAttributeTypes[5] := piatUseFlag;       DefaultPriCtrlMethod[5] := cmByDefault;
  PriAttributeTypes[6] := piatPlotSymbol;    DefaultPriCtrlMethod[6] := cmByGroup;
  PriAttributeTypes[7] := piatWtMultiplier;  DefaultPriCtrlMethod[7] := cmByDefault;
  PriAttributeTypes[8] := piatCovMatrix;     DefaultPriCtrlMethod[8] := cmByDefault;
  PriAttributeTypes[9] := piatPriorName;     DefaultPriCtrlMethod[9] := cmByDefault;
  //
  PriorSetupDefault := TPriorSetup.Create(aOwner);
  PriorSetupCurrent := TPriorSetup.Create(aOwner);
  PriorSetupLastSaved := TPriorSetup.Create(aOwner);
  PriSetCurrent := TPriSet.Create;
  PriGpsDefault := TPriSet.Create;
  PriGpsDefault.Add;
  PriGpsDefault.Items[0].Initialize('DefaultPrior','DefaultPrior');
  PriGpsCurrent := TPriSet.Create;
  PriGpsCurrent.Assign(PriGpsDefault);
  PriSetTemp := TPriSet.Create;
  PriGpsTemp := TPriSet.Create;
  //
  slPriStatFlag := TStringList.Create;
  slPriStatFlag.Add('VAR');
  slPriStatFlag.Add('SD');
  slPriStatFlag.Add('CV');
  slPriStatFlag.Add('WT');
  slPriStatFlag.Add('SQRWT');
  //
  slCMPriDefault := TStringList.Create;
  slCMPriDefault.Add('Prior-Information Table');
  slCMPriDefault.Add('No Table (use default)');
  //
  slCMPriGpsDefault := TStringList.Create;
  slCMPriGpsDefault.Add('Prior-Information Groups Table');
  slCMPriGpsDefault.Add('No Table (use default)');
  //
  slCMPriGps := TStringList.Create;
  slCMPriGps.Add('Prior-Information Table');
  slCMPriGps.Add('Prior-Information Groups Table');
  //
  slCMPriAllOptions := TStringList.Create;
  slCMPriAllOptions.Add('Prior-Information Table');
  slCMPriAllOptions.Add('Prior-Information Groups Table');
  slCMPriAllOptions.Add('No Table (use default)');
  //
  PriorChanged := False;
end;

procedure PI_Free;
begin
  SetLength(PriAttributeTypes, 0);
  SetLength(DefaultPriCtrlMethod, 0);
  FreeAndNil(slPriStatFlag);
  FreeAndNil(slCMPriGpsDefault);
  FreeAndNil(PriGpsDefault);
  FreeAndNil(PriSetCurrent);
  FreeAndNil(PriGpsCurrent);
  FreeAndNil(slCMPriDefault);
  FreeAndNil(slCMPriGps);
  FreeAndNil(slCMPriAllOptions);
  FreeAndNil(PriorSetupDefault);
  FreeAndNil(PriorSetupCurrent);
  FreeAndNil(PriorSetupLastSaved);
  FreeAndNil(PriSetTemp);
  FreeAndNil(PriGpsTemp);
end;

//###################################################################

{ TPriorSetup }

procedure TPriorSetup.Assign(Source: TPersistent);
var
  Src: TPriorSetup;
begin
  if Source is TPriorSetup then
    begin
      Src := Source as TPriorSetup;
      NumAtt := Src.NumAtt;
      PriAttributes.Assign(Src.PriAttributes);
    end
  else
    inherited;
end;

constructor TPriorSetup.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  // Assign defaults for prior information attribute type
  // and control method.
  self.SetSubComponent(True);
  NumAtt := NumPriAttributes;
  fAllPriAttributes := TPriorSetupAttributes.Create;
  for I := 0 to NumAtt - 1 do
    begin
      PriAttributes.Add;
      PriAttributes.Items[I].PriAttType := PriAttributeTypes[I];
      PriAttributes.Items[I].ControlMethod := DefaultPriCtrlMethod[I];
    end;
end;

destructor TPriorSetup.Destroy;
begin
  fAllPriAttributes.Free;
  inherited;
end;

procedure TPriorSetup.SetControlMethod(priaType: TPriAttType;
  ControlMethod: TControlMethod);
var
  IAT: integer;
begin
  IAT := PriAttPos(priaType);
  self.PriAttributes.Items[IAT].ControlMethod := ControlMethod;
end;

//###################################################################

{ TPriorAttribute }

procedure TPriorAttribute.Assign(Source: TPersistent);
var
  Item: TPriorAttribute;
begin
  //inherited;
  if Source is TPriorAttribute then
    begin
      Item := Source as TPriorAttribute;
      PriAttType := Item.PriAttType;
      Text := Item.Text;
    end
  else
    inherited Assign(Source);
end;

function TPriorAttribute.Caption: string;
begin
  case PriAttType of
    piatPriorName: result := 'Prior Name';
    piatPriValue: result := 'Prior Info Value';
    piatStatistic: result := 'Statistic';
    piatStatFlag: result := 'StatFlag';
    piatGroupName: result := 'Group Name';
    piatEquation: result := 'Equation';
    piatUseFlag: result := 'UseFlag';
    piatPlotSymbol: result := 'PlotSymbol';
    piatWtMultiplier: result := 'Weight Multiplier';
    piatCovMatrix: result := 'Variance-Covariance Matrix Name';
  end;
end;

function TPriorAttribute.DefaultText: string;
begin
  case PriAttType of
    piatPriorName: result := '';
    piatPriValue: result := '';
    piatStatistic: result := '';
    piatStatFlag: result := '';
    piatGroupName: result := 'DefaultPrior';
    piatEquation: result := '';
    piatUseFlag: result := 'Yes';
    piatPlotSymbol: result := '1';
    piatWtMultiplier: result := '1.0';
    piatCovMatrix: result := '';
  end;
end;

destructor TPriorAttribute.Destroy;
begin
  inherited;
end;

function TPriorAttribute.Hint: string;
begin
  case PriAttType of
    piatPriorName: result := 'Identifier, up to 20 characters';
    piatPriValue: result := 'Numeric value';
    piatStatistic: result := 'Value used to calculate the prior info weight';
    piatStatFlag: result := 'Flag indicating what Statistic is -- VAR: Variance; SD: Standard Deviation; CV: Coefficient of Variation; WT: Weight; SQRWT: Square Root of the Weight';
    piatGroupName: result := 'Identifier, up to 12 characters';
    piatEquation: result := 'Linear prior-information equation, in terms of parameter names';
    piatUseFlag: result := 'Is group to be used in analysis?';
    piatPlotSymbol: result := 'Integer to be listed in data-exchange files';
    piatWtMultiplier: result := 'Value used to multiply weight associated with each prior info equation';
    piatCovMatrix: result := 'Name of variance-covariance matrix for group';
  end;
end;

procedure TPriorAttribute.Initialize(PiAType: TPriAttType);
begin
  PriAttType := PiAType;
  Text := DefaultText;
end;

function TPriorAttribute.ItemType: TItemType;
begin
  case PriAttType of
    piatPriorName: result := itString;
    piatPriValue: result := itDouble;
    piatStatistic: result := itDouble;
    piatStatFlag: result := itString;
    piatGroupName: result := itString;
    piatEquation: result := itString;
    piatUseFlag: result := itBool;
    piatPlotSymbol: result := itInteger;
    piatWtMultiplier: result := itDouble;
    piatCovMatrix: result := itString;
  else
    result := itString;
  end;
end;

//###################################################################

{ TPriorAttributes }

function TPriorAttributes.Add: TPriorAttribute;
begin
  Result := inherited Add as TPriorAttribute;
end;

procedure TPriorAttributes.Assign(Source: TPersistent);
var
  Item: TPriorAttributes;
  I: Integer;
begin
  if Source is TPriorAttributes then
    begin
      Item := Source as TPriorAttributes;
      if Count < Item.Count then
        begin
          for I := Count to Item.Count - 1 do
            Add;
        end;
      for I := 0 to Count - 1 do
        begin
          Items[I].PriAttType := Item.Items[I].PriAttType;
          Items[I].Text := Item.Items[I].Text;
        end;
    end
  else
    inherited Assign(Source);
end;

constructor TPriorAttributes.Create;
var
  I: Integer;
begin
  inherited Create(TPriorAttribute);
  for I := 0 to NumPriAttributes - 1 do
    begin
      Add;
    end;
end;

destructor TPriorAttributes.Destroy;
begin
  Clear;
  inherited;
end;

function TPriorAttributes.GetItem(Index: Integer): TPriorAttribute;
begin
  Result := TPriorAttribute(inherited GetItem(Index));
end;

procedure TPriorAttributes.SetItem(Index: Integer;
  aPiAttribute: TPriorAttribute);
begin
  Items[Index].Assign(aPiAttribute);
end;

//###################################################################

{ TPriorSetupAttribute }

procedure TPriorSetupAttribute.Assign(Source: TPersistent);
var
  item: TPriorSetupAttribute;
begin
  if Source is TPriorSetupAttribute then
    begin
      item := TPriorSetupAttribute(Source);
      ControlMethod := item.ControlMethod;
    end;
  inherited Assign(Source);
end;

destructor TPriorSetupAttribute.Destroy;
begin
  inherited;
end;

//###################################################################

{ TPriorSetupAttributes }

function TPriorSetupAttributes.Add: TPriorSetupAttribute;
begin
  Result := TPriorSetupAttribute(inherited Add);
end;

constructor TPriorSetupAttributes.Create;
begin
  inherited Create(TPriorSetupAttribute);
end;

function TPriorSetupAttributes.GetItem(
  Index: Integer): TPriorSetupAttribute;
begin
  Result := TPriorSetupAttribute(inherited GetItem(Index));
end;

procedure TPriorSetupAttributes.SetItem(Index: Integer;
  aPiSetupAttribute: TPriorSetupAttribute);
begin
  inherited SetItem(Index, aPiSetupAttribute);
end;

//###################################################################

{ TPri }

procedure TPri.Assign(Source: TPersistent);
var
  Item: TPri;
  I: Integer;
begin
  if Source is TPri then
    begin
      Item := Source as TPri;
      Name := Item.Name;
      if AllAtts.Count < NumPriAttributes then
        begin
          for I := 0 to NumPriAttributes - 1 do
            AllAtts.Add;
        end;
      AllAtts.Assign(Item.AllAtts); // a TPriorAttributes (a TCollection)
    end
  else
    inherited;
end;

constructor TPri.Create(aParent: TCollection);
begin
  inherited Create(aParent);
  Initialize('', '');
end;

constructor TPri.CreateAndName(aParent: TCollection; aName: string20;
  aGroupName: string12);
begin
  inherited Create(aParent);
  Initialize(aName, aGroupName);
end;

procedure TPri.DefinePri(aName: string20; aGroupName: string12; aValue: double;
  aPlotSymbol: integer);
begin
  Initialize(aName, aGroupName);
  AllAtts[PriAttPos(piatPriValue)].Text := FreeFormattedReal(aValue);
  AllAtts[PriAttPos(piatPlotSymbol)].Text := IntToStr(aPlotSymbol);
end;

destructor TPri.Destroy;
begin
  fAllAtts.Free;
  inherited;
end;

procedure TPri.Initialize(aName: string20; aGroupName: string12);
var
  I: Integer;
begin
  if fAllAtts = nil then
    begin
      fAllAtts := TPriorAttributes.Create;
    end;
  for I := 0 to NumPriAttributes - 1 do
    begin
      AllAtts[I].Initialize(PriAttributeTypes[I]);
    end;
  Name := aName;
  if aGroupName <> '' then AllAtts[PriAttPos(piatGroupName)].Text := ConvertString(aGroupName);
end;

procedure TPri.SetAllAtts(const Value: TPriorAttributes);
begin
  fAllAtts.Assign(Value);
end;

procedure TPri.SetAttributeByType(piaType: TPriAttType; Text: string);
var
  I: integer;
begin
  for I := 0 to AllAtts.Count - 1 do
    begin
      if AllAtts.Items[I].PriAttType = piaType then
        begin
          AllAtts.Items[I].Text := Text;
          if (piaType = piatPriorName) then
            begin
              self.Name := ConvertString20(Text);
            end;
          if (piaType = piatGroupName) and (Name = '') then
            begin
              Name := ConvertString20(Text);
            end;
        end;
    end;
end;

//###################################################################

{ TPriSet }

function TPriSet.Add: TPri;
begin
  Result := TPri(inherited Add); // Result is a TPri.
end;

procedure TPriSet.Append(Source: TPriSet);
var
  I, Index: integer;
begin
  for I := 0 to Source.Count - 1 do
    begin
      Index := self.Count;
      self.Add;
      self.Items[Index].Assign(Source.Items[I]);
    end;
end;

procedure TPriSet.Assign(Source: TPersistent);
var
  Item: TPriSet;
  I: Integer;
begin
  if Source is TPriSet then
    begin
      Item := Source as TPriSet;
      Clear;
      for I := 0 to Item.Count - 1 do
        begin
          Add;
          Items[I].Assign(Item.Items[I]); // Copy TPri objects
        end;
    end
  else
    inherited;
end;

procedure TPriSet.ChangeGroupNames(OldGroup, NewGroup: string);
var
  I, AttPos: integer;
begin
  AttPos := PriAttPos(piatGroupName);
  if self.Count > 0 then
    begin
      for I := 0 to Count - 1 do
        begin
          if Items[I].AllAtts.Items[AttPos].Text = OldGroup then
            Items[I].AllAtts.Items[AttPos].Text := NewGroup;
        end;
    end;
end;

constructor TPriSet.Create;
begin
  inherited Create(TPri);
end;

destructor TPriSet.Destroy;
begin
  Clear;
  inherited;
end;

function TPriSet.GetItem(Index: integer): TPri;
begin
  result := TPri(inherited GetItem(Index));
end;

function TPriSet.NumPriByGroup(GpName: string): integer;
// Return number of prior info items for which Group is GpName.
var
  AttPos, J, K: integer;
begin
  AttPos := PriAttPos(piatGroupName);
  K := 0;
  for J := 0 to self.Count - 1 do
    begin
      if AnsiSameText(self.Items[J].AllAtts.Items[AttPos].Text, GpName) then
        begin
          K := K + 1;
        end;
    end;
  result := K;
end;

procedure TPriSet.SetGpDefault;
var
  AttPos: integer;
begin
  if Count = 0 then
    begin
      Add;
      Items[0].Initialize('DefaultPrior','DefaultPrior');
      AttPos := PriAttPos(piatUseFlag);
      Items[0].AllAtts.Items[AttPos].Text := 'Yes';
    end;
end;

procedure TPriSet.SetItem(Index: integer; const Value: TPri);
begin
  inherited SetItem(Index, Value);
end;

//###################################################################

initialization
  PI_Initialize(nil);

finalization
  PI_Free;  

end.
