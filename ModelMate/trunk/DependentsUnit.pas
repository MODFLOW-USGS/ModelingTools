unit DependentsUnit;

interface

  uses Classes, SysUtils, Dialogs, Windows,
       GlobalTypesUnit, GlobalBasicData, Utilities;

  type

    // Enumerations.
    // Dependent category.
    TDepCat = (dcObs, dcPred, dcUnknown);

    // Sets.
    TDepAttTypes = set of TDepAttType;

    //  DEPENDENTS.

    // Dependent attribute data.
    TDependentAttribute = class(TCollectionItem)
      private
        fDepAttType: TDepAttType;
        fText: string;
      published
        property DepAttType: TDepAttType read fDepAttType
                               write fDepAttType;
        property Text: string read fText write fText;
      public
        destructor Destroy; override;
        function Caption: string;
        function DefaultText(aDepCat: TDepCat): string;
        function Hint(aDepCat: TDepCat): string;
        function ItemType: TItemType;
        procedure Assign(Source: TPersistent); override;
        procedure Initialize(DAType: TDepAttType; aDepCat: TDepCat);
    end;

    TDependentAttributes = class(TCollection)
      protected
        function GetItem(Index: Integer): TDependentAttribute;
        procedure SetItem(Index: Integer;
                          aDAttribute: TDependentAttribute);
      public
        property Items[Index: Integer]: TDependentAttribute
             read GetItem write SetItem; default;
        constructor Create;
        destructor Destroy; override;
        function Add: TDependentAttribute;
        procedure Assign(Source: TPersistent); override;
//        procedure Empty;
    end;

    // Dependent setup attribute data.
    { A TDependentSetupAttribute is a TDependentAttribute with the addition of
      property ControlMethod, which is used to select the data grid
      (e.g. Observation Groups or Observations) used to control the dependent attribute }
    TDependentSetupAttribute = class(TDependentAttribute)
      private
        fControlMethod: TControlMethod;
      published
        property ControlMethod: TControlMethod read fControlMethod
                                write fControlMethod;
      public
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
    end; // end of TDependentSetupAttribute

    TDependentSetupAttributes = class(TCollection)
      protected
        function GetItem(Index: Integer): TDependentSetupAttribute;
        procedure SetItem(Index: Integer;
                          aDSetupAttribute: TDependentSetupAttribute);
      public
        property Items[Index: Integer]: TDependentSetupAttribute read GetItem
             write SetItem; default;
        constructor Create;
        function Add: TDependentSetupAttribute;
    end;

    TDep = class(TCollectionItem)
      // One instance defines one Dependent.
      private
        // Fields
        fDepName: string20;
        fAllAtts: TDependentAttributes;
        fDepCat: TDepCat;
        procedure SetAllAtts(const Value: TDependentAttributes); // a TCollection
      public
        //  Methods
        constructor Create(aParent: TCollection); override;
        constructor CreateAndName(aParent: TCollection; aName: string20;
                         aGroupName: string12; aDepCat: TDepCat);
        procedure Assign(Source: TPersistent); override;
        destructor Destroy; override;
        procedure Initialize(aName: string20; aGroupName: string12; aDepCat: TDepCat);
        procedure DefineDep(aName: string20; aGroupName: string12;
                            aValue: double; aPlotSymbol: integer; aDepCat: TDepCat); overload;
        procedure SetAttributeByType(daType: TDepAttType; Text: string);
      published
        property Name: string20 read fDepName write fDepName;
        property AllAtts: TDependentAttributes read fAllAtts write SetAllAtts;
        property DepCat: TDepCat read fDepCat write fDepCat;
    end; // end of TDep.

    TDepSet = class(TCollection)
      private
        function GetItem(Index: integer): TDep;
        procedure SetItem(Index: integer; const Value: TDep);
      public
        property Items[I: integer]: TDep read GetItem write SetItem;
        constructor Create;
        destructor Destroy; override;
        function Add: TDep;
        procedure Append(Source: TDepSet);
        procedure Assign(Source: TPersistent); override;
        procedure ChangeGroupNames(OldGroup, NewGroup: string);
        function NumDepByGroup(GpName: string): integer;
        procedure SetGpDefault(const DepCat: TDepCat);
   end;  // end of TDepSet

    TObservationSetup = class(TComponent)
    { TObservationSetup is used to define and store the configuration of e.g. the
      Observation Groups and Observations data grids }
      private
        fNumAtt: integer;
        fAllObsAttributes: TDependentSetupAttributes;
      published
        property NumAtt: integer read fNumAtt write fNumAtt;
        property ObsAttributes: TDependentSetupAttributes read fAllObsAttributes
             write fAllObsAttributes;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        //procedure SetCombinedMethods;
        procedure Assign(Source: TPersistent); override;
        procedure SetControlMethod(daType: TDepAttType; ControlMethod: TControlMethod);
    end; // end of TObservationSetup.

    TPredictionSetup = class(TComponent)
    { TPredictionSetup is used to define and store the configuration of e.g. the
      Prediction Groups and Predictions data grids }
      private
        fNumAtt: integer;
        fAllPredAttributes: TDependentSetupAttributes;
      published
        property NumAtt: integer read fNumAtt write fNumAtt;
        property PredAttributes: TDependentSetupAttributes read fAllPredAttributes
             write fAllPredAttributes;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure SetControlMethod(daType: TDepAttType; ControlMethod: TControlMethod);
    end; // end of TPredictionSetup

  procedure InitializeDependentsMemoryObjects;
  function DepAttPos(DAT: TDepAttType): integer;
  function PosDepCap(Cap: string): integer;
  procedure D_Initialize(aOwner: TComponent);
  procedure D_Free;

  var

    // Dependent attribute type sets.

    AttTypesObs: TDepAttTypes = [datObsName, datObsValue, datStatistic,
      datStatFlag, datGroupName, datEquation, datUseFlag, datPlotSymbol,
      datWtMultiplier, datCovMatrix, datNonDetect, datWtOSConstant];

    AttTypesPred: TDepAttTypes = [datPredName, datRefValue, datMeasStatistic,
      datMeasStatFlag, datGroupName, datEquation, datUseFlag, datPlotSymbol];

    DepAttributeTypes: array of TDepAttType;

    ObservationSetupChanged: boolean;
    DefaultDepCtrlMethod: array of TControlMethod;
    ObservationSetupDefault: TObservationSetup;
    ObservationSetupCurrent: TObservationSetup;
    ObservationSetupLastSaved: TObservationSetup;
    ObsSetCurrent: TDepSet;
    ObsGpsDefault: TDepSet;
    ObsGpsCurrent: TDepSet;
    ObsGpsLastSaved: TDepSet;

    PredictionSetupChanged: boolean;
    //DefaultPredCtrlMethod: array of TDepControlMethod;
    PredictionSetupDefault: TPredictionSetup;
    PredictionSetupCurrent: TPredictionSetup;
    PredictionSetupLastSaved: TPredictionSetup;
    PredSetCurrent: TDepSet;
    PredGpsDefault: TDepSet;
    PredGpsCurrent: TDepSet;
    PredGpsLastSaved: TDepSet;

    slStatFlag: TStringList;
    slOCMObsDefault: TStringList;
    slOCMGpsDefault: TStringList;
    slOCMObsGps: TStringList;
    slOCMAllOptions: TStringList;

    slMeasStatFlag: TStringList;
    slPrCMPredDefault: TStringList;
    slPrCMGpsDefault: TStringList;
    slPrCMPredGps: TStringList;
    slPrCMAllOptions: TStringList;

    DepSetTemp: TDepSet;
    DepGpsTemp: TDepSet;

implementation

//###################################################################

// Public procedures

procedure InitializeDependentsMemoryObjects;
begin
  ObservationSetupCurrent.Assign(ObservationSetupDefault);
  ObsSetCurrent.Clear;
  ObsGpsCurrent.Assign(ObsGpsDefault);
  PredictionSetupCurrent.Assign(PredictionSetupDefault);
  PredSetCurrent.Clear;
  PredGpsCurrent.Assign(PredGpsDefault);
end;

function DepAttPos(DAT: TDepAttType): integer;
// Assign Dependent attribute index in arrays
var
  I: Integer;
begin
  result := -9;
  I := 0;
  while (I < NumDepAttributes) and (result = -9) do
    begin
      if DepAttributeTypes[I] = DAT then
        result := I;
      I := I+1;
    end;
  { This order determines order of attributes in data grids, and default
    assignment to Observation Groups or Observations table }
end;

function PosDepCap(Cap: string): integer;
// Find dependent attribute index that has Cap as caption.
var
  DepTemp: TDep;
  I: integer;
begin
  DepTemp := TDep.CreateAndName(nil, 'AnyName', 'AnyGroup', dcUnknown);
  try
    result := -8;
    for I := 0 to NumDepAttributes - 1 do
      begin
        if DepTemp.AllAtts[I].Caption = Cap then
          result := I;
      end;
  finally
    DepTemp.Free;
  end;
end;

//###################################################################

{ TObservationSetup }

procedure TObservationSetup.Assign(Source: TPersistent);
var
  Src: TObservationSetup;
begin
  if Source is TObservationSetup then
    begin
      Src := Source as TObservationSetup;
      NumAtt := Src.NumAtt;
      ObsAttributes.Assign(Src.ObsAttributes);
    end
  else
    inherited;
end;

constructor TObservationSetup.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  // Assign defaults for observation attribute type and control method
  self.SetSubComponent(True);
  NumAtt := NumDepAttributes;
  fAllObsAttributes := TDependentSetupAttributes.Create;
  for I := 0 to NumAtt - 1 do
    begin
      ObsAttributes.Add;
      ObsAttributes.Items[I].DepAttType := DepAttributeTypes[I];
      ObsAttributes.Items[I].ControlMethod := DefaultDepCtrlMethod[I];
    end;
end;

destructor TObservationSetup.Destroy;
begin
  fAllObsAttributes.Free;
  inherited;
end;

procedure TObservationSetup.SetControlMethod(daType: TDepAttType; ControlMethod: TControlMethod);
var
  IAT: integer;
begin
  IAT := DepAttPos(daType);
  self.ObsAttributes.Items[IAT].ControlMethod := ControlMethod;
end;

//###################################################################

{ Public procedures }

procedure D_Initialize(aOwner: TComponent);
begin
  // Allocate and populate arrays needed below
  SetLength(DepAttributeTypes, NumDepAttributes);
  SetLength(DefaultDepCtrlMethod, NumDepAttributes);
//  SetLength(DefaultPredCtrlMethod, NumDepAttributes);
  { This order determines order of attributes in data grids, and default
    assignment to Observation Groups or Observations table }
  DepAttributeTypes[0] := datGroupName;     DefaultDepCtrlMethod[0] := cmByDefault;
  DepAttributeTypes[1] := datObsValue;      DefaultDepCtrlMethod[1] := cmByItem;
  DepAttributeTypes[2] := datStatistic;     DefaultDepCtrlMethod[2] := cmByItem;
  DepAttributeTypes[3] := datStatFlag;      DefaultDepCtrlMethod[3] := cmByItem;
  DepAttributeTypes[4] := datRefValue;      DefaultDepCtrlMethod[4] := cmByItem;
  DepAttributeTypes[5] := datMeasStatistic; DefaultDepCtrlMethod[5] := cmByItem;
  DepAttributeTypes[6] := datMeasStatFlag;  DefaultDepCtrlMethod[6] := cmByItem;
  DepAttributeTypes[7] := datUseFlag;       DefaultDepCtrlMethod[7] := cmByDefault;
  DepAttributeTypes[8] := datPlotSymbol;    DefaultDepCtrlMethod[8] := cmByDefault;
  DepAttributeTypes[9] := datEquation;      DefaultDepCtrlMethod[9] := cmByDefault;
  DepAttributeTypes[10] := datWtMultiplier; DefaultDepCtrlMethod[10] := cmByDefault;
  DepAttributeTypes[11] := datCovMatrix;    DefaultDepCtrlMethod[11] := cmByDefault;
  DepAttributeTypes[12] := datNonDetect;    DefaultDepCtrlMethod[12] := cmByDefault;
  DepAttributeTypes[13] := datWtOSConstant; DefaultDepCtrlMethod[13] := cmByDefault;
  DepAttributeTypes[14] := datObsName;      DefaultDepCtrlMethod[14] := cmByDefault;
  DepAttributeTypes[15] := datPredName;     DefaultDepCtrlMethod[15] := cmByDefault;
  //
  ObservationSetupDefault := TObservationSetup.Create(aOwner);
  ObservationSetupCurrent := TObservationSetup.Create(aOwner);
  ObservationSetupLastSaved := TObservationSetup.Create(aOwner);
  ObsSetCurrent := TDepSet.Create;
  ObsGpsDefault := TDepSet.Create;
  ObsGpsDefault.Add;
  ObsGpsDefault.Items[0].Initialize('DefaultObs','DefaultObs',dcObs);
  ObsGpsCurrent := TDepSet.Create;
  ObsGpsCurrent.Assign(ObsGpsDefault);

  PredictionSetupDefault := TPredictionSetup.Create(aOwner);
  PredictionSetupCurrent := TPredictionSetup.Create(aOwner);
  PredictionSetupLastSaved := TPredictionSetup.Create(aOwner);
  PredSetCurrent := TDepSet.Create;
  PredGpsDefault := TDepSet.Create;
  PredGpsDefault.Add;
  PredGpsDefault.Items[0].Initialize('DefaultPreds','DefaultPreds',dcPred);
  PredGpsCurrent := TDepSet.Create;
  PredGpsCurrent.Assign(PredGpsDefault);

  DepSetTemp := TDepSet.Create;
  DepGpsTemp := TDepSet.Create;

  slStatFlag := TStringList.Create;
  slStatFlag.Add('VAR');
  slStatFlag.Add('SD');
  slStatFlag.Add('CV');
  slStatFlag.Add('WT');
  slStatFlag.Add('SQRWT');
  //
  slOCMObsDefault := TStringList.Create;
  slOCMObsDefault.Add('Observations Table');
  slOCMObsDefault.Add('No Table (use default)');
  //
  slOCMGpsDefault := TStringList.Create;
  slOCMGpsDefault.Add('Observation Groups Table');
  slOCMGpsDefault.Add('No Table (use default)');
  //
  slOCMObsGps := TStringList.Create;
  slOCMObsGps.Add('Observations Table');
  slOCMObsGps.Add('Observation Groups Table');
  //
  slOCMAllOptions := TStringList.Create;
  slOCMAllOptions.Add('Observations Table');
  slOCMAllOptions.Add('Observation Groups Table');
  slOCMAllOptions.Add('No Table (use default)');
  //
  ObservationsChanged := False;
  //
  slMeasStatFlag := TStringList.Create;
  slMeasStatFlag.Add('VAR');
  slMeasStatFlag.Add('SD');
  //
  slPrCMPredDefault := TStringList.Create;
  slPrCMPredDefault.Add('Predictions Table');
  slPrCMPredDefault.Add('No Table (use default)');
  //
  slPrCMGpsDefault := TStringList.Create;
  slPrCMGpsDefault.Add('Prediction Groups Table');
  slPrCMGpsDefault.Add('No Table (use default)');
  //
  slPrCMPredGps := TStringList.Create;
  slPrCMPredGps.Add('Predictions Table');
  slPrCMPredGps.Add('Prediction Groups Table');
  //
  slPrCMAllOptions := TStringList.Create;
  slPrCMAllOptions.Add('Predictions Table');
  slPrCMAllOptions.Add('Prediction Groups Table');
  slPrCMAllOptions.Add('No Table (use default)');
  //
  PredictionsChanged := False;
end;

procedure D_Free;
begin
  // Dependents
  SetLength(DepAttributeTypes, 0);
  // Observations.
  SetLength(DefaultDepCtrlMethod, 0);
  FreeAndNil(slStatFlag);
  FreeAndNil(slOCMGpsDefault);
  FreeAndNil(ObsGpsDefault);
  FreeAndNil(ObsSetCurrent);
  FreeAndNil(ObsGpsCurrent);
  FreeAndNil(slOCMObsDefault);
  FreeAndNil(slOCMObsGps);
  FreeAndNil(slOCMAllOptions);
  FreeAndNil(ObservationSetupDefault);
  FreeAndNil(ObservationSetupCurrent);
  FreeAndNil(ObservationSetupLastSaved);
  // Predictions.
//  SetLength(PredAttributeTypes, 0);
//  SetLength(DefaultPredCtrlMethod, 0);
  FreeAndNil(slMeasStatFlag);
  FreeAndNil(slPrCMGpsDefault);
  FreeAndNil(PredGpsDefault);
  FreeAndNil(PredSetCurrent);
  FreeAndNil(PredGpsCurrent);
  FreeAndNil(slPrCMPredDefault);
  FreeAndNil(slPrCMPredGps);
  FreeAndNil(slPrCMAllOptions);
  FreeAndNil(PredictionSetupDefault);
  FreeAndNil(PredictionSetupCurrent);
  FreeAndNil(PredictionSetupLastSaved);

  FreeAndNil(DepSetTemp);
  FreeAndNil(DepGpsTemp);

end;

//###################################################################

{ TPredictionSetup }

procedure TPredictionSetup.Assign(Source: TPersistent);
var
  Src: TPredictionSetup;
begin
  if Source is TPredictionSetup then
    begin
      Src := Source as TPredictionSetup;
      NumAtt := Src.NumAtt;
      PredAttributes.Assign(Src.PredAttributes);
    end
  else
    inherited;
end;

constructor TPredictionSetup.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  // Assign defaults for observation attribute type and control method
  self.SetSubComponent(True);
  NumAtt := NumDepAttributes;
  fAllPredAttributes := TDependentSetupAttributes.Create;
  for I := 0 to NumAtt - 1 do
    begin
      PredAttributes.Add;
      PredAttributes.Items[I].DepAttType := DepAttributeTypes[I];
      PredAttributes.Items[I].ControlMethod := DefaultDepCtrlMethod[I];
    end;
end;

destructor TPredictionSetup.Destroy;
begin
  fAllPredAttributes.Free;
  inherited;
end;

procedure TPredictionSetup.SetControlMethod(daType: TDepAttType;
  ControlMethod: TControlMethod);
var
  IAT: integer;
begin
  IAT := DepAttPos(daType);
  self.PredAttributes.Items[IAT].ControlMethod := ControlMethod;
end;

//###################################################################

{ TDependentAttribute }

procedure TDependentAttribute.Assign(Source: TPersistent);
var
  Item: TDependentAttribute;
begin
//  inherited;  // from TPersistent.
  if Source is TDependentAttribute then
    begin
      Item := Source as TDependentAttribute;
      DepAttType := Item.DepAttType;
      Text := Item.Text;
//      DepCat := Item.DepCat;
    end
  else
    inherited Assign(Source);
end;

function TDependentAttribute.Caption: string;
begin
  case DepAttType of
    datObsName: result := 'Observation Name';
    datObsValue: result := 'Observed Value';
    datStatistic: result := 'Statistic';
    datStatFlag: result := 'StatFlag';
    datPredName: result := 'Prediction Name';
    datRefValue: result := 'Reference Value';
    datMeasStatistic: result := 'MeasStatistic';
    datMeasStatFlag: result := 'MeasStatFlag';
    datGroupName: result := 'Group Name';
    datEquation: result := 'Equation';
    datUseFlag: result := 'UseFlag';
    datPlotSymbol: result := 'PlotSymbol';
    datWtMultiplier: result := 'Weight Multiplier';
    datCovMatrix: result := 'Variance-Covariance Matrix Name';
    datNonDetect: result := 'NonDetect';
    datWtOSConstant: result := 'WtOSConstant';
  end;
end;

function TDependentAttribute.DefaultText(aDepCat: TDepCat): string;
begin
  case DepAttType of
    datObsName: result := '';
    datObsValue: result := '';
    datStatistic: result := '';
    datStatFlag: result := '';
    datPredName: result := '';
    datRefValue: result := '0.0';
    datMeasStatistic: result := '';
    datMeasStatFlag: result := '';
    datGroupName:
      case aDepCat of
        dcObs:  result := 'DefaultObs';
        dcPred: result := 'DefaultDep';
        dcUnknown: result := 'Unknown';
      end;
    datEquation: result := '';
    datUseFlag: result := 'Yes';
    datPlotSymbol: result := '1';
    datWtMultiplier: result := '1.0';
    datCovMatrix: result := '';
    datNonDetect: result := 'No';
    datWtOSConstant: result := '0.0';
  end;
end;

destructor TDependentAttribute.Destroy;
begin
  inherited;
end;

function TDependentAttribute.Hint(aDepCat: TDepCat): string;
begin
  case DepAttType of
    datObsName: result := 'Identifier, up to 20 characters';
    datObsValue: result := 'Numeric value';
    datStatistic: result := 'Value used to calculate the observation weight';
    datStatFlag: result := 'Flag indicating what Statistic is -- VAR: Variance; SD: Standard Deviation; CV: Coefficient of Variation; WT: Weight; SQRWT: Square Root of the Weight';
    datPredName: result := 'Identifier, up to 20 characters';
    datRefValue: result := 'Reference value';
    datMeasStatistic: result := 'Value used to calculate the expected prediction variance';
    datMeasStatFlag: result := 'Flag indicating what MeasStatistic is -- VAR: Variance; SD: Standard Deviation';
    datGroupName: result := 'Identifier, up to 12 characters';
    datEquation:
      case aDepCat of
        dcObs: result := 'Equation defining this observation as function of previous observations';
        dcPred: result := 'Equation defining this prediction as function of previous predictions';
        dcUnknown: result := 'Equation defining this dependent as function of previous dependents';
      end;
    datUseFlag: result := 'Is group to be used in analysis?';
    datPlotSymbol: result := 'Integer to be listed in data-exchange files';
    datWtMultiplier: result := 'Value used to multiply weight associated with each observation';
    datCovMatrix: result := 'Name of variance-covariance matrix for group';
    datNonDetect: result := 'Yes indicates the observed value is below detection limit. See TM6-A11, p. 84';
    datWtOSConstant: result := 'Constant Eta in equation 3 of TM6-A11 (p. 19 and 85)';
  end;
end;

procedure TDependentAttribute.Initialize(DAType: TDepAttType; aDepCat: TDepCat);
begin
  DepAttType := DAType;
  Text := DefaultText(aDepCat);
//  DepCat := aDepCat;
end;

function TDependentAttribute.ItemType: TItemType;
begin
  case DepAttType of
    datObsName: result := itString;
    datObsValue: result := itDouble;
    datStatistic: result := itDouble;
    datStatFlag: result := itString;
    datPredName: result := itString;
    datRefValue: result := itDouble;
    datMeasStatistic: result := itDouble;
    datMeasStatFlag: result := itString;
    datGroupName: result := itString;
    datEquation: result := itString;
    datUseFlag: result := itBool;
    datPlotSymbol: result := itInteger;
    datWtMultiplier: result := itDouble;
    datCovMatrix: result := itString;
    datNonDetect: result := itBool;
    datWtOSConstant: result := itDouble;
  else
    result := itString;
  end;
end;

//###################################################################

{ TDependentAttributes }

function TDependentAttributes.Add: TDependentAttribute;
begin
  Result := inherited Add as TDependentAttribute;
end;

procedure TDependentAttributes.Assign(Source: TPersistent);
var
  Item: TDependentAttributes;
  I: Integer;
begin
  if Source is TDependentAttributes then
    begin
      Item := Source as TDependentAttributes;
      if Count < Item.Count then
        begin
          for I := Count to Item.Count - 1 do
            Add;
        end;
      for I := 0 to Count - 1 do
        begin
          Items[I].DepAttType := Item.Items[I].DepAttType;
          Items[I].Text := Item.Items[I].Text;
        end;
    end
  else
    inherited Assign(Source);
end;

constructor TDependentAttributes.Create;
var
  I: Integer;
begin
  inherited Create(TDependentAttribute);
  for I := 0 to NumDepAttributes - 1 do
    begin
      Add;
//      Items[I].DepCat := dcUnknown;
    end;
end;

destructor TDependentAttributes.Destroy;
begin
  Clear;
  inherited;
end;

//procedure TDependentAttributes.Empty;
//var
//  I: integer;
//begin
//  if Count > 0 then
//    begin
//      for I := Count - 1 downto 0 do
//        begin
//          Delete(I);
//        end;
//    end;
//end;

function TDependentAttributes.GetItem(Index: Integer): TDependentAttribute;
begin
  Result := TDependentAttribute(inherited GetItem(Index));
end;

procedure TDependentAttributes.SetItem(Index: Integer;
  aDAttribute: TDependentAttribute);
begin
  Items[Index].Assign(aDAttribute);
end;

//###################################################################

{ TDependentSetupAttribute }

procedure TDependentSetupAttribute.Assign(Source: TPersistent);
var
  item: TDependentSetupAttribute;
begin
  if Source is TDependentSetupAttribute then
    begin
      item := TDependentSetupAttribute(Source);
      ControlMethod := item.ControlMethod;
    end;
  inherited Assign(Source);
end;

destructor TDependentSetupAttribute.Destroy;
begin
  inherited;
end;

//###################################################################

{ TDependentSetupAttributes }

function TDependentSetupAttributes.Add: TDependentSetupAttribute;
begin
  Result := TDependentSetupAttribute(inherited Add);
end;

constructor TDependentSetupAttributes.Create;
begin
  inherited Create(TDependentSetupAttribute);
end;

function TDependentSetupAttributes.GetItem(
  Index: Integer): TDependentSetupAttribute;
begin
  Result := TDependentSetupAttribute(inherited GetItem(Index)); // error here?
end;

procedure TDependentSetupAttributes.SetItem(Index: Integer;
  aDSetupAttribute: TDependentSetupAttribute);
begin
  inherited SetItem(Index, aDSetupAttribute);
end;

//###################################################################

{ TDep }

procedure TDep.Assign(Source: TPersistent);
var
  Item: TDep;
  I: Integer;
begin
  if Source is TDep then
    begin
      Item := Source as TDep;
      Name := Item.Name;
      if AllAtts.Count < NumDepAttributes then
        begin
          for I := AllAtts.Count to NumDepAttributes - 1 do
            AllAtts.Add;
        end;
      AllAtts.Assign(Item.AllAtts); // a TDependentAttributes (a TCollection)
      DepCat := Item.DepCat;
    end
  else
    inherited;
end;

constructor TDep.Create(aParent: TCollection);
begin
  inherited Create(aParent);
  Initialize('', '', dcUnknown);
end;

constructor TDep.CreateAndName(aParent: TCollection; aName: string20;
  aGroupName: string12; aDepCat: TDepCat);
begin
  inherited Create(aParent);
  Initialize(aName, aGroupName, aDepCat);
end;

procedure TDep.DefineDep(aName: string20; aGroupName: string12; aValue: double;
  aPlotSymbol: integer; aDepCat: TDepCat);
  // aValue can be either an observation value or a reference value,
  // depending on aDepCat.
begin
  Initialize(aName, aGroupName, aDepCat);
  case aDepCat of
    dcObs: AllAtts[DepAttPos(datObsValue)].Text := FreeFormattedReal(aValue);
    dcPred: AllAtts[DepAttPos(datRefValue)].Text := FreeFormattedReal(aValue);
    dcUnknown: ShowMessage('Debugging note: Dependent category unknown in TDep.DefineDep');
  end;
  AllAtts[DepAttPos(datPlotSymbol)].Text := IntToStr(aPlotSymbol);
end;

destructor TDep.Destroy;
begin
  fAllAtts.Free;
  inherited;
end;

procedure TDep.Initialize(aName: string20; aGroupName: string12; aDepCat: TDepCat);
var
  I: Integer;
begin
  if fAllAtts = nil then
    begin
      fAllAtts := TDependentAttributes.Create;
    end;
  for I := 0 to NumDepAttributes - 1 do
    begin
      AllAtts[I].Initialize(DepAttributeTypes[I], aDepCat);
    end;
  Name := aName;
  if aGroupName <> '' then AllAtts[DepAttPos(datGroupName)].Text := ConvertString(aGroupName);
  DepCat := aDepCat;
end;

procedure TDep.SetAllAtts(const Value: TDependentAttributes);
begin
  fAllAtts.Assign(Value);
end;

procedure TDep.SetAttributeByType(daType: TDepAttType; Text: string);
var
  I: integer;
begin
  for I := 0 to AllAtts.Count - 1 do
    begin
      if AllAtts.Items[I].DepAttType = daType then
        begin
          AllAtts.Items[I].Text := Text;
          if (daType = datObsName) or (daType = datPredName) then
            begin
              self.Name := ConvertString20(Text);
            end;
          if (daType = datGroupName) and (Name = '') then
            begin
              Name := ConvertString20(Text);
            end;
        end;
    end;
end;

//###################################################################

{ TDepSet }

function TDepSet.Add: TDep;
begin
  Result := TDep(inherited Add); // Result is a TDep.
end;

procedure TDepSet.Append(Source: TDepSet);
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

procedure TDepSet.Assign(Source: TPersistent);
var
  Item: TDepSet;
  I: Integer;
begin
  if Source is TDepSet then
    begin
      Item := Source as TDepSet;
      Clear;
      for I := 0 to Item.Count - 1 do
        begin
          Add;
          Items[I].Assign(Item.Items[I]); // Copy TDep objects
        end;
    end
  else
    inherited;
end;

procedure TDepSet.ChangeGroupNames(OldGroup, NewGroup: string);
var
  I, AttPos: integer;
begin
  AttPos := DepAttPos(datGroupName);
  if self.Count > 0 then
    begin
      for I := 0 to Count - 1 do
        begin
          if Items[I].AllAtts.Items[AttPos].Text = OldGroup then
            Items[I].AllAtts.Items[AttPos].Text := NewGroup;
        end;
    end;
end;

constructor TDepSet.Create;
begin
  inherited Create(TDep);
end;

destructor TDepSet.Destroy;
begin
  Clear;
  inherited;
end;

//procedure TDepSet.Empty;
//var
//  I: integer;
//begin
//  if Count > 0 then
//    begin
//      for I := Count - 1 downto 0 do
//        Delete(I);
//    end;
//end;

function TDepSet.GetItem(Index: integer): TDep;
begin
  result := TDep(inherited GetItem(Index));
end;

function TDepSet.NumDepByGroup(GpName: string): integer;
// Return number of dependents for which Group is GpName.
var
  AttPos, J, K: integer;
begin
  AttPos := DepAttPos(datGroupName);
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

procedure TDepSet.SetGpDefault(const DepCat: TDepCat);
var
  AttPos: integer;
  Def: string;
begin
  case DepCat of
    dcObs: Def := 'DefaultObs';
    dcPred: Def := 'DefaultPreds';
    dcUnknown: Def := 'DefaultUnknown';
  end;
  if Count = 0 then
    begin
      Add;
      Items[0].Initialize(ConvertString20(Def),ConvertString12(Def),DepCat);
      AttPos := DepAttPos(datUseFlag);
      Items[0].AllAtts.Items[AttPos].Text := 'Yes';
      AttPos := DepAttPos(datNonDetect);
      Items[0].AllAtts.Items[AttPos].Text := 'No';
    end;
end;

procedure TDepSet.SetItem(Index: integer; const Value: TDep);
begin
  inherited SetItem(Index, Value);
end;

initialization
  D_Initialize(nil);

finalization
  D_Free;

end.
