unit FormulaManagerUnit;

interface

uses SysUtils, Classes, Contnrs, RbwParser, IntListUnit, Dialogs,
  HashTableFacadeUnit, Modflow6TimeSeriesUnit, GoPhastTypes;

type
  TChangeSubscription = procedure (Sender: TObject;
    Subject: TObject; const AName: string);
  PChangeSubscription = ^TChangeSubscription;

  TFormulaObject = class(TComponent)
  private
    FPosition: integer;
    FExpression: TExpression;
    FParser: TRbwParser;
    FFormula: string;
    FNewSubscriptions: TStringList;
    FNotifies: Boolean;
    FReferenceCount: integer;
    FOnRemoveSubscriptionList: TList;
    FOnRestoreSubscriptionList: TList;
    FReferenceCountList: TIntegerList;
    FSubjectList: TList;
    FTimeSeries: TMf6TimeSeries;
    procedure SetFormula(Value: string);
    procedure SetParser(const Value: TRbwParser);
    procedure CompileFormula(var Value: string);
    function GetFormula: string;
    procedure ResetFormula;
    function GetExpression: TExpression;
    // When a @link(TDataArray) or @link(TGlobalVariable) is
    // being renamed, @name is called.  It removes subscriptions
    // to the items in OldSubscriptions and stores the items in
    // NewSubscriptions for use in RestoreSubscriptions.
    procedure RemoveSubscriptions(OldSubscriptions, NewSubscriptions: TStringList);
    procedure RestoreSubscriptions;
    procedure FixSubscriptions;
    procedure DeleteSubscriptionEvents(OnRemoveSubscription,
    OnRestoreSubscription: TChangeSubscription; Subject: TObject);
    function GetDisplayFormula: string;
//    function GetHasTimeSeries: Boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Expression: TExpression read GetExpression;
    property Formula: string read GetFormula;
    property DisplayFormula: string read GetDisplayFormula;
    property Parser: TRbwParser read FParser write SetParser;
    // if @name is called with new events, be sure to update
    // @link(RemoveSubscriptions), @link(RestoreSubscriptions), and
    // @link(FixSubscriptions).
    procedure AddSubscriptionEvents(OnRemoveSubscription,
      OnRestoreSubscription: TChangeSubscription; Subject: TObject);
//    property HasTimeSeries: Boolean read GetHasTimeSeries;
  end;

  {@name is used to update formulas when the names of variables used
  in the formulas are changed.  The procedures @link(RemoveSubscriptions)
  and @link(RestoreSubscriptions) are used for this purpose.
  It is also used to ensure that all formulas
  are properly linked to the items that depend on them.
  @link(FixSubscriptions) is used for this purpose.}
  TFormulaManager = class(TObject)
  private
    // @name is actually a TObjectList.
    FList: TList;
    FSortedList: THashTableFacade;
    FEmptyFormula: TFormulaObject;
    FModel: TBaseModel;
  public
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    function Add: TFormulaObject;
    procedure Remove(FormulaObject: TFormulaObject;
      OnRemoveSubscription, OnRestoreSubscription:TChangeSubscription;
      Subject: TObject);
    procedure ResetFormulas;
    procedure RemoveSubscriptions(OldSubscriptions, NewSubscriptions: TStringList);
    procedure RestoreSubscriptions;
    procedure FixSubscriptions;
    procedure ChangeFormula(var FormulaObject: TFormulaObject;
      NewFormula: string; Parser: TRbwParser; OnRemoveSubscription,
      OnRestoreSubscription: TChangeSubscription; Subject: TObject);
    procedure Pack;
    procedure Clear;
    function FunctionUsed(AString: string): boolean;
  end;

implementation

uses
  frmGoPhastUnit, DataSetUnit, ScreenObjectUnit, ModflowBoundaryUnit, 
  ModflowEtsUnit, ModflowSfrTable, SubscriptionUnit, GIS_Functions,
  PhastModelUnit, Math, ModflowHfbUnit, Mt3dmsChemUnit,
  ModflowRipPlantGroupsUnit, ModflowMvrUnit, Modflow6ObsUnit;

{ TFormulaObject }

constructor TFormulaObject.Create(AOwner: TComponent);
begin
  inherited;
  FTimeSeries := nil;
  FNewSubscriptions := TStringList.Create;
  FReferenceCount := 1;
  FOnRemoveSubscriptionList := TList.Create;
  FOnRestoreSubscriptionList := TList.Create;
  FReferenceCountList:= TIntegerList.Create;
  FSubjectList:= TObjectList.Create;
end;

procedure TFormulaObject.DeleteSubscriptionEvents(OnRemoveSubscription,
  OnRestoreSubscription: TChangeSubscription; Subject: TObject);
var
  Index: Integer;
  Subjects: TList;
  SubjectIndex: Integer;
begin
  if not Assigned(OnRemoveSubscription) then
  begin
    Assert(not Assigned(OnRestoreSubscription));
  end
  else
  begin
    Assert(Assigned(OnRestoreSubscription));
    Index := FOnRemoveSubscriptionList.IndexOf(Addr(OnRemoveSubscription));
    if Index >= 0 then
    begin
      Assert(FOnRestoreSubscriptionList[Index] = Addr(OnRestoreSubscription));
      FReferenceCountList[Index] := FReferenceCountList[Index]-1;
      Subjects := FSubjectList[Index];
      for SubjectIndex := Subjects.Count - 1 downto 0 do
      begin
        if Subjects[SubjectIndex] = Subject then
        begin
          Subjects[SubjectIndex] := nil;
          break;
        end;
      end;
      if (Subjects.Count > 100) then
      begin
        if (FReferenceCountList[Index] < (Subjects.Count div 6)) then
        begin
          Subjects.Pack;
        end;
      end;
    end;
  end;
end;

destructor TFormulaObject.Destroy;
begin
  if FTimeSeries <> nil then
  begin
    RemoveFreeNotification(FTimeSeries.NotifierComponent);
  end;
  FSubjectList.Free;
  FReferenceCountList.Free;
  FOnRestoreSubscriptionList.Free;
  FOnRemoveSubscriptionList.Free;
  FNewSubscriptions.Free;
  inherited;
end;

procedure TFormulaObject.FixSubscriptions;
var
  LocalExpression: TExpression;
  UsedVariables: TStringList;
  VariableIndex: Integer;
  EventIndex: Integer;
  RestoreEvent: PChangeSubscription;
  Subjects: TList;
  SubjectIndex: Integer;
  Subject: TObject;
begin
  if (FExpression = nil) and (FOnRemoveSubscriptionList.Count > 0) then
  begin
    LocalExpression := Expression;
    if LocalExpression <> nil then
    begin
      UsedVariables := LocalExpression.VariablesUsed;
      for VariableIndex := 0 to UsedVariables.Count - 1 do
      begin
        for EventIndex := 0 to FOnRestoreSubscriptionList.Count - 1 do
        begin
          if (FReferenceCountList[EventIndex] > 0) then
          begin
            RestoreEvent := FOnRestoreSubscriptionList[EventIndex];
            if Assigned(RestoreEvent^) then
            begin
              Subjects := FSubjectList[EventIndex];
              for SubjectIndex := 0 to Subjects.Count - 1 do
              begin
                Subject := Subjects[SubjectIndex];
                if Subject = nil then
                begin
                  Continue;
                end;
                if RestoreEvent = Addr(GlobalDataArrayRestoreSubscription) then
                begin
                  GlobalDataArrayRestoreSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestoreScreenObjectDataArraySubscription) then
                begin
                  GlobalRestoreScreenObjectDataArraySubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestoreElevationSubscription) then
                begin
                  GlobalRestoreElevationSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestoreHigherElevationSubscription) then
                begin
                  GlobalRestoreHigherElevationSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestoreLowerElevationSubscription) then
                begin
                  GlobalRestoreLowerElevationSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestoreBoundaryDataArraySubscription) then
                begin
                  GlobalRestoreBoundaryDataArraySubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestorePhastBoundarySubscription) then
                begin
                  GlobalRestorePhastBoundarySubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(GlobalRestoreModflowBoundaryItemSubscription) then
                begin
                  GlobalRestoreModflowBoundaryItemSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(StringValueRestoreSubscription) then
                begin
                  StringValueRestoreSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(TableRowRestoreSubscription) then
                begin
                  TableRowRestoreSubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(RestoreScreenObjectPropertySubscription) then
                begin
                  RestoreScreenObjectPropertySubscription(self, Subject, UsedVariables[VariableIndex]);
                end
                else if RestoreEvent = Addr(Mt3dmsStringValueRestoreSubscription) then
                begin
                  Mt3dmsStringValueRestoreSubscription(self, Subject, UsedVariables[VariableIndex])
                end
                else if RestoreEvent = Addr(GlobalRestoreFormulaObjectSubscription) then
                begin
                  GlobalRestoreFormulaObjectSubscription(self, Subject, UsedVariables[VariableIndex])
                end
                else if RestoreEvent = Addr(MvrStringValueRestoreSubscription) then
                begin
                  MvrStringValueRestoreSubscription(self, Subject, UsedVariables[VariableIndex])
                end
                else if RestoreEvent = Addr(GlobalRestoreMf6CalibrationObsSubscription) then
                begin
                  GlobalRestoreMf6CalibrationObsSubscription(self, Subject, UsedVariables[VariableIndex])
                end
                else
                begin
                  Assert(False);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TFormulaObject.GetDisplayFormula: string;
begin
  if FTimeSeries <> nil then
  begin
    result := FTimeSeries.SeriesName;
  end
  else if (FExpression = nil) or not FNotifies then
  begin
    if (FParser <> nil) and (FFormula <> '') then
    begin
      CompileFormula(FFormula);
    end;
    result := FFormula;
    if FExpression <> nil then
    begin
      result := FExpression.DecompileDisplay;
    end;
  end
  else
  begin
    result := FExpression.DecompileDisplay;
  end;
end;

function TFormulaObject.GetExpression: TExpression;
begin
  if (FExpression = nil) or not FNotifies then
  begin
    if (FParser <> nil) and (FFormula <> '') then
    begin
      CompileFormula(FFormula);
    end;
  end;
  result := FExpression;
end;

function TFormulaObject.GetFormula: string;
begin
  if (FTimeSeries <> nil) and not FTimeSeries.Deleted then
  begin
    result := FTimeSeries.SeriesName;
  end
  else if (FExpression = nil) or not FNotifies then
  begin
    if (FParser <> nil) and (FFormula <> '') then
    begin
      CompileFormula(FFormula);
    end;
    result := FFormula;
  end
  else
  begin
    result := FExpression.Decompile;
    FFormula := result;
  end;
end;

//function TFormulaObject.GetHasTimeSeries: Boolean;
//begin
//  result := FTimeSeries <> nil;
//end;

procedure TFormulaObject.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (FExpression <> nil)
      and (FExpression.Notifier = AComponent) then
    begin
      FExpression := nil;
    end;
    if (FTimeSeries <> nil)
      and (FTimeSeries.NotifierComponent = AComponent) then
    begin
      FTimeSeries := nil;
    end;
  end;
end;

procedure TFormulaObject.RemoveSubscriptions(OldSubscriptions,
  NewSubscriptions: TStringList);
var
  LocalExpression: TExpression;
  UsedVariables: TStringList;
  VariableIndex: Integer;
  EventIndex: Integer;
  PRemoveEvent: PChangeSubscription;
  Subjects: TList;
  SubjectIndex: Integer;
  Subject: TObject;
begin
  Assert(OldSubscriptions.Count = NewSubscriptions.Count);
  if (FOnRemoveSubscriptionList.Count > 0) then
  begin
    LocalExpression := Expression;
    if LocalExpression <> nil then
    begin
      UsedVariables := LocalExpression.VariablesUsed;
      if UsedVariables.Count > 0 then
      begin
        FNewSubscriptions.Clear;
        for VariableIndex := 0 to OldSubscriptions.Count - 1 do
        begin
          if UsedVariables.IndexOf(OldSubscriptions[VariableIndex]) >= 0 then
          begin
            for EventIndex := 0 to FOnRemoveSubscriptionList.Count - 1 do
            begin
              if (FReferenceCountList[EventIndex] > 0) then
              begin
                PRemoveEvent := FOnRemoveSubscriptionList[EventIndex];
                if Assigned(PRemoveEvent^) then
                begin
                  Subjects := FSubjectList[EventIndex];
                  for SubjectIndex := 0 to Subjects.Count - 1 do
                  begin
                    Subject := Subjects[SubjectIndex];
                    if Subject = nil then
                    begin
                      Continue;
                    end;
                    if PRemoveEvent = Addr(GlobalDataArrayRemoveSubscription) then
                    begin
                      GlobalDataArrayRemoveSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveScreenObjectDataArraySubscription) then
                    begin
                      GlobalRemoveScreenObjectDataArraySubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveElevationSubscription) then
                    begin
                      GlobalRemoveElevationSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveHigherElevationSubscription) then
                    begin
                      GlobalRemoveHigherElevationSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveLowerElevationSubscription) then
                    begin
                      GlobalRemoveLowerElevationSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveBoundaryDataArraySubscription) then
                    begin
                      GlobalRemoveBoundaryDataArraySubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemovePhastBoundarySubscription) then
                    begin
                      GlobalRemovePhastBoundarySubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveModflowBoundaryItemSubscription) then
                    begin
                      GlobalRemoveModflowBoundaryItemSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(StringValueRemoveSubscription) then
                    begin
                      StringValueRemoveSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(TableRowRemoveSubscription) then
                    begin
                      TableRowRemoveSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(RemoveScreenObjectPropertySubscription) then
                    begin
                      RemoveScreenObjectPropertySubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalDummyHandleSubscription) then
                    begin
                      GlobalDummyHandleSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(Mt3dmsStringValueRemoveSubscription) then
                    begin
                      Mt3dmsStringValueRemoveSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveFormulaObjectSubscription) then
                    begin
                      GlobalRemoveFormulaObjectSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(MvrStringValueRemoveSubscription) then
                    begin
                      MvrStringValueRemoveSubscription(self, Subject, OldSubscriptions[VariableIndex]);
                    end
                    else if PRemoveEvent = Addr(GlobalRemoveMf6CalibrationObsSubscription) then
                    begin
                      GlobalRemoveMf6CalibrationObsSubscription(self, Subject, OldSubscriptions[VariableIndex])
                    end
                    else
                    begin
                      Assert(False);
                    end;
                  end;
                  FNewSubscriptions.Add(NewSubscriptions[VariableIndex]);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormulaObject.ResetFormula;
begin
  if FExpression <> nil then
  begin
    FFormula := FExpression.Decompile;
  end;
end;

procedure TFormulaObject.RestoreSubscriptions;
var
  VariableIndex: Integer;
  EventIndex: Integer;
  RestoreEvent: PChangeSubscription;
  Subjects: TList;
  SubjectIndex: Integer;
  Subject: TObject;
begin
  if (FOnRestoreSubscriptionList.Count > 0)
    and (FNewSubscriptions.Count > 0) then
  begin
    for VariableIndex := 0 to FNewSubscriptions.Count - 1 do
    begin
      for EventIndex := 0 to FOnRestoreSubscriptionList.Count - 1 do
      begin
        if (FReferenceCountList[EventIndex] > 0) then
        begin
          RestoreEvent := FOnRestoreSubscriptionList[EventIndex];
          if Assigned(RestoreEvent^) then
          begin
            Subjects := FSubjectList[EventIndex];
            for SubjectIndex := 0 to Subjects.Count - 1 do
            begin
              Subject := Subjects[SubjectIndex];
              if Subject = nil then
              begin
                Continue;
              end;
              if RestoreEvent = Addr(GlobalDataArrayRestoreSubscription) then
              begin
                GlobalDataArrayRestoreSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreScreenObjectDataArraySubscription) then
              begin
                GlobalRestoreScreenObjectDataArraySubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreElevationSubscription) then
              begin
                GlobalRestoreElevationSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreHigherElevationSubscription) then
              begin
                GlobalRestoreHigherElevationSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreLowerElevationSubscription) then
              begin
                GlobalRestoreLowerElevationSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreBoundaryDataArraySubscription) then
              begin
                GlobalRestoreBoundaryDataArraySubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestorePhastBoundarySubscription) then
              begin
                GlobalRestorePhastBoundarySubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreModflowBoundaryItemSubscription) then
              begin
                GlobalRestoreModflowBoundaryItemSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(StringValueRestoreSubscription) then
              begin
                StringValueRestoreSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(TableRowRestoreSubscription) then
              begin
                TableRowRestoreSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(RestoreScreenObjectPropertySubscription) then
              begin
                RestoreScreenObjectPropertySubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(Mt3dmsStringValueRestoreSubscription) then
              begin
                Mt3dmsStringValueRestoreSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreFormulaObjectSubscription) then
              begin
                GlobalRestoreFormulaObjectSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(MvrStringValueRestoreSubscription) then
              begin
                MvrStringValueRestoreSubscription(self, Subject, FNewSubscriptions[VariableIndex]);
              end
              else if RestoreEvent = Addr(GlobalRestoreMf6CalibrationObsSubscription) then
              begin
                GlobalRestoreMf6CalibrationObsSubscription(self, Subject, FNewSubscriptions[VariableIndex])
              end
              else
              begin
                Assert(False);
              end;
            end;
          end;
        end;
      end;
    end;
    FNewSubscriptions.Clear;
  end;
end;

procedure TFormulaObject.AddSubscriptionEvents(OnRemoveSubscription,
  OnRestoreSubscription: TChangeSubscription; Subject: TObject);
var
  Index: Integer;
  Subjects: TList;
begin
  if not Assigned(OnRemoveSubscription) then
  begin
    Assert(not Assigned(OnRestoreSubscription));
  end
  else
  begin
    Assert(Assigned(OnRestoreSubscription));
    Index := FOnRemoveSubscriptionList.IndexOf(Addr(OnRemoveSubscription));
    if Index >= 0 then
    begin
      FReferenceCountList[Index] := FReferenceCountList[Index] + 1;
      Subjects := FSubjectList[Index];
      Subjects.Add(Subject)
    end
    else
    begin
      FOnRemoveSubscriptionList.Add(Addr(OnRemoveSubscription));
      FOnRestoreSubscriptionList.Add(Addr(OnRestoreSubscription));
      FReferenceCountList.Add(1);
      Subjects := TList.Create;
      FSubjectList.Add(Subjects);
      Subjects.Add(Subject)
    end;
  end;
end;

procedure TFormulaObject.CompileFormula(var Value: string);
var
  TempValue: string;
  ADummyValue: double;
begin
  if (FExpression <> nil) and FNotifies then
  begin
    begin
      FExpression.Notifier.RemoveFreeNotification(self);
    end;
  end;
  if Value = '' then
  begin
    FExpression := nil;
    FNotifies := False;
  end
  else if (frmGoPhast.PhastModel <> nil)
    and ((frmGoPhast.PhastModel.ComponentState * [csLoading, csReading]) <> [])
    and (Value <> 'True') and (Value <> 'False')
    and not TryStrToFloat(Value, ADummyValue) then
  begin
    FExpression := nil;
    FNotifies := False;
  end
  else
  begin
    TempValue := Value;
    try
      FParser.Compile(Value);
      FExpression := FParser.CurrentExpression;
      Assert(FExpression <> nil);
      begin
        FNotifies := True;
        FExpression.Notifier.FreeNotification(self);
      end;
    except on ERbwParserError do
      begin
        FExpression := nil;
        Value := TempValue;
        FNotifies := False;
      end;
    end;
  end;
end;

procedure TFormulaObject.SetFormula(Value: string);
begin
  if (FFormula <> Value) or (FExpression = nil) then
  begin
    if FParser <> nil then
    begin
      CompileFormula(Value);
    end;
    FFormula := Value;
  end;
end;

procedure TFormulaObject.SetParser(const Value: TRbwParser);
begin
  if FParser <> Value then
  begin
    FParser := Value;
    if (FParser <> nil) and (FFormula <> '') then
    begin
      try
        CompileFormula(FFormula);
      except on ERbwParserError do
        begin
          // ignore.
        end;
      end;
    end;
  end;
end;

{ TFormulaManager }

function TFormulaManager.Add: TFormulaObject;
begin
  if FEmptyFormula = nil then
  begin
    FEmptyFormula := TFormulaObject.Create(nil);
  end;
  result := FEmptyFormula;
end;

procedure TFormulaManager.ChangeFormula(var FormulaObject: TFormulaObject;
  NewFormula: string; Parser: TRbwParser; OnRemoveSubscription,
  OnRestoreSubscription: TChangeSubscription; Subject: TObject);
var
  APointer: Pointer;
  AnObject: TObject;
  Listener: TObserver;
  Notifier: TObserver;
  PhastModel: TPhastModel;
begin
  Remove(FormulaObject, OnRemoveSubscription, OnRestoreSubscription, Subject);

  APointer := nil;
  if NewFormula = '' then
  begin
    FormulaObject := Add;
  end
  else if FSortedList.Search(NewFormula, APointer) then
  begin
    AnObject := APointer;
    FormulaObject := AnObject as TFormulaObject;
    Inc(FormulaObject.FReferenceCount);
  end
  else
  begin
    FormulaObject := TFormulaObject.Create(nil);

    FormulaObject.Parser := Parser;

    {$IFDEF Mf6TimeSeries}
    if FModel.ModelSelection = msModflow2015 then
    begin
      FormulaObject.FTimeSeries :=
        TCustomModel(FModel).Mf6TimesSeries.GetTimeSeriesByName(NewFormula);
    end
    else
    begin
      FormulaObject.FTimeSeries := nil;
    end;
    {$ELSE}
    FormulaObject.FTimeSeries := nil;
    {$ENDIF}
    if FormulaObject.FTimeSeries <> nil then
    begin
      FormulaObject.FTimeSeries.NotifierComponent.FreeNotification(FormulaObject);
      FormulaObject.FFormula := '0';
    end
    else
    begin
      FormulaObject.SetFormula(NewFormula);
    end;
    if FSortedList.Search(FormulaObject.Formula, APointer)
    then
    begin
      AnObject := APointer;
      FormulaObject.Free;
      FormulaObject := AnObject as TFormulaObject;
      Inc(FormulaObject.FReferenceCount);
    end
    else
    begin
      FormulaObject.FPosition := FList.Add(FormulaObject);
      FSortedList.Insert(FormulaObject.Formula, FormulaObject);
    end;
  end;

  FormulaObject.AddSubscriptionEvents(OnRemoveSubscription,
    OnRestoreSubscription, Subject);

  // This is imperfect because not all Subjects will
  // be TObservers.
  if Subject is TObserver then
  begin
    if FormulaObject.FExpression <> nil then
    begin
      PhastModel := frmGoPhast.PhastModel;
      if PhastModel <> nil then
      begin
        Notifier :=PhastModel.LayerStructure.SimulatedNotifier;
        if Notifier <> nil then
        begin
          Listener := TObserver(Subject);
          if FormulaObject.FExpression.UsesFunction(StrBcfVCONT)
            or FormulaObject.FExpression.UsesFunction(StrHufKx)
            or FormulaObject.FExpression.UsesFunction(StrHufKy)
            or FormulaObject.FExpression.UsesFunction(StrHufKz)
            or FormulaObject.FExpression.UsesFunction(StrHufSs)
            or FormulaObject.FExpression.UsesFunction(StrHufAverageSy)
            or FormulaObject.FExpression.UsesFunction(StrHufSy)
            then
          begin
            Notifier.TalksTo(Listener);
          end
          else
          begin
            Notifier.StopsTalkingTo(Listener);
          end;
        end;
        Notifier :=PhastModel.LayerStructure.AquiferTypeNotifier;
        if Notifier <> nil then
        begin
          Listener := TObserver(Subject);
          if FormulaObject.FExpression.UsesFunction(StrHufKx)
            then
          begin
            Notifier.TalksTo(Listener);
            PhastModel.HufKxNotifier.TalksTo(Listener);
          end
          else
          begin
            Notifier.StopsTalkingTo(Listener);
            PhastModel.HufKxNotifier.StopsTalkingTo(Listener);
          end;
          if FormulaObject.FExpression.UsesFunction(StrHufKy)
            then
          begin
            Notifier.TalksTo(Listener);
            PhastModel.HufKyNotifier.TalksTo(Listener);
          end
          else
          begin
            Notifier.StopsTalkingTo(Listener);
            PhastModel.HufKyNotifier.StopsTalkingTo(Listener);
          end;
          if FormulaObject.FExpression.UsesFunction(StrHufKz)
            then
          begin
            Notifier.TalksTo(Listener);
            PhastModel.HufKzNotifier.TalksTo(Listener);
          end
          else
          begin
            Notifier.StopsTalkingTo(Listener);
            PhastModel.HufKzNotifier.StopsTalkingTo(Listener);
          end;
          if FormulaObject.FExpression.UsesFunction(StrHufSs)
            then
          begin
            Notifier.TalksTo(Listener);
            PhastModel.HufSsNotifier.TalksTo(Listener);
          end
          else
          begin
            Notifier.StopsTalkingTo(Listener);
            PhastModel.HufSsNotifier.StopsTalkingTo(Listener);
          end;
          if FormulaObject.FExpression.UsesFunction(StrHufAverageSy)
            or FormulaObject.FExpression.UsesFunction(StrHufSy)
            then
          begin
            Notifier.TalksTo(Listener);
            PhastModel.HufSyNotifier.TalksTo(Listener);
          end
          else
          begin
            Notifier.StopsTalkingTo(Listener);
            PhastModel.HufSyNotifier.StopsTalkingTo(Listener);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormulaManager.Clear;
begin
  FList.Clear;
  FSortedList.Free;
  FSortedList:= THashTableFacade.Create;
  FSortedList.IgnoreCase := False;
end;

constructor TFormulaManager.Create(Model: TBaseModel);
begin
  Assert((Model <> nil) and (Model is TCustomModel));
  FModel := Model;
  FList := TObjectList.Create;
  FSortedList:= THashTableFacade.Create;
  FSortedList.IgnoreCase := False;
end;

destructor TFormulaManager.Destroy;
begin
  FSortedList.Free;
  FList.Free;
  FEmptyFormula.Free;
  inherited;
end;

procedure TFormulaManager.FixSubscriptions;
var
  Index: Integer;
  FormulaObject: TFormulaObject;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    if FormulaObject <> nil then
    begin
      FormulaObject.FixSubscriptions;
    end;
  end;
end;

function TFormulaManager.FunctionUsed(AString: string): boolean;
var
  FunctionIndex: Integer;
  FormulaObject: TFormulaObject;
  AFormula: string;
begin
  result := False;
  AString := UpperCase(AString);
  for FunctionIndex := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[FunctionIndex];
    if FormulaObject <> nil then
    begin
      AFormula := FormulaObject.Formula;
      AFormula := UpperCase(AFormula);
      result := Pos(AString, AFormula) >= 1;
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TFormulaManager.Pack;
var
  Index: Integer;
  FormulaObject: TFormulaObject;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    if FormulaObject <> nil then
    begin
      Assert(FormulaObject.FReferenceCount >= 0);
      if FormulaObject.FReferenceCount = 0 then
      begin
        FSortedList.Delete(FormulaObject.Formula);
        Assert(FList[FormulaObject.FPosition] = FormulaObject);
        FList[FormulaObject.FPosition] := nil;
      end;
    end;
  end;

  FList.Pack;
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    FormulaObject.FPosition := Index;
    FormulaObject.FSubjectList.Pack;
  end;

end;

procedure TFormulaManager.Remove(FormulaObject: TFormulaObject;
  OnRemoveSubscription, OnRestoreSubscription:TChangeSubscription; Subject: TObject);
begin
  if (FormulaObject = nil)
    or ((frmGoPhast.PhastModel <> nil)
    and ((csDestroying in frmGoPhast.PhastModel.ComponentState)
    or frmGoPhast.PhastModel.Clearing)) then
  begin
    Exit;
  end;
  if FormulaObject = FEmptyFormula then
  begin
    FormulaObject.FOnRemoveSubscriptionList.Clear;
    FormulaObject.FOnRestoreSubscriptionList.Clear;
  end
  else
  begin
    FormulaObject.DeleteSubscriptionEvents(OnRemoveSubscription,
      OnRestoreSubscription, Subject);
    Dec(FormulaObject.FReferenceCount);
    Assert(FormulaObject.FReferenceCount >= 0);
    if (FormulaObject.FReferenceCount = 0)
      and (frmGoPhast.PhastModel <> nil) and
      not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FSortedList.Delete(FormulaObject.Formula);

      if (FormulaObject.FPosition < FList.Count)
        and (FList[FormulaObject.FPosition] = FormulaObject) then
      begin
        FList[FormulaObject.FPosition] := nil;
        while (FList.Count > 0) and (FList[FList.Count -1] = nil) do
        begin
          FList.Delete(FList.Count -1);
        end;
      end
      else
      begin
        Assert(FList.IndexOf(FormulaObject)<0);
      end;
    end;
  end;
end;

procedure TFormulaManager.RemoveSubscriptions(OldSubscriptions,
  NewSubscriptions: TStringList);
var
  Index: Integer;
  FormulaObject: TFormulaObject;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    if FormulaObject <> nil then
    begin
      FormulaObject.RemoveSubscriptions(OldSubscriptions,
        NewSubscriptions);
    end;
  end;
end;

procedure TFormulaManager.ResetFormulas;
var
  Index: Integer;
  FormulaObject : TFormulaObject;
  DataArrayManager: TDataArrayManager;
  AnotherFormulaObject: pointer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    if FormulaObject <> nil then
    begin
      FormulaObject.ResetFormula;
    end;
  end;

  FSortedList.Free;
  FSortedList:= THashTableFacade.Create(Max(211, FList.Count*2-1));
  FSortedList.IgnoreCase := True;
//  FSortedList.TableSize := Max(211, FList.Count*2-1);
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    if FormulaObject <> nil then
    begin
      if not FSortedList.Search(FormulaObject.Formula, AnotherFormulaObject) then
      begin
        FSortedList.Insert(FormulaObject.Formula, FormulaObject);
      end;
    end;
  end;

  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataArrayManager.DataSets[Index].RefreshFormula;
  end;
end;

procedure TFormulaManager.RestoreSubscriptions;
var
  Index: Integer;
  FormulaObject: TFormulaObject;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    FormulaObject := FList[Index];
    if FormulaObject <> nil then
    begin
      FormulaObject.RestoreSubscriptions;
    end;
  end;
end;

end.
