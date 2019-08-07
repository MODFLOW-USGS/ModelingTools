unit frameTimeEditsUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  MetaDataInterfacesUnit, FMX.DateTimeCtrls, FMX.Edit, FMX.EditBox,
  FMX.NumberBox, FMX.Controls.Presentation, System.Generics.Collections;

type
  TframeTimeEdits = class(TFrame)
    grpDifferential: TGroupBox;
    nmbrbxHours: TNumberBox;
    lblHours: TLabel;
    lblMinutes: TLabel;
    nmbrbxMinutes: TNumberBox;
    lblTime: TLabel;
    rbLocalTime: TRadioButton;
    rbTimeOffset: TRadioButton;
    rbUniversalTime: TRadioButton;
    tmdtTime: TTimeEdit;
    procedure tmdtTimeChange(Sender: TObject);
    procedure rbTimeChange(Sender: TObject);
    procedure nmbrbxHoursChange(Sender: TObject);
    procedure nmbrbxMinutesChange(Sender: TObject);
  private
    FTime: ITime;
    FGettingData: Boolean;
    FRadioList: TList<TRadioButton>;
    FOnTimeChanged: TNotifyEvent;
    procedure DoTimeChange;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetMetaData(Data: ITime);
    property OnTimeChanged: TNotifyEvent read FOnTimeChanged write FOnTimeChanged;
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TframeTimeEdits }

constructor TframeTimeEdits.Create(AOwner: TComponent);
begin
  inherited;
  FRadioList := TList<TRadioButton>.Create;
  FRadioList.Add(rbLocalTime);
  FRadioList.Add(rbTimeOffset);
  FRadioList.Add(rbUniversalTime);
end;

destructor TframeTimeEdits.Destroy;
begin
  FRadioList.Free;
  inherited;
end;

procedure TframeTimeEdits.GetMetaData(Data: ITime);
begin
  inherited;
  FGettingData := True;
  try
    FTime := Data;
    tmdtTime.Time := FTime.Time;
    case FTime.FormatChoice of
      tcLocal:
        begin
          rbLocalTime.IsChecked := True;
        end;
      tcLocalDifferential:
        begin
          rbTimeOffset.IsChecked := True;
        end;
      tcUniversal:
        begin
          rbUniversalTime.IsChecked := True;
        end;
    end;
    nmbrbxHours.Value := FTime.DifferentialHours;
    nmbrbxMinutes.Value := FTime.DifferentialMinutes
  finally
    FGettingData := False;
  end;
end;

procedure TframeTimeEdits.DoTimeChange;
begin
  if (not FGettingData) and Assigned(FOnTimeChanged) then
  begin
    FOnTimeChanged(Self);
  end;
end;

procedure TframeTimeEdits.nmbrbxHoursChange(Sender: TObject);
begin
  if not FGettingData then
  begin
    FTime.DifferentialHours := Round(nmbrbxHours.Value);
    DoTimeChange;
  end;
end;

procedure TframeTimeEdits.nmbrbxMinutesChange(Sender: TObject);
begin
  if not FGettingData then
  begin
    FTime.DifferentialMinutes := Round(nmbrbxMinutes.Value);
    DoTimeChange;
  end;
end;

procedure TframeTimeEdits.rbTimeChange(Sender: TObject);
var
  RadioButton: TRadioButton;
  RadioIndex: Integer;
begin
  inherited;
  if not FGettingData then
  begin
    RadioButton := Sender as TRadioButton;
    if RadioButton.IsChecked then
    begin
      RadioIndex := FRadioList.IndexOf(RadioButton);
      if RadioIndex >= 0 then
      begin
        FTime.FormatChoice := TTimeChoice(RadioIndex);
      end;
    end;
    DoTimeChange;
  end;
  nmbrbxHours.Enabled := rbTimeOffset.IsChecked;
  nmbrbxMinutes.Enabled := rbTimeOffset.IsChecked;
end;

procedure TframeTimeEdits.tmdtTimeChange(Sender: TObject);
begin
  if not FGettingData then
  begin
    FTime.Time := tmdtTime.Time;
    DoTimeChange;
  end;
end;

end.
