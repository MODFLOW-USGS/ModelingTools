{@abstract(The main purpose of @name is to define @link(TfrmGridAngle) which
  is used to change the @link(TCustomModelGrid.GridAngle).)}
unit frmGridAngleUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ArgusDataEntry;

type
  TActionToTake = (attRotateAroundCenter, attRotateAroundGridOrigin);

  // @abstract(@name is used to change the @link(TCustomModelGrid.GridAngle).)
  // See TfrmGoPhast.@link(TfrmGoPhast.acGridAngleExecute).
  TfrmGridAngle = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // The user clicks @name to close @classname
    // without changing the grid angle.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // The user clicks @name to get help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // The user clicks @name to change the grid angle.
    btnOK: TBitBtn;
    // @name: TLabel;
    // @name displays "Grid angle:"
    lblGridAngle: TLabel;
    // @name: TRbwDataEntry;
    // The user specifies the grid angle in @name.
    rdeGridAngle: TRbwDataEntry;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name updates the display of the grid angle in @link(frmGoPhast).
    procedure rdeGridAngleChange(Sender: TObject);
  private
    FActionToTake: TActionToTake;
    // @name retrieves the grid angle and converts it to degrees.
    // See TCustomModelGrid.@link(TCustomModelGrid.GridAngle).
    procedure GetData;
    // @name changes the grid angle to the value specified by the user.
    procedure SetData;
    { Private declarations }
  public
    property ActionToTake: TActionToTake read FActionToTake write FActionToTake;
    { Public declarations }
  end;

var
  // @name is the @link(TfrmGridAngle) that is displayed
  // to change the grid angle.
  frmGridAngle: TfrmGridAngle;

implementation

uses frmGoPhastUnit, UndoItems, GoPhastTypes;

{$R *.dfm}

procedure TfrmGridAngle.FormCreate(Sender: TObject);
begin
  inherited;
  // Display the grid angle in degrees.
  GetData;
end;

procedure TfrmGridAngle.btnOKClick(Sender: TObject);
begin
  inherited;
  // change the grid angle.
  SetData;
end;

procedure TfrmGridAngle.GetData;
begin
  // Display the grid angle in degrees.
  if frmGoPhast.Grid <> nil then
  begin
    rdeGridAngle.Text := FloatToStr(frmGoPhast.Grid.
      GridAngle / Pi * 180);
  end;
end;

procedure TfrmGridAngle.SetData;
var
  NewAngle: real;
begin
  // convert the new grid angle to radians.
  NewAngle := StrToFloat(rdeGridAngle.Text) / 180 * Pi;
  // change the grid angle.
  if NewAngle <> frmGoPhast.Grid.GridAngle then
  begin
    case ActionToTake of
      attRotateAroundCenter:
        begin
          frmGoPhast.UndoStack.Submit(TUndoSetAngle.Create(NewAngle));
        end;
      attRotateAroundGridOrigin:
        begin
          frmGoPhast.UndoStack.Submit(TUndoRotateGridAroundGridOrigin.Create(NewAngle));
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TfrmGridAngle.rdeGridAngleChange(Sender: TObject);
var
  NewAngle: real;
begin
  inherited;
  if Text <> '' then
  begin
    try
      NewAngle := StrToFloat(rdeGridAngle.Text) / 180 * Pi
        - frmGoPhast.Grid.GridAngle;
      frmGoPhast.CursorGrid := cgTop;
      frmGoPhast.frameTopView.DeltaGridAngle := NewAngle;
    except on EConvertError do
      begin
      end;
    end;
  end;
end;

end.

