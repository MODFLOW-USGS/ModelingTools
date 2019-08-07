{@abstract(The main purpose of @name is to define @link(TfrmFreeSurface)
  which is used to set options relating to the use of a free surface
  in PHAST.)}
unit frmFreeSurfaceUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons;

type
  {@abstract(@name is used to set options relating to the use of a free surface
    in PHAST.)
   See @link(TPhastModel).@link(TPhastModel.FreeSurface)
   and @link(TPhastModel).@link(TPhastModel.UseWaterTable). }
  TfrmFreeSurface = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name closes the @classname without making any changes.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // @name shows help for the @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name is used to enable or disable the use of a free surface.
    cbFreeSurface: TCheckBox;
    // @name: TCheckBox;
    // @name is used to enable or disable the user of a water table as
    // the initial condition.
    cbWaterTable: TCheckBox;
    // @name sets the options relating to the use of a free surface
    // in PHAST and closes the dialog box.
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @link(cbWaterTable) should only be enabled if  @link(cbFreeSurface)
    // is checked.
    procedure cbFreeSurfaceClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name retrieves the options relating to a free surface from the model
    // and displays them in the dialog box.
    // See @link(TPhastModel).@link(TPhastModel.FreeSurface)
    // and @link(TPhastModel).@link(TPhastModel.UseWaterTable).
    procedure GetData;
    // @name sets the options relating to a free surface in the model
    // based on what's in the dialog box..
    // See @link(TPhastModel).@link(TPhastModel.FreeSurface)
    // and @link(TPhastModel).@link(TPhastModel.UseWaterTable).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, UndoItems;

{$R *.dfm}

{ TfrmFreeSurface }

procedure TfrmFreeSurface.GetData;
begin
  with frmGoPhast.PhastModel do
  begin
    cbFreeSurface.Checked := FreeSurface;
    cbWaterTable.Checked := UseWaterTable;
  end;
end;

procedure TfrmFreeSurface.SetData;
var
  Undo: TUndoFreeSurface;
begin
  Undo := TUndoFreeSurface.Create;
  try
    Undo.NewFreeSurface := cbFreeSurface.Checked;
    Undo.NewInitialWaterTable := cbWaterTable.Checked;
    if Undo.Changed then
    begin
      frmGoPhast.UndoStack.Submit(Undo)
    end
    else
    begin
      FreeAndNil(Undo);
    end;
  except
    Undo.Free;
    raise;
  end;
end;

procedure TfrmFreeSurface.cbFreeSurfaceClick(Sender: TObject);
begin
  inherited;
  cbWaterTable.Enabled := cbFreeSurface.Checked;
  if not cbWaterTable.Enabled then
  begin
    cbWaterTable.Checked := False;
  end;
end;

procedure TfrmFreeSurface.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmFreeSurface.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

end.

