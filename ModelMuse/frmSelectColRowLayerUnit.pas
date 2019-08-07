{ @abstract(The main purpose of @name is to define @link(TfrmSelectColRowLayer)
  which is used to set
  frmGoPhast.Grid.@link(TCustomModelGrid.SelectedColumn),
  frmGoPhast.Grid.@link(TCustomModelGrid.SelectedRow), and
  frmGoPhast.Grid.@link(TCustomModelGrid.SelectedLayer).)}
unit frmSelectColRowLayerUnit;  

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ComCtrls, Spin, Mask,
  JvExMask, JvSpin;

type
  { @abstract(@name is used to set
   frmGoPhast.Grid.@link(TCustomModelGrid.SelectedColumn),
   frmGoPhast.Grid.@link(TCustomModelGrid.SelectedRow), and
   frmGoPhast.Grid.@link(TCustomModelGrid.SelectedLayer).)}
  TfrmSelectColRowLayer = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name displays "Selected column".
    lblCol: TLabel;
    // @name displays "Selected layer".
    lblLayer: TLabel;
    // @name displays "Selected row".
    lblRow: TLabel;
    // @name is used to specify the selected column.
    seCol: TJvSpinEdit;
    // @name is used to specify the selected layer.
    seLayer: TJvSpinEdit;
    // @name is used to specify the selected row.
    seRow: TJvSpinEdit;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name sets the properties of @link(seCol),
    // @link(seLayer), and @link(seRow).
    procedure GetData;
    // @name sets
    // frmGoPhast.Grid.@link(TCustomModelGrid.SelectedColumn),
    // frmGoPhast.Grid.@link(TCustomModelGrid.SelectedRow), and
    // frmGoPhast.Grid.@link(TCustomModelGrid.SelectedLayer).
    procedure SetData; { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit;

{$R *.dfm}

{ TfrmSelectColRowLayer }

procedure TfrmSelectColRowLayer.GetData;
begin
  seCol.Value := frmGoPhast.PhastModel.SelectedModel.SelectedColumn + 1;
  seCol.MaxValue := frmGoPhast.PhastModel.SelectedModel.Grid.ColumnCount;
  seCol.MinValue := 1;
  seRow.Value := frmGoPhast.PhastModel.SelectedModel.SelectedRow + 1;
  seRow.MaxValue := frmGoPhast.PhastModel.SelectedModel.Grid.RowCount;
  seRow.MinValue := 1;
  seLayer.Value := frmGoPhast.PhastModel.SelectedModel.SelectedLayer + 1;
  seLayer.MaxValue := frmGoPhast.PhastModel.SelectedModel.Grid.LayerCount;
  seLayer.MinValue := 1;
end;

procedure TfrmSelectColRowLayer.SetData;
begin
  frmGoPhast.PhastModel.SelectedModel.SelectedColumn := seCol.AsInteger - 1;
  frmGoPhast.PhastModel.SelectedModel.SelectedRow := seRow.AsInteger - 1;
  frmGoPhast.PhastModel.SelectedModel.SelectedLayer := seLayer.AsInteger - 1;
  frmGoPhast.PhastModel.UpdateCombinedDisplayColumn;
  frmGoPhast.frameSideView.ItemChange(nil);
  frmGoPhast.PhastModel.UpdateCombinedDisplayRow;
  frmGoPhast.frameFrontView.ItemChange(nil);
  frmGoPhast.PhastModel.UpdateCombinedDisplayLayer;
  frmGoPhast.frameTopView.ItemChange(nil);
end;

procedure TfrmSelectColRowLayer.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSelectColRowLayer.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

end.

