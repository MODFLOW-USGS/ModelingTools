unit frmUndoUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, Undo, StdCtrls, Mask, JvExMask, JvSpin,
  JvExStdCtrls, JvCombobox, JvListComb;

type
  {@abstract(@name is a form with a built-in @link(TUndoStack).)
   @SeeAlso(TUndoItem)}
  TUndoForm = class(TfrmCustomGoPhast)
  private
    // See @link(UndoStack);
    FUndoStack: TUndoStack;
    { Private declarations }
  public
    // @name is the @link(TUndoStack) for the form.
    property UndoStack: TUndoStack read FUndoStack;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TfrmUndo }

constructor TUndoForm.Create(AOwner: TComponent);
begin
  inherited;
  FUndoStack := TUndoStack.Create(100);
end;

destructor TUndoForm.Destroy;
begin
  FUndoStack.Free;
  inherited;
end;

end.
