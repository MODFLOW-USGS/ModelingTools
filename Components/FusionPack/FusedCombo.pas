unit FusedCombo;
{
 This component is a part of the freeware Fusion Package, containing
 a set of visual components. If redistributed, this unit should not be
 separated from the rest of the files in the package. If modified,
 all author credits must remain. Latest version of the package, bug
 reports, info on authors etc. can be found at
 http://www.geocities.com/SiliconValley/Drive/6381/
 Terms of use and other stuff explained in Fusion.hlp and FusionPack.txt.

 Component info:

 TFusedCombo inherited TCustomComboBox and added the following

  - can be Flat              by Favio E. Ugalde, fugalde@geocities.com, http://www.geocities.com/~fugalde
  - keeps history            by Milos Dragovic
  - finding out cursor's (caret's) position by Larry J. Rutledge

Here comes some info from Favio E. Ugalde concerning some of the used code
********************************************************************************
Version:	1.10
Description:	ComboBox with Office97 style inherited from TCustomComboBox. With very small code and very high performance. FREEWARE
Author:	        Favio E. Ugalde Corral
Creation: 	09/04/98
Modification:	09/15/98
Country:	Mexico
E-Mail:	        fugalde@geocities.com
URL:		http://www.geocities.com/~fugalde
Observaciones: Ideal para utilizarse en Barras de Herramientas tales como
               Toolbar97 de Jordan Russell http://www.connect.net/jordanr/
               o cualquier otra.

Warning (as usual)
        This product is FREEWARE, then you can use it for personal or
commercial in any way. You don't need to put my name, but it's always
nice to see an acknowledges for me.
THE CODE IS PROVIDED AS IS WITH NO GUARANTEES OF ANY KIND.
USE THIS AT YOUR OWN RISK - YOU ARE THE ONLY PERSON RESPONSIBLE FOR
ANY DAMAGE THIS CODE MAY CAUSE.

Hierarchy

         TComboBox97 inherits from TCustomComboBox, so you can use any
property, method or event of TCustomComboBox.

Properties

          Flat Determines whether the ComboBox has a a 3D border that
provides a raised or lowered look like Office97.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, FusedEdit;

type
  TComboState97 = set of (csButtonPressed, csMouseCaptured);

  TCustomFusedCombo = class(TCustomComboBox)
  private
    FFlat: Boolean;
    FOldColor: TColor;
    FOldParentColor: Boolean;
    FButtonWidth: Integer;
    FEditState: TComboState97;
    FMouseInControl: Boolean;  // Indica si el ratón está sobre el ComboBox
    procedure SetFlat(const Value: Boolean);
    procedure DrawButtonBorder(DC: HDC); // Dibuja el borde del botón incluido
    procedure DrawControlBorder(DC: HDC); // Dibuja el borde del ComboBox
    procedure DrawBorders; // Dibuja los bordes del ComboBox y del botón incluido
    function  NeedDraw3DBorder: Boolean;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    FButtonCaption, FCaption, FFileName: string;
    FHasButton, FCaseSensitiveHistory, FUseHistory : boolean;
    FOnItemFound, FOnButtonClick : TNotifyEvent;
    FButton : TFusedSpeedButton;
    FLabel : TLabel;
    FBitmap : TBitmap;
    procedure ButtonClick (Sender:TObject);
    function GetCursorPos : integer;
    procedure SetCursorPos (i: integer);
    procedure SetFileName (fn : string);
    procedure SetUseHistory (b : boolean);
    procedure Loaded; override;
    procedure KeyUp(var Key : Word; Shift: TShiftState); override;
    procedure KeyDown(var Key : Word; Shift: TShiftState); override;
    procedure DoExit; override;
//    procedure SetTransparent(t : boolean);
    procedure TrackButtonPressed(X, Y: Integer); // Verifica si el botón todavía deba estar presionado
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property Flat: Boolean read FFlat write SetFlat default False;
    procedure SetHasButton (b : boolean);
    procedure SetButtonGlyph (g : TBitmap);
    procedure SetParent (AControl : TWinControl); override;
    procedure SetButtonCaption (c : string);
    procedure SetCaption (c : string);
    procedure UpdateControls;
    procedure WMMove(var Message: TMessage); message WM_MOVE;
    procedure WMSIZE(var Message: TMessage); message WM_SIZE;
    property HasButton : boolean read FHasButton write SetHasButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadHistory;
    procedure SaveHistory;
    property Caption : string read FCaption write SetCaption;
    property ButtonCaption : string read FButtonCaption write SetButtonCaption;
    property ButtonGlyph : TBitmap read FBitmap write SetButtonGlyph;
    property CursorPos : integer read GetCursorPos write SetCursorPos;
    property UseHistory: boolean read FUseHistory write SetUseHistory;
    property FileName : string read FFileName write FFileName;
    property CaseSensitiveHistory : boolean read FCaseSensitiveHistory write FCaseSensitiveHistory;
    property OnItemFound : TNotifyEvent read FOnItemFound write FOnItemFound;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

  TFusedCombo = class(TCustomFusedCombo)
  published
    property Style; // Debe ser siempre la primera
    property Flat;
    property CaseSensitiveHistory;
    property CursorPos;
    property FileName;
    property UseHistory;
    property OnItemFound;
    property OnButtonClick;
    property Caption;
    property ButtonCaption;
    property ButtonGlyph;
    {$IFDEF VER120}
    property Anchors;
    property BiDiMode;
    property Constraints;
    {$ENDIF}
    property Color;
    property Ctl3D;
    property DragCursor;
    {$IFDEF VER120}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property Items;
    property MaxLength;
    {$IFDEF VER120}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    {$IFDEF VER120}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

procedure Register;

implementation
{$R *.res}

{ TCustomFusedCombo }

constructor TCustomFusedCombo.Create(AOwner: TComponent);
begin
   inherited;

   FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
   // Valores por omisión
   FOldColor       := inherited Color;
   FOldParentColor := inherited ParentColor;
   FBitmap := TBitmap.Create;
end;
{procedure TCustomFusedCombo.SetTransparent(t : boolean);
begin
   if t = FTransparent then exit;
   FTransparent := t;
   Invalidate;
end;}

procedure TCustomFusedCombo.Loaded;
begin
   if not (csDesigning in ComponentState) then LoadHistory;
   if not FBitmap.Empty then HasButton := true;
   inherited;
end;

procedure TCustomFusedCombo.SetUseHistory (b : boolean);
begin
     if FUseHistory = b then exit;
     FUseHistory:=b;
     if not (csDesigning in ComponentState) and b then LoadHistory;
end;
procedure TCustomFusedCombo.SaveHistory;
begin
    if not(csDesigning in ComponentState) and FUseHistory
       and (FFilename<>'')
           then Items.SaveToFile(FFileName);
end;
procedure TCustomFusedCombo.LoadHistory;
begin
    if not FUseHistory or (csDesigning in ComponentState) then exit;
    if (FFileName<>'') and (ExtractFilePath(FFilename)='') then
        FFileName := ExtractFilePath(Application.ExeName)+FFileName;
    Items.Clear;
    if FileExists(FFileName) then
       Items.LoadFromFile(FFileName);
end;

procedure TCustomFusedCombo.SetFileName (fn : string);
begin
  FFileName := fn;
  LoadHistory;
end;

function TCustomFusedCombo.GetCursorPos : integer;
begin
     result := LoWord(SendMessage(Handle,EM_GETSEL, 0, 0));
end;
procedure TCustomFusedCombo.SetCursorPos (i: integer);
begin
    SendMessage(Handle,EM_SETSEL, i, i);
end;
procedure TCustomFusedCombo.KeyDown(var Key : Word; Shift: TShiftState);
begin
  if (key in [VK_DOWN, VK_UP])
     then ItemIndex := Items.IndexOf (Text);
  inherited;
end;

procedure TCustomFusedCombo.KeyUp(var Key : Word; Shift: TShiftState);
var t2 : string;i, l : integer;
begin
  inherited;
  if not UseHistory or (key in VKIgnore) or (Style=csDropDownList)
     then exit;
  if not (key in [VK_DOWN, VK_UP]) then
    begin
       l := Length(Text);
       if (l=0) then exit;
       if not FCaseSensitiveHistory then t2 := LowerCase(Text);
       for i := 0 to Items.Count-1 do
           if (copy (LowerCase(Items.Strings[i]),1,l)=t2) and (t2<>Items[i]) then
              begin
                ItemIndex := i;
                SelStart := l;
                SelLength := Length(Text)-l;
                Change;
                if assigned (FOnItemFound) then FOnItemFound(self);
                exit;
              end;
    end;
end;

procedure TCustomFusedCombo.DoExit;
begin
  inherited;
  if FUseHistory and (Text<>'') and (Items.IndexOf(Text)=-1)
     then Items.Add(Text);
end;

procedure TCustomFusedCombo.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
    begin
    FFlat := Value;
    Ctl3D := not Value;
    Invalidate;
    end;
end;

// Verifica si el botón todavía deba estar presionado
procedure TCustomFusedCombo.CMEnter(var Message: TCMEnter);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    DrawBorders;
end;

procedure TCustomFusedCombo.CMExit(var Message: TCMExit);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    DrawBorders;
end;

procedure TCustomFusedCombo.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  // Si por primera vez el ratón está sobre el ComboBox se redibuja su borde
  if not FMouseInControl and Enabled then
    begin
    FMouseInControl := True;
    DrawBorders;
    end;
end;

procedure TCustomFusedCombo.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  // Si el ratón estaba sobre el ComboBox se redibuja su borde
  if FMouseInControl and Enabled then
    begin
    FMouseInControl := False;
    DrawBorders
    end;
end;

procedure TCustomFusedCombo.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;

  // Si se desea plano
  if FFlat then
    // Si se habilita se recupera su color anterior ó
    // si se inhabilita se guarda su color actual y se utiliza el del contenedor,
    // así se dá la apariencia del ComboBox inhabilitado de Office97. 
    if Enabled then
      begin
      inherited Color       := FOldColor;
      inherited ParentColor := FOldParentColor;
      end
    else
      begin
      FOldParentColor := inherited Parentcolor;
      FOldColor       := inherited Color;
      inherited ParentColor := True;
      end;
end;

procedure TCustomFusedCombo.WMPaint(var Message: TWMPaint);
var
   DC: HDC;
   PS: TPaintStruct;
procedure DrawButton;
var
  ARect: TRect;
begin
  // Obtiene las coordenadas de los límites del botón
  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth);
  InflateRect(ARect, -1, -1);
  // Dibuja el botón
  DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT);
  // Notifica a Windows que ya no tiene que dibujar el botón
  ExcludeClipRect(DC, ClientWidth - FButtonWidth - 4, 0, ClientWidth, ClientHeight);
end;
begin
  // Si no es plano sólo se hacer lo de omisión
  if not FFlat then
    begin
    inherited;
    Exit;
    end;

  // Utiliza o crea el dispositivo de contexto
  if Message.DC = 0 then
    DC := BeginPaint(Handle, PS)
  else
    DC := Message.DC;
  try
    // Si el estilo así lo requiere dibuja el botón y una base
    if Style <> csSimple then
      begin
      FillRect(DC, ClientRect, Brush.Handle);
      DrawButton;//(DC);
      end;
    // Dibuja el ComboBox
    PaintWindow(DC);
  finally
    // Elimina el dispositivo de contexto si fué creado aquí
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;
  // Dibuja los bordes del ComboBox y del botón incluido
  DrawBorders;
end;

function TCustomFusedCombo.NeedDraw3DBorder: Boolean;
begin
  // Se requiere dibujar el borde cuando el ratón esta encima
  // o cuando es el control activo.
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

// Dibuja el borde del botón incluido
procedure TCustomFusedCombo.DrawButtonBorder(DC: HDC);
const
   Flags: array[Boolean] of Integer = (0, BF_FLAT);
var
   ARect: TRect;
   BtnFaceBrush: HBRUSH;
begin
  // Notifica a Windows que no tiene que dibujar sobre botón
  ExcludeClipRect(DC, ClientWidth - FButtonWidth + 4, 4,
                  ClientWidth - 4, ClientHeight - 4);
  // Obtiene las coordenadas de los límites del botón
  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth - 2);
  InflateRect(ARect, -2, -2);

  // Dibuja un borde 3D o plano según se requiera
  if NeedDraw3DBorder then
    DrawEdge(DC, ARect, EDGE_RAISED, BF_RECT or Flags[csButtonPressed in FEditState])
  else
    begin
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
    try
      InflateRect(ARect, -1, -1);
      FillRect(DC, ARect, BtnFaceBrush);
    finally
      DeleteObject(BtnFaceBrush);
    end;
    end;

  // Notifica a Windows que ya no tiene que dibujar el botón
  ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

// Dibuja el borde del ComboBox
procedure TCustomFusedCombo.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH; // Brochas necesarias para el efecto 3D
begin
  // Crea las brochas necesarias para el efecto 3D
  BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));
  try
    // Obtiene las coordenadas de los límites del ComboBox
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    // Dibuja un borde 3D o plano según se requiera
    if NeedDraw3DBorder then
      begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
      end
    else
      begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
      end;
  finally
    // Elimina las brochas
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

// Dibuja los bordes del ComboBox y del botón incluido
procedure TCustomFusedCombo.DrawBorders;
var
  DC: HDC;
begin
  // Sólo se continua si es plano
  if not FFlat then
    Exit;

  // Dibuja el borde de la caja y si se requiere del botón incluido
  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    if Style <> csSimple then
      DrawButtonBorder(DC);
  finally
    ReleaseDC(DC, Handle);
  end;
end;

procedure TCustomFusedCombo.TrackButtonPressed(X, Y: Integer);
var
   ARect: TRect;
begin
  // Dibuja correctamente el borde cuando se acaba de presionar el botón y
  // las coordenadas X,Y están fuera del botón del ComboBox.
  SetRect(ARect, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  if (csButtonPressed in FEditState) and not PtInRect(ARect, Point(X, Y)) then
    begin
    Exclude(FEditState, csButtonPressed); // Ya no se está presionando el botón
    DrawBorders;
    end;
end;

procedure TCustomFusedCombo.MouseDown(Button: TMouseButton; Shift: TShiftState;
          X, Y: Integer);
begin
  // Si el click del ratón produjo que se abriera la lista desplegable, entonces
  // se presionó el botón y se capturó el ratón.
  if DroppedDown then
    begin
    Include(FEditState, csButtonPressed);
    Include(FEditState, csMouseCaptured);
    Invalidate; // Muestra el botón presionado (sumido)
    end;

  inherited;
end;

procedure TCustomFusedCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Dibuja correctamente el borde cuando se mueve el ratón a la lista desplegable
  if csMouseCaptured in FEditState then
    TrackButtonPressed(X, Y);

  inherited;
end;

procedure TCustomFusedCombo.MouseUp(Button: TMouseButton; Shift: TShiftState;
          X, Y: Integer);
begin
  // Dibuja correctamente el borde cuando se suelta el botón fuera del botón del ComboBox
  TrackButtonPressed(-1, -1);
  inherited;
end;

procedure TCustomFusedCombo.SetHasButton (b : boolean);
begin
  if b = FHasButton then exit;
  FHasButton := b;
  if b then
    begin
      FButton := TFusedSpeedButton.Create(self);
      FButton.Parent := Parent;
      UpdateControls;
      FButton.Caption := FButtonCaption;
      FButton.Glyph := FBitmap;
      FButton.Flat := Flat;
      FButton.OnClick := ButtonClick;
    end
  else FButton.Free;
end;

procedure TCustomFusedCombo.ButtonClick (Sender:TObject);
begin
     if assigned(FOnButtonClick) then FOnButtonClick(self);
end;
procedure TCustomFusedCombo.SetButtonCaption (c : string);
begin
     if c = FButtonCaption then exit;
     FButtonCaption := c;
     if (c='') and FBitmap.Empty then HasButton := false
     else
       begin
         HasButton := true;
         FButton.Caption := c;
         UpdateControls;
       end
end;

procedure TCustomFusedCombo.SetButtonGlyph (g : TBitmap);
begin
   FBitmap.Assign(g);
   if (FButtonCaption='') and (g=nil) then HasButton := false
   else
     begin
       HasButton := true;
       FButton.Glyph := g;
       UpdateControls;
     end
end;
procedure TCustomFusedCombo.SetParent (AControl : TWinControl);
begin
  if FHasButton then FButton.parent := parent;
  if FCaption<>'' then FLabel.parent := parent;
  inherited;
end;

procedure TCustomFusedCombo.SetCaption (c : string);
begin
  if c = FCaption then exit;
  FCaption := c;
  if c='' then
    begin
      FLabel.Free;
      FLabel := nil;
    end
  else
    begin
      if FLabel = nil then
        begin
          FLabel := TLabel.Create(self);
          FLabel.AutoSize := true;
          FLabel.Transparent := FFlat;
          FLabel.Parent := Parent;
        end;
      FLabel.Caption := c;
      UpdateControls;
    end;
end;

procedure TCustomFusedCombo.UpdateControls;
var w : integer;
begin
   if FHasButton then
     begin
       FButton.Top := Top;
       FButton.Left := Left+Width;
       if FButtonCaption='' then FButton.Width := FButton.Height
       else
         begin
            w := FButton.Canvas.TextWidth(FButtonCaption)+5+FButton.Glyph.Width;
            if w>FButton.Height
               then FButton.Width := w
               else FButton.Width := FButton.Height;
         end;
     end;
   if FLabel<>nil then
     begin
          FLabel.Left := Left;
          FLabel.Top := Top-FLabel.Height;
     end;
end;

procedure TCustomFusedCombo.WMSIZE(var Message: TMessage);
begin
  inherited;
  UpdateControls;
end;

procedure TCustomFusedCombo.WMMove(var Message: TMessage);
begin
  inherited;
  UpdateControls;
end;

destructor TCustomFusedCombo.Destroy;
begin
   FBitmap.Free;
   inherited;
end;

procedure Register;
begin
  RegisterComponents('Fused', [TFusedCombo]);
end;

end.

