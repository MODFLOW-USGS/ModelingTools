{------------------------------------------------------------------------------}
{ Copyright © 2000-2001, Simon Armstrong.  All Rights Reserved.                }
{                                                                              }
{ Copyright:                                                                   }
{ All SadMan Software source code is copyrighted by Simon Armstrong (hereafter }
{ "author"), and shall remain the exclusive property of the author. This       }
{ software and any accompanying documentation are protected by United Kingdom  }
{ copyright law and also by International Treaty provisions. Any use of this   }
{ software in violation of copyright law or the terms of this agreement will   }
{ be prosecuted to the best of the author's ability.                           }
{                                                                              }
{ Under no conditions may you remove the copyright notices made part of the    }
{ software or documentation.                                                   }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the source code unless specifically stated otherwise.                        }
{ You are further granted permission to redistribute any of the source code in }
{ source code form, provided that the original archive as found on the         }
{ SadMan Software web site (www.sadmansoftware.com) is distributed unmodified. }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any SadMan Software source code by       }
{     itself. You must include the original archive as you found it at the web }
{     site.                                                                    }
{   * Sell or lease any portion of SadMan Software source code. You are, of    }
{     course, free to sell any of your own original code that works with,      }
{     enhances, etc. SadMan Software source code.                              }
{   * Distribute SadMan Software source code for profit.                       }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of this      }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Simon Armstrong, be held accountable  }
{ for any damages or losses that may occur from use or misuse of the software. }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via email to Support@sadmansoftware.com. However, I can  }
{ not guarantee any support whatsoever. While I do try to answer all questions }
{ that I receive and address all problems that are reported to me, I can not   }
{ guarantee that this will always be so.                                       }
{------------------------------------------------------------------------------}
unit SsButtonEd;

interface
uses
  {$IFDEF VER80}
  WinTypes, WinProcs, Menus,
  {$ELSE}
  Windows,
  {$ENDIF}
  Buttons, Classes, Controls, Forms, Graphics, Messages, StdCtrls;
{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=15}
   {$DEFINE COMPILER_4_UP}
  {$ifend}
{$endif}

{$IFDEF VER190} // Delphi 2007
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER180} // Delphi 2006
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER170} // Delphi 2005
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER160} // Delphi 8
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER150} // Delphi 7
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER140} // Delphi 6 or C++ Builder 6
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER130} // Delphi 5
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

{$IFDEF VER120} // Delphi 4
 {$DEFINE COMPILER_4_UP}
{$ENDIF}

type
  TssCustomButtonEdit = class(TCustomEdit)
  private
     FButton: TBitBtn;
     procedure ButtonClick(Sender: TObject);
     function  GetGlyph: TBitmap;
     function  GetNumGlyphs: integer;
     procedure SetGlyph(const g: TBitmap);
     procedure SetNumGlyphs(const n: integer);
  protected
     procedure CreateParams(var Params: TCreateParams); override;
     procedure DoButtonClick; virtual; abstract;
     function  GetEnabled: boolean; {$IFDEF COMPILER_4_UP} reintroduce; {$ENDIF}
     procedure SetEnabled(e: boolean); {$IFDEF COMPILER_4_UP} reintroduce; {$ENDIF}
     procedure WMSize(var Message: TWMSize); message WM_SIZE;
     property  Button: TBitBtn read FButton write FButton;
     property  Enabled: boolean read GetEnabled write SetEnabled;
     property  Glyph: TBitmap read GetGlyph write SetGlyph;
     property  NumGlyphs: integer read GetNumGlyphs write SetNumGlyphs;
  public
     constructor Create(AOwner: TComponent); override;
  published
  end;

  TssButtonEdit = class(TssCustomButtonEdit)
  private
     FOnButtonClick: TNotifyEvent;
  protected
     procedure DoButtonClick; override;
  public
  published
     property AutoSelect;
     property BorderStyle;
     property Color;
     property Ctl3d;
     property DragCursor;
     property DragMode;
     property Enabled;
     property Font;
     property Glyph;
     property HideSelection;
     property NumGlyphs;
     property ParentColor;
     property ParentCtl3D;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ReadOnly;
     property ShowHint;
     property TabOrder;
     property Text;
     property Visible;
     property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
     property OnChange;
     property OnClick;
     property OnDblClick;
     property OnDragDrop;
     property OnDragOver;
     property OnEndDrag;
     property OnEnter;
     property OnExit;
     property OnKeyDown;
     property OnKeyPress;
     property OnKeyUp;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     {$IFNDEF VER80}
     property OnStartDrag;
     {$ENDIF}
  end;

procedure Register;

implementation

{$IFDEF WIN64}
     {$R *.r32}
{$ELSE}
  {$IFDEF WIN32}
     {$R *.r32}
  {$ELSE}
     {$R *.r16}
  {$ENDIF}
{$ENDIF}

procedure Register;
begin
   RegisterComponents('SadMan',[TssButtonEdit]);
end;

{ TssCustomButtonEdit }

constructor TssCustomButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];

  FButton := TBitBtn.Create(self);
  with FButton do begin
     Parent := self;
     TabStop := false;
     Visible := true;
     OnClick := ButtonClick;
  end;
end;

procedure TssCustomButtonEdit.ButtonClick(Sender: TObject);
begin
  DoButtonClick;
  SetFocus;
end;

procedure TssCustomButtonEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

function TssCustomButtonEdit.GetEnabled: boolean;
begin
   result := inherited Enabled;
end;

function TssCustomButtonEdit.GetGlyph: TBitmap;
begin
   result := FButton.Glyph;
end;

function TssCustomButtonEdit.GetNumGlyphs: integer;
begin
   result := FButton.NumGlyphs;
end;

procedure TssCustomButtonEdit.SetEnabled(e: boolean);
begin
   inherited Enabled := e;
   FButton.Enabled := e;
end;

procedure TssCustomButtonEdit.SetGlyph(const g: TBitmap);
begin
   FButton.Glyph := g;
end;

procedure TssCustomButtonEdit.SetNumGlyphs(const n: integer);
begin
   FButton.NumGlyphs := n;
end;

procedure TssCustomButtonEdit.WMSize(var Message: TWMSize);
var
  b: integer;
begin
  if (BorderStyle = bsSingle) and not Ctl3d then
     b := 1
  else
     b := 0;
  FButton.Top := b;
  FButton.Height := ClientHeight - b * 2;
  FButton.Width := FButton.Height;
  FButton.Left := ClientWidth - FButton.Width - b;
end;

{ TssButtonEdit }

procedure TssButtonEdit.DoButtonClick;
begin
   if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
end;

end.
