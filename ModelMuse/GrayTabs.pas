{
This unit interposes a new definition of TTabSheet that will always be drawn
with clBtnFace color.

modified from https://theroadtodelphi.com/2012/04/12/creating-colorful-tabsheets-with-the-vcl-styles/
by Rodrigo Ruz.
}

unit GrayTabs;

interface

uses
  Vcl.ComCtrls, Winapi.Messages, System.Types, Vcl.Graphics, Vcl.Themes,
  Winapi.Windows;

type
  TTabSheet = class(Vcl.ComCtrls.TTabSheet)
  private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

implementation

type
  TPageControlHelper = class helper for TPageControl
  public
    procedure UpdateTab2(Page: Vcl.ComCtrls.TTabSheet);
  end;

procedure TPageControlHelper.UpdateTab2(Page: Vcl.ComCtrls.TTabSheet);
begin
  if Page = nil then
  begin
    Exit;
  end;
  if (Page.TabIndex >= 0) and (Page.TabIndex < PageCount) then
  begin
    Tabs[Page.TabIndex] := Page.Caption;
  end;
//  Self.UpdateTab(Page);
end;

{ TTabSheet }

procedure TTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  LRect  : TRect;
  LSize  : Integer;
  LCanvas: TCanvas;
begin
  if (PageControl <> nil) and StyleServices.Enabled and
     ((PageControl.Style = tsTabs) or TStyleManager.IsCustomStyleActive) then
  begin
    //Get the bounds of the Tabsheet
    GetWindowRect(Handle, LRect);
    OffsetRect(LRect, -LRect.Left, -LRect.Top);
    //Get the size of the border
    LSize := ClientToParent(Point(0, 0)).X;
    InflateRect(LRect, LSize, LSize); // remove the border
    //create a TCanvas for erase the background, using the DC of the message
    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := Message.DC;
      LCanvas.Brush.Color:=clBtnFace;
      LCanvas.FillRect(LRect);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

    Message.Result := 1;
    //the call to this method produces which the Style hook paint the active tabsheet
    PageControl.UpdateTab2(PageControl.ActivePage);
  end
  else
    inherited;
end;

end.
