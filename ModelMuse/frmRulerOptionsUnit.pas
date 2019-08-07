{@abstract(The main purpose of @name is to define @link(TfrmRulerOptions)
  which is used to specify how the rulers on the various
  views of the model will be formatted.)}
unit frmRulerOptionsUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, frameRulerOptionsUnit, Buttons,
  ExtCtrls, ComCtrls, GrayTabs;

type
  {@abstract(@name is used to specify how the rulers on the various
    views of the model will be formatted.)}
  TfrmRulerOptions = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: @link(TframeRulerOptions);
    // @name displays the options related to the X-axis on the front view
    // of the model.
    frameFrontX: TframeRulerOptions;
    // @name: @link(TframeRulerOptions);
    // @name displays the options related to the Z-axis on the front view
    // of the model.
    frameFrontZ: TframeRulerOptions;
    // @name: @link(TframeRulerOptions);
    // @name displays the options related to the Y-axis on the side view
    // of the model.
    frameSideY: TframeRulerOptions;
    // @name: @link(TframeRulerOptions);
    // @name displays the options related to the Z-axis on the side view
    // of the model.
    frameSideZ: TframeRulerOptions;
    // @name: @link(TframeRulerOptions);
    // @name displays the options related to the X-axis on the top view
    // of the model.
    frameTopX: TframeRulerOptions;
    // @name: @link(TframeRulerOptions);
    // @name displays the options related to the Y-axis on the top view
    // of the model.
    frameTopY: TframeRulerOptions;
    // @name: TPageControl;
    // @name holds tabs relating to each of the axes in each view of the model.
    pcMain: TPageControl;
    // @name: TPanel;
    // @name holds the buttons on the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TTabSheet;
    // @name holds @link(frameFrontX).
    tabFrontX: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(frameFrontZ).
    tabFrontZ: TTabSheet;
    // @name holds @link(frameSideY).
    // @name: TTabSheet;
    tabSideY: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(frameSideZ).
    tabSideZ: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(frameTopX).
    tabTopX: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(frameTopY).
    tabTopY: TTabSheet;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name calls TframeRulerOptions.@link(TframeRulerOptions.GetData)
    // for each @link(TframeRulerOptions) and then sets
    // @link(pcMain).ActivePageIndex.
    procedure GetData;
    // @name calls TframeRulerOptions.@link(TframeRulerOptions.SetData)
    // for each @link(TframeRulerOptions).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit;

{$R *.dfm}

{ TfrmRulerOptions }

procedure TfrmRulerOptions.GetData;
var
  RulerList: TList;
  ItemIndex: integer;
begin
  frameTopX.GetData(frmGoPhast.frameTopView.rulHorizontal);
  frameTopY.GetData(frmGoPhast.frameTopView.rulVertical);
  frameFrontX.GetData(frmGoPhast.frameFrontView.rulHorizontal);
  frameFrontZ.GetData(frmGoPhast.frameFrontView.rulVertical);
  frameSideY.GetData(frmGoPhast.frameSideView.rulVertical);
  frameSideZ.GetData(frmGoPhast.frameSideView.rulHorizontal);

  RulerList := TList.Create;
  try
    RulerList.Add(frmGoPhast.frameTopView.rulHorizontal);
    RulerList.Add(frmGoPhast.frameTopView.rulVertical);
    RulerList.Add(frmGoPhast.frameFrontView.rulHorizontal);
    RulerList.Add(frmGoPhast.frameFrontView.rulVertical);
    RulerList.Add(frmGoPhast.frameSideView.rulVertical);
    RulerList.Add(frmGoPhast.frameSideView.rulHorizontal);
    ItemIndex := RulerList.IndexOf(frmGoPhast.ClickedRuler);
    if ItemIndex >= 0 then
    begin
      pcMain.ActivePageIndex := ItemIndex;
    end;

  finally
    RulerList.Free;
  end;

end;

procedure TfrmRulerOptions.SetData;
begin
  frameTopX.SetData;
  frameTopY.SetData;
  frameFrontX.SetData;
  frameFrontZ.SetData;
  frameSideY.SetData;
  frameSideZ.SetData;
end;

procedure TfrmRulerOptions.FormCreate(Sender: TObject);
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmRulerOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

end.

