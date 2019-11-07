
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeBar;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs, Controls, ExtCtrls,
  ShellApi, Registry;


const
  // AppBar's user notification message
  WM_APPBARNOTIFY = WM_USER + 100;

  // Timer interval
  SLIDE_DEF_TIMER_INTERVAL = 400;  // milliseconds

  // Defaults
  AB_DEF_SIZE_INC_X  = 320;
  AB_DEF_SIZE_INC_Y  = 36;
  AB_DEF_DOCK_SIZE = 32;
  AB_DEF_ROOT_KEY  = HKEY_CURRENT_USER;
  AB_DEF_KEY_NAME: string  = 'Software\Office Panel';


type
  // You can send to the Windows shell one of the following messages:
  // Message             Description
  // --------------      --------------------------------------------------
  // ABM_NEW             Register a new AppBar to the system
  // ABM_REMOVE          Remove a previously created AppBar from the system
  // ABM_QUERYPOS        Query the AppBar position
  // ABM_SETPOS          Set the AppBar position
  // ABM_GETSTATE        Get the edge the Appbar is docked to
  // ABM_GETksoApplicationBarPOS   Get the Explorer AppBar position
  // ABM_ACTIVATE        Activate the AppBar
  // ABM_GETAUTOHIDEBAR  Query if AppBar has Auto-hide behavior
  // ABM_SETAUTOHIDEBAR  Set the AppBar's Auto-hide behavior

  // The ABM_message constants are defined in SHELLAPI.PAS as follows:
  // ABM_NEW              = $00000000;
  // ABM_REMOVE           = $00000001;
  // ABM_QUERYPOS         = $00000002;
  // ABM_SETPOS           = $00000003;
  // ABM_GETSTATE         = $00000004;
  // ABM_GETksoApplicationBarPOS    = $00000005;
  // ABM_ACTIVATE         = $00000006;
  // ABM_GETAUTOHIDEBAR   = $00000007;
  // ABM_SETAUTOHIDEBAR   = $00000008;
  // ABM_WINDOWPOSCHANGED = $00000009;

  // The following enumerated type defines the constants in the table
  TksoApplicationBarMessage = (abmNew, abmRemove, abmQueryPos, abmSetPos, abmGetState,
                    abmGeTksoApplicationBarPos, abmActivate, abmGetAutoHideBar,
                    abmSetAutoHideBar, abmWindowPosChanged);

  // An AppBar can be in one of 6 states shown in the table below:
  // State          Description
  // -----------    -----------------------------------------------------
  // ABE_UNKNOWN    The AppBar is in an unknown state
  //                (usually during construction/destruction)
  // ABE_FLOAT      The AppBar is floating on the screen
  // ABE_LEFT       The AppBar is docked on the left   edge of the screen
  // ABE_TOP        The AppBar is docked on the top    edge of the screen
  // ABE_RIGHT      The AppBar is docked on the right  edge of the screen
  // ABE_BOTTOM     The AppBar is docked on the bottom edge of the screen

  // The ABE_edge state constants are defined in SHELLAPI.PAS as follows:
  // ABE_LEFT    = 0;
  // ABE_TOP     = 1;
  // ABE_RIGHT   = 2;
  // ABE_BOTTOM  = 3;

  // The ABE_UNKNOWN and ABE_FLOAT constants are defined here as follows:
  // ABE_UNKNOWN = 4;
  // ABE_FLOAT   = 5;

  // The following enumerated type defines the constants in the table
  // (Values are mutually exclusive)

  TksoApplicationBarEdge = (abeLeft, abeTop, abeRight, abeBottom, abeUnknown, abeFloat);

  // An AppBar can have several behavior flags as shown below:
  // Flag                        Description
  // --------------------------- -----------------------------------
  // ABF_ALLOWLEFT               Allow dock on left   of screen
  // ABF_ALLOWRIGHT              Allow dock on right  of screen
  // ABF_ALLOWTOP                Allow dock on top    of screen
  // ABF_ALLOWBOTTOM             Allow dock on bottom of screen
  // ABF_ALLOWFLOAT              Allow float in the middle of screen

  // The following enumerated type defines the constants in the table
  TksoApplicationBarFlag = (abfAllowLeft, abfAllowTop, abfAllowRight, abfAllowBottom,
                 abfAllowFloat);
  TksoApplicationBarFlags = set of TksoApplicationBarFlag;

  // The following enumerated type defines the AppBar behavior in the AppBar
  TksoApplicationBarTaskEntry = (abtShow, abtHide, abtFloatDependent);

  // The record below contains all of the AppBar settings that
  // can be saved/loaded in/from the Registry
  TksoApplicationBarSettings = record
    cbSize         : Integer;        // Size of this structure
    abEdge         : TksoApplicationBarEdge;  // ABE_UNKNOWN, ABE_FLOAT, or ABE_edge
    abFlags        : TksoApplicationBarFlags; // ABF_* flags
    bAutohide      : Boolean;      // Should AppBar be auto-hidden when docked?
    bAlwaysOnTop   : Boolean;      // Should AppBar always be on top?
    bSlideEffect   : Boolean;      // Should AppBar slide?
    nTimerInterval : Integer;      // Slide Timer Interval (determines speed)
    szSizeInc      : TSize;        // Discrete width/height size increments
    szDockSize     : TSize;        // Width/Height for docked bar
    rcFloat        : TRect;        // Floating rectangle in screen coordinates
    nMinWidth      : Integer;      // Min allowed width
    nMinHeight     : Integer;      // Min allowed height
    nMaxWidth      : Integer;      // Max allowed width
    nMaxHeight     : Integer;      // Max allowed height
    szMinDockSize  : TSize;        // Min Width/Height when docked
    szMaxDockSize  : TSize;        // Max Width/Height when docked
    abTaskEntry    : TksoApplicationBarTaskEntry; // AppBar behavior in the AppBar
  end;

  // The record below contains the settings location in the registry
  TksoApplicationBarSettingsLocation = record
    nRootKey : Integer;  // HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE
    KeyName  : String;   // Key Name starting from root
  end;

  // TksoApplicationBar class ////////////////////////////////////////////////////////////
  TksoApplicationBar = class(TForm)
  private
  { Internal implementation state variables }
    // This AppBar's settings info
    FABS : TksoApplicationBarSettings;
    // We need a member variable which tracks the proposed edge of the
    // AppBar while the user is moving it, deciding where to position it.
    // While not moving, this member must contain ABE_UNKNOWN so that
    // GetEdge returns the current edge contained in FABS.abEdge.
    // While moving the AppBar, FabEdgeProposedPrev contains the
    // proposed edge based on the position of the AppBar.  The proposed
    // edge becomes the new edge when the user stops moving the AppBar.
    FabEdgeProposedPrev : TksoApplicationBarEdge;
    // We need a member variable which tracks whether a full screen
    // application window is open
    FbFullScreenAppOpen : Boolean;
    // We need a member variable which tracks whether our autohide window
    // is visible or not
    FbAutoHideIsVisible : Boolean;
    // We need a timer to to determine when the AppBar should be re-hidden
    FTimer : TTimer;
    // We need a member variable to store the settings location in the registry
    FabSettingsLocation : TksoApplicationBarSettingsLocation;
    FFlags: TksoApplicationBarFlags;
  { Internal implementation functions }
    // Modifies window creation flags
    procedure CreateParams (var Params: TCreateParams); override;
    // These functions encapsulate the shell's SHAppBarMessage function
    function AppBarMessage (abMessage : TksoApplicationBarMessage;
                            abEdge    : TksoApplicationBarEdge;
                            lParam    : LPARAM;
                            bRect     : Boolean;
                            var rc    : TRect) : UINT;
    // Gets a edge (ABE_FLOAT or ABE_edge) from a point (screen coordinates)
    function CalcProposedState (var pt : TSmallPoint) : TksoApplicationBarEdge;
    // Gets a retangle position (screen coordinates) from a proposed state
    procedure GetRect(abEdgeProposed : TksoApplicationBarEdge; var rcProposed : TRect);
    // Adjusts the AppBar's location to account for autohide
    // Returns TRUE if rectangle was adjusted
    function AdjustLocationForAutohide (bShow  : Boolean;
                                        var rc : TRect) : Boolean;
    // If AppBar is Autohide and docked, shows/hides the AppBar
    procedure ShowHiddenAppBar (bShow : Boolean);
    // When Autohide AppBar is shown/hidden, slides in/out of view
    procedure SlideWindow (var rcEnd : TRect);
    // Returns which edge we're autohidden on or ABE_UNKNOWN
    function GetAutohideEdge : TksoApplicationBarEdge;
    // Returns a TSmallPoint that gives the cursor position in screen coords
    function GetMessagePosition : TSmallPoint;
    // Changes the style of a window (translated from AfxModifyStyle)
    function ModifyStyle (hWnd         : THandle;
                          nStyleOffset : Integer;
                          dwRemove     : Integer;
                          dwAdd        : Integer;
                          nFlags       : UINT) : Boolean;
    // Property sets and gets
    function GetKeyName: String;
    procedure SetFlags(const Value: TksoApplicationBarFlags);
    procedure SetKeyName(const Value: String);
    function GetRootKey: Integer;
    procedure SetRootKey(const Value: Integer);
    function GetTaskEntry: TksoApplicationBarTaskEntry;
    procedure SetTaskEntry(const Value: TksoApplicationBarTaskEntry);
    function GetMaxVertDockSize: Longint;
    procedure SetMaxVertDockSize(const Value: Longint);
    function GetMaxHorzDockSize: Longint;
    procedure SetMaxHorzDockSize(const Value: Longint);
    function GetHorzSizeInc: Longint;
    procedure SetHorzSizeInc(const Value: Longint);
    function GetVertSizeInc: Longint;
    procedure SetVertSizeInc(const Value: Longint);
    function GetAutoHide: Boolean;
    procedure SetAutoHide(const Value: Boolean);
    function GetAlwaysOnTop: Boolean;
    procedure SetAlwaysOnTop(const Value: Boolean);
    function GetSlideEffect: Boolean;
    procedure SetSlideEffect(const Value: Boolean);
    function GetHorzDockSize: Longint;
    procedure SetHorzDockSize(const Value: Longint);
    function GetVertDockSize: Longint;
    procedure SetVertDockSize(const Value: Longint);
    function GetFloatLeft: Integer;
    procedure SetFloatLeft(const Value: Integer);
    function GetFloatTop: Integer;
    procedure SetFloatTop(const Value: Integer);
    function GetFloatRight: Integer;
    procedure SetFloatRight(const Value: Integer);
    function GetFloatBottom: Integer;
    procedure SetFloatBottom(const Value: Integer);
    function GetMinWidth: Integer;
    procedure SetMinWidth(const Value: Integer);
    function GetMinHeight: Integer;
    procedure SetMinHeight(const Value: Integer);
    function GetMaxWidth: Integer;
    procedure SetMaxWidth(const Value: Integer);
    function GetMaxHeight: Integer;
    procedure SetMaxHeight(const Value: Integer);
    function GetMinHorzDockSize: Longint;
    procedure SetMinHorzDockSize(const Value: Longint);
    function GetMinVertDockSize: Longint;
    procedure SetMinVertDockSize(const Value: Longint);
  protected
  { Property selector functions }
    // Retrieves the AppBar's edge.  If the AppBar is being positioned, its
    // Changes the slide time interval
    procedure SetSlideTime (nInterval : Integer);
  { Overridable functions }
    // Called when the AppBar's proposed state changes
    procedure OnAppBarStateChange (bProposed      : Boolean;
                                   abEdgeProposed : TksoApplicationBarEdge); virtual;
    // Called if user attempts to dock an Autohide AppBar on
    // an edge that already contains an Autohide AppBar
    procedure OnAppBarForcedToDocked; virtual;
    // Called when AppBar gets an ABN_FULLSCREENAPP notification
    procedure OnABNFullScreenApp (bOpen : Boolean); virtual;
    // Called when AppBar gets an ABN_POSCHANGED notification
    procedure OnABNPosChanged; virtual;
    // Called when AppBar gets an ABN_WINDOWARRANGE notification
    procedure OnABNWindowArrange (bBeginning : Boolean); virtual;
  { Message handlers }
    // Called when the AppBar receives a WM_AppBarNOTIFY window message
    procedure OnAppBarCallbackMsg(var Msg : TMessage); message WM_APPBARNOTIFY;
    // Called when the AppBar form is first created
    procedure OnCreate (var Msg: TWMCreate); message WM_CREATE;
    // Called when the AppBar form is about to be destroyed
    procedure OnDestroy (var Msg : TWMDestroy); message WM_DESTROY;
    procedure OnGetMinMaxInfo (var Msg : TWMGetMinMaxInfo);
                                                      message WM_GETMINMAXINFO;
  { AppBar-specific helper functions }
    // Returns TRUE if abEdge is ABE_LEFT or ABE_RIGHT, else FALSE is returned
    function IsEdgeLeftOrRight (abEdge : TksoApplicationBarEdge) : Boolean;
    // Returns TRUE if abEdge is ABE_TOP or ABE_BOTTOM, else FALSE is returned
    function IsEdgeTopOrBottom (abEdge : TksoApplicationBarEdge) : Boolean;
    // Returns TRUE if abEdge is ABE_FLOAT, else FALSE is returned
    function IsFloating (abEdge : TksoApplicationBarEdge) : Boolean;
    // Returns TRUE if abFlags contain an at least allowed edge to dock on
    function IsDockable (abFlags : TksoApplicationBarFlags) : Boolean;
    // Returns TRUE if abFlags contain abfAllowLeft and abfAllowRight
    function IsDockableVertically (abFlags : TksoApplicationBarFlags) : Boolean;
    // Returns TRUE if abFlags contain abfAllowTop and abfAllowBottom
    function IsDockableHorizontally (abFlags : TksoApplicationBarFlags) : Boolean;
    // Forces the shell to update its AppBar list and the workspace area
    procedure ResetSystemKnowledge;
    // Returns a proposed edge or ABE_FLOAT based on ABF_* flags and a
    // point specified in screen coordinates
    function GetEdgeFromPoint (abFlags : TksoApplicationBarFlags;
                               pt      : TSmallPoint) : TksoApplicationBarEdge;
  public
  { Public member functions }
    // proposed state is returned instead
    function GetEdge : TksoApplicationBarEdge;
    // Changes the AppBar's edge to ABE_UNKNOWN, ABE_FLOAT or an ABE_edge
    procedure SetEdge (abEdge : TksoApplicationBarEdge);
    function AppBarMessage1 (abMessage : TksoApplicationBarMessage) : UINT;
    function AppBarMessage2 (abMessage : TksoApplicationBarMessage;
                             abEdge    : TksoApplicationBarEdge) : UINT;
    function AppBarMessage3 (abMessage : TksoApplicationBarMessage;
                             abEdge    : TksoApplicationBarEdge;
                             lParam    : LPARAM) : UINT;
    function AppBarMessage4 (abMessage : TksoApplicationBarMessage;
                             abEdge    : TksoApplicationBarEdge;
                             lParam    : LPARAM;
                             var rc    : TRect) : UINT;
    // Called when the AppBar receives a WM_WINDOWPOSCHANGED message
    procedure OnWindowPosChanged (var Msg : TWMWindowPosChanged);
                                                   message WM_WINDOWPOSCHANGED;
    // Called when the AppBar receives a WM_ACTIVATE message
    procedure OnActivate (var Msg : TWMActivate); message WM_ACTIVATE;
    // Called every timer tick
    procedure OnAppBarTimer (Sender : TObject);
    // Called when the AppBar receives a WM_NCMOUSEMOVE message
    procedure OnNcMouseMove (var Msg : TWMNCMouseMove); message WM_NCMOUSEMOVE;
    // Called when the AppBar receives a WM_NCHITTEST message
    procedure OnNcHitTest (var Msg: TWMNCHitTest); message WM_NCHITTEST;
    // Called when the AppBar receives a WM_ENTERSIZEMOVE message
    procedure OnEnterSizeMove (var Msg : TMessage); message WM_ENTERSIZEMOVE;
    // Called when the AppBar receives a WM_EXITSIZEMOVE message
    procedure OnExitSizeMove (var Msg : TMessage); message WM_EXITSIZEMOVE;
    // Called when the AppBar receives a WM_MOVING message
    procedure OnMoving (var Msg : TMessage); message WM_MOVING;
    // Called when the AppBar receives a WM_SIZING message
    procedure OnSizing (var Msg : TMessage); message WM_SIZING;
    // Called when the AppBar receives a WM_GETMINMAXINFO message
    // Constructs an AppBar
    constructor Create (Owner : TComponent); override;
    // Destroys a previously created AppBar
    destructor Destroy; override;
    // Forces the AppBar's visual appearance to match its internal state
    procedure UpdateBar; virtual;
    // Loads settings from the registry at RootKey and KeyName location.
    // Returns TRUE if the settings are available, else FALSE
    function LoadSettings : Boolean; virtual;
    // Saves settings into the registry at RootKey and KeyName location.
    // Returns TRUE if succeeded, else FALSE
    function SaveSettings : Boolean; virtual;
  published
  { Properties }
    // Allowed dockable edges
    property Flags : TksoApplicationBarFlags read FFlags write SetFlags;
    property HorzSizeInc: Longint read GetHorzSizeInc
      write SetHorzSizeInc;
    property VertSizeInc: Longint read GetVertSizeInc
      write SetVertSizeInc;
    property Edge: TksoApplicationBarEdge read GetEdge write SetEdge;
    property AutoHide: Boolean read GetAutoHide write SetAutoHide;
    property AlwaysOnTop: Boolean read GetAlwaysOnTop
      write SetAlwaysOnTop;
    property SlideEffect: Boolean read GetSlideEffect write SetSlideEffect;
    property SlideTime: Integer read FABS.nTimerInterval write SetSlideTime;
    property HorzDockSize: Longint read GetHorzDockSize
      write SetHorzDockSize;
    property VertDockSize: Longint read GetVertDockSize
      write SetVertDockSize;
    property FloatLeft: Integer read GetFloatLeft write SetFloatLeft;
    property FloatTop: Integer read GetFloatTop write SetFloatTop;
    property FloatRight: Integer read GetFloatRight write SetFloatRight;
    property FloatBottom: Integer read GetFloatBottom write SetFloatBottom;
    property MinWidth: Integer read GetMinWidth write SetMinWidth;
    property MinHeight: Integer read GetMinHeight write SetMinHeight;
    property MaxWidth: Integer read GetMaxWidth write SetMaxWidth;
    property MaxHeight: Integer read GetMaxHeight write SetMaxHeight;
    property MinHorzDockSize: Longint read GetMinHorzDockSize
      write SetMinHorzDockSize;
    property MinVertDockSize: Longint read GetMinVertDockSize
      write SetMinVertDockSize;
    property MaxHorzDockSize: Longint read GetMaxHorzDockSize
      write SetMaxHorzDockSize;
    property MaxVertDockSize: Longint read GetMaxVertDockSize
      write SetMaxVertDockSize;
    property TaskEntry: TksoApplicationBarTaskEntry read GetTaskEntry
      write SetTaskEntry;
    property RootKey: Integer read GetRootKey write SetRootKey;
    property KeyName: String read GetKeyName write SetKeyName;
  end;


implementation {===============================================================}

{ Internal implementation functions }

procedure TksoApplicationBar.CreateParams (var Params: TCreateParams);
var
  dwAdd, dwRemove, dwAddEx, dwRemoveEx : Integer;
begin
  // Call the inherited first
  inherited CreateParams(Params);

  // Styles to be added
  dwAdd := 0;
  dwAddEx := WS_EX_TOOLWINDOW or WS_EX_WINDOWEDGE;

  // Styles to be removed
  dwRemove := WS_SYSMENU or WS_MAXIMIZEBOX or
    WS_MINIMIZEBOX or WS_CAPTION;
  dwRemoveEx := WS_EX_APPWINDOW;

  // Modify style flags
  {$R-}
  with Params do begin
    Style := Style and (not dwRemove);
    Style := Style or dwAdd;
    ExStyle := ExStyle and (not dwRemoveEx);
    ExStyle := ExStyle or dwAddEx;
  end;
end;

function TksoApplicationBar.AppBarMessage (abMessage : TksoApplicationBarMessage;
                                abEdge    : TksoApplicationBarEdge;
                                lParam    : LPARAM;
                                bRect     : Boolean;
                                var rc    : TRect) : UINT;
var
  abd : TAppBarData;
begin
  // Initialize an AppBarDATA structure
  abd.cbSize := sizeof(abd);
  abd.hWnd := Handle;
  abd.uCallbackMessage := WM_APPBARNOTIFY;
  abd.uEdge := Ord(abEdge);
  if bRect then
    abd.rc := rc
  else
    abd.rc := Rect(0, 0, 0, 0);
  abd.lParam := lParam;
  Result := SHAppBarMessage(Ord(abMessage), abd);

  // If the caller passed a rectangle, return the updated rectangle
  if bRect then
    rc := abd.rc;
end;

function TksoApplicationBar.AppBarMessage1 (abMessage : TksoApplicationBarMessage) : UINT;
var
  rc : TRect;
begin
  Result := AppBarMessage(abMessage, abeFloat, 0, False, rc);
end;

function TksoApplicationBar.AppBarMessage2 (abMessage : TksoApplicationBarMessage;
                                 abEdge    : TksoApplicationBarEdge) : UINT;
var
  rc : TRect;
begin
  Result := AppBarMessage(abMessage, abEdge, 0, False, rc);
end;

function TksoApplicationBar.AppBarMessage3 (abMessage : TksoApplicationBarMessage;
                                 abEdge    : TksoApplicationBarEdge;
                                 lParam    : LPARAM) : UINT;
var
  rc : TRect;
begin
  Result := AppBarMessage(abMessage, abEdge, lParam, False, rc);
end;

function TksoApplicationBar.AppBarMessage4 (abMessage : TksoApplicationBarMessage;
                                 abEdge    : TksoApplicationBarEdge;
                                 lParam    : LPARAM;
                                 var rc    : TRect) : UINT;
begin
  Result := AppBarMessage(abMessage, abEdge, lParam, True, rc);
end;

function TksoApplicationBar.CalcProposedState (var pt : TSmallPoint) : TksoApplicationBarEdge;
var
  bForceFloat : Boolean;
begin
  // Force the AppBar to float if the user is holding down the Ctrl key
  // and the AppBar's style allows floating
  bForceFloat := ((GetKeyState(VK_CONTROL) and $8000) <> 0) and
                 (abfAllowFloat in FABS.abFlags);
  if bForceFloat then
    Result := abeFloat
  else
    Result := GetEdgeFromPoint(FABS.abFlags, pt);
end;

procedure TksoApplicationBar.GetRect (abEdgeProposed : TksoApplicationBarEdge;
                           var rcProposed : TRect);
begin
  // This function finds the x, y, cx, cy of the AppBar window
  if abEdgeProposed = abeFloat then begin
    // The AppBar is floating, the proposed rectangle is correct
  end else begin
    // The AppBar is docked or auto-hide
    // Set dimensions to full screen
    with rcProposed do begin
      Left   := 0;
      Top    := 0;
      Right  := GetSystemMetrics(SM_CXSCREEN);
      Bottom := GetSystemMetrics(SM_CYSCREEN);
    end;

    // Subtract off what we want from the full screen dimensions
    if not FABS.bAutohide then
      // Ask the shell where we can dock
      AppBarMessage4(abmQueryPos, abEdgeProposed, LPARAM(False), rcProposed);

    case abEdgeProposed of
      abeLeft:
        rcProposed.Right  := rcProposed.Left   + FABS.szDockSize.cx;
      abeTop:
        rcProposed.Bottom := rcProposed.Top    + FABS.szDockSize.cy;
      abeRight:
        rcProposed.Left   := rcProposed.Right  - FABS.szDockSize.cx;
      abeBottom:
        rcProposed.Top    := rcProposed.Bottom - FABS.szDockSize.cy;
    end; // end of case

  end; // end of else
end;

function TksoApplicationBar.AdjustLocationForAutohide (bShow  : Boolean;
                                            var rc : TRect) : Boolean;
var
  x, y : Integer;
  cxVisibleBorder, cyVisibleBorder : Integer;
begin
  if ((GetEdge = abeUnknown) or (GetEdge = abeFloat) or
      (not FABS.bAutohide)) then begin
    // If we are not docked on an edge OR we are not auto-hidden, there is
    // nothing for us to do; just return
    Result := False;
    Exit;
  end;

  // Showing/hiding doesn't change our size; only our position
  x := 0; y := 0; // Assume a position of (0, 0)

  if bShow then
    // If we are on the right or bottom, calculate our visible position
    case GetEdge of
      abeRight:
        x := GetSystemMetrics(SM_CXSCREEN) - (rc.Right - rc.Left);
      abeBottom:
        y := GetSystemMetrics(SM_CYSCREEN) - (rc.Bottom - rc.Top);
    end
  else begin
    // Keep a part of the AppBar visible at all times
    cxVisibleBorder := 2 * GetSystemMetrics(SM_CXBORDER);
    cyVisibleBorder := 2 * GetSystemMetrics(SM_CYBORDER);

    // Calculate our x or y coordinate so that only the border is visible
    case GetEdge of
      abeLeft:
        x := -((rc.Right - rc.Left) - cxVisibleBorder);
      abeRight:
        x := GetSystemMetrics(SM_CXSCREEN) - cxVisibleBorder;
      abeTop:
        y := -((rc.Bottom - rc.Top) - cyVisibleBorder);
      abeBottom:
        y := GetSystemMetrics(SM_CYSCREEN) - cyVisibleBorder;
    end;
  end;

  with rc do begin
    Right  := x + (Right - Left);
    Bottom := y + (Bottom - Top);
    Left   := x;
    Top    := y;
  end;

  Result := True;
end;

procedure TksoApplicationBar.ShowHiddenAppBar (bShow : Boolean);
var
  rc : TRect;
begin
  // Get our window location in screen coordinates
  GetWindowRect(Handle, rc);

  // Assume  that we are visible
  FbAutoHideIsVisible := True;

  if AdjustLocationForAutohide(bShow, rc) then begin
    // the rectangle was adjusted, we are an autohide bar
    // Remember whether we are visible or not
    FbAutoHideIsVisible := bShow;

    // Slide window in from or out to the edge
    SlideWindow(rc);
  end;
end;

procedure TksoApplicationBar.SlideWindow (var rcEnd : TRect);
var
  bFullDragOn : LongBool;
  rcStart : TRect;
  dwTimeStart, dwTimeEnd, dwTime: Integer;
  x, y, w, h : Integer;
begin
  // Only slide the window if the user has FullDrag turned on
  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, @bFullDragOn, 0);

  // Get the current window position
  GetWindowRect(Handle, rcStart);
  if (FABS.bSlideEffect and bFullDragOn and
      ((rcStart.Left   <> rcEnd.Left  ) or
       (rcStart.Top    <> rcEnd.Top   ) or
       (rcStart.Right  <> rcEnd.Right ) or
       (rcStart.Bottom <> rcEnd.Bottom))) then begin

    // Get our starting and ending time
    dwTimeStart := GetTickCount;
    dwTimeEnd := dwTimeStart + FABS.nTimerInterval;
    dwTime := dwTimeStart;
    while (dwTime < dwTimeEnd) do begin
      // While we are still sliding, calculate our new position
      x := rcStart.Left - (rcStart.Left - rcEnd.Left)
           * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      y := rcStart.Top  - (rcStart.Top  - rcEnd.Top)
           * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      w := (rcStart.Right - rcStart.Left)
           - ((rcStart.Right - rcStart.Left) - (rcEnd.Right - rcEnd.Left))
           * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      h := (rcStart.Bottom - rcStart.Top)
           - ((rcStart.Bottom - rcStart.Top) - (rcEnd.Bottom - rcEnd.Top))
           * Integer(dwTime - dwTimeStart) div FABS.nTimerInterval;

      // Show the window at its changed position
      SetWindowPos(Handle, 0, x, y, w, h,
                   SWP_NOZORDER or SWP_NOACTIVATE or SWP_DRAWFRAME);
      UpdateWindow(Handle);
      dwTime := GetTickCount;
    end;
  end;

  // Make sure that the window is at its final position
  Left   := rcEnd.Left;
  Top    := rcEnd.Top;
  Width  := rcEnd.Right - rcEnd.Left;
  Height := rcEnd.Bottom - rcEnd.Top;
end;

function TksoApplicationBar.GetAutohideEdge : TksoApplicationBarEdge;
begin
  if Handle = AppBarMessage2(abmGetAutoHideBar, abeLeft) then
    Result := abeLeft
  else if Handle = AppBarMessage2(abmGetAutoHideBar, abeTop) then
    Result := abeTop
  else if Handle = AppBarMessage2(abmGetAutoHideBar, abeRight) then
    Result := abeRight
  else if Handle = AppBarMessage2(abmGetAutoHideBar, abeBottom) then
    Result := abeBottom
  else
    // NOTE: If AppBar is docked but not auto-hidden, we return ABE_UNKNOWN
    Result := abeUnknown;
end;

function TksoApplicationBar.GetMessagePosition : TSmallPoint;
var
  pt : TSmallPoint;
  dw : Integer;
begin
  dw := GetMessagePos;
  pt.X := SHORT(dw);
  pt.Y := SHORT((dw and $FFFF0000) shr 16);
  Result := pt;
end;

function TksoApplicationBar.ModifyStyle (hWnd : THandle;
                              nStyleOffset : Integer;
                              dwRemove     : Integer;
                              dwAdd        : Integer;
                              nFlags       : UINT) : Boolean;
var
  dwStyle : Integer;
  dwNewStyle : Integer;
begin
  dwStyle := GetWindowLong(hWnd, nStyleOffset);
  dwNewStyle := (dwStyle and (not dwRemove)) or dwAdd;

  if dwStyle = dwNewStyle then begin
    Result := False;
    Exit;
  end;

  SetWindowLong(hWnd, nStyleOffset, dwNewStyle);

  if nFlags <> 0 then
    SetWindowPos(hWnd, 0, 0, 0, 0, 0,
      SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or nFlags);

  Result := True;
end;

{ Property selector functions }

function TksoApplicationBar.GetEdge : TksoApplicationBarEdge;
begin
  if FabEdgeProposedPrev <> abeUnknown then
    Result := FabEdgeProposedPrev
  else
    Result := FABS.abEdge;
end;

procedure TksoApplicationBar.SetEdge (abEdge : TksoApplicationBarEdge);
var
  abCurrentEdge : TksoApplicationBarEdge;
  currentRect : TRect;
  rc : TRect;
  hWnd : THandle;
begin
  // If the AppBar is registered as auto-hide, unregister it
  abCurrentEdge := GetAutohideEdge;

  if abCurrentEdge <> abeUnknown then
    // Our AppBar is auto-hidden, unregister it
    AppBarMessage3(abmSetAutoHideBar, abCurrentEdge, LPARAM(False));

  // Save the new requested state
  FABS.abEdge := abEdge;

  case abEdge of

    abeUnknown: begin
      // We are being completely unregistered.
      // Probably, the AppBar window is being destroyed.
      // If the AppBar is registered as NOT auto-hide, unregister it
      AppBarMessage1(abmRemove);
    end;

    abeFloat: begin
      // We are floating and therefore are just a regular window.
      // Tell the shell that the docked AppBar should be of 0x0 dimensions
      // so that the workspace is not affected by the AppBar
      currentRect := Rect(0, 0, 0, 0);
      AppBarMessage4(abmSetPos, abEdge, LPARAM(False), currentRect);
      Left   := FABS.rcFloat.Left;
      Top    := FABS.rcFloat.Top;
      Width  := FABS.rcFloat.Right - FABS.rcFloat.Left;
      Height := FABS.rcFloat.Bottom - FABS.rcFloat.Top;
    end;

    else begin
      if FABS.bAutohide and
         (AppBarMessage3(abmSetAutoHideBar,
                         GetEdge,
                         LPARAM(True)) = 0) then begin
        // We couldn't set the AppBar on a new edge, let's dock it instead
        FABS.bAutohide := False;
        // Call a virtual function to let derived classes know that the AppBar
        // changed from auto-hide to docked
        OnAppBarForcedToDocked;
      end;

      GetRect(GetEdge, rc);
      if FABS.bAutohide then begin
        currentRect := Rect(0, 0, 0, 0);
        AppBarMessage4(abmSetPos, abeLeft, LPARAM(False), currentRect);
      end else begin
        // Tell the shell where the AppBar is
        AppBarMessage4(abmSetPos, abEdge, LPARAM(False), rc);
      end;

      AdjustLocationForAutohide(FbAutoHideIsVisible, rc);

      // Slide window in from or out to the edge
      SlideWindow(rc);

    end; // end of else

  end; // end of case

  // Set the AppBar's z-order appropriately
  hWnd := HWND_NOTOPMOST; // Assume normal Z-Order
  if FABS.bAlwaysOnTop then begin
    // If we are supposed to be always-on-top, put us there
    hWnd := HWND_TOPMOST;
    if FbFullScreenAppOpen then
      // But, if a full-screen window is opened, put ourself at the bottom
      // of the z-order so that we don't cover the full-screen window
      hWnd := HWND_BOTTOM;
  end;
  SetWindowPos(Handle,
               hWnd,
               0, 0, 0, 0,
               SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);

  // Make sure that any auto-hide AppBars stay on top of us after we move
  // even though our activation state has not changed
  AppBarMessage1(abmActivate);

  // Tell our derived class that there is a state change
  OnAppBarStateChange(False, abEdge);

  // Show or hide the AppBar entry depending on AppBar position
  case FABS.abTaskEntry of
    abtShow :
      ShowWindow(Application.Handle, SW_SHOW);
    abtHide :
      ShowWindow(Application.Handle, SW_HIDE);
    abtFloatDependent :
      case abEdge of
        abeFloat:
          ShowWindow(Application.Handle, SW_SHOW);
        abeLeft, abeTop, abeRight, abeBottom :
          ShowWindow(Application.Handle, SW_HIDE);
      end;
  end;
end;

procedure TksoApplicationBar.SetSlideTime (nInterval : Integer);
begin
  FABS.nTimerInterval := nInterval;
  FTimer.Interval := nInterval;
end;

{ Overridable functions }

procedure TksoApplicationBar.OnAppBarStateChange (bProposed      : Boolean;
                                       abEdgeProposed : TksoApplicationBarEdge);
var
  bFullDragOn : LongBool;
begin
  // Find out if the user has FullDrag turned on
  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, @bFullDragOn, 0);

  // If FullDrag is turned on OR the AppBar has changed position
  if bFullDragOn or not bProposed then begin
    if abEdgeProposed = abeFloat then
      // Show the window adornments
      ModifyStyle(Handle,
                  GWL_STYLE,
                  WS_CAPTION or WS_SYSMENU,
                  0,
                  SWP_DRAWFRAME)
    else
      // Hide the window adornments
      ModifyStyle(Handle,
                  GWL_STYLE,
                  WS_CAPTION or WS_SYSMENU,
                  0,
                  SWP_DRAWFRAME);
  end;
end;

procedure TksoApplicationBar.OnAppBarForcedToDocked;
const
  CRLF = #10#13;
begin
  // Display the application name as the message box caption text.
  MessageDlg('There is already an auto hidden window on this edge.' + CRLF +
             'Only one auto hidden window is allowed on each edge.',
             mtInformation,
             [mbOk],
             0);
end;

procedure TksoApplicationBar.OnABNFullScreenApp (bOpen : Boolean);
begin
  // This function is called when a FullScreen window is openning or
  // closing. A FullScreen window is a top-level window that has its caption
  // above the top of the screen allowing the entire screen to be occupied
  // by the window's client area.

  // If the AppBar is a topmost window when a FullScreen window is activated,
  // we need to change our window to a non-topmost window so that the AppBar
  // doesn't cover the FullScreen window's client area.

  // If the FullScreen window is closing, we need to set the AppBar's
  // Z-Order back to when the user wants it to be.
  FbFullScreenAppOpen := bOpen;
  UpdateBar;
end;

procedure TksoApplicationBar.OnABNPosChanged;
begin
  // The AppBar or another AppBar has changed its size or position
  if (GetEdge <> abeFloat) and (not FABS.bAutohide) then
    // If we're not floating and we're not auto-hidden, we have to
    // reposition our window
    UpdateBar;
end;

procedure TksoApplicationBar.OnABNWindowArrange (bBeginning : Boolean);
begin
  // This function intentionally left blank
end;

{ Message handlers }

procedure TksoApplicationBar.OnAppBarCallbackMsg (var Msg : TMessage);
begin
  case Msg.WParam of

    ABN_FULLSCREENAPP:
      OnABNFullScreenApp(Msg.LParam <> 0);

    ABN_POSCHANGED:
      OnABNPosChanged;

    ABN_WINDOWARRANGE:
      OnABNWindowArrange(Msg.LParam <> 0);
  end;
end;

procedure TksoApplicationBar.OnCreate (var Msg : TWMCreate);
var
  hMenu : THandle;
begin
  inherited;
  // Associate a timer with the AppBar.  The timer is used to determine
  // when a visible, inactive, auto-hide AppBar should be re-hidden
  FTimer := TTimer.Create(Self);
  with FTimer do begin
    Interval := FABS.nTimerInterval;
    OnTimer := OnAppBarTimer;
    Enabled := True;
  end;

  // Save the initial position of the floating AppBar
  FABS.rcFloat.Left   := Left;
  FABS.rcFloat.Top    := Top;

  // Register our AppBar window with the Shell
  AppBarMessage1(abmNew);

  // Update AppBar internal state
  UpdateBar;

  // Save the initial size of the floating AppBar
  PostMessage(Handle, WM_ENTERSIZEMOVE, 0, 0);
  PostMessage(Handle, WM_EXITSIZEMOVE,  0, 0);

  // Remove system menu
  hMenu := GetSystemMenu(Handle, False);
  DeleteMenu(hMenu, SC_RESTORE,  MF_BYCOMMAND);
  DeleteMenu(hMenu, SC_MINIMIZE, MF_BYCOMMAND);
  DeleteMenu(hMenu, SC_MAXIMIZE, MF_BYCOMMAND);
end;

procedure TksoApplicationBar.OnDestroy (var Msg : TWMDestroy);
begin
  // Free the Autohide timer
  FTimer.Enabled := False;
  FTimer.Free;
  // Unregister our AppBar window with the Shell
  SetEdge(abeUnknown);
  inherited;
end;

procedure TksoApplicationBar.OnWindowPosChanged (var Msg : TWMWindowPosChanged);
begin
  inherited;
  // When our window changes position, tell the Shell so that any
  // auto-hidden AppBar on our edge stays on top of our window making it
  // always accessible to the user
  AppBarMessage1(abmWindowPosChanged);
end;

procedure TksoApplicationBar.OnActivate (var Msg : TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
    // Hide the AppBar if we are docked and auto-hidden
    ShowHiddenAppBar(False);
  // When our window changes position, tell the Shell so that any
  // auto-hidden AppBar on our edge stays on top of our window making it
  // always accessible to the user.
  AppBarMessage1(abmActivate);
end;

procedure TksoApplicationBar.OnAppBarTimer (Sender : TObject);
var
  pt : TSmallPoint;
  rc : TRect;
begin
  if GetActiveWindow <> Handle then begin
    // Possibly hide the AppBar if we are not the active window
    // Get the position of the mouse and the AppBar's position
    // Everything must be in screen coordinates
    pt := GetMessagePosition;
    GetWindowRect(Handle, rc);
    // Add a little margin around the AppBar
    InflateRect(rc,
                2 * GetSystemMetrics(SM_CXDOUBLECLK),
                2 * GetSystemMetrics(SM_CYDOUBLECLK));
    if not PtInRect(rc, SmallPointToPoint(pt)) then
      // If the mouse is NOT over the AppBar, hide the AppBar
      ShowHiddenAppBar(False);
  end;
  inherited;
end;


// TksoApplicationBar.OnNcMouseMove //////////////////////////////////////////////////////
procedure TksoApplicationBar.OnNcMouseMove (var Msg : TWMNCMouseMove);
begin
  // If we are a docked, auto-hidden AppBar, shown us
  // when the user moves over our non-client area
  ShowHiddenAppBar(True);
  inherited;
end;

procedure TksoApplicationBar.OnNcHitTest (var Msg: TWMNCHitTest);
var
  u : UINT;
  bPrimaryMouseBtnDown : Boolean;
  rcClient : TRect;
  pt : TPoint;
  vKey : Integer;
begin
  // Find out what the system thinks is the hit test code
  inherited;
  u := Msg.Result;

  // NOTE: If the user presses the secondary mouse button, pretend that the
  // user clicked on the client area so that we get WM_CONTEXTMENU messages
  if GetSystemMetrics(SM_SWAPBUTTON) <> 0 then
    vKey := VK_RBUTTON
  else
    vKey := VK_LBUTTON;
  bPrimaryMouseBtnDown := ((GetAsyncKeyState(vKey) and $8000) <> 0);

  pt.X := Msg.XPos;
  pt.Y := Msg.YPos;
  pt := ScreenToClient(pt);
  if (u = HTCLIENT) and bPrimaryMouseBtnDown
     {and (ControlAtPos(pt, False) = nil)} then
    // User clicked in client area, allow AppBar to move.  We get this
    // behavior by pretending that the user clicked on the caption area
    u := HTCAPTION;

  // If the AppBar is floating and the hittest code is a resize code...
  if ((GetEdge = abeFloat) and
      (HTSIZEFIRST <= u) and (u <= HTSIZELAST)) then begin
    case u of
      HTLEFT, HTRIGHT:
        if FABS.szSizeInc.cx = 0
          then u := HTBORDER;
      HTTOP, HTBOTTOM:
        if FABS.szSizeInc.cy = 0
          then u := HTBORDER;
      HTTOPLEFT:
        if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy = 0)
          then u := HTBORDER
        else if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy <> 0)
          then u := HTTOP
        else if (FABS.szSizeInc.cx <> 0) and (FABS.szSizeInc.cy = 0)
          then u := HTLEFT;
      HTTOPRIGHT:
        if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy = 0)
          then u := HTBORDER
        else if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy <> 0)
          then u := HTTOP
        else if (FABS.szSizeInc.cx <> 0) and (FABS.szSizeInc.cy = 0)
          then u := HTRIGHT;
      HTBOTTOMLEFT:
        if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy = 0)
          then u := HTBORDER
        else if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy <> 0)
          then u := HTBOTTOM
        else if (FABS.szSizeInc.cx <> 0) and (FABS.szSizeInc.cy = 0)
          then u := HTLEFT;
      HTBOTTOMRIGHT:
        if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy = 0)
          then u := HTBORDER
        else if (FABS.szSizeInc.cx = 0) and (FABS.szSizeInc.cy <> 0)
          then u := HTBOTTOM
        else if (FABS.szSizeInc.cx <> 0) and (FABS.szSizeInc.cy = 0)
          then u := HTRIGHT;
    end;
  end;

  // When the AppBar is docked, the user can resize only one edge.
  // This next section determines which edge the user can resize.
  // To allow resizing, the AppBar window must have the WS_THICKFRAME style

  // If the AppBar is docked and the hittest code is a resize code...
  if ((GetEdge <> abeFloat) and (GetEdge <> abeUnknown) and
      (HTSIZEFIRST <= u) and (u <= HTSIZELAST)) then begin

    if (IsEdgeLeftOrRight(GetEdge) and (FABS.szSizeInc.cx = 0)) or
       (not IsEdgeLeftOrRight(GetEdge) and (FABS.szSizeInc.cy = 0)) then begin
      // If the width/height size increment is zero, then resizing is NOT
      // allowed for the edge that the AppBar is docked on
      u := HTBORDER; // Pretend that the mouse is not on a resize border
    end else begin
      // Resizing IS allowed for the edge that the AppBar is docked on
      // Get the location of the AppBar's client area in screen coordinates
      rcClient := GetClientRect;
      pt.X := rcClient.Left;
      pt.Y := rcClient.Top;
      pt := ClientToScreen(pt);
      rcClient.Left := pt.X;
      rcClient.Top  := pt.Y;
      pt.X := rcClient.Right;
      pt.Y := rcClient.Bottom;
      pt := ClientToScreen(pt);
      rcClient.Right  := pt.X;
      rcClient.Bottom := pt.Y;

      u := HTBORDER;  // Assume that we can't resize
      case GetEdge of
        abeLeft:
          if Msg.XPos > rcClient.Right then
            u := HTRIGHT;
        abeTop:
          if Msg.YPos > rcClient.Bottom then
            u := HTBOTTOM;
        abeRight:
          if Msg.XPos < rcClient.Left then
            u := HTLEFT;
        abeBottom:
          if Msg.YPos < rcClient.Top then
            u := HTTOP;
      end; // end of case
    end; // end of else
  end;

  // Return the hittest code
  Msg.Result := u;
end;

procedure TksoApplicationBar.OnEnterSizeMove (var Msg : TMessage);
begin
  // The user started moving/resizing the AppBar, save its current state
  FabEdgeProposedPrev := GetEdge;
end;

procedure TksoApplicationBar.OnExitSizeMove (var Msg : TMessage);
var
  abEdgeProposedPrev : TksoApplicationBarEdge;
  rc, rcWorkArea : TRect;
  w, h : Integer;
begin
  // The user stopped moving/resizing the AppBar, set the new state
  // Save the new proposed state of the AppBar
  abEdgeProposedPrev := FabEdgeProposedPrev;

  // Set the proposed state back to unknown.  This causes GetState
  // to return the current state rather than the proposed state
  FabEdgeProposedPrev := abeUnknown;

  // Get the location of the window in screen coordinates
  GetWindowRect(Handle, rc);

  // If the AppBar's state has changed...
  if GetEdge = abEdgeProposedPrev then
    case GetEdge of
      abeLeft, abeRight:
        // Save the new width of the docked AppBar
        FABS.szDockSize.cx := rc.Right - rc.Left;
      abeTop, abeBottom:
        // Save the new height of the docked AppBar
        FABS.szDockSize.cy := rc.Bottom - rc.Top;
    end;

  // Always save the new position of the floating AppBar
  if abEdgeProposedPrev = abeFloat then begin
    // If AppBar was floating and keeps floating...
    if GetEdge = abeFloat then begin
      FABS.rcFloat := rc;
    // If AppBar was docked and is going to float...
    end else begin
      // Propose width and height depending on the current window position
      w := rc.Right - rc.Left;
      h := rc.Bottom - rc.Top;
      // Adjust width and height
      SystemParametersInfo(SPI_GETWORKAREA, 0, @rcWorkArea, 0);
      if (w >= (rcWorkArea.Right - rcWorkArea.Left)) or
         (h >= (rcWorkArea.Bottom - rcWorkArea.Top)) then begin
        w := FABS.rcFloat.Right - FABS.rcFloat.Left;
        h := FABS.rcFloat.Bottom - FABS.rcFloat.Top;
      end;
      // Save new floating position
      FABS.rcFloat.Left   := rc.Left;
      FABS.rcFloat.Top    := rc.Top;
      FABS.rcFloat.Right  := rc.Left + w;
      FABS.rcFloat.Bottom := rc.Top + h;
    end;
  end;

  // After setting the dimensions, set the AppBar to the proposed state
  SetEdge(abEdgeProposedPrev);
end;

procedure TksoApplicationBar.OnMoving (var Msg : TMessage);
var
  prc            : PRect;
  pt             : TSmallPoint;
  abEdgeProposed : TksoApplicationBarEdge;
  w, h           : Integer;
begin
  // We control the moving of the AppBar.  For example, if the mouse moves
  // close to an edge, we want to dock the AppBar
  // The lParam contains the window's position proposed by the system
  prc := PRect(Msg.LParam);

  // Get the location of the mouse cursor
  pt := GetMessagePosition;

  // Where should the AppBar be based on the mouse position?
  abEdgeProposed := CalcProposedState(pt);

  if ((FabEdgeProposedPrev <> abeFloat) and
      (abEdgeProposed = abeFloat)) then begin
    // While moving, the user took us from a docked/autohidden state to
    // the float state.  We have to calculate a rectangle location so that
    // the mouse cursor stays inside the window.
    prc^ := FABS.rcFloat;
    w := prc^.Right - prc^.Left;
    h := prc^.Bottom - prc^.Top;
    with prc^ do begin
      Left   := pt.X - w div 2;
      Top    := pt.Y;
      Right  := pt.X - w div 2 + w;
      Bottom := pt.Y + h;
    end;
  end;

  // Remember the most-recently proposed state
  FabEdgeProposedPrev := abEdgeProposed;

  // Tell the system where to move the window based on the proposed state
  GetRect(abEdgeProposed, prc^);

  // Tell our derived class that there is a proposed state change
  OnAppBarStateChange(True, abEdgeProposed);
end;

procedure TksoApplicationBar.OnSizing (var Msg : TMessage);
var
  prc : PRect;
  rcBorder : TRect;
  nWidthNew, nHeightNew : Integer;
begin
  // We control the sizing of the AppBar.  For example, if the user re-sizes
  // an edge, we want to change the size in discrete increments.
  // The lParam contains the window's position proposed by the system
  prc := PRect(Msg.LParam);

  // Get the minimum allowed size of the window depending on current edge.
  // This is the width/height of the window that must always be present
  with FABS do
    case abEdge of
      abeFloat:
        rcBorder := Rect(0, 0, nMinWidth, nMinHeight);
      else
        rcBorder := Rect(0, 0, szMinDockSize.cx, szMinDockSize.cy);
    end;

  // We force the window to resize in discrete units set by the FABS.szSizeInc
  // member.  From the new, proposed window dimensions passed to us, round
  // the width/height to the nearest discrete unit
  if FABS.szSizeInc.cx <> 0 then
    nWidthNew  := ((prc^.Right - prc^.Left) - (rcBorder.Right - rcBorder.Left)
                   + FABS.szSizeInc.cx div 2) div FABS.szSizeInc.cx
                   * FABS.szSizeInc.cx + (rcBorder.Right - rcBorder.Left)
  else
    nWidthNew  := prc^.Right - prc^.Left;

  if FABS.szSizeInc.cy <> 0 then
    nHeightNew := ((prc^.Bottom - prc^.Top) - (rcBorder.Bottom - rcBorder.Top)
                   + FABS.szSizeInc.cy div 2) div FABS.szSizeInc.cy
                   * FABS.szSizeInc.cy + (rcBorder.Bottom - rcBorder.Top)
  else
    nHeightNew := prc^.Bottom - prc^.Top;

  // Adjust the rectangle's dimensions
  case Msg.wParam of
    WMSZ_LEFT:
      prc^.Left   := prc^.Right  - nWidthNew;

    WMSZ_TOP:
      prc^.Top    := prc^.Bottom - nHeightNew;

    WMSZ_RIGHT:
      prc^.Right  := prc^.Left   + nWidthNew;

    WMSZ_BOTTOM:
      prc^.Bottom := prc^.Top    + nHeightNew;

    WMSZ_BOTTOMLEFT: begin
      prc^.Bottom := prc^.Top    + nHeightNew;
      prc^.Left   := prc^.Right  - nWidthNew;
    end;

    WMSZ_BOTTOMRIGHT: begin
      prc^.Bottom := prc^.Top    + nHeightNew;
      prc^.Right  := prc^.Left   + nWidthNew;
    end;

    WMSZ_TOPLEFT: begin
      prc^.Left   := prc^.Right  - nWidthNew;
      prc^.Top    := prc^.Bottom - nHeightNew;
    end;

    WMSZ_TOPRIGHT: begin
      prc^.Top    := prc^.Bottom - nHeightNew;
      prc^.Right  := prc^.Left   + nWidthNew;
    end;
  end; // end of case
end;

procedure TksoApplicationBar.OnGetMinMaxInfo (var Msg : TWMGetMinMaxInfo);
begin
  if GetEdge = abeFloat then
    with Msg.MinMaxInfo^ do begin
      ptMinTrackSize.X := FABS.nMinWidth;
      ptMinTrackSize.Y := FABS.nMinHeight;
      ptMaxTrackSize.X := FABS.nMaxWidth;
      ptMaxTrackSize.Y := FABS.nMaxHeight;
    end
  else
    with Msg.MinMaxInfo^ do begin
      ptMinTrackSize.X := FABS.szMinDockSize.cx;
      ptMinTrackSize.Y := FABS.szMinDockSize.cy;
      ptMaxTrackSize.X := GetSystemMetrics(SM_CXSCREEN);
      ptMaxTrackSize.Y := GetSystemMetrics(SM_CYSCREEN);
      if not IsEdgeTopOrBottom(GetEdge) then
        ptMaxTrackSize.X := FABS.szMaxDockSize.cx;
      if not IsEdgeLeftOrRight(GetEdge) then
        ptMaxTrackSize.Y := FABS.szMaxDockSize.cy;
    end;
end;


{ AppBar-specific helper functions }

function TksoApplicationBar.IsEdgeLeftOrRight (abEdge : TksoApplicationBarEdge) : Boolean;
begin
  Result := (abEdge in [abeLeft, abeRight]);
end;

function TksoApplicationBar.IsEdgeTopOrBottom (abEdge : TksoApplicationBarEdge) : Boolean;
begin
  Result := (abEdge in [abeTop, abeBottom]);
end;

function TksoApplicationBar.IsFloating (abEdge : TksoApplicationBarEdge) : Boolean;
begin
  Result := (abEdge = abeFloat);
end;

function TksoApplicationBar.IsDockable (abFlags : TksoApplicationBarFlags) : Boolean;
begin
  Result := ((abFlags * [abfAllowLeft .. abfAllowBottom]) <> []);
end;

function TksoApplicationBar.IsDockableVertically (abFlags : TksoApplicationBarFlags) : Boolean;
begin
  Result := ((abFlags * [abfAllowLeft, abfAllowRight]) <> []);
end;


function TksoApplicationBar.IsDockableHorizontally (abFlags : TksoApplicationBarFlags) : Boolean;
begin
  Result := ((abFlags * [abfAllowTop, abfAllowBottom]) <> []);
end;


procedure TksoApplicationBar.ResetSystemKnowledge;
begin
  // nothing to do when not in debug mode
end;

function TksoApplicationBar.GetKeyName: String;
begin
  Result := FabSettingsLocation.KeyName;
end;

procedure TksoApplicationBar.SetFlags(const Value: TksoApplicationBarFlags);
begin
  FFlags := Value;
end;

procedure TksoApplicationBar.SetKeyName(const Value: String);
begin
  FabSettingsLocation.KeyName := Value;
end;

function TksoApplicationBar.GetRootKey: Integer;
begin
  Result := FabSettingsLocation.nRootKey;
end;

procedure TksoApplicationBar.SetRootKey(const Value: Integer);
begin
  FabSettingsLocation.nRootKey := Value;
end;

function TksoApplicationBar.GetTaskEntry: TksoApplicationBarTaskEntry;
begin
  Result := FABS.abTaskEntry;
end;

procedure TksoApplicationBar.SetTaskEntry(const Value: TksoApplicationBarTaskEntry);
begin
  FABS.abTaskEntry := Value;
end;

function TksoApplicationBar.GetEdgeFromPoint (abFlags : TksoApplicationBarFlags;
                                   pt      : TSmallPoint) : TksoApplicationBarEdge;
var
  rc             : TRect;
  cxScreen       : Integer;
  cyScreen       : Integer;
  ptCenter       : TSmallPoint;
  ptOffset       : TSmallPoint;
  bIsLeftOrRight : Boolean;
  abSubstEdge    : TksoApplicationBarEdge;
begin
  // Let's get floating out of the way first
  if abfAllowFloat in abFlags then begin

    // Get the rectangle that bounds the size of the screen
    // minus any docked (but not-autohidden) AppBars
    SystemParametersInfo(SPI_GETWORKAREA, 0, @rc, 0);

    // Leave a 1/2 width/height-of-a-scrollbar gutter around the workarea
    InflateRect(rc,
                -GetSystemMetrics(SM_CXVSCROLL),
                -GetSystemMetrics(SM_CYHSCROLL));

    // If the point is in the adjusted workarea OR no edges are allowed
    if PtInRect(rc, SmallPointToPoint(pt)) or
       not IsDockable(abFlags) then begin
      // The AppBar should float
      Result := abeFloat;
      Exit;
    end;
  end;

  // If we get here, the AppBar should be docked; determine the proper edge
  // Get the dimensions of the screen
  cxScreen := GetSystemMetrics(SM_CXSCREEN);
  cyScreen := GetSystemMetrics(SM_CYSCREEN);

  // Find the center of the screen
  ptCenter.X := cxScreen div 2;
  ptCenter.Y := cyScreen div 2;

  // Find the distance from the point to the center
  ptOffset.X := pt.X - ptCenter.X;
  ptOffset.Y := pt.Y - ptCenter.Y;

  // Determine if the point is farther from the left/right or top/bottom
  bIsLeftOrRight :=
    ((Abs(ptOffset.Y) * cxScreen) <= (Abs(ptOffset.X) * cyScreen));

  // Propose an edge
  if bIsLeftOrRight then begin
    if 0 <= ptOffset.X then
      Result := abeRight
    else
      Result := abeLeft;
  end else begin
    if 0 <= ptOffset.Y then
      Result := abeBottom
    else
      Result := abeTop;
  end;

  // Calculate an edge substitute
  if abfAllowFloat in abFlags then
    abSubstEdge := abeFloat
  else
    abSubstEdge := FABS.abEdge;

  // Check if the proposed edge is allowed. If not, return the edge substitute
  case Result of
    abeLeft  : if not (abfAllowLeft   in abFlags) then Result := abSubstEdge;
    abeTop   : if not (abfAllowTop    in abFlags) then Result := abSubstEdge;
    abeRight : if not (abfAllowRight  in abFlags) then Result := abSubstEdge;
    abeBottom: if not (abfAllowBottom in abFlags) then Result := abSubstEdge;
  end;

end;

{ Public member functions }

constructor TksoApplicationBar.Create (Owner : TComponent);
begin
  // Force the shell to update its list of AppBars and the workarea.
  // This is a precaution and is very useful when debugging.  If you create
  // an AppBar and then just terminate the application, the shell still
  // thinks that the AppBar exists and the user's workarea is smaller than
  // it should be.  When a new AppBar is created, calling this function
  // fixes the user's workarea.
  ResetSystemKnowledge;

  // Set default state of AppBar to float with no width & height
  FABS.cbSize            := sizeof(TksoApplicationBarSettings);
  FABS.abEdge            := abeFloat;
  FABS.abFlags           := [abfAllowLeft .. abfAllowFloat];
  FABS.bAutohide         := False;
  FABS.bAlwaysOnTop      := True;
  FABS.bSlideEffect      := True;
  FABS.nTimerInterval    := SLIDE_DEF_TIMER_INTERVAL;
  FABS.szSizeInc.cx      := AB_DEF_SIZE_INC_X;
  FABS.szSizeInc.cy      := AB_DEF_SIZE_INC_Y;
  FABS.szDockSize.cx     := AB_DEF_DOCK_SIZE;
  FABS.szDockSize.cy     := AB_DEF_DOCK_SIZE;
  FABS.rcFloat.Left      := 0;
  FABS.rcFloat.Top       := 0;
  FABS.rcFloat.Right     := 0;
  FABS.rcFloat.Bottom    := 0;
  FABS.nMinWidth         := 360;
  FABS.nMinHeight        := 36;
  FABS.nMaxWidth         := GetSystemMetrics(SM_CXSCREEN);
  FABS.nMaxHeight        := GetSystemMetrics(SM_CYSCREEN);
  FABS.szMinDockSize.cx  := 0;
  FABS.szMinDockSize.cy  := 0;
  FABS.szMaxDockSize.cx  := GetSystemMetrics(SM_CXSCREEN) div 2;
  FABS.szMaxDockSize.cy  := GetSystemMetrics(SM_CYSCREEN) div 2;
  FABS.abTaskEntry       := abtFloatDependent;
  FabEdgeProposedPrev    := abeUnknown;
  FbFullScreenAppOpen    := False;
  FbAutoHideIsVisible    := False;

  // Set default location of the settings in the registry
  with FabSettingsLocation do begin
    RootKey := AB_DEF_ROOT_KEY;
    KeyName := AB_DEF_KEY_NAME;
  end;

  // Call base class
  inherited Create(Owner);
end;

destructor TksoApplicationBar.Destroy;
begin
  ResetSystemKnowledge;

  // Call base class
  inherited Destroy;
end;

procedure TksoApplicationBar.UpdateBar;
begin
  SetEdge(GetEdge);
end;

function TksoApplicationBar.LoadSettings : Boolean;
var
  reg : TRegistry;
  abs : TksoApplicationBarSettings;
begin
  // Set the default return value
  Result := False;
  // Create a TRegistry object
  reg := TRegistry.Create;
  // Set the RootKey
  reg.RootKey := FabSettingsLocation.nRootKey;
  // Open the KeyName
  if reg.OpenKey(FabSettingsLocation.KeyName, False) then
  begin
    // Load the FABS record from the 'default' value
    If reg.ReadBinaryData('Settings', abs, sizeof(abs)) = sizeof(abs) Then
    begin
      FABS := abs;
      Result := True;
    end;
  end;
  // Free the TRegistry object
  reg.Destroy
end;

function TksoApplicationBar.SaveSettings : Boolean;
var
  reg : TRegistry;
begin
  // Set the default return value
  Result := False;
  // Create a TRegistry object
  reg := TRegistry.Create;
  // Set the RootKey
  reg.RootKey := FabSettingsLocation.nRootKey;
  // Open the KeyName, creating it if not exists
  if reg.OpenKey(FabSettingsLocation.KeyName, True) then
  begin
    // Save the FABS record in the 'default' value
    reg.WriteBinaryData('Settings', FABS, sizeof(FABS));
    Result := True;
  end;
  // Free the TRegistry object
  reg.Destroy
end;

function TksoApplicationBar.GetMaxVertDockSize: Longint;
begin
  Result := FABS.szMaxDockSize.cx;
end;

procedure TksoApplicationBar.SetMaxVertDockSize(const Value: Longint);
begin
  FABS.szMaxDockSize.cx := Value;
end;

function TksoApplicationBar.GetMaxHorzDockSize: Longint;
begin
  Result := FABS.szMaxDockSize.cy;
end;

procedure TksoApplicationBar.SetMaxHorzDockSize(const Value: Longint);
begin
  FABS.szMaxDockSize.cy := Value;
end;

function TksoApplicationBar.GetHorzSizeInc: Longint;
begin
  Result := FABS.szSizeInc.cx;
end;

procedure TksoApplicationBar.SetHorzSizeInc(const Value: Longint);
begin
  FABS.szSizeInc.cx := Value;
end;

function TksoApplicationBar.GetVertSizeInc: Longint;
begin
  Result := FABS.szSizeInc.cy;
end;

procedure TksoApplicationBar.SetVertSizeInc(const Value: Longint);
begin
  FABS.szSizeInc.cy := Value;
end;

function TksoApplicationBar.GetAutoHide: Boolean;
begin
  Result := FABS.bAutohide;
end;

procedure TksoApplicationBar.SetAutoHide(const Value: Boolean);
begin
  FABS.bAutohide := Value;
end;

function TksoApplicationBar.GetAlwaysOnTop: Boolean;
begin
  Result := FABS.bAlwaysOnTop;
end;

procedure TksoApplicationBar.SetAlwaysOnTop(const Value: Boolean);
begin
  FABS.bAlwaysOnTop := Value;
end;

function TksoApplicationBar.GetSlideEffect: Boolean;
begin
  Result := FABS.bSlideEffect;
end;

procedure TksoApplicationBar.SetSlideEffect(const Value: Boolean);
begin
  FABS.bSlideEffect := Value;
end;

function TksoApplicationBar.GetHorzDockSize: Longint;
begin
  Result := FABS.szDockSize.cy;
end;

procedure TksoApplicationBar.SetHorzDockSize(const Value: Longint);
begin
  FABS.szDockSize.cy := Value;
end;

function TksoApplicationBar.GetVertDockSize: Longint;
begin
  Result := FABS.szDockSize.cx;
end;

procedure TksoApplicationBar.SetVertDockSize(const Value: Longint);
begin
  FABS.szDockSize.cx := Value;
end;

function TksoApplicationBar.GetFloatLeft: Integer;
begin
  Result := FABS.rcFloat.Left;
end;

procedure TksoApplicationBar.SetFloatLeft(const Value: Integer);
begin
  FABS.rcFloat.Left := Value;
end;

function TksoApplicationBar.GetFloatTop: Integer;
begin
  Result := FABS.rcFloat.Top;
end;

procedure TksoApplicationBar.SetFloatTop(const Value: Integer);
begin
  FABS.rcFloat.Top := Value;
end;

function TksoApplicationBar.GetFloatRight: Integer;
begin
  Result := FABS.rcFloat.Right;
end;

procedure TksoApplicationBar.SetFloatRight(const Value: Integer);
begin
  FABS.rcFloat.Right := Value;
end;

function TksoApplicationBar.GetFloatBottom: Integer;
begin
  Result := FABS.rcFloat.Bottom;
end;

procedure TksoApplicationBar.SetFloatBottom(const Value: Integer);
begin
  FABS.rcFloat.Bottom := Value;
end;

function TksoApplicationBar.GetMinWidth: Integer;
begin
  Result := FABS.nMinWidth;
end;

procedure TksoApplicationBar.SetMinWidth(const Value: Integer);
begin
  FABS.nMinWidth := Value;
end;

function TksoApplicationBar.GetMinHeight: Integer;
begin
  Result := FABS.nMinHeight;
end;

procedure TksoApplicationBar.SetMinHeight(const Value: Integer);
begin
  FABS.nMinHeight := Value;
end;

function TksoApplicationBar.GetMaxWidth: Integer;
begin
  Result := FABS.nMaxWidth;
end;

procedure TksoApplicationBar.SetMaxWidth(const Value: Integer);
begin
  FABS.nMaxWidth := Value;
end;

function TksoApplicationBar.GetMaxHeight: Integer;
begin
  Result := FABS.nMaxHeight;
end;

procedure TksoApplicationBar.SetMaxHeight(const Value: Integer);
begin
  FABS.nMaxHeight := Value;
end;

function TksoApplicationBar.GetMinHorzDockSize: Longint;
begin
  Result := FABS.szMinDockSize.cy;
end;

procedure TksoApplicationBar.SetMinHorzDockSize(const Value: Longint);
begin
  FABS.szMinDockSize.cy := Value;
end;

function TksoApplicationBar.GetMinVertDockSize: Longint;
begin
  Result := FABS.szMinDockSize.cx;
end;

procedure TksoApplicationBar.SetMinVertDockSize(const Value: Longint);
begin
  FABS.szMinDockSize.cx := Value;
end;

end.
