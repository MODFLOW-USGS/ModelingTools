{@abstract(@name is used to define a series of @link(TCustomUndo)s that
  relate to @link(TScreenObject)s.)  
  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit UndoItemsScreenObjects;

interface

uses SubscriptionUnit, PhastModelUnit, Types, SysUtils, Classes, Graphics,
  Contnrs, QuadTreeClass, UndoItems, AbstractGridUnit, ScreenObjectUnit,
  FastGEO, GoPhastTypes, ValueArrayStorageUnit, ModflowPackageSelectionUnit,
  System.Generics.Collections, ModflowTransientListParameterUnit,
  ModflowConstantHeadBoundaryUnit;

type
  EIllegalMerge = class(Exception);

  TScreenObjectEditItem = class(TCollectionItem)
  private
    FScreenObject: TScreenObject;
    FOwnScreenObject: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    property OwnScreenObject: boolean read FOwnScreenObject
      write FOwnScreenObject;
    property ScreenObject: TScreenObject read FScreenObject
      write FScreenObject;
  end;

  TScreenObjectEditCollection = class(TCollection)
  private
    FOwnScreenObject: boolean;
    procedure SetOwnScreenObject(const Value: boolean);
    function GetItems(Index: integer): TScreenObjectEditItem;
  public
    function Add: TScreenObjectEditItem;
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    property OwnScreenObject: boolean read FOwnScreenObject
      write SetOwnScreenObject;
    property Items[Index: integer]: TScreenObjectEditItem read GetItems; default;
    function IndexOfScreenObjectName(AScreenObjectName: string): integer;
  end;

  TCustomUpdateScreenObjectDisplayUndo =  class(TCustomUndo)
  protected
    FShouldUpdateShowHideObjectsDisplay: Boolean;
    FShouldUpdateShowHideObjects: boolean;
    // @name updates @link(frmShowHideObjects) if @link(frmShowHideObjects)
    // is not nil and  @link(FShouldUpdateShowHideObjects) is @true.
    // It also enables the "Invert Selection"
    procedure UpdateDisplay;
    procedure UpdateShowHideObjects;
    procedure EnableInvertSelection;
    procedure WarnSfrLengthProblems(List: TList);
    procedure UpdateChildGrids;
//    procedure BeginUpdate;
//    procedure EndUpdate;
  end;

  { @abstract(@name is used to change or restore which
    @link(TScreenObject)s or
    vertices in an individual @link(TScreenObject) are selected.)
    To use, Create a
    @name, change the @link(TScreenObject)s
    that are selected, and call
    @link(TUndoChangeSelection.SetPostSelection).
    Then Undo and Redo can be used to restore or redo
    the changes in what is selected.

    @longcode(#
      UndoChangeSelection := TUndoChangeSelection.Create;
      try
        // Change the selected TScreenObjects here.
        UndoChangeSelection.SetPostSelection;
        if UndoChangeSelection.SelectionChanged then
        begin
          frmGoPhast.UndoStack.Submit(UndoChangeSelection);
        end
        else
        begin
          UndoChangeSelection.Free;
        end;
      except
        UndoChangeSelection.Free;
        raise;
      end;
    #)
  }
  TUndoChangeSelection = class(TCustomUpdateScreenObjectDisplayUndo)
  strict private
    FOldChildModelScreenObjects: TList;
  private
    FOldSegment: TSegment2D;
    procedure ResetVisible(const ScreenObjects: TList);
    // @name stores the selected vertices of a single @link(TScreenObject) in
    // SelectedVertices.
    procedure StoreSelectedVerticies(var SelectedVertices: T2DBoolArray;
      const ScreenObjects: TList);
    procedure RestoreSelectedVertices(const ScreenObjects: TList; const SelectedVertices: T2DBoolArray);
  protected
    {
     @name records the selected vertices of a single @link(TScreenObject) when
     @link(SetPostSelection) was called.}
    FNewSelectedVertices: T2DBoolArray;
    {
    @name records the selected @link(TScreenObject)s when
     @link(SetPostSelection) was called.}
    FNewSelectedScreenObjects: TList;
    {
     @name records the selected vertices of a single @link(TScreenObject) when
     the @classname was created.}
    FOldSelectedVertices: T2DBoolArray;
    {
     @name records the selected @link(TScreenObject)s when
     the @classname was created.}
    FOldSelectedScreenObjects: TList;
    FOldVisibleScreenObjects: TList;
    FNewVisibleScreenObjects: TList;
    FUpdateVisible: boolean;
    {@name sets the selected @link(TScreenObject)s and the selected vertices
     of a single @link(TScreenObject) to the values specified by
     ScreenObjects and SelectedVertices.}
    procedure ResetSelection(const ScreenObjects: TList;
      const SelectedVertices: T2DBoolArray);
    // @name stores the selected @link(TScreenObject)s and the selected
    // vertices if any) of a @link(TScreenObject) in
    // @link(FOldSelectedScreenObjects) and @link(FOldSelectedVertices).
    procedure SetPriorSelection;
    // @name stores the selected @link(TScreenObject)s in ScreenObjects
    // and the selected vertices of a single @link(TScreenObject) in
    // SelectedVertices. It does this by calling @link(StoreSelectedObjects)
    // followed by @link(StoreSelectedVerticies). Override @link(StoreSelectedObjects)
    // to store a different set of @link(TScreenObject)s in ScreenObjects.
    procedure SetSelection(const ScreenObjects: TList;
      var SelectedVertices: T2DBoolArray);
    procedure StoreVisible(const ScreenObjects: TList);
    // @name stores the selected @link(TScreenObject)s in ScreenObjects
    procedure StoreSelectedObjects(const ScreenObjects: TList); virtual;
    // @name returns the selected @link(TScreenObject)s in ScreenObjects to
    // what they were previously.
    procedure ResetSelectedObjects(const ScreenObjects: TList); virtual;
  protected
    // Description tells what @classname does.
    function Description: string; override;
    // @name causes all the @link(TQRbwZoomBox2 zoomboxes) to redraw.
    procedure InvalidateImages;
  public
    // @name creates an instance of @classname and stores the
    // selected @link(TScreenObject)s.
    constructor Create;
    // Destroy gets rid of the instance of TUndoChangeSelection.
    destructor Destroy; override;
    // @name causes the paint boxes showing the selected
    // @link(TScreenObject)s to
    // be invalidated.
    procedure DoCommand; override;
    // @name sets the selection to what it was when @Link(SetPostSelection)
    // was called.
    procedure Redo; override;
    // @name indicates whether or not the selection changed.
    function SelectionChanged: boolean;
    // @name stores the selected @link(TScreenObject)s and the selected
    // vertices if any) of a @link(TScreenObject) in
    // @link(FNewSelectedScreenObjects) and @link(FNewSelectedVertices).
    procedure SetPostSelection;
    // @name sets the selection to what it was when the TUndoChangeSelection
    // was created.
    procedure Undo; override;
  end;

  TCustomImportMultipleScreenObjects = class(TUndoChangeSelection)
  private
    FScreenObjectsToDelete: TScreenObjectList;
    procedure ApplyDeletedStatus(SetDeleted: Boolean);
  protected
    // @name holds the newly imported @link(TScreenObject)s.
    FNewScreenObjects: TList;
    procedure UnDeleteNewScreenObjects;
    procedure DeleteNewScreenObjects;
  public
    constructor Create;
    Destructor Destroy; override;
    // @name deselects all the existing @link(TScreenObject)s,
    // creates @link(FNewScreenObjects) and puts the contents of
    // ListOfScreenObjects in @link(FNewScreenObjects).
    procedure StoreNewScreenObjects(const ListOfScreenObjects: TList); virtual;
    procedure UpdateScreenObject(const AScreenObject: TScreenObject);
    procedure DoCommand; override;
    procedure Redo; override;
    procedure Undo; override;
    property ScreenObjectsToDelete: TScreenObjectList read FScreenObjectsToDelete;
  end;

  TUndoPasteScreenObjects = class(TCustomImportMultipleScreenObjects)
  private
    FOldChildModelScreenObjects: TList;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StoreNewScreenObjects(const ListOfScreenObjects: TList); override;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure Redo; override;
  end;

  TCustomUndoDivideScreenObject = class(TCustomImportMultipleScreenObjects)
  private
    FOldChildModelScreenObjects: TList;
    FObjectToSplit: TList;
    FOldScreenObjectSettings: TList;
  protected
    function ShouldDivideScreenScreenObject(
      AScreenObject: TScreenObject): boolean; virtual; abstract;
    procedure UnselectAllVertices(AScreenObject: TScreenObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TUndoMakeSelectedVerticesNewScreenObject = class(TCustomUndoDivideScreenObject)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
    function ShouldDivideScreenScreenObject(
      AScreenObject: TScreenObject): boolean; override;
  public
    constructor Create;
    procedure Redo; override;
  end;

  TUndoSplitScreenObject = class(TCustomUndoDivideScreenObject)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
    function ShouldDivideScreenScreenObject(
      AScreenObject: TScreenObject): boolean; override;
  public
    constructor Create;
    procedure Redo; override;
  end;

  TUndoExplodeScreenObject = class(TCustomUndoDivideScreenObject)
  private
    procedure DeleteAllButFirstSection;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
    function ShouldDivideScreenScreenObject(
      AScreenObject: TScreenObject): boolean; override;
  public
    constructor Create;
    procedure Redo; override;
  end;

  // @name provides methods that descendants can use to notify the
  // GUI of changes.
  TCustomUpdateScreenObjectUndo = class(TUndoChangeSelection)
  protected
    // @name tells @link(frmGoPhastUnit.frmGoPhast) which views of the model
    // need updating and then sets ScreenObject.@link(TObserver.UpToDate)
    // to @true.
    procedure UpdateScreenObject(const ScreenObject: TScreenObject);
    // @name tells @link(frmGoPhastUnit.frmGoPhast) to update the
    // selection rectangle.
    procedure UpdateSelectionRectangle;
  end;

  {@abstract(@name is used to undo or redo the creation of
    a @link(TScreenObject).)  The @link(TScreenObject) is not actually
    created or destroyed by @name.  Instead @name just sets
    TScreenObject.@link(TScreenObject.Deleted).}
  TUndoCreateScreenObject = class(TCustomUpdateScreenObjectUndo)
  private
    // See @link(HasBeenUsed).
    FHasBeenUsed: boolean;
    // @name: @link(TScreenObject);
    // @name is the @link(TScreenObject) that has been created.
    FScreenObject: TScreenObject;
    FUndoEditFluxObservations: TUndoEditFluxObservations;
    FSectionStarts: TValueArrayStorage;
    FChildModelName: string;
    FOldChildModelScreenObjects: TList;
    procedure WarnSfrLengthProblem;
    procedure DisplayScreenObject;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates and instance of @classname.  AScreenObject is
    // the @link(TScreenObject) that has been created.
    constructor Create(const AScreenObject: TScreenObject);
    destructor Destroy; override;
    // @name sets Deleted to @False and makes other required changes.
    procedure DoCommand; override;
    // @name is set to @true in @link(DoCommand), @link(Redo) and @link(Undo);
    // It is used to indicate that the @classname needs to be freed when @name
    // is false/
    property HasBeenUsed: boolean read FHasBeenUsed write FHasBeenUsed;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name sets Deleted to @true and makes other required changes.
    procedure Undo; override;
    procedure UpdateObservations;
  end;

  {@abstract(@name is used to undo or redo the deletion of
    a one or more @link(TScreenObject)s.)
    The @link(TScreenObject)s are not actually
    created or destroyed by @name.  Instead @name just sets
    TScreenObject.@link(TScreenObject.Deleted).}
  TUndoDeleteScreenObjects = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: TList;
    // @name is a list of @link(TScreenObject)s that are being deleted.
    FScreenObjects: TList;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(ListOfScreenObjects is a TList containing @link(TScreenObject)s
    // that are to be deleted.)
    constructor Create(const ListOfScreenObjects: TScreenObjectList);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name sets Deleted to @True and makes other required changes.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name sets Deleted to @False and makes other required changes.
    procedure Undo; override;
  end;

  TUndoDeleteModelResults = class(TUndoChangeDataSets)
  private
    FDeleteScreenObjects: TUndoDeleteScreenObjects;
  protected
    // @name describes what @classname does.
    function Description: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name sets Deleted to @False and makes other required changes.
    procedure Undo; override;
  end;


  TUndoCutScreenObjects = class(TUndoDeleteScreenObjects)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Redo; override;
  end;

  {@abstract(@name moves one or more @link(TScreenObject)s
    or points within them.)}
  TUndoMoveScreenObject = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: array of Boolean;
    // each element of @name indicates whether the corresponding
    // @link(TScreenObject) can be moved.
    FCanMoveScreenObject: array of Boolean;
    // @name: @link(TViewDirection);
    // @name indicates the @link(TViewDirection) of the @link(TScreenObject)s
    // that will be moved.
    FDirection: TViewDirection;
    // @name: double;
    // @name is the distance in the X direction that the
    // @link(TScreenObject)s will be moved.
    FX: double;
    // @name: double;
    // @name is the distance in the Y direction that the
    // @link(TScreenObject)s will be moved.
    FY: double;
    FSelectedNode: Integer;
    FSelectedNodeNewLocation: TPoint2D;
    FSelectedNodeOldLocation: TPoint2D;
    // @name moves @link(TScreenObject)s by the amount XOffset, YOffset.
    procedure Move(const XOffset, YOffset: double;
      const Undoing: boolean);
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(X is the distance in the X direction that the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject) should be moved.)
    // @param(Y is the distance in the Y direction that the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject) should be moved.)
    // @param(ADirection is the @link(TViewDirection) of the
    // @link(TScreenObject)s that should be moved.)
    constructor Create(const X, Y: double; const ADirection: TViewDirection;
      SelectedNode: integer; SelectedNodeNewLocation: TPoint2D);
    // @name moves the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject).
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name reverses the movement of the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject).
    procedure Undo; override;
  end;

  TPointStorage = class(TObject)
    OldLocations: TRealPointArray;
    NewLocations: TRealPointArray;
    ScreenObject: TScreenObject;
  end;

  TUndoAnonymizeScreenObject = class(TCustomUpdateScreenObjectUndo)
  private
    FOldPositions: TObjectList<TPointStorage>;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    // @name moves each vertex of the selected screen object to the center
    // of the cell or element.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name reverses the movement of the nodes
    procedure Undo; override;
  end;

  {@abstract(@name inserts a point into a @link(TScreenObject).)}
  TUndoInsertPoint = class(TCustomUpdateScreenObjectUndo)
  private
    // @name indicates whether or not the point can be inserted.
    FCanInsert: boolean;
    // @name is the point to be inserted.
    FPoint: TPoint2D;
    // @name is the position in TScreenObject.@link(TScreenObject.Points)
    // where @link(FPoint) should be inserted.
    FPosition: integer;
    // @name is the @link(TScreenObject) into
    // which @link(FPoint) is being inserted.
    FScreenObject: TScreenObject;
    FSectionStarts: TValueArrayStorage;
    FPoints: TRealPointArray;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(AScreenObject is is the @link(TScreenObject) into
    // which APoint is being inserted.)
    // @param(APosition is the position in
    // TScreenObject.@link(TScreenObject.Points)
    // where APoint should be inserted.)
    // @param(APoint is the point to be inserted.)
    constructor Create(const AScreenObject: TScreenObject;
      const APosition: integer; const APoint: TPoint2D);
    destructor Destroy; override;
    // @name inserts the point.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name deletes the point.
    procedure Undo; override;
  end;

  // @abstract(@name is an abstract base class for classes that change the
  // order of @link(TScreenObject)s.)
  TCustomUndoChangeOrder = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: TList;
    // @name is a list of the @link(TScreenObject)s whose positions will
    // be changed.
    FScreenObjectList: TScreenObjectList;
    // @name: array of integer;
    // @name is the original positions of the @link(TScreenObject)s in
    // @link(FScreenObjectList).
    FPositions: array of integer;
  public
    // @name creates an instance of @classname and initializes the private
    // fields.
    constructor Create(const ViewDirection: TViewDirection);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call free instead.
    destructor Destroy; override;
    // @name calls @link(TUndoChangeSelection.DoCommand).
    procedure Redo; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s to the end of
    the list of @link(TScreenObject)s.)}
  TUndoToFront = class(TCustomUndoChangeOrder)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s to the end of
    // the list of @link(TScreenObject)s.
    procedure DoCommand; override;
    // @name restore the original order of the @link(TScreenObject)s.
    procedure Undo; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s to the beginning of
    the list of @link(TScreenObject)s.)}
  TUndoToBack = class(TCustomUndoChangeOrder)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s to the beginning of
    // the list of @link(TScreenObject)s.
    procedure DoCommand; override;
    // @name restore the original order of the @link(TScreenObject)s.
    procedure Undo; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s toward the end of
    the list of @link(TScreenObject)s by one position.)}
  TUndoMoveUp = class(TUndoToFront)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s toward the end of
    // the list of @link(TScreenObject)s by one position.
    procedure DoCommand; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s toward the
    beginning of the list of @link(TScreenObject)s by one position.)}
  TUndoMoveDown = class(TUndoToBack)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s toward the beginning of
    // the list of @link(TScreenObject)s by one position.
    procedure DoCommand; override;
  end;

  {@abstract(@name is used to change the order of @link(TScreenObject)s and
    to rename them.)}
  TUndoRearrangeScreenObjects = class(TCustomUpdateScreenObjectUndo)
  private
    // @name is a list of the @link(TScreenObject)s in their original order.
    // All the @link(TScreenObject)s must be included in @name.
    FList: TScreenObjectList;
    // @name is a list of the names of the @link(TScreenObject)s in
    // their original order.
    // All the names must be included in @name.
    FNames: TStringList;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
    // @name Changes the order of the @link(TScreenObject)s to
    procedure SetOrder(const ScreenObjectList: TScreenObjectList;
      const NameList: TStringList);
  public
    // @name is a list of the @link(TScreenObject)s in their new order.
    // All the @link(TScreenObject)s must be included in @name.
    FNewList: TScreenObjectList;
    // @name is a list of the names of the @link(TScreenObject)s in
    // their new order.
    // All the names must be included in @name.
    FNewNames: TStringList;
    // @name sets the order of the @link(TScreenObject)s to be the order
    // specified in @link(FNewList) and the names to be the names specified
    // in @link(FNewNames).
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name restores the order and names of the @link(TScreenObject)s to be
    // the order order and names.
    procedure Undo; override;
    // @name creates an instance of @classname and initializes the private
    // fields.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to show or hide @link(TScreenObject)s.)}
  TUndoShowHideScreenObject = class(TUndoChangeSelection)
  protected
    // @name is a list of the @link(TScreenObject)s whose
    // @link(TScreenObject.Visible) property will be changed.
    FScreenObjectsToChange: TList;

    FUndoCrossSection: TUndoMoveCrossSection;

    // @name tells what @classname does.
    function Description: string; override;
    // @name changes the @link(TScreenObject.Visible) property
    // of each @link(TScreenObject) in @link(FScreenObjectsToChange).
    procedure ToggleVisibility;
    procedure SetNewCrossSectionAngle;
  public
    // @name stores ScreenObject and will toggle its
    // @link(TScreenObject.Visible) property when required.
    function AddScreenObjectToChange(const ScreenObject: TScreenObject):
      integer;
    // @name creates and instance of @classname.
    constructor Create;
    // @name destroys the current @classname.  Do not call @name directly.
    // Call Free instead.
    destructor Destroy; override;
    // @name changes the visibility of all the @link(TScreenObject)s
    // in @link(FScreenObjectsToChange).
    procedure DoCommand; override;
    // @name redoes the changes in visibility of all the @link(TScreenObject)s
    // in @link(FScreenObjectsToChange).
    procedure Redo; override;
    // @name undoes the changes in visibility of all the @link(TScreenObject)s
    // in @link(FScreenObjectsToChange).
    procedure Undo; override;
  end;

  TCustomLockUnlockScreenObjects = class(TCustomUndo)
  protected
    FListOfScreenObjects: TList;
    FOriginalLocks: array of boolean;
  public
    constructor Create(const AListOfScreenObjects: TList);
    destructor Destroy; override;
    procedure Undo; override;
  end;

  TUndoLockScreenObjects = class(TCustomLockUnlockScreenObjects)
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
  end;

  TUndoUnlockScreenObjects = class(TCustomLockUnlockScreenObjects)
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
  end;

  // @abstract(@name is used to set or undo the setting of a
  // @link(TScreenObject).)
  TUndoSetScreenObjectProperties = class(TCustomUpdateScreenObjectDisplayUndo)
  private
    FOldChildModelScreenObjects: TList;
    FNewScreenObjects: TScreenObjectEditCollection;
    FOldScreenObjects: TScreenObjectEditCollection;
    FExistingScreenObjects: TScreenObjectEditCollection;
    // @name: TList;
    // @name contains all the @link(TScreenObject)'s whose properties
    // are being changed.
    FListOfScreenObjects: TList;
    FUndoEditFluxObservations: TUndoEditFluxObservations;
    FNewChildModelScreenObjects: TList;
  protected
    // If @name is true then @link(frmShowHideObjects) can be updated.
    // Descendants should set @name to @false before calling inherited
    // @link(DoCommand) or @link(Undo) if they will update
    // @link(frmShowHideObjects) with @link(UpdateShowHideObjects).
    // Description describes what this TCustomUndo does for display in
    // a menu or tool tip.
    function Description: string; override;
  private
    // @name is called to make sure each
    // @link(TScreenObject)'s subscriptions are set appropriately.
    // @name calls TScreenObject.@link(
    // TScreenObject.ResetMixtureSubscriptions)
    // and TScreenObject.@link(
    // TScreenObject.ResetBoundaryMixtureSubscriptions) for each
    // @link(TScreenObject).
    procedure ResetScreenObjectDataSetSubscriptions;
    procedure SetObjectProperties;
  public
    // @name determines whether or not the
    // @link(TUndoSetScreenObjectProperties)
    // will need to notify the grid that the
    // colors of the cells needs to be updated.
    FSetCellsColor: boolean;
    // @name assigns the new values to the @link(TScreenObject)s.
    procedure DoCommand; override;
    procedure Redo; override;
    // @name restores the old values to the @link(TScreenObject)s.
    procedure Undo; override;
    // @name stores the @link(TScreenObject)s in AListOfScreenObjects and reads
    // and stores their properties.
    // @param(AListOfScreenObjects contains the @link(TScreenObject)s
    // that will be affected by this @classname.)
    // @link(UpdateObservations) must be called before the @classname is used.
    constructor Create(const AListOfScreenObjects: TList; var NewScreenObjects,
      OldScreenObjects: TScreenObjectEditCollection;
      var OldChildModelScreenObjects: TList);
    // @name destroys the @link(TUndoSetScreenObjectProperties)
    // and releases its memory.
    destructor Destroy; override;
    procedure UpdateObservations;
  end;

  TUndoConvertFhbToMf6 = class(TUndoSetScreenObjectProperties)
  private
    FOldChdUsed: Boolean;
    FOldWellUsed: Boolean;
    FNewChdUsed: Boolean;
    FNewWellUsed: Boolean;
  protected
    function Description: string; override;
  public
    constructor Create{(const AListOfScreenObjects: TList; var NewScreenObjects,
      OldScreenObjects: TScreenObjectEditCollection;
      var OldChildModelScreenObjects: TList)};
    // @name destroys the @link(TUndoSetScreenObjectProperties)
    // and releases its memory.
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;


  {@abstract(@name is used to update the view of a @link(TScreenObject).)}
  {@abstract(@name is used to delete a segment of a @link(TScreenObject).)}
  TUndoDeleteSegment = class(TCustomUpdateScreenObjectUndo)
  private
    FSections: TValueArrayStorage;
    // @name indicates which segment is being deleted.
    FEdge: integer;
    // If deleting a segment divides the @link(TScreenObject) into two
    // @link(TScreenObject)s, @name will be one of @link(TScreenObject).
    FNewScreenObject: TScreenObject;
    // @name stores the @link(TScreenObject.Points) of the @link(TScreenObject)
    // which is having a segment deleted.
    FPoints: TRealPointArray;
    // @name stores the @link(TScreenObject.SelectedVertices)
    // of the @link(TScreenObject)
    // which is having a segment deleted.
    FSelectedVertices: TBooleanDynArray;
    // @name is the @link(TScreenObject) from which a segment is being deleted.
    FScreenObject: TScreenObject;
    FSetPropertiesOfEnclosed: boolean;
  protected
    // @name tells what @classname does.
    function Description: string; override;
  public
    // @name creates an instance of @classname and store the information
    // needed to delete or restore a segment.
    constructor Create(const AScreenObject: TScreenObject;
      const AnEdge: integer);
    destructor Destroy; override;
    // @name deletes a segment in a @link(TScreenObject).
    procedure DoCommand; override;
    // @name re-deletes a segment in a @link(TScreenObject).
    procedure Redo; override;
    // @name restores a segment in a @link(TScreenObject).
    procedure Undo; override;
  end;

  TUndoMergeObjects = class(TCustomUpdateScreenObjectUndo)
  private
    FScreenObjects: TList;
    FOldPoints: T2DRealPointArray;
    FQuadTree: TRbwQuadTree;
    FDeleted: array of boolean;
    FNewPoints: T2DRealPointArray;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Undo; override;
    procedure DoCommand; override;
    procedure Redo; override;
    function ShouldUse: boolean;
    Function Description: string; override;
  end;

  TUndoReverseVerticies = class(TCustomUpdateScreenObjectUndo)
  private
    FScreenObjects: TList;
    procedure ReverseObjects;
  protected
    // @name tells what @classname does.
    function Description: string; override;
  public
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  {@abstract(@name is used to delete a vertex of a @link(TScreenObject).)}
  TUndoDeleteVertices = class(TCustomUpdateScreenObjectUndo)
  strict protected
    // @name: boolean;
    // @name indicates whether the vertices can be deleted from the selected
    // @link(TScreenObject).  If deleting a vertex would cause the
    // @link(TScreenObject) to cross itself, the vertices can not be deleted.
    FCanDeleteVertices: boolean;
    // @name: array of array of TPoint2D;
    // @name holds the TPoint2Ds
    // of the @link(TScreenObject)s.
    FPoints: array of TRealPointArray;
    FSectionStarts: array of TValueArrayStorage;
    // @name: TList;
    // @name is a list of the @link(TScreenObject)s from which vertices will
    // be deleted.
    FScreenObjects: TList;
    FVertexValues: TList;
    FOldImportedElevations: TList;
    FOldHigherImportedElevations: TList;
    FOldLowerImportedElevations: TList;
    FOldImportedValues: TList;

  protected
    // @name tells what @classname does.
    function Description: string; override;
    procedure StoreData(const ListOfScreenObjects: TScreenObjectList);
    function ShouldStoreData(AScreenObject: TScreenObject): Boolean; virtual;
  public
    // @name creates an instance of @classname and stores a list of the
    // @link(TScreenObject)s from which vertices will be deleted.
    constructor Create(const ListOfScreenObjects: TScreenObjectList);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name deletes the selected vertices
    // from the selected @link(TScreenObject)s
    procedure DoCommand; override;
    // @name re-deletes the selected vertices from the selected
    // @link(TScreenObject)s
    procedure Redo; override;
    // @name restores the deleted vertices
    // from the selected @link(TScreenObject)s
    procedure Undo; override;
  end;

  TUndoSimplifyObjects = class(TUndoDeleteVertices)
  private
    FRequiredSpacing: Double;
    FMaxDeltaAngle: double;
    procedure SetMaxDeltaAngle(const Value: double);
    procedure SetRequiredSpacing(const Value: Double);
  protected
    function ShouldStoreData(AScreenObject: TScreenObject): Boolean; override;
    function Description: string; override;
  public
    // @name deletes the vertices that are on a straight line.
    // from the selected @link(TScreenObject)s
    procedure DoCommand; override;
    property MaxDeltaAngle: double read FMaxDeltaAngle write SetMaxDeltaAngle;
    property RequiredSpacing: Double read FRequiredSpacing write SetRequiredSpacing;
  end;

  TUndoAddPart = class(TCustomUpdateScreenObjectUndo)
  private
    FScreenObject: TScreenObject;
    FNewPoints: TRealPointArray;
    FCount: integer;
    FCapacity: integer;
    procedure Grow;
    procedure SetCapacity(const Value: integer);
  protected
    // @name tells what @classname does.
    property Capacity: integer read FCapacity write SetCapacity;
    function Description: string; override;
  public
    property Count: integer read FCount;
    procedure AddPoint(Point: TPoint2D);
    Constructor Create(ScreenObject: TScreenObject);
    procedure DoCommand; override;
    procedure Redo; override;
    procedure Undo; override;
    procedure DeleteLastPoint;
  end;

  TUndoConvertHfbMf6 = class(TCustomUndo)
  private
    FScreenObjects: TScreenObjectList;
    function GetShouldConvert: Boolean;
  protected
    function Description: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    property ShouldConvert: Boolean read GetShouldConvert;
  end;

//  TUndoConvertChd = class(TCustomUndo)
//  private
//    FScreenObjects: TScreenObjectList;
//    FNewChdParam: TModflowTransientListParameter;
//    FChdBoundaries: TObjectList<TChdBoundary>;
//    function GetShouldConvert: Boolean;
//  protected
//    function Description: string; override;
//  public
//    constructor Create;
//    destructor Destroy; override;
//    procedure DoCommand; override;
//    procedure Undo; override;
//    property ShouldConvert: Boolean read GetShouldConvert;
//  end;

  TUndoConvertObservationsMf6 = class(TCustomUpdateScreenObjectDisplayUndo)
  private
    FHobScreenObjects: TScreenObjectList;
    FChobScreenObjects: TScreenObjectList;
    FOldMf6ObsUsed: Boolean;
    FDrobScreenObjects: TScreenObjectList;
    FGhbobScreenObjects: TScreenObjectList;
    FRivobScreenObjects: TScreenObjectList;
    function GetShouldConvert: Boolean;
  protected
    function Description: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    property ShouldConvert: Boolean read GetShouldConvert;
  end;

  TUndoConvertMnw2ToMaw = class(TCustomImportMultipleScreenObjects)
  private
    FMawSelected: Boolean;
  protected
    function Description: string; override;
  public
    constructor Create;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure Redo; override;
  end;

  TUndoConvertUzfToUzf6 = class(TCustomImportMultipleScreenObjects)
  private
    FUzf6Packge: TUzfMf6PackageSelection;
    FScreenObjectList: TScreenObjectList;
  protected
    function Description: string; override;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure Redo; override;
  end;

  TUndoConvertSubAndSwtToCSub = class(TCustomImportMultipleScreenObjects)
    FOldCSubPackage: TCSubPackageSelection;
    FNewCSubPackage: TCSubPackageSelection;
  protected
    function Description: string; override;
  public
    constructor Create;
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure Redo; override;
  end;



implementation

uses Math, frmGoPhastUnit, frmSelectedObjectsUnit, frmShowHideObjectsUnit,
  InteractiveTools, PhastDataSets, DataSetUnit, CountObjectsUnit, 
  ModflowSfrReachUnit, frmErrorsAndWarningsUnit, IntListUnit,
  frmSelectResultToImportUnit, SutraMeshUnit, DisplaySettingsUnit,
  ModflowHfbUnit, ModflowTimeUnit, FluxObservationUnit, 
  LayerStructureUnit, ModflowCSubInterbed, ModflowSubsidenceDefUnit,
  ModflowCsubUnit, MeshRenumberingTypes, ModflowPackagesUnit,
  OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowFhbUnit,
  ModflowWellUnit, framePackageMf6ObsUnit, Modflow6ObsUnit;

resourcestring
  StrChangeSelection = 'change selection';
  StrPasteObjects = 'paste object(s)';
  StrCreateObject = 'create object';
  StrDeleteObjects = 'delete objects';
  StrDeleteObject = 'delete object';
  StrMoveObjects = 'move objects';
  StrInsertNode = 'insert node';
  StrTopviewObjectsToFront = 'top-view objects to front';
  StrTopviewObjectsToBack = 'top-view objects to back';
  StrTopviewObjectsFor = 'top-view objects forward one';
  StrTopviewObjectsBac = 'top-view objects back one';
  StrRearrangeObjects = 'rearrange objects';
  StrShowOrHideObjects = 'show or hide objects';
  StrEditObjectProperti = 'edit object properties';
  StrDeleteSegment = 'delete segment';
  StrDeleteVertices = 'delete vertices';
  StrAddPartToObject = 'add part to object';
  StrMergeObjects = 'merge objects';
  StrReverseObjectOrder = 'reverse object order';
  StrCutObjects = 'cut objects';
  StrCutObject = 'cut object';
  StrSplitSelectedObjec = 'split selected objects';
  StrMakeSelectedVertic = 'make selected vertices a separate object';
  StrSplitObjectAtSele = 'split object at selected vertices';
  StrLockSelectedObject = 'lock selected objects';
  StrUnlockSelectedObje = 'unlock selected objects';
  WarningRootReachLength = 'SFR Reach Length is zero.';
  StrTheEndpointsOfThe = 'The endpoints of the two objects "%0:s" and "%1:s"' +
  ' must be at exactly the same point to merge them into a single object.';
  StrConvert_HFB_Objects = 'convert MODFLOW-2005 horizontal flow barriers to ' +
  'MODFLOW 6 horizontal flow barriers';
  StrConvertMODFLOW2005_HOB = 'convert MODFLOW-2005 observations to MOD' +
  'FLOW-6 observation locations';
  StrConvertUZFToUZF6 = 'convert UZF to UZF6';
  StrConvertMNW2WellsT = 'convert MNW2 wells to MAW wells';
  StrNoDelayInterbedss = 'NoDelayInterbeds_%s';
  StrDelayInterbed0s = 'DelayInterbed_%0:s_%1:s';
  StrWaterTableInterbeds = 'WaterTableInterbeds_%s';

{ TCustomUpdateScreenObjectDisplayUndo }

//procedure TCustomUpdateScreenObjectDisplayUndo.BeginUpdate;
//begin
//  if frmShowHideObjects <> nil then
//  begin
//    frmShowHideObjects.vstObjects.BeginUpdate;
//  end;
//end;

procedure TCustomUpdateScreenObjectDisplayUndo.EnableInvertSelection;
begin
  frmGoPhast.EnableInvertSelection;
end;

//procedure TCustomUpdateScreenObjectDisplayUndo.EndUpdate;
//begin
//  if frmShowHideObjects <> nil then
//  begin
//    frmShowHideObjects.vstObjects.EndUpdate;
//  end;
//end;

procedure TCustomUpdateScreenObjectDisplayUndo.UpdateChildGrids;
var
  ChildIndex: Integer;
begin
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel.CanUpdateGrid := True;
  end;
end;

procedure TCustomUpdateScreenObjectDisplayUndo.UpdateDisplay;
begin
  frmSelectedObjects.UpdateDisplay;
  UpdateShowHideObjects;
  EnableInvertSelection;
end;

procedure TCustomUpdateScreenObjectDisplayUndo.UpdateShowHideObjects;
begin
  if frmShowHideObjects <> nil then
  begin
    if FShouldUpdateShowHideObjects then
    begin
      frmShowHideObjects.UpdateScreenObjects;
    end
    else if FShouldUpdateShowHideObjectsDisplay then
    begin
      frmShowHideObjects.SupressUndo := True;
      try
        frmShowHideObjects.UpdateDisplay;
      finally
        frmShowHideObjects.SupressUndo := False;
      end;
    end;
  end;
end;

procedure TCustomUpdateScreenObjectDisplayUndo.WarnSfrLengthProblems(
  List: TList);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Item: TSfrItem;
  ErrorFound: boolean;
begin
  if not frmGoPhast.PhastModel.SfrIsSelected then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel,
    WarningRootReachLength);
  ErrorFound := False;
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    AScreenObject := List[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    if AScreenObject.ModflowSfrBoundary = nil then
    begin
      Continue;
    end;
    if not AScreenObject.ModflowSfrBoundary.Used then
    begin
      Continue;
    end;

    Assert(AScreenObject.ModflowSfrBoundary.Values.Count> 0);
    Item := AScreenObject.ModflowSfrBoundary.Values[0] as TSfrItem;
    if (Trim(Item.ReachLength) = '0') then
    begin
      ErrorFound := True;
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
        WarningRootReachLength, AScreenObject.Name, AScreenObject);
      Continue;
    end;

    if AScreenObject.ScreenObjectLength > 0 then
    begin
      Continue;
    end;

    if (UpperCase(Item.ReachLength) = UpperCase(StrObjectIntersectLength)) then
    begin
      ErrorFound := True;
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
        WarningRootReachLength, AScreenObject.Name, AScreenObject);
    end;
  end;
  if ErrorFound then
  begin
    frmErrorsAndWarnings.ShowAfterDelay;
  end;
end;

{ TUndoChangeSelection }

constructor TUndoChangeSelection.Create;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  FOldChildModelScreenObjects := TList.Create;
  FOldSelectedScreenObjects := TList.Create;
  FNewSelectedScreenObjects := TList.Create;
  FOldVisibleScreenObjects := TList.Create;
  FNewVisibleScreenObjects := TList.Create;

  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    FOldChildModelScreenObjects.Add(ChildModel.HorizontalPositionScreenObject);
  end;

  SetPriorSelection;
  FShouldUpdateShowHideObjectsDisplay := True;
end;

function TUndoChangeSelection.Description: string;
begin
  result := StrChangeSelection;
end;

destructor TUndoChangeSelection.Destroy;
begin
  FOldSelectedScreenObjects.Free;
  FNewSelectedScreenObjects.Free;
  FOldVisibleScreenObjects.Free;
  FNewVisibleScreenObjects.Free;
  FOldChildModelScreenObjects.Free;
  inherited;
end;

procedure TUndoChangeSelection.DoCommand;
begin
  InvalidateImages;
  FShouldUpdateShowHideObjectsDisplay := True;
  UpdateDisplay;
end;

procedure TUndoChangeSelection.InvalidateImages;
begin
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;

  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TUndoChangeSelection.Redo;
begin
  ResetSelection(FNewSelectedScreenObjects, FNewSelectedVertices);
  if FUpdateVisible then
  begin
    ResetVisible(FNewVisibleScreenObjects);
  end;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle :=
    FNewSelectedScreenObjects.Count > 0;
  InvalidateImages;
  FShouldUpdateShowHideObjectsDisplay := True;
  UpdateDisplay;
end;

procedure TUndoChangeSelection.ResetVisible(const ScreenObjects: TList);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObjects.IndexOf(AScreenObject) >= 0 then
    begin
      AScreenObject.Visible := True;
    end
    else
    begin
      AScreenObject.Visible := False;
    end;
  end;
end;

procedure TUndoChangeSelection.ResetSelection(const ScreenObjects: TList;
  const SelectedVertices: T2DBoolArray);
begin
  frmGoPhast.ChangingSelection := True;
  try
    ResetSelectedObjects(ScreenObjects);
    RestoreSelectedVertices(ScreenObjects, SelectedVertices);
    FShouldUpdateShowHideObjects := True;
    UpdateDisplay;
  finally
    frmGoPhast.ChangingSelection := False;
  end;
end;

function TUndoChangeSelection.SelectionChanged: boolean;
var
  Index, VertexIndex: integer;
begin
  result := (FOldSelectedScreenObjects.Count <> FNewSelectedScreenObjects.Count);
  if result then
    Exit;
  for Index := 0 to FOldSelectedScreenObjects.Count - 1 do
  begin
    result := (FOldSelectedScreenObjects[Index] <>
      FNewSelectedScreenObjects[Index]);
    if result then
      Exit;
  end;
  for Index := 0 to Length(FOldSelectedVertices) - 1 do
  begin
    result := Length(FOldSelectedVertices[Index]) <>
      Length(FNewSelectedVertices[Index]);
    if result then
      Exit;
    for VertexIndex := 0 to Length(FOldSelectedVertices[Index]) - 1 do
    begin
      result := FOldSelectedVertices[Index, VertexIndex] <>
        FNewSelectedVertices[Index, VertexIndex];
      if result then
        Exit;
    end;
  end;
end;

procedure TUndoChangeSelection.SetPostSelection;
var
  Index: Integer;
begin
  SetSelection(FNewSelectedScreenObjects, FNewSelectedVertices);
  SelectScreenObjectTool.ShouldDrawSelectionRectangle :=
    FNewSelectedScreenObjects.Count > 0;
  StoreVisible(FNewVisibleScreenObjects);
  FUpdateVisible :=
    FNewVisibleScreenObjects.Count <> FOldVisibleScreenObjects.Count;
  if not FUpdateVisible  then
  begin
    for Index := 0 to FNewVisibleScreenObjects.Count - 1 do
    begin
      if FNewVisibleScreenObjects[Index] <> FOldVisibleScreenObjects[Index] then
      begin
        FUpdateVisible := True;
        break;
      end;
    end;
  end;
end;

procedure TUndoChangeSelection.SetPriorSelection;
begin
  SetSelection(FOldSelectedScreenObjects, FOldSelectedVertices);
  StoreVisible(FOldVisibleScreenObjects);
  if frmGoPhast.PhastModel.Mesh <> nil then
  begin
    FOldSegment := (frmGoPhast.PhastModel.Mesh as TSutraMesh3D).CrossSection.Segment;
  end;
end;

procedure TUndoChangeSelection.StoreVisible(const ScreenObjects: TList);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  ScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Visible then
    begin
      ScreenObjects.Add(AScreenObject)
    end;
  end;
  ScreenObjects.Capacity := ScreenObjects.Count;
end;

procedure TUndoChangeSelection.SetSelection(const ScreenObjects: TList;
  var SelectedVertices: T2DBoolArray);
begin
  StoreSelectedObjects(ScreenObjects);
  StoreSelectedVerticies(SelectedVertices, ScreenObjects);
end;

procedure TUndoChangeSelection.Undo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  AScreenObject: TScreenObject;
begin
  ResetSelection(FOldSelectedScreenObjects, FOldSelectedVertices);
  if FUpdateVisible then
  begin
    ResetVisible(FOldVisibleScreenObjects);
  end;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle :=
    FOldSelectedScreenObjects.Count > 0;

  Assert(frmGoPhast.PhastModel.ChildModels.Count =
    FOldChildModelScreenObjects.Count);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AScreenObject := FOldChildModelScreenObjects[ChildIndex];
    ChildModel.HorizontalPositionScreenObject := AScreenObject;
  end;

  InvalidateImages;
  FShouldUpdateShowHideObjectsDisplay := True;
  if frmGoPhast.PhastModel.Mesh <> nil then
  begin
    (frmGoPhast.PhastModel.Mesh as TSutraMesh3D).CrossSection.Segment := FOldSegment;
  end;

  UpdateDisplay;
end;

procedure TUndoChangeSelection.RestoreSelectedVertices(const ScreenObjects: TList; const SelectedVertices: T2DBoolArray);
var
  AScreenObject: TScreenObject;
  VertexIndex: Integer;
  ScreenObjectIndex: Integer;
begin
  for ScreenObjectIndex := 0 to ScreenObjects.Count - 1 do
  begin
    AScreenObject := ScreenObjects[ScreenObjectIndex];
    if Length(SelectedVertices[ScreenObjectIndex]) > 0 then
    begin
      for VertexIndex := 0 to Min(AScreenObject.Count, Length(SelectedVertices[ScreenObjectIndex])) - 1 do
      begin
        AScreenObject.SelectedVertices[VertexIndex] := SelectedVertices[ScreenObjectIndex, VertexIndex];
      end;
    end
    else
    begin
      if AScreenObject.SelectedVertexCount > 0 then
      begin
        AScreenObject.ClearSelectedVertices;
      end;
    end;
  end;
end;

procedure TUndoChangeSelection.ResetSelectedObjects(const ScreenObjects: TList);
var
  AScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
begin
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObjects.IndexOf(AScreenObject) >= 0 then
    begin
      AScreenObject.Selected := True;
    end
    else
    begin
      AScreenObject.Selected := False;
    end;
  end;
end;

procedure TUndoChangeSelection.StoreSelectedObjects(const ScreenObjects: TList);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  ScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Selected then
    begin
      ScreenObjects.Add(AScreenObject);
    end;
  end;
end;

procedure TUndoChangeSelection.StoreSelectedVerticies(
  var SelectedVertices: T2DBoolArray; const ScreenObjects: TList);
var
  ScreenObjectIndex: Integer;
  VertexIndex: Integer;
  AScreenObject: TScreenObject;
begin
  SetLength(SelectedVertices, ScreenObjects.Count);
  for ScreenObjectIndex := 0 to ScreenObjects.Count - 1 do
  begin
    AScreenObject := ScreenObjects[ScreenObjectIndex];
    if AScreenObject.SelectedVertexCount > 0 then
    begin
      SetLength(SelectedVertices[ScreenObjectIndex], AScreenObject.Count);
      for VertexIndex := 0 to AScreenObject.Count - 1 do
      begin
        SelectedVertices[ScreenObjectIndex, VertexIndex] := AScreenObject.SelectedVertices[VertexIndex];
      end;
    end
    else
    begin
      SetLength(SelectedVertices[ScreenObjectIndex], 0);
    end;
  end;
end;

{ TUndoCreateScreenObject }

constructor TUndoCreateScreenObject.Create(const AScreenObject: TScreenObject);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited Create;

  FOldChildModelScreenObjects := TList.Create;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    FOldChildModelScreenObjects.Add(ChildModel.HorizontalPositionScreenObject)
  end;

  HasBeenUsed := False;
  Assert(AScreenObject <> nil);
  FScreenObject := AScreenObject;
  FSectionStarts:= TValueArrayStorage.Create;
  FSectionStarts.Assign(FScreenObject.SectionStarts);
  FUndoEditFluxObservations:= TUndoEditFluxObservations.Create;
//  UpdateObservations;
end;

function TUndoCreateScreenObject.Description: string;
begin
  result := StrCreateObject;
end;

destructor TUndoCreateScreenObject.Destroy;
begin
  FUndoEditFluxObservations.Free;
  FSectionStarts.Free;
  FOldChildModelScreenObjects.Free;
  inherited;
end;

procedure TUndoCreateScreenObject.DisplayScreenObject;
begin
  HasBeenUsed := True;
  if FScreenObject.Count = 0 then
  begin
    FScreenObject.Deleted := true;
  end;
  if FScreenObject.Deleted and (FScreenObject.Count > 0) then
  begin
    FScreenObject.Deleted := False;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    FScreenObject.SectionStarts.Assign(FSectionStarts);
  end;
  FUndoEditFluxObservations.DoCommand;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
  UpdateChildGrids;
  WarnSfrLengthProblem;

end;

procedure TUndoCreateScreenObject.DoCommand;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  DisplayScreenObject;
  FChildModelName := '';
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel.HorizontalPositionScreenObject = FScreenObject then
    begin
      if (FScreenObject.ElevationCount = ecZero)
        and (FScreenObject.ViewDirection = vdTop) then
      begin
        FChildModelName := ChildModel.ModelName;
        FScreenObject.ChildModel := ChildModel;
      end
      else
      begin
        ChildModel.HorizontalPositionScreenObject := nil;
      end;
      break;
    end;
  end;

  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
  
end;

procedure TUndoCreateScreenObject.Redo;
begin
  DisplayScreenObject;
  FScreenObject.ChildModelName := FChildModelName;
  inherited;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
  UpdateChildGrids;
  WarnSfrLengthProblem;
end;

procedure TUndoCreateScreenObject.Undo;
var
  ChildModelIndex: Integer;
  ChildModel: TChildModel;
  AScreenObject: TScreenObject;
begin
  inherited;
  HasBeenUsed := True;
  if not FScreenObject.Deleted or (FScreenObject.Count = 0) then
  begin
    FScreenObject.Deleted := True;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    FScreenObject.ChildModelName := '';
  end;
  FUndoEditFluxObservations.Undo;

  Assert(frmGoPhast.PhastModel.ChildModels.Count =
    FOldChildModelScreenObjects.Count);
  for ChildModelIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildModelIndex].ChildModel;
    AScreenObject := FOldChildModelScreenObjects[ChildModelIndex];
    ChildModel.HorizontalPositionScreenObject := AScreenObject;
  end;

  UpdateChildGrids;

  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;

  WarnSfrLengthProblem;
end;

procedure TUndoCreateScreenObject.UpdateObservations;
var
  Model: TPhastModel;
  DummyNewMtsdObs: TMassFluxObs;
begin
  Model := frmGoPhast.PhastModel;
  DummyNewMtsdObs.NilAll;
  FUndoEditFluxObservations.AssignNewObservations(Model.HeadFluxObservations,
    Model.DrainObservations, Model.GhbObservations, Model.RiverObservations,
    Model.StreamObservations, DummyNewMtsdObs);
end;

procedure TUndoCreateScreenObject.WarnSfrLengthProblem;
var
  List: TList;
begin
  List := TList.Create;
  try
    List.Add(FScreenObject);
    WarnSfrLengthProblems(List);
  finally
    List.Free;
  end;
end;

{ TUndoDeleteScreenObjects }

constructor TUndoDeleteScreenObjects.Create(
  const ListOfScreenObjects: TScreenObjectList);
var
  Index: Integer;
begin
  inherited Create;
  FScreenObjects := TList.Create;
  FScreenObjects.Capacity := ListOfScreenObjects.Count;
  for Index := 0 to ListOfScreenObjects.Count - 1 do
  begin
    FScreenObjects.Add(ListOfScreenObjects[Index]);
  end;
end;

function TUndoDeleteScreenObjects.Description: string;
begin
  if FScreenObjects.Count > 1 then
  begin
    result := StrDeleteObjects;
  end
  else
  begin
    result := StrDeleteObject;
  end;
end;

destructor TUndoDeleteScreenObjects.Destroy;
begin
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoDeleteScreenObjects.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
//  BeginUpdate;
  Assert(FScreenObjects.Count > 0);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    if not AScreenObject.Deleted then
    begin
      if not AScreenObject.UpToDate then
      begin
        AScreenObject.UpToDate := True;
      end;
      AScreenObject.Deleted := True;
      AScreenObject.Invalidate;
      UpdateScreenObject(AScreenObject);
    end;
  end;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle := False;
  UpdateSelectionRectangle;
  FShouldUpdateShowHideObjects := True;
//  EndUpdate;
  UpdateDisplay;
end;

procedure TUndoDeleteScreenObjects.Redo;
begin
  inherited;
  DoCommand;
end;

procedure TUndoDeleteScreenObjects.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    if AScreenObject.Deleted then
    begin
      if not AScreenObject.UpToDate then
      begin
        AScreenObject.UpToDate := True;
      end;
      AScreenObject.Deleted := False;
      AScreenObject.Invalidate;
      UpdateScreenObject(AScreenObject);
    end;
  end;
  inherited;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle := True;
  UpdateSelectionRectangle;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

{ TUndoMoveScreenObject }

constructor TUndoMoveScreenObject.Create(const X, Y: double;
  const ADirection: TViewDirection; SelectedNode: integer;
  SelectedNodeNewLocation: TPoint2D);
begin
  inherited Create;
  FDirection := ADirection;
  FX := X;
  FY := Y;
  FSelectedNode := SelectedNode;
  FSelectedNodeNewLocation := SelectedNodeNewLocation;
end;

function TUndoMoveScreenObject.Description: string;
begin
  result := StrMoveObjects
end;

procedure TUndoMoveScreenObject.DoCommand;
begin
  Move(FX, FY, False);
end;

procedure TUndoMoveScreenObject.Move(const XOffset, YOffset: double;
  const Undoing: boolean);
var
  ScreenObjectIndex, PointIndex: integer;
  AScreenObject: TScreenObject;
  APoint: TPoint2D;
  MoveAll: boolean;
  Points: array of TPoint2D;
  TempScreenObject: TScreenObject;
  SectionIndex: integer;
  NextStart: Integer;
  NewSection: Boolean;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenObjectCount: Integer;
  EndNode: Integer;
begin
  if not Undoing then
  begin
    ScreenObjectCount := frmGoPhast.PhastModel.ScreenObjectCount;
    SetLength(FCanMoveScreenObject, ScreenObjectCount);
  end
  else
  begin
    ScreenObjectCount := Min(Length(FCanMoveScreenObject),
      frmGoPhast.PhastModel.ScreenObjectCount);
  end;
  for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.PositionLocked then
    begin
      Continue;
    end;
    AScreenObject.ResetSubscriptions;
    if not Undoing then
    begin
      FCanMoveScreenObject[ScreenObjectIndex] := False;
    end;
    if (AScreenObject.ViewDirection = FDirection) and AScreenObject.Selected then
    begin
      if not Undoing then
      begin
        FCanMoveScreenObject[ScreenObjectIndex] := True;
      end;
      MoveAll := not ((AScreenObject.SelectedVertexCount > 0)
        and (AScreenObject.SelectedVertexCount < AScreenObject.Count));
      if Length(Points) < AScreenObject.Count then
      begin
        SetLength(Points, AScreenObject.Count);
      end;
      EndNode := -1;
      for PointIndex := 0 to AScreenObject.Count - 1 do
      begin
        if MoveAll or AScreenObject.SelectedVertices[PointIndex] then
        begin
          if (PointIndex = FSelectedNode) or (PointIndex = EndNode) then
          begin
            if Undoing then
            begin
              APoint := FSelectedNodeOldLocation;
            end
            else
            begin
              APoint := FSelectedNodeNewLocation;
              FSelectedNodeOldLocation := AScreenObject.Points[PointIndex];
            end;

            for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
            begin
              if AScreenObject.SectionClosed[SectionIndex]then
              begin
                if AScreenObject.SectionStart[SectionIndex] = FSelectedNode then
                begin
                  EndNode := AScreenObject.SectionEnd[SectionIndex];
                  break;
                end
                else if AScreenObject.SectionStart[SectionIndex] > FSelectedNode then
                begin
                  break;
                end;
              end;
            end;
          end
          else
          begin
            APoint := AScreenObject.Points[PointIndex];
            APoint.X := APoint.X + XOffset;
            APoint.Y := APoint.Y + YOffset;
          end;
          Points[PointIndex] := APoint;
        end
        else
        begin
          Points[PointIndex] := AScreenObject.Points[PointIndex];
        end;
      end;
      if not Undoing and not MoveAll then
      begin
        TempScreenObject := frmGoPhast.PhastModel.ScreenObjectClass.Create(nil);
        try
          TempScreenObject.Capacity := AScreenObject.Count;
          SectionIndex := 0;
          NextStart := AScreenObject.SectionStart[SectionIndex];

          for PointIndex := 0 to AScreenObject.Count - 1 do
          begin
            NewSection := PointIndex = NextStart;
            if NewSection then
            begin
              Inc(SectionIndex);
              if (SectionIndex < AScreenObject.SectionCount) then
              begin
                NextStart := AScreenObject.SectionStart[SectionIndex];
              end;
            end;
            try
              if (PointIndex > 0) and (SectionIndex < AScreenObject.SectionCount)
                and (PointIndex = AScreenObject.SectionStart[SectionIndex]) then
              begin
                TempScreenObject.SectionStarts.Add;
                TempScreenObject.SectionStarts.IntValues[
                  TempScreenObject.SectionStarts.Count-1] := PointIndex;
                Inc(SectionIndex);
              end;
              TempScreenObject.AddPoint(Points[PointIndex], NewSection);
            except on E: EScreenObjectError do
              begin
                //ignore
              end;
            end;
            if TempScreenObject.Count <> PointIndex + 1 then
            begin
              FCanMoveScreenObject[ScreenObjectIndex] := False;
              Break;
            end;
          end;
        finally
          TempScreenObject.Free;
        end;
      end;

      if FCanMoveScreenObject[ScreenObjectIndex] then
      begin
        AScreenObject.BeginUpdate;
        try
          for PointIndex := 0 to AScreenObject.Count - 1 do
          begin
            if MoveAll or AScreenObject.SelectedVertices[PointIndex] then
            begin
              AScreenObject.Points[PointIndex] := Points[PointIndex];
            end;
          end;
        finally
          AScreenObject.EndUpdate;
        end;
        UpdateScreenObject(AScreenObject);
      end;
      AScreenObject.Invalidate;
      AScreenObject.UpToDate := True;
    end;
  end;

  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.ModflowGrid.UpdateCellElevations;
    end;
  end;

  UpdateSelectionRectangle;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

procedure TUndoMoveScreenObject.Redo;
begin
  inherited;
  DoCommand;
end;

procedure TUndoMoveScreenObject.Undo;
begin
  Move(-FX, -FY, True);
end;

{ TUndoInsertPoint }

constructor TUndoInsertPoint.Create(const AScreenObject: TScreenObject;
  const APosition: integer; const APoint: TPoint2D);
begin
  inherited Create;
  FScreenObject := AScreenObject;
  FSectionStarts:= TValueArrayStorage.Create;
  FSectionStarts.Assign(AScreenObject.SectionStarts);
  AScreenObject.MovePoints(FPoints);
  FPosition := APosition;
  FPoint := APoint;
end;

function TUndoInsertPoint.Description: string;
begin
  result := StrInsertNode;
end;

destructor TUndoInsertPoint.Destroy;
begin
  FSectionStarts.Free;
  inherited;
end;

procedure TUndoInsertPoint.DoCommand;
var
  TempScreenObject: TScreenObject;
  Index: integer;
  SectionIndex: Integer;
  NewPart: Boolean;
  NextStart: Integer;
begin
  inherited;
  FCanInsert := True;
  TempScreenObject := frmGoPhast.PhastModel.ScreenObjectClass.Create(nil);
  try
    TempScreenObject.Capacity := FScreenObject.Count + 1;
    SectionIndex := 0;
    NextStart := 0;
    for Index := 0 to FPosition - 1 do
    begin
      NewPart := Index = NextStart;
      if NewPart then
      begin
        Inc(SectionIndex);
        if SectionIndex < FScreenObject.SectionCount then
        begin
          NextStart := FScreenObject.SectionStart[SectionIndex];
        end;
      end;
      try
        TempScreenObject.AddPoint(FScreenObject.Points[Index], NewPart);
      except on E: EScreenObjectError do
        begin
          // ignore
        end;
      end;
      if TempScreenObject.Count <> Index + 1 then
      begin
        FCanInsert := False;
        break;
      end;
    end;
    if FCanInsert then
    begin
      try
        TempScreenObject.AddPoint(FPoint, False);
      except on E: EScreenObjectError do
        begin
          // ignore
        end;
      end;
      if TempScreenObject.Count <> FPosition + 1 then
      begin
        FCanInsert := False;
      end;
    end;
    if FCanInsert then
    begin
      for Index := FPosition to FScreenObject.Count - 1 do
      begin
        NewPart := Index = NextStart;
        if NewPart then
        begin
          Inc(SectionIndex);
          if SectionIndex < FScreenObject.SectionCount then
          begin
            NextStart := FScreenObject.SectionStart[SectionIndex];
          end;
        end;
        try
          TempScreenObject.AddPoint(FScreenObject.Points[Index], NewPart);
        except on E: EScreenObjectError do
          begin
            // ignore
          end;
        end;
        if TempScreenObject.Count <> Index + 2 then
        begin
          FCanInsert := False;
          break;
        end;
      end;
    end;
  finally
    TempScreenObject.Free;
  end;

  if FCanInsert then
  begin
    FScreenObject.InsertPoint(FPosition, FPoint);
    FScreenObject.SelectedVertices[FPosition] := True;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    UpdateSelectionRectangle;
    FShouldUpdateShowHideObjects := False;
    UpdateDisplay;
  end;
end;

procedure TUndoInsertPoint.Redo;
begin
  if FCanInsert then
  begin
    DoCommand;
    inherited;
  end;
end;

procedure TUndoInsertPoint.Undo;
begin
  if FCanInsert then
  begin
    FScreenObject.MoveToPoints(FPoints);
    FScreenObject.Count := Length(FPoints);
    FScreenObject.SectionStarts := FSectionStarts;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    UpdateSelectionRectangle;
    FShouldUpdateShowHideObjects := False;
    UpdateDisplay;
    inherited;
  end;
end;

{ TUndoToFront }

function TUndoToFront.Description: string;
begin
  result := StrTopviewObjectsToFront;
end;

procedure TUndoToFront.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  //  inherited;
  for Index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

procedure TUndoToFront.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(FPositions[Index], AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
end;

{ TCustomUndoChangeOrder }

constructor TCustomUndoChangeOrder.Create(const ViewDirection: TViewDirection);
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited Create;
  FScreenObjectList := TScreenObjectList.Create;
  FScreenObjectList.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  SetLength(FPositions, FScreenObjectList.Capacity);
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      FPositions[FScreenObjectList.Add(AScreenObject)] := Index;
    end;
  end;
  SetLength(FPositions, FScreenObjectList.Count);
end;

destructor TCustomUndoChangeOrder.Destroy;
begin
  FScreenObjectList.Free;
  inherited;
end;

procedure TCustomUndoChangeOrder.Redo;
begin
  inherited;
  DoCommand;
end;

{ TUndoToBack }

function TUndoToBack.Description: string;
begin
  result := StrTopviewObjectsToBack;
end;

procedure TUndoToBack.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  //  inherited;
  for Index := FScreenObjectList.Count - 1 downto 0 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(0, AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

procedure TUndoToBack.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := FScreenObjectList.Count - 1 downto 0 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(FPositions[Index], AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
end;

{ TUndoMoveUp }

function TUndoMoveUp.Description: string;
begin
  result := StrTopviewObjectsFor;
end;

procedure TUndoMoveUp.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
  NewPosition: integer;
begin
  //  inherited;
  for Index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[Index];
    NewPosition := FPositions[Index] + 1;
    if NewPosition = frmGoPhast.PhastModel.ScreenObjectCount then
      break;
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(NewPosition, AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

{ TUndoMoveDown }

function TUndoMoveDown.Description: string;
begin
  result := StrTopviewObjectsBac;
end;

procedure TUndoMoveDown.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
  NewPosition: integer;
begin
  //  inherited;
  for Index := FScreenObjectList.Count - 1 downto 0 do
  begin
    AScreenObject := FScreenObjectList[Index];
    NewPosition := FPositions[Index] - 1;
    if NewPosition = -1 then
      break;
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(NewPosition, AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

{ TUndoRearrangeScreenObjects }

constructor TUndoRearrangeScreenObjects.Create;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  FList := TScreenObjectList.Create;
  FNames := TStringList.Create;
  FNewList := TScreenObjectList.Create;
  FNewNames := TStringList.Create;
  FList.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  FNames.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  FNewList.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  FNewNames.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    FList.Add(AScreenObject);
    FNames.Add(AScreenObject.Name)
  end;
end;

function TUndoRearrangeScreenObjects.Description: string;
begin
  result := StrRearrangeObjects;
end;

destructor TUndoRearrangeScreenObjects.Destroy;
begin
  FList.Free;
  FNames.Free;
  FNewList.Free;
  FNewNames.Free;
  inherited;
end;

procedure TUndoRearrangeScreenObjects.DoCommand;
begin
  inherited;
  SetOrder(FNewList, FNewNames);
end;

procedure TUndoRearrangeScreenObjects.Redo;
begin
  DoCommand;
  inherited;
end;

procedure TUndoRearrangeScreenObjects.SetOrder(
  const ScreenObjectList: TScreenObjectList;
  const NameList: TStringList);
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  frmGoPhast.PhastModel.OwnsScreenObjects := False;
  frmGoPhast.PhastModel.ClearScreenObjects;
  frmGoPhast.PhastModel.OwnsScreenObjects := True;
  for Index := 0 to ScreenObjectList.Count - 1 do
  begin
    AScreenObject := ScreenObjectList[Index];
    AScreenObject.Name := NameList[Index];
    frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
end;

procedure TUndoRearrangeScreenObjects.Undo;
begin
  inherited;
  SetOrder(FList, FNames);
end;

function TUndoShowHideScreenObject.AddScreenObjectToChange(
  const ScreenObject: TScreenObject): integer;
begin
  result := FScreenObjectsToChange.IndexOf(ScreenObject);
  if result < 0 then
  begin
    result := FScreenObjectsToChange.Add(ScreenObject);
  end;
end;

constructor TUndoShowHideScreenObject.Create;
begin
  inherited;
  FScreenObjectsToChange := TList.Create;
  FUndoCrossSection := nil;
end;

function TUndoShowHideScreenObject.Description: string;
begin
  result := StrShowOrHideObjects;
end;

destructor TUndoShowHideScreenObject.Destroy;
begin
  FUndoCrossSection.Free;
  FScreenObjectsToChange.Free;
  inherited;
end;

procedure TUndoShowHideScreenObject.DoCommand;
begin
  ToggleVisibility;
  SetNewCrossSectionAngle;
  if FUndoCrossSection <> nil then
  begin
    FUndoCrossSection.DoCommand;
  end;
  inherited;
end;

procedure TUndoShowHideScreenObject.Redo;
begin
  inherited;
  ToggleVisibility;
  FShouldUpdateShowHideObjects := True;
  if FUndoCrossSection <> nil then
  begin
    FUndoCrossSection.Redo;
  end;
  UpdateDisplay;
end;

procedure TUndoShowHideScreenObject.SetNewCrossSectionAngle;
var
  index: Integer;
  AScreenObject: TScreenObject;
  NewLocation: TSegment2D;
begin

  if (frmGoPhast.ModelSelection in SutraSelection)
    and (frmGoPhast.PhastModel.SutraMesh.MeshType = mt3D) then
  begin
    for index := 0 to FScreenObjectsToChange.Count - 1 do
    begin
      AScreenObject := FScreenObjectsToChange[index];
      if (AScreenObject.ViewDirection = vdFront)
        and (AScreenObject.Visible)
        then
      begin
        if AScreenObject.SutraAngle <>
          frmGoPhast.PhastModel.SutraMesh.CrossSection.Angle then
        begin
          InteractiveTools.SetNewCrossSectionAngle(AScreenObject.SutraAngle,
            NewLocation);
          FUndoCrossSection:= TUndoMoveCrossSection.Create(NewLocation);
        end;
        break;
      end;
    end;
  end;
end;

procedure TUndoShowHideScreenObject.ToggleVisibility;
var
  Index: integer;
  AScreenObject: TScreenObject;
  ShouldUpdateSelectRectangle: boolean;
begin
  ShouldUpdateSelectRectangle := False;
  for Index := 0 to FScreenObjectsToChange.Count - 1 do
  begin
    AScreenObject := FScreenObjectsToChange[Index];
    if AScreenObject.Selected then
    begin
      ShouldUpdateSelectRectangle := True;
    end;
    AScreenObject.Visible := not AScreenObject.Visible;
  end;
  FShouldUpdateShowHideObjectsDisplay := True;
  if ShouldUpdateSelectRectangle then
  begin
    frmGoPhast.frameTopView.UpdateSelectRectangle;
    frmGoPhast.frameFrontView.UpdateSelectRectangle;
    frmGoPhast.frameSideView.UpdateSelectRectangle;
  end;
//  UpdateSelectionRectangle;
//  UpdateDisplay;
end;

procedure TUndoShowHideScreenObject.Undo;
begin
  ToggleVisibility;
  if FUndoCrossSection <> nil then
  begin
    FUndoCrossSection.Undo;
  end;
  inherited;
end;


{ TUndoSetScreenObjectProperties }

constructor TUndoSetScreenObjectProperties.Create(
  const AListOfScreenObjects: TList; var NewScreenObjects,
  OldScreenObjects: TScreenObjectEditCollection;
  var OldChildModelScreenObjects: TList);
var
  AScreenObject: TScreenObject;
  ScreenObjectIndex: integer;
  Item: TScreenObjectEditItem;
begin
  inherited Create;
  FOldChildModelScreenObjects := OldChildModelScreenObjects;
  OldChildModelScreenObjects := nil;

  FNewChildModelScreenObjects := TList.Create;

  FExistingScreenObjects := TScreenObjectEditCollection.Create;
  FExistingScreenObjects.OwnScreenObject := False;
  FNewScreenObjects := NewScreenObjects;
  NewScreenObjects := nil;
  FOldScreenObjects := OldScreenObjects;
  OldScreenObjects := nil;

  FShouldUpdateShowHideObjects := True;
  FListOfScreenObjects := TList.Create;
  FListOfScreenObjects.Assign(AListOfScreenObjects);
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    Item := FExistingScreenObjects.Add;
    AScreenObject := FListOfScreenObjects[ScreenObjectIndex];
    Item.ScreenObject := AScreenObject;
  end;

  FUndoEditFluxObservations:= TUndoEditFluxObservations.Create;
//  UpdateObservations;
end;

function TUndoSetScreenObjectProperties.Description: string;
begin
  result := StrEditObjectProperti;
end;

destructor TUndoSetScreenObjectProperties.Destroy;
begin
  FUndoEditFluxObservations.Free;
  FListOfScreenObjects.Free;
  FExistingScreenObjects.Free;
  FNewScreenObjects.Free;
  FOldScreenObjects.Free;
  FOldChildModelScreenObjects.Free;
  FNewChildModelScreenObjects.Free;
  inherited;
end;

procedure TUndoSetScreenObjectProperties.DoCommand;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  SetObjectProperties;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    FNewChildModelScreenObjects.Add(ChildModel.HorizontalPositionScreenObject);
  end;
  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.ShowAfterDelay;
  end;
end;

procedure TUndoSetScreenObjectProperties.SetObjectProperties;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  FShouldUpdateShowHideObjects := False;
  ResetScreenObjectDataSetSubscriptions;
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[ScreenObjectIndex];
    AScreenObject.Invalidate;


    if FSetCellsColor then
    begin
      case AScreenObject.ViewDirection of
        vdTop:
          begin
            frmGoPhast.TopScreenObjectsChanged := True;
          end;
        vdFront:
          begin
            frmGoPhast.FrontScreenObjectsChanged := True;
          end;
        vdSide:
          begin
            frmGoPhast.SideScreenObjectsChanged := True;
          end;
      else
        Assert(False);
      end;
    end;
    if AScreenObject.Count = 0 then
    begin
      AScreenObject.Deleted := True;
    end;
    AScreenObject.UpToDate := True;
  end;
//  UpdateDisplay;

  FExistingScreenObjects.Assign(FNewScreenObjects);
  FUndoEditFluxObservations.DoCommand;

  UpdateChildGrids;

  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;

  WarnSfrLengthProblems(FListOfScreenObjects);
end;

procedure TUndoSetScreenObjectProperties.Redo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenObject: TScreenObject;
begin
  SetObjectProperties;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ScreenObject := FNewChildModelScreenObjects[ChildIndex];
    ChildModel.HorizontalPositionScreenObject := ScreenObject;
  end;
end;

procedure TUndoSetScreenObjectProperties.ResetScreenObjectDataSetSubscriptions;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.ResetSubscriptions;
  end;

  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.ResetMixtureSubscriptions;
    AScreenObject.ResetBoundaryMixtureSubscriptions;
  end;
end;

procedure TUndoSetScreenObjectProperties.Undo;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  ChildModelIndex: Integer;
  ChildModel: TChildModel;
begin
  FShouldUpdateShowHideObjects := False;
  ResetScreenObjectDataSetSubscriptions;
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[ScreenObjectIndex];
    AScreenObject.ClearDataSets;
    AScreenObject.Invalidate;

    AScreenObject.Deleted := False;
    AScreenObject.UpToDate := True;
  end;
  UpdateDisplay;
  FShouldUpdateShowHideObjects := True;
  FExistingScreenObjects.Assign(FOldScreenObjects);
  FUndoEditFluxObservations.Undo;
  if FOldChildModelScreenObjects <> nil then
  begin
    Assert(frmGoPhast.PhastModel.ChildModels.Count =
      FOldChildModelScreenObjects.Count);
    for ChildModelIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildModelIndex].ChildModel;
      AScreenObject := FOldChildModelScreenObjects[ChildModelIndex];
      ChildModel.HorizontalPositionScreenObject := AScreenObject;
    end;
  end;

  UpdateChildGrids;
  UpdateShowHideObjects;
  WarnSfrLengthProblems(FListOfScreenObjects);
end;

procedure TUndoSetScreenObjectProperties.UpdateObservations;
var
  Model: TPhastModel;
  DummyNewMtsdObs: TMassFluxObs;
begin
  Model := frmGoPhast.PhastModel;
  DummyNewMtsdObs.NilAll;
  FUndoEditFluxObservations.AssignNewObservations(Model.HeadFluxObservations,
    Model.DrainObservations, Model.GhbObservations, Model.RiverObservations,
    Model.StreamObservations, DummyNewMtsdObs);
end;

{ TCustomUpdateScreenObjectUndo }

procedure TCustomUpdateScreenObjectUndo.UpdateScreenObject(const ScreenObject:
  TScreenObject);
begin
  if (ScreenObject.ElevationCount <> ecZero) then
  begin
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
  end
  else
  begin
    case ScreenObject.ViewDirection of
      vdTop:
        begin
          frmGoPhast.TopScreenObjectsChanged := True;
        end;
      vdFront:
        begin
          frmGoPhast.FrontScreenObjectsChanged := True;
        end;
      vdSide:
        begin
          frmGoPhast.SideScreenObjectsChanged := True;
        end;
    else
      Assert(False);
    end;
  end;
  if ScreenObject.ModflowHfbBoundary <> nil then
  begin
    ScreenObject.ModflowHfbBoundary.InvalidateDisplay
  end;
  ScreenObject.UpToDate := True;
end;

procedure TCustomUpdateScreenObjectUndo.UpdateSelectionRectangle;
begin
  if frmGoPhast.TopScreenObjectsChanged then
  begin
    frmGoPhast.frameTopView.UpdateSelectRectangle;
  end;
  if frmGoPhast.FrontScreenObjectsChanged then
  begin
    frmGoPhast.frameFrontView.UpdateSelectRectangle;
  end;
  if frmGoPhast.SideScreenObjectsChanged then
  begin
    frmGoPhast.frameSideView.UpdateSelectRectangle;
  end;
end;

{ TUndoDeleteSegment }

constructor TUndoDeleteSegment.Create(const AScreenObject: TScreenObject;
  const AnEdge: integer);
begin
  inherited Create;
  FSetPropertiesOfEnclosed := AScreenObject.SetValuesOfEnclosedCells;
  FSections:= TValueArrayStorage.Create;
  FSections.Assign(AScreenObject.SectionStarts);
  FScreenObject := AScreenObject;
  FNewScreenObject := nil;
  FEdge := AnEdge;
  FScreenObject.MovePoints(FPoints);
//  SetLength(FPoints, FScreenObject.Count);
  FScreenObject.MoveSelectedPoints(FSelectedVertices);
end;

function TUndoDeleteSegment.Description: string;
begin
  result := StrDeleteSegment;
end;

destructor TUndoDeleteSegment.Destroy;
begin
  FSections.Free;
  inherited;
end;

procedure TUndoDeleteSegment.DoCommand;
var
  DataSetIndex: integer;
begin
  inherited;
  FScreenObject.DeleteEdge(FEdge);

  for DataSetIndex := 0 to FScreenObject.DataSetCount - 1 do
  begin
    FScreenObject.DataSets[DataSetIndex].Invalidate;
    FScreenObject.UpToDate := True;
  end;

  frmGoPhast.InvalidateModel;

  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;

  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoDeleteSegment.Redo;
begin
  DoCommand;
  inherited;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoDeleteSegment.Undo;
var
  DataSetIndex: integer;
begin
  FScreenObject.Count := Length(FPoints);
  FScreenObject.MoveToPoints(FPoints);
  FScreenObject.MoveToSelectedPoints(FSelectedVertices);
  FScreenObject.NeedToResetSelectedVertexCount := True;
  FScreenObject.SectionStarts.Assign(FSections);
  FScreenObject.SetValuesOfEnclosedCells := FSetPropertiesOfEnclosed;

  for DataSetIndex := 0 to FScreenObject.DataSetCount - 1 do
  begin
    FScreenObject.DataSets[DataSetIndex].Invalidate;
    FScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  inherited;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

{ TUndoDeleteVertices }

constructor TUndoDeleteVertices.Create(
  const ListOfScreenObjects: TScreenObjectList);
begin
  inherited Create;
  FScreenObjects := TList.Create;
  FVertexValues := TObjectList.Create;

  FOldImportedElevations := TObjectList.Create;
  FOldHigherImportedElevations := TObjectList.Create;
  FOldLowerImportedElevations := TObjectList.Create;
  FOldImportedValues := TObjectList.Create;

  FScreenObjects.Capacity := ListOfScreenObjects.Count;

  StoreData(ListOfScreenObjects);
end;

function TUndoDeleteVertices.Description: string;
begin
  result := StrDeleteVertices;
end;

destructor TUndoDeleteVertices.Destroy;
var
  Index: Integer;
begin
  FOldImportedElevations.Free;
  FOldHigherImportedElevations.Free;
  FOldLowerImportedElevations.Free;
  FOldImportedValues.Free;
  FVertexValues.Free;
  FScreenObjects.Free;
  for Index := 0 to Length(FSectionStarts) - 1 do
  begin
    FSectionStarts[Index].Free;
  end;
  inherited;
end;

procedure TUndoDeleteVertices.DoCommand;
var
  Index, VertexIndex: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
  TempScreenObject: TScreenObject;
  PointCount: integer;
  CloseScreenObject: boolean;
  SectionIndex: Integer;
  NextPart: Boolean;
  NextEnd: Integer;
  ClosedSection: boolean;
  LastPoint: TPoint2D;
  TempSectionIndex: integer;
  TempVertextIndex: integer;
  NewSection: boolean;
  NeedToCloseSection: boolean;
  CurrentStart: integer;
  CurrentEnd: integer;
  TempIndex: integer;
  InnerVertexIndex: Integer;
  NextStart: Integer;
begin
  frmGoPhast.CanDraw := False;
  try
    FCanDeleteVertices := True;
    for Index := 0 to FScreenObjects.Count - 1 do
    begin
      AScreenObject := FScreenObjects[Index];
      if AScreenObject.SelectedVertexCount = AScreenObject.Count then
      begin
        AScreenObject.Deleted := True;
      end
      else
      begin
        if AScreenObject.SelectedVertexCount > 0 then
        begin
          CloseScreenObject := AScreenObject.Closed
            and AScreenObject.SelectedVertices[0];

          TempScreenObject := frmGoPhast.PhastModel.ScreenObjectClass.Create(nil);
          try
            PointCount := 0;
            SectionIndex := AScreenObject.SectionCount -1;
            NextEnd := AScreenObject.SectionEnd[SectionIndex];
            NextStart := AScreenObject.SectionStart[SectionIndex];
            NewSection := False;
            ClosedSection := AScreenObject.SectionClosed[SectionIndex];
            for VertexIndex := AScreenObject.Count - 1 downto 0 do
            begin
              // Test if you have reached the end of a section in the object.
              // If so, start a new section.
              NextPart := VertexIndex = NextEnd;
              if NextPart then
              begin
                // Test whether the previous section was closed.
                ClosedSection := AScreenObject.SectionClosed[SectionIndex];
                NewSection := True;
                Dec(SectionIndex);
                if SectionIndex >= 0  then
                begin
                  // mark the end of the next section
                  NextEnd := AScreenObject.SectionEnd[SectionIndex];
                  NextStart := AScreenObject.SectionStart[SectionIndex];
                end;
              end;
              if AScreenObject.SelectedVertices[VertexIndex] then
              begin
                TempIndex := TempScreenObject.SectionCount-1;
                if ClosedSection
                  and not TempScreenObject.SectionClosed[TempIndex]
                  and (TempScreenObject.SectionLength[TempIndex] > 2)
                  and (VertexIndex = NextStart) then
                begin
                  TempIndex := TempScreenObject.SectionStart[TempIndex];
                  LastPoint := TempScreenObject.Points[TempIndex];
                  TempScreenObject.AddPoint(LastPoint, False);
                  Inc(PointCount);
                  if PointCount <> TempScreenObject.Count then
                  begin
                    FCanDeleteVertices := false;
                  end;
                end;
              end
              else
              begin
                TempScreenObject.AddPoint(AScreenObject.Points[VertexIndex],
                  NewSection);
                NewSection := False;
                Inc(PointCount);
                if PointCount <> TempScreenObject.Count then
                begin
                  FCanDeleteVertices := false;
                  break;
                end;
              end;
            end;

            if FCanDeleteVertices and CloseScreenObject then
            begin
              TempSectionIndex := TempScreenObject.SectionCount -1;
              TempVertextIndex := TempScreenObject.SectionStart[TempSectionIndex];
              TempScreenObject.AddPoint(
                TempScreenObject.Points[TempVertextIndex], False);
              Inc(PointCount);
              if PointCount <> TempScreenObject.Count then
              begin
                FCanDeleteVertices := false;
              end;
            end;
          finally
            TempScreenObject.Free;
          end;

          if FCanDeleteVertices then
          begin
            SectionIndex := AScreenObject.SectionCount -1;
            NextEnd := AScreenObject.SectionEnd[SectionIndex];
            CurrentStart := AScreenObject.SectionStart[SectionIndex];
            CurrentEnd := NextEnd;
            NeedToCloseSection := False;
            ClosedSection := AScreenObject.SectionClosed[SectionIndex];
            for VertexIndex := AScreenObject.Count - 1 downto 0 do
            begin
              NextPart := VertexIndex = NextEnd;
              if NextPart then
              begin
                NeedToCloseSection := False;
                CurrentStart := AScreenObject.SectionStart[SectionIndex];
                CurrentEnd := AScreenObject.SectionEnd[SectionIndex];
                ClosedSection := AScreenObject.SectionClosed[SectionIndex];
                Dec(SectionIndex);
                if SectionIndex >= 0  then
                begin
                  NextEnd := AScreenObject.SectionEnd[SectionIndex];
                end;
              end;
              if AScreenObject.SelectedVertices[VertexIndex] then
              begin
                if ClosedSection and (VertexIndex = CurrentEnd) then
                begin
                  NeedToCloseSection := False;
                  for InnerVertexIndex := CurrentStart to CurrentEnd do
                  begin
                    if not AScreenObject.SelectedVertices[InnerVertexIndex] then
                    begin
                      NeedToCloseSection := True;
                      break;
                    end;
                  end;
                end;
                AScreenObject.DeletePoint(VertexIndex);
              end;
              if (VertexIndex < AScreenObject.Count) then
              begin
                if NeedToCloseSection and (VertexIndex = CurrentStart)
                  and not AScreenObject.SectionClosed[SectionIndex+1]
                  and (AScreenObject.SectionLength[SectionIndex+1] > 2) then
                begin
                  LastPoint := AScreenObject.Points[VertexIndex];
                  CurrentEnd := AScreenObject.SectionEnd[SectionIndex+1];
                  AScreenObject.InsertPoint(CurrentEnd+1, LastPoint);
                  NeedToCloseSection := False;
                end;
              end;
            end;
            if not AScreenObject.UpToDate then
            begin
              AScreenObject.UpToDate := True;
            end;
            AScreenObject.Invalidate;
            AScreenObject.UpToDate := True;
          end;
        end;
      end;
      for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
      begin
        AScreenObject.DataSets[DataSetIndex].Invalidate;
        AScreenObject.UpToDate := True;
      end;
    end;
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
    FShouldUpdateShowHideObjects := True;
  finally
    frmGoPhast.CanDraw := True;
  end;
  UpdateDisplay;
end;

procedure TUndoDeleteVertices.Redo;
begin
  if FCanDeleteVertices then
  begin
    DoCommand;
    inherited;
  end;
end;

procedure TUndoDeleteVertices.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
  OldPointPositionValues: TPointPositionValues;
  Varray: TValueArrayStorage;
  VCollection: TValueCollection;
  ImportIndex: Integer;
begin
  if FCanDeleteVertices then
  begin
    frmGoPhast.CanDraw := False;
    try
      for Index := 0 to FScreenObjects.Count - 1 do
      begin
        AScreenObject := FScreenObjects[Index];
        if AScreenObject.Deleted then
        begin
          AScreenObject.Deleted := False;
        end
        else
        begin
          AScreenObject.Count := Length(FPoints[Index]);
          AScreenObject.MoveToPoints(FPoints[Index]);
          AScreenObject.SectionStarts := FSectionStarts[Index];
          OldPointPositionValues := FVertexValues[Index];
          AScreenObject.PointPositionValues := OldPointPositionValues;
          AScreenObject.Invalidate;
          AScreenObject.UpToDate := True;
        end;

        Varray := FOldImportedElevations[Index];
        Varray.RestoreData;
        AScreenObject.ImportedSectionElevations := Varray;

        Varray := FOldHigherImportedElevations[Index];
        Varray.RestoreData;
        AScreenObject.ImportedHigherSectionElevations := Varray;

        Varray := FOldLowerImportedElevations[Index];
        Varray.RestoreData;
        AScreenObject.ImportedLowerSectionElevations := Varray;

        VCollection := FOldImportedValues[Index];
        for ImportIndex := 0 to VCollection.Count - 1 do
        begin
          VCollection.Items[ImportIndex].Values.RestoreData;
        end;
        AScreenObject.ImportedValues := VCollection;

        for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
        begin
          AScreenObject.DataSets[DataSetIndex].Invalidate;
          AScreenObject.UpToDate := True;
        end;
      end;
      frmGoPhast.TopScreenObjectsChanged := True;
      frmGoPhast.FrontScreenObjectsChanged := True;
      frmGoPhast.SideScreenObjectsChanged := True;
      inherited;
    finally
      frmGoPhast.CanDraw := True;
    end;
  end;
end;

function TUndoDeleteVertices.ShouldStoreData(AScreenObject: TScreenObject): Boolean;
begin
  result := AScreenObject.SelectedVertexCount > 0;
end;

procedure TUndoDeleteVertices.StoreData(const ListOfScreenObjects: TScreenObjectList);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  PointPositionValues: TPointPositionValues;
  OldPointPositionValues: TPointPositionValues;
  Varray: TValueArrayStorage;
  VCollection: TValueCollection;
  ImportIndex: Integer;
begin
  for Index := 0 to ListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := ListOfScreenObjects[Index];
    if ShouldStoreData(AScreenObject) then
    begin
      FScreenObjects.Add(AScreenObject);
      PointPositionValues := AScreenObject.PointPositionValues;
      if PointPositionValues = nil then
      begin
        FVertexValues.Add(nil);
      end
      else
      begin
        OldPointPositionValues := TPointPositionValues.Create(nil);
        OldPointPositionValues.Assign(PointPositionValues);
        FVertexValues.Add(OldPointPositionValues);
      end;
      Varray := TValueArrayStorage.Create;
      Varray.Assign(AScreenObject.ImportedHigherSectionElevations);
      Varray.RestoreData;
      FOldHigherImportedElevations.Add(Varray);
      Varray := TValueArrayStorage.Create;
      Varray.Assign(AScreenObject.ImportedLowerSectionElevations);
      Varray.RestoreData;
      FOldLowerImportedElevations.Add(Varray);
      Varray := TValueArrayStorage.Create;
      Varray.Assign(AScreenObject.ImportedSectionElevations);
      Varray.RestoreData;
      FOldImportedElevations.Add(Varray);
      VCollection := TValueCollection.Create;
      for ImportIndex := 0 to AScreenObject.ImportedValues.Count - 1 do
      begin
        AScreenObject.ImportedValues.Items[ImportIndex].Values.RestoreData;
      end;
      VCollection.Assign(AScreenObject.ImportedValues);
      FOldImportedValues.Add(VCollection);
    end;
  end;
  SetLength(FPoints, FScreenObjects.Count);
  SetLength(FSectionStarts, FScreenObjects.Count);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    AScreenObject.MovePoints(FPoints[Index]);
    FSectionStarts[Index] := TValueArrayStorage.Create;
    FSectionStarts[Index].Assign(AScreenObject.SectionStarts);
  end;
end;

{ TScreenObjectEditItem }

procedure TScreenObjectEditItem.Assign(Source: TPersistent);
var
  SourceItem: TScreenObjectEditItem;
begin
  if Source is TScreenObjectEditItem then
  begin
    SourceItem := TScreenObjectEditItem(Source);
    Assert(FScreenObject <> nil);
    Assert(SourceItem.FScreenObject <> nil);
    ScreenObject.Assign(SourceItem.ScreenObject);
  end
  else
  begin
    inherited;
  end;
end;

constructor TScreenObjectEditItem.Create(Collection: TCollection);
begin
  inherited;
  OwnScreenObject :=
    (Collection as TScreenObjectEditCollection).OwnScreenObject;
end;

destructor TScreenObjectEditItem.Destroy;
begin
  if OwnScreenObject then
  begin
    FreeAndNil(FScreenObject);
  end;
  inherited;
end;

{ TScreenObjectEditCollection }

function TScreenObjectEditCollection.Add: TScreenObjectEditItem;
begin
  result := inherited Add as TScreenObjectEditItem;
end;

procedure TScreenObjectEditCollection.Assign(Source: TPersistent);
var
  SourceCollection: TScreenObjectEditCollection;
  ItemIndex: Integer;
  Item, SourceItem: TScreenObjectEditItem;
begin
  SourceCollection := Source as TScreenObjectEditCollection;
  Assert(Count = SourceCollection.Count);
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := Items[ItemIndex] as TScreenObjectEditItem;
    SourceItem := SourceCollection.Items[ItemIndex] as TScreenObjectEditItem;
    Item.Assign(SourceItem);
  end;
end;

constructor TScreenObjectEditCollection.Create;
begin
  inherited Create(TScreenObjectEditItem);
end;

function TScreenObjectEditCollection.GetItems(
  Index: integer): TScreenObjectEditItem;
begin
  result := inherited Items[Index] as TScreenObjectEditItem;
end;

function TScreenObjectEditCollection.IndexOfScreenObjectName(
  AScreenObjectName: string): integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if Items[index].ScreenObject.Name = AScreenObjectName then
    begin
      Result := index;
      Exit;
    end;
  end;
end;

procedure TScreenObjectEditCollection.SetOwnScreenObject(const Value: boolean);
var
  Item: TScreenObjectEditItem;
  ItemIndex: Integer;
begin
  FOwnScreenObject := Value;
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := Items[ItemIndex] as TScreenObjectEditItem;
    Item.OwnScreenObject := Value;
  end;
end;

{ TUndoAddPart }

procedure TUndoAddPart.AddPoint(Point: TPoint2D);
begin
  if Count = Capacity then
  begin
    Grow;
  end;
  FNewPoints[Count] := Point;
  Inc(FCount);
end;

constructor TUndoAddPart.Create(ScreenObject: TScreenObject);
begin
  inherited Create;
  FScreenObject := ScreenObject;
end;

procedure TUndoAddPart.DeleteLastPoint;
begin
  Assert(Count > 0);
  Dec(FCount);
end;

function TUndoAddPart.Description: string;
begin
  result := StrAddPartToObject;
end;

procedure TUndoAddPart.DoCommand;
begin
  inherited;
  UpdateScreenObject(FScreenObject);
  UpdateSelectionRectangle;
end;

procedure TUndoAddPart.Grow;
var
  Delta: integer;
begin
  if FCapacity < 16 then
  begin
    Delta := 4;
  end
  else
  begin
    Delta := FCapacity div 4;
  end;
  Capacity := FCapacity + Delta;
end;

procedure TUndoAddPart.Redo;
var
  Index: integer;
begin
  for Index := 0 to Count - 1 do
  begin
    FScreenObject.AddPoint(FNewPoints[Index], (Index = 0));
  end;
  inherited;
  DoCommand;
end;

procedure TUndoAddPart.SetCapacity(const Value: integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;
    SetLength(FNewPoints, FCapacity);
  end;
end;

procedure TUndoAddPart.Undo;
begin
  FScreenObject.Count := FScreenObject.Count - Count;
  UpdateScreenObject(FScreenObject);
  UpdateSelectionRectangle;
  inherited;
end;

{ TUndoMergeObjects }

constructor TUndoMergeObjects.Create;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  MaxX: double;
  MinX: Real;
  MaxY: Real;
  MinY: Real;
  X, Y: double;
  StoredObjects: TPointerArray;
  APoint: TPoint2D;
  AnotherPoint: TPoint2D;
  OtherScreenObject: TScreenObject;
  OriginalCount: Integer;
  TempScreenObject: TScreenObject;
  PointIndex: Integer;
  TempPoints: TRealPointArray;
  LocalEpsilon: double;
  P1: TPoint2D;
  P2: TPoint2D;
  StartPoint: TPoint2D;
  EndPoint: TPoint2D;
  PP1: TPoint2D;
  PP2: TPoint2D;
  SI_Index: Integer;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < LocalEpsilon;
    end;
  end;
begin
  inherited;
  FScreenObjects:= TList.Create;
  FQuadTree := TRbwQuadTree.Create(nil);
  FScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected
      and (ScreenObject.SectionCount = 1)
      and (not ScreenObject.Closed)
      and (not (ScreenObject.Count = 1)) then
    begin
      StartPoint := ScreenObject.Points[0];
      EndPoint := ScreenObject.Points[ScreenObject.Count -1];
      if (StartPoint.x <> EndPoint.x) or (StartPoint.y <> EndPoint.y) then
      begin
        // the object will not be considered closed if it doesn't have
        // at least 4 vertices.
        FScreenObjects.Add(ScreenObject);
      end;
    end;
  end;
  FScreenObjects.Capacity := FScreenObjects.Count;
  SetLength(FOldPoints, FScreenObjects.Count);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    SetLength(FOldPoints[Index], ScreenObject.Count);
    ScreenObject.MovePoints(FOldPoints[Index]);
  end;
  LocalEpsilon := 0;
  if FScreenObjects.Count > 0 then
  begin
    ScreenObject := FScreenObjects[0];
    MaxX := ScreenObject.MaxX;
    MinX := ScreenObject.MinX;
    MaxY := ScreenObject.MaxY;
    MinY := ScreenObject.MinY;
    for Index := 1 to FScreenObjects.Count - 1 do
    begin
      ScreenObject := FScreenObjects[Index];
      MaxX := Max(MaxX, ScreenObject.MaxX);
      MinX := Min(MinX, ScreenObject.MinX);
      MaxY := Max(MaxY, ScreenObject.MaxY);
      MinY := Min(MinY, ScreenObject.MinY);
    end;
    FQuadTree.XMax := MaxX;
    FQuadTree.XMin := MinX;
    FQuadTree.YMax := MaxY;
    FQuadTree.YMin := MinY;
    LocalEpsilon := Max(Abs(MaxX), Abs(MinX));
    LocalEpsilon := Max(LocalEpsilon, Abs(MaxY));
    LocalEpsilon := Max(LocalEpsilon, Abs(MinY));
    LocalEpsilon := LocalEpsilon/1E14;
  end;
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    APoint := ScreenObject.Points[0];
    X := APoint.x;
    Y := APoint.y;
    if FQuadTree.Count > 0 then
    begin
      FQuadTree.FindClosestPointsData(X, Y, StoredObjects);
      if NearlyTheSame(X, APoint.x) and NearlyTheSame(Y, APoint.y)
        and (Length(StoredObjects) > 0) then
      begin
        OriginalCount := ScreenObject.Count;
        try
          OtherScreenObject := ScreenObject;
          for SI_Index := 0 to Length(StoredObjects) - 1 do
          begin
            OtherScreenObject := StoredObjects[SI_Index];
            if ScreenObject <> OtherScreenObject then
            begin
              break;
            end;
          end;

          if ScreenObject = OtherScreenObject then
          begin
            // the object will not be considered closed if it doesn't have
            // at least 4 vertices.
            Continue;
          end;

          AnotherPoint := OtherScreenObject.Points[0];
          if NearlyTheSame(APoint.x, AnotherPoint.x)
            and NearlyTheSame(APoint.y, AnotherPoint.y) then
          begin
            // The first point of ScreenObject is at the same location as
            // the first point of OtherScreenObject
            PP1 := OtherScreenObject.Points[1];
            PP2 := ScreenObject.Points[1];
            if (PP1.x = PP2.x) and (PP1.y = PP2.y) then
            begin
              // Don't connect if it's backtracking.
              Continue;
            end;
            TempScreenObject := TScreenObject.Create(nil);
            try
              TempScreenObject.Capacity :=
                OtherScreenObject.Count + ScreenObject.Count -1;
              for PointIndex := OtherScreenObject.Count -1 downto 0 do
              begin
                TempScreenObject.AddPoint(
                  OtherScreenObject.Points[PointIndex], False);
              end;
              for PointIndex := 1 to ScreenObject.Count - 1 do
              begin
                TempScreenObject.AddPoint(ScreenObject.Points[PointIndex], False);
              end;
              SetLength(TempPoints, TempScreenObject.Count);
              TempScreenObject.MovePoints(TempPoints);
              ScreenObject.Count := TempScreenObject.Count;
              ScreenObject.MoveToPoints(TempPoints);
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
              Assert(FQuadTree.RemovePoint(
                AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if not ScreenObject.Closed then
              begin
                P1 := ScreenObject.Points[0];
                P2 := ScreenObject.Points[ScreenObject.Count-1];
                if NearlyTheSame(P1.x, P2.x) and NearlyTheSame(P1.y, P2.y) then
                begin
                  ScreenObject.Points[ScreenObject.Count-1] := ScreenObject.Points[0];
                end
                else
                begin
                  FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
                end;
              end;
              OtherScreenObject.Deleted := True;
            finally
              TempScreenObject.Free;
            end;
          end
          else
          begin
            AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
            Assert(NearlyTheSame(APoint.x, AnotherPoint.x)
              and NearlyTheSame(APoint.y, AnotherPoint.y));
            PP1 := OtherScreenObject.Points[OtherScreenObject.Count-2];
            PP2 := ScreenObject.Points[1];
            if (PP1.x = PP2.x) and (PP1.y = PP2.y) then
            begin
              // Don't connect if it's backtracking.
              Continue;
            end;
            TempScreenObject := TScreenObject.Create(nil);
            try
              // The first point of ScreenObject is at the same location as
              // the last point of OtherScreenObject
              TempScreenObject.Capacity :=
                OtherScreenObject.Count + ScreenObject.Count -1;
              for PointIndex := 0 to OtherScreenObject.Count -1 do
              begin
                TempScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              for PointIndex := 1 to ScreenObject.Count - 1 do
              begin
                TempScreenObject.AddPoint(ScreenObject.Points[PointIndex], False);
              end;
              SetLength(TempPoints, TempScreenObject.Count);
              TempScreenObject.MovePoints(TempPoints);
              ScreenObject.Count := TempScreenObject.Count;
              ScreenObject.MoveToPoints(TempPoints);
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[0];
              Assert(FQuadTree.RemovePoint(
                AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
//              end;
              if ScreenObject.Closed then
              begin
                FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end
              else
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
              OtherScreenObject.Deleted := True;
            finally
              TempScreenObject.Free;
            end;
          end;
        except on E: EScreenObjectError do
          ScreenObject.Count := OriginalCount;
        end;
      end
      else
      begin
        FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
      end;
    end
    else
    begin
      FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
    end;
    if (not ScreenObject.Closed) and (ScreenObject.Count > 1) then
    begin
      APoint := ScreenObject.Points[ScreenObject.Count-1];
      X := APoint.x;
      Y := APoint.y;
      if FQuadTree.Count > 0 then
      begin
        FQuadTree.FindClosestPointsData(X, Y, StoredObjects);
        if NearlyTheSame(X, APoint.x) and NearlyTheSame(Y, APoint.y)
          and (Length(StoredObjects) > 0) then
        begin
          OriginalCount := ScreenObject.Count;
          try
            for SI_Index := 0 to Length(StoredObjects) - 1 do
            begin
              OtherScreenObject := StoredObjects[SI_Index];
              if ScreenObject <> OtherScreenObject then
              begin
                break;
              end;
            end;
            if ScreenObject = OtherScreenObject then
            begin
              FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
              // the object will not be considered closed if it doesn't have
              // at least 4 vertices.
              Continue;
            end;
            ScreenObject.Capacity := ScreenObject.Count + OtherScreenObject.Count -1;
            AnotherPoint := OtherScreenObject.Points[0];
            if (APoint.x = AnotherPoint.x) and (APoint.y = AnotherPoint.y) then
            begin
              // The last point of ScreenObject is at the same location as
              // the first point of OtherScreenObject
              PP1 := OtherScreenObject.Points[1];
              PP2 := ScreenObject.Points[ScreenObject.Count-2];
              if (PP1.x = PP2.x) and (PP1.y = PP2.y) then
              begin
                // Don't connect if it's backtracking.
                FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
                Continue;
              end;
              for PointIndex := 1 to OtherScreenObject.Count - 1 do
              begin
                ScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
              Assert(FQuadTree.RemovePoint(
                AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if ScreenObject.Closed then
              begin
                FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end
              else
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
            end
            else
            begin
              // The last point of ScreenObject is at the same location as
              // the last point of OtherScreenObject
              AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];

              if ((APoint.x <> AnotherPoint.x) or (APoint.y <> AnotherPoint.y)) then
              begin
                raise EIllegalMerge.Create(Format(StrTheEndpointsOfThe,
                  [ScreenObject.Name, OtherScreenObject.Name ]));
              end;
              PP1 := OtherScreenObject.Points[OtherScreenObject.Count-2];
              PP2 := ScreenObject.Points[ScreenObject.Count-2];
              if (PP1.x = PP2.x) and (PP1.y = PP2.y) then
              begin
                // Don't connect if it's backtracking.
                FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
                Continue;
              end;

              for PointIndex := OtherScreenObject.Count - 2 downto 0 do
              begin
                ScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[0];
              Assert(FQuadTree.RemovePoint(
                AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if ScreenObject.Closed then
              begin
                FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end
              else
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
            end;
            OtherScreenObject.Deleted := True;
          except on E: EScreenObjectError do
            ScreenObject.Count := OriginalCount;
          end;
        end
        else
        begin
          FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
        end;
      end
      else
      begin
        FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
      end;
    end;
  end;
  SetLength(FDeleted, FScreenObjects.Count);
  SetLength(FNewPoints, FScreenObjects.Count);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    FDeleted[Index] := ScreenObject.Deleted;
    if ScreenObject.Deleted then
    begin
      SetLength(FNewPoints[Index], 0);
    end
    else
    begin
      SetLength(FNewPoints[Index], ScreenObject.Count);
      ScreenObject.MovePoints(FNewPoints[Index]);
    end;
  end;
end;

function TUndoMergeObjects.Description: string;
begin
  result := StrMergeObjects;
end;

destructor TUndoMergeObjects.Destroy;
begin
  FQuadTree.Free;
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoMergeObjects.DoCommand;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    ScreenObject.Deleted := FDeleted[Index];
    if not ScreenObject.Deleted then
    begin
      ScreenObject.Count := Length(FNewPoints[Index]);
      ScreenObject.MoveToPoints(FNewPoints[Index]);
      ScreenObject.Invalidate;
    end;
  end;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoMergeObjects.Redo;
begin
  DoCommand;

end;

function TUndoMergeObjects.ShouldUse: boolean;
begin
  result := FScreenObjects.Count > 1;
end;

procedure TUndoMergeObjects.Undo;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    ScreenObject.Count := Length(FOldPoints[Index]);
    ScreenObject.MoveToPoints(FOldPoints[Index]);
    ScreenObject.Deleted := False;
    ScreenObject.Invalidate;
  end;
  FShouldUpdateShowHideObjects := True;
  inherited;
end;

{ TUndoReverseVerticies }

constructor TUndoReverseVerticies.Create;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  FScreenObjects := TList.Create;
  FScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index]
      as TScreenObject;
    if AScreenObject.Selected and not AScreenObject.Deleted then
    begin
      FScreenObjects.Add(AScreenObject)
    end;
  end;
  FScreenObjects.Capacity := FScreenObjects.Count;
  SetPostSelection;
end;

function TUndoReverseVerticies.Description: string;
begin
  result := StrReverseObjectOrder;
end;

destructor TUndoReverseVerticies.Destroy;
begin
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoReverseVerticies.DoCommand;
begin
  inherited;
  ReverseObjects;
end;

procedure TUndoReverseVerticies.ReverseObjects;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    AScreenObject.ReverseDirection;
  end;
  if (frmGoPhast.PhastModel.SfrStreamLinkPlot.StreamsToPlot <> stpNone)
    or (frmGoPhast.PhastModel.StrStreamLinkPlot.StreamsToPlot <> stpNone) then
  begin
    frmGoPhast.frameTopView.ModelChanged := True;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  end;
end;

procedure TUndoReverseVerticies.Undo;
begin
  inherited;
  ReverseObjects;
end;

destructor TCustomImportMultipleScreenObjects.Destroy;
begin
  FScreenObjectsToDelete.Free;
  FNewScreenObjects.Free;
  inherited;
end;

procedure TCustomImportMultipleScreenObjects.DoCommand;
begin
  FShouldUpdateShowHideObjects := True;
  ApplyDeletedStatus(True);
  inherited;
end;

procedure TCustomImportMultipleScreenObjects.Redo;
begin
  FShouldUpdateShowHideObjects := True;
  ApplyDeletedStatus(True);
  inherited;
end;

procedure TCustomImportMultipleScreenObjects.StoreNewScreenObjects(
  const ListOfScreenObjects: TList);
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  FNewScreenObjects := TList.Create;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.Selected := false;
  end;
  FNewScreenObjects.Assign(ListOfScreenObjects);
  frmGoPhast.PhastModel.IncreaseScreenObjectCapacity(ListOfScreenObjects.Count);
  for Index := 0 to FNewScreenObjects.Count - 1 do
  begin
    AScreenObject := FNewScreenObjects[Index];
    frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
    AScreenObject.ImportedValues.CacheData;
  end;
  SetPostSelection;
end;

procedure TCustomImportMultipleScreenObjects.UpdateScreenObject(
  const AScreenObject: TScreenObject);
begin
  if (AScreenObject.ElevationCount <> ecZero) then
  begin
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
  end
  else
  begin
    case AScreenObject.ViewDirection of
      vdTop:
        begin
          frmGoPhast.TopScreenObjectsChanged := True;
        end;
      vdFront:
        begin
          frmGoPhast.FrontScreenObjectsChanged := True;
        end;
      vdSide:
        begin
          frmGoPhast.SideScreenObjectsChanged := True;
        end;
    else
      Assert(False);
    end;
  end;
  AScreenObject.UpToDate := True;
end;

procedure TCustomImportMultipleScreenObjects.UnDeleteNewScreenObjects;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FNewScreenObjects.Count - 1 do
  begin
    AScreenObject := FNewScreenObjects[Index];
    // Always invalidate AScreenObject so that any data sets
    // that are set by new objects get invalidated.
    AScreenObject.Deleted := False;
    AScreenObject.Invalidate;
    UpdateScreenObject(AScreenObject);
  end;
end;

procedure TCustomImportMultipleScreenObjects.Undo;
begin
  FShouldUpdateShowHideObjects := True;
  ApplyDeletedStatus(False);
  inherited;
end;

procedure TCustomImportMultipleScreenObjects.ApplyDeletedStatus(SetDeleted: Boolean);
var
  index: Integer;
begin
  for index := 0 to FScreenObjectsToDelete.Count - 1 do
  begin
    FScreenObjectsToDelete[index].Deleted := SetDeleted;
  end;
end;

constructor TCustomImportMultipleScreenObjects.Create;
begin
  inherited;
  FScreenObjectsToDelete := TScreenObjectList.Create;
end;

procedure TCustomImportMultipleScreenObjects.DeleteNewScreenObjects;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FNewScreenObjects.Count - 1 do
  begin
    AScreenObject := FNewScreenObjects[Index];
    if not AScreenObject.Deleted then
    begin
      AScreenObject.Deleted := True;
      AScreenObject.Invalidate;
      UpdateScreenObject(AScreenObject);
    end;
  end;
end;

{ TUndoCutScreenObjects }

function TUndoCutScreenObjects.Description: string;
begin
  if FScreenObjects.Count > 1 then
  begin
    result := StrCutObjects;
  end
  else
  begin
    result := StrCutObject;
  end;
end;

procedure TUndoCutScreenObjects.DoCommand;
begin
  frmGoPhast.PhastModel.CopyScreenObjectsToClipboard;
  inherited;
end;

procedure TUndoCutScreenObjects.Redo;
begin
  frmGoPhast.PhastModel.CopyScreenObjectsToClipboard;
  inherited;
end;

{ TUndoPasteScreenObjects }


constructor TUndoPasteScreenObjects.Create;
begin
  FOldChildModelScreenObjects := TList.Create;
  inherited;
end;

function TUndoPasteScreenObjects.Description: string;
begin
  result := StrPasteObjects;
end;

destructor TUndoPasteScreenObjects.Destroy;
begin
  FOldChildModelScreenObjects.Free;
  inherited;
end;

procedure TUndoPasteScreenObjects.DoCommand;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    FOldChildModelScreenObjects.Add(ChildModel.HorizontalPositionScreenObject)
  end;

  inherited;
end;

procedure TUndoPasteScreenObjects.Redo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenObject: TScreenObject;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ScreenObject := FOldChildModelScreenObjects[ChildIndex];
    ChildModel.HorizontalPositionScreenObject := ScreenObject;
  end;

  inherited;
end;

procedure TUndoPasteScreenObjects.StoreNewScreenObjects(
  const ListOfScreenObjects: TList);
begin
  FNewScreenObjects := TList.Create;
  FNewScreenObjects.Assign(ListOfScreenObjects);
  frmGoPhast.PhastModel.IncreaseScreenObjectCapacity(ListOfScreenObjects.Count);
  SetPostSelection;
end;

procedure TUndoPasteScreenObjects.Undo;
begin
  DeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  inherited;
end;

{ TUndoExplodeScreenObject }

constructor TUndoExplodeScreenObject.Create;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  SectionIndex: Integer;
  NewScreenObject: TScreenObject;
  InnerSectionIndex: Integer;
  PointIndex: Integer;
  NewScreenObjects: TList;
  NewIndex: Integer;
begin
  inherited Create;

  NewIndex := frmGoPhast.PhastModel.ScreenObjectCount;
  NewScreenObjects := TList.Create;
  try
    for Index := 0 to FObjectToSplit.Count - 1 do
    begin
      AScreenObject := FObjectToSplit[Index];
      for SectionIndex := 1 to AScreenObject.SectionCount - 1 do
      begin
        NewScreenObject :=  TScreenObjectClass(AScreenObject.ClassType).
          Create(frmGoPhast.PhastModel);
        NewScreenObjects.Add(NewScreenObject);
        NewScreenObject.Assign(AScreenObject);
        NewScreenObject.Name := ObjectPrefix + IntToStr(NewIndex);
        Inc(NewIndex);
        for InnerSectionIndex := NewScreenObject.SectionCount - 1 downto 0 do
        begin
          if InnerSectionIndex = SectionIndex then
          begin
            Continue
          end;
          for PointIndex := NewScreenObject.SectionEnd[InnerSectionIndex]
            downto NewScreenObject.SectionStart[InnerSectionIndex] do
          begin
            NewScreenObject.DeletePoint(PointIndex);
          end;
        end;
      end;
      for PointIndex := AScreenObject.Count -1 downto
        AScreenObject.SectionEnd[0]+1 do
      begin
        AScreenObject.DeletePoint(PointIndex);
      end;
      UnselectAllVertices(AScreenObject);
    end;
    for Index := 0 to NewScreenObjects.Count - 1 do
    begin
      AScreenObject := NewScreenObjects[Index];
      UnselectAllVertices(AScreenObject);
    end;
    StoreNewScreenObjects(NewScreenObjects);
    DeleteAllButFirstSection;
    for Index := 0 to FObjectToSplit.Count - 1 do
    begin
      AScreenObject := FObjectToSplit[Index];
      AScreenObject.Selected := True;
    end;
    SetPostSelection;
  finally
    NewScreenObjects.Free;
  end;

end;

function TUndoExplodeScreenObject.Description: string;
begin
  result := StrSplitSelectedObjec;
end;

constructor TCustomUndoDivideScreenObject.Create;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  OldSettings: TScreenObject;
begin
  inherited Create;
  FOldChildModelScreenObjects := TList.Create;
  FObjectToSplit := TList.Create;
  FOldScreenObjectSettings := TObjectList.Create;
  for Index := 0 to FOldSelectedScreenObjects.Count - 1 do
  begin
    AScreenObject := FOldSelectedScreenObjects[Index];
    if ShouldDivideScreenScreenObject(AScreenObject) then
    begin
      FObjectToSplit.Add(AScreenObject);
      OldSettings := TScreenObjectClass(AScreenObject.ClassType).Create(nil);
      OldSettings.Assign(AScreenObject);
      FOldScreenObjectSettings.Add(OldSettings)
    end;
  end;
end;

destructor TCustomUndoDivideScreenObject.Destroy;
begin
  FObjectToSplit.Free;
  FOldScreenObjectSettings.Free;
  FOldChildModelScreenObjects.Free;
  inherited;
end;

procedure TCustomUndoDivideScreenObject.DoCommand;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    FOldChildModelScreenObjects.Add(ChildModel.HorizontalPositionScreenObject)
  end;

  inherited;
end;

procedure TUndoExplodeScreenObject.Redo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenObject: TScreenObject;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ScreenObject := FOldChildModelScreenObjects[ChildIndex];
    ChildModel.HorizontalPositionScreenObject := ScreenObject;
  end;
  DeleteAllButFirstSection;
  inherited;
end;

function TUndoExplodeScreenObject.ShouldDivideScreenScreenObject(
  AScreenObject: TScreenObject): boolean;
begin
  result := AScreenObject.SectionCount > 1;
end;

procedure TUndoExplodeScreenObject.DeleteAllButFirstSection;
var
  Index: Integer;
  PointIndex: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FObjectToSplit.Count - 1 do
  begin
    AScreenObject := FObjectToSplit[Index];
    for PointIndex := AScreenObject.Count - 1
      downto AScreenObject.SectionEnd[0] + 1 do
    begin
      AScreenObject.DeletePoint(PointIndex);
    end;
  end;
end;

procedure TCustomUndoDivideScreenObject.Undo;
var
  Index: Integer;
  AScreenObject, OldSettings: TScreenObject;
begin
  DeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  Assert(FObjectToSplit.Count = FOldScreenObjectSettings.Count);
  for Index := 0 to FObjectToSplit.Count - 1 do
  begin
    AScreenObject := FObjectToSplit[Index];
    OldSettings := FOldScreenObjectSettings[Index];
    AScreenObject.Assign(OldSettings);
  end;
  inherited;
end;

{ TUndoSplitScreenObject }

constructor TUndoMakeSelectedVerticesNewScreenObject.Create;
var
  NewIndex: Integer;
  NewScreenObjects: TList;
  Index: Integer;
  AScreenObject: TScreenObject;
  NewScreenObject: TScreenObject;
  PointIndex: Integer;
begin
  inherited Create;
  NewIndex := frmGoPhast.PhastModel.ScreenObjectCount;
  NewScreenObjects := TList.Create;
  try
    for Index := 0 to FObjectToSplit.Count - 1 do
    begin
      AScreenObject := FObjectToSplit[Index];

      NewScreenObject :=  TScreenObjectClass(AScreenObject.ClassType).
        Create(frmGoPhast.PhastModel);
      NewScreenObjects.Add(NewScreenObject);
      NewScreenObject.Assign(AScreenObject);
      NewScreenObject.Name := ObjectPrefix + IntToStr(NewIndex);
      Inc(NewIndex);

      for PointIndex := AScreenObject.Count - 1 downto 0 do
      begin
        if AScreenObject.SelectedVertices[PointIndex] then
        begin
          AScreenObject.DeletePoint(PointIndex);
        end
        else
        begin
          NewScreenObject.DeletePoint(PointIndex);
        end;
      end;
      UnselectAllVertices(AScreenObject);
      UnselectAllVertices(NewScreenObject);
    end;
    StoreNewScreenObjects(NewScreenObjects);
    for Index := 0 to FObjectToSplit.Count - 1 do
    begin
      AScreenObject := FObjectToSplit[Index];
      AScreenObject.Selected := True;
    end;
    SetPostSelection;
  finally
    NewScreenObjects.Free;
  end;
end;

function TUndoMakeSelectedVerticesNewScreenObject.Description: string;
begin
  result := StrMakeSelectedVertic;
end;

procedure TUndoMakeSelectedVerticesNewScreenObject.Redo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenObject: TScreenObject;
  Index: Integer;
  AScreenObject: TScreenObject;
  PointIndex: Integer;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ScreenObject := FOldChildModelScreenObjects[ChildIndex];
    ChildModel.HorizontalPositionScreenObject := ScreenObject;
  end;
  for Index := 0 to FObjectToSplit.Count - 1 do
  begin
    AScreenObject := FObjectToSplit[Index];

    for PointIndex := AScreenObject.Count - 1 downto 0 do
    begin
      if AScreenObject.SelectedVertices[PointIndex] then
      begin
        AScreenObject.DeletePoint(PointIndex);
      end
    end;
  end;
  inherited;
end;

function TUndoMakeSelectedVerticesNewScreenObject.
  ShouldDivideScreenScreenObject(AScreenObject: TScreenObject): boolean;
begin
  result := (AScreenObject.SelectedVertexCount > 0)
    and (AScreenObject.SelectedVertexCount < AScreenObject.Count);
end;

{ TUndoSplitScreenObject }

constructor TUndoSplitScreenObject.Create;
var
  NewIndex: Integer;
  NewScreenObjects: TList;
  Index: Integer;
  AScreenObject: TScreenObject;
  NewScreenObject: TScreenObject;
  PointIndex: Integer;
  ScreenObjects: array[0..1] of TScreenObject;
  SelectedIndex: integer;
begin
  inherited Create;
  NewIndex := frmGoPhast.PhastModel.ScreenObjectCount;
  NewScreenObjects := TList.Create;
  try
    for Index := 0 to FObjectToSplit.Count - 1 do
    begin
      AScreenObject := FObjectToSplit[Index];

      NewScreenObject :=  TScreenObjectClass(AScreenObject.ClassType).
        Create(frmGoPhast.PhastModel);
      NewScreenObjects.Add(NewScreenObject);
      NewScreenObject.Assign(AScreenObject);
      NewScreenObject.Name := ObjectPrefix + IntToStr(NewIndex);
      Inc(NewIndex);

      ScreenObjects[0] := AScreenObject;
      ScreenObjects[1] := NewScreenObject;
      SelectedIndex := 0;

      for PointIndex := AScreenObject.Count - 1 downto 0 do
      begin
        if AScreenObject.SelectedVertices[PointIndex] then
        begin
          SelectedIndex := 1 - SelectedIndex;
        end
        else
        begin
          ScreenObjects[SelectedIndex].DeletePoint(PointIndex);
        end;
      end;
      UnselectAllVertices(AScreenObject);
      UnselectAllVertices(NewScreenObject);
    end;
    StoreNewScreenObjects(NewScreenObjects);
    for Index := 0 to FObjectToSplit.Count - 1 do
    begin
      AScreenObject := FObjectToSplit[Index];
      AScreenObject.Selected := True;
    end;
    SetPostSelection;
  finally
    NewScreenObjects.Free;
  end;
end;

function TUndoSplitScreenObject.Description: string;
begin
  result := StrSplitObjectAtSele;
end;

procedure TUndoSplitScreenObject.Redo;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ScreenObject: TScreenObject;
  Index: Integer;
  AScreenObject: TScreenObject;
  PointIndex: Integer;
  ShouldDelete: Boolean;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    ScreenObject := FOldChildModelScreenObjects[ChildIndex];
    ChildModel.HorizontalPositionScreenObject := ScreenObject;
  end;
  for Index := 0 to FObjectToSplit.Count - 1 do
  begin
    AScreenObject := FObjectToSplit[Index];
    ShouldDelete := True;
    for PointIndex := AScreenObject.Count - 1 downto 0 do
    begin
      if AScreenObject.SelectedVertices[PointIndex] then
      begin
        ShouldDelete := not ShouldDelete;
      end
      else if ShouldDelete then
      begin
        AScreenObject.DeletePoint(PointIndex);
      end;
    end;
  end;
  inherited;
end;

procedure TCustomUndoDivideScreenObject.UnselectAllVertices(
  AScreenObject: TScreenObject);
begin
  AScreenObject.ClearSelectedVertices;
end;

function TUndoSplitScreenObject.ShouldDivideScreenScreenObject(
  AScreenObject: TScreenObject): boolean;
begin
  result := (AScreenObject.SelectedVertexCount > 0);
end;

{ TCustomLockUnlockScreenObjects }

constructor TCustomLockUnlockScreenObjects.Create(
  const AListOfScreenObjects: TList);
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  FListOfScreenObjects := TList.Create;
  FListOfScreenObjects.Assign(AListOfScreenObjects);
  SetLength(FOriginalLocks, FListOfScreenObjects.Count);
  for Index := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[Index];
    FOriginalLocks[Index] := AScreenObject.PositionLocked;
  end;
end;

destructor TCustomLockUnlockScreenObjects.Destroy;
begin
  FListOfScreenObjects.Free;
  inherited;
end;

procedure TCustomLockUnlockScreenObjects.Undo;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[Index];
    AScreenObject.PositionLocked := FOriginalLocks[Index];
  end;
end;

{ TUndoLockScreenObjects }

function TUndoLockScreenObjects.Description: string;
begin
  result := StrLockSelectedObject;
end;

procedure TUndoLockScreenObjects.DoCommand;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[Index];
    AScreenObject.PositionLocked := True
  end;
end;

{ TUndoUnlockScreenObjects }

function TUndoUnlockScreenObjects.Description: string;
begin
  result := StrUnlockSelectedObje;
end;

procedure TUndoUnlockScreenObjects.DoCommand;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[Index];
    AScreenObject.PositionLocked := False
  end;
end;

{ TUndoDeleteModelResults }

constructor TUndoDeleteModelResults.Create;
var
  DataArrayManager: TDataArrayManager;
  DataSetIndex: Integer;
  ADataArray: TDataArray;
  DataSetsToDelete: TList;
  NewDataSets: TList;
  NewDataSetProperties: TObjectList;
  PhastModel: TPhastModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  DeletedScreenObjects: TScreenObjectList;
  DataStorage: TPhastDataSetStorage;
begin
  FDeleteScreenObjects := nil;
  PhastModel := frmGoPhast.PhastModel;
  DataArrayManager := PhastModel.DataArrayManager;
  DataSetsToDelete := TList.Create;
  // There are no new data sets but the following two variables
  // are needed for the inherited constructor.
  NewDataSets := TList.Create;
  NewDataSetProperties := TObjectList.Create;
//  NewDataSets: TList;
  try
    for DataSetIndex := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      ADataArray := DataArrayManager.DataSets[DataSetIndex];
      if Pos(StrModelResults, ADataArray.Classification) > 0 then
      begin
        DataSetsToDelete.Add(ADataArray);
      end
      else
      begin
        DataStorage := TPhastDataSetStorage.Create;
        NewDataSetProperties.Add(DataStorage);
        DataStorage.DataSet := ADataArray;

        DataStorage.Name := ADataArray.Name;
        DataStorage.Orientation := ADataArray.Orientation;

        DataStorage.EvaluatedAt := ADataArray.EvaluatedAt;

        DataStorage.Datatype := ADataArray.Datatype;
        DataStorage.Units := ADataArray.Units;
        DataStorage.AngleType := ADataArray.AngleType;

        DataStorage.TwoDInterpolator := ADataArray.TwoDInterpolator;
        DataStorage.Formula := ADataArray.Formula;

        DataStorage.Comment := ADataArray.Comment;
        DataStorage.Classification := ADataArray.Classification;

        if ADataArray is TCustomPhastDataSet then
        begin
          DataStorage.PhastInterpolationValues.Assign(ADataArray);
        end;
      end;
    end;
    DeletedScreenObjects:= TScreenObjectList.Create;
    try
      for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        for DataSetIndex := 0 to DataSetsToDelete.Count - 1 do
        begin
          ADataArray := DataSetsToDelete[DataSetIndex];
          if AScreenObject.IndexOfDataSet(ADataArray) >= 0 then
          begin
            DeletedScreenObjects.Add(AScreenObject);
            Break;
          end;
        end;
      end;
      if DeletedScreenObjects.Count > 0 then
      begin
        FDeleteScreenObjects := TUndoDeleteScreenObjects.
          Create(DeletedScreenObjects);
      end;
    finally
      DeletedScreenObjects.Free;
    end;
    inherited Create(DataSetsToDelete, NewDataSets, NewDataSetProperties);
  finally
    DataSetsToDelete.Free;
    NewDataSets.Free;
    NewDataSetProperties.Free;
  end;
end;

function TUndoDeleteModelResults.Description: string;
begin
  result := 'delete "Model Results" data sets and objects';
end;

destructor TUndoDeleteModelResults.Destroy;
begin
  FDeleteScreenObjects.Free;
  inherited;
end;

procedure TUndoDeleteModelResults.DoCommand;
begin
  if FDeleteScreenObjects <> nil then
  begin
    FDeleteScreenObjects.DoCommand;
  end;
  inherited;
end;

procedure TUndoDeleteModelResults.Redo;
begin
  if FDeleteScreenObjects <> nil then
  begin
    FDeleteScreenObjects.Redo;
  end;
  inherited;
end;

procedure TUndoDeleteModelResults.Undo;
begin
  inherited;
  if FDeleteScreenObjects <> nil then
  begin
    FDeleteScreenObjects.Undo;
  end;
end;

{ TUndoConvertHfbMf6 }

constructor TUndoConvertHfbMf6.Create;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  inherited Create;
  FScreenObjects := TScreenObjectList.Create;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if (not AScreenObject.Deleted)
      and (AScreenObject.ModflowHfbBoundary <> nil) then
    begin
      if AScreenObject.ModflowHfbBoundary.Used
        and not AScreenObject.ModflowHfbBoundary.UsedMf6 then
      begin
        FScreenObjects.Add(AScreenObject);
      end
      else if AScreenObject.ModflowHfbBoundary.UsedMf6 then
      begin
        FScreenObjects.Clear;
        Exit;
      end;
    end;
  end;
end;

function TUndoConvertHfbMf6.Description: string;
begin
  result := StrConvert_HFB_Objects;
end;

destructor TUndoConvertHfbMf6.Destroy;
begin
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoConvertHfbMf6.DoCommand;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  HfbItem: THfbItem;
  Boundary: THfbBoundary;
  StressPeriods: TModflowStressPeriods;
  StartTime: double;
  EndTime: Double;
begin
  inherited;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
  StartTime := StressPeriods.First.StartTime;
  EndTime := StressPeriods.Last.EndTime;
  for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[ScreenObjectIndex];
    Boundary := AScreenObject.ModflowHfbBoundary;
    HfbItem := Boundary.Values.Add;
    HfbItem.StartTime := StartTime;
    HfbItem.EndTime := EndTime;
    HfbItem.HydraulicConductivity := Boundary.HydraulicConductivityFormula;
    HfbItem.Thickness := Boundary.ThicknessFormula;
  end;
end;

function TUndoConvertHfbMf6.GetShouldConvert: Boolean;
begin
  result := (FScreenObjects.Count > 0)
end;

procedure TUndoConvertHfbMf6.Undo;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[ScreenObjectIndex];
    AScreenObject.ModflowHfbBoundary.Values.Clear;
  end;
end;

{ TUndoConvertObservationsMf6 }

constructor TUndoConvertObservationsMf6.Create;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  FluxGroupIndex: Integer;
  FluxGroup: TFluxObservationGroup;
  FactorIndex: Integer;
  ShouldBreak: boolean;
begin
  inherited Create;
  FOldMf6ObsUsed := frmGoPhast.PhastModel.ModflowPackages.Mf6ObservationUtility.IsSelected;
  FHobScreenObjects := TScreenObjectList.Create;
  FChobScreenObjects := TScreenObjectList.Create;
  FDrobScreenObjects := TScreenObjectList.Create;
  FGhbobScreenObjects := TScreenObjectList.Create;
  FRivobScreenObjects := TScreenObjectList.Create;
  if frmGoPhast.PhastModel.HobIsSelected then
  begin
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if (AScreenObject.ModflowHeadObservations <> nil) then
      begin
        if (AScreenObject.Modflow6Obs <> nil)
          and (ogHead in AScreenObject.Modflow6Obs.General) then
        begin
          FHobScreenObjects.Clear;
          break;
        end
        else
        begin
          FHobScreenObjects.Add(AScreenObject);
        end;
      end;
    end;
  end;
  
  if frmGoPhast.PhastModel.ChobIsSelected then
  begin
    ShouldBreak := False;
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.HeadFluxObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.HeadFluxObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        if (AScreenObject.Modflow6Obs <> nil)
          and (ogCHD in AScreenObject.Modflow6Obs.General) then
        begin
          FChobScreenObjects.Clear;
          ShouldBreak := True;
          break;
        end
        else
        begin
          FChobScreenObjects.Add(AScreenObject);
        end;
      end;
      if ShouldBreak then
      begin
          break;
      end;
    end;
  end;
  
  if frmGoPhast.PhastModel.DrobIsSelected then
  begin
    ShouldBreak := False;
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.DrainObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.DrainObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        if (AScreenObject.Modflow6Obs <> nil)
          and (ogDrain in AScreenObject.Modflow6Obs.General) then
        begin
          FDrobScreenObjects.Clear;
          ShouldBreak := True;
          break;
        end
        else
        begin
          FDrobScreenObjects.Add(AScreenObject);
        end;
      end;
      if ShouldBreak then
      begin
          break;
      end;
    end;
  end;
  
  if frmGoPhast.PhastModel.RvobIsSelected then
  begin
    ShouldBreak := False;
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.GhbObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.GhbObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        if (AScreenObject.Modflow6Obs <> nil)
          and (ogGHB in AScreenObject.Modflow6Obs.General) then
        begin
          FGhbobScreenObjects.Clear;
          ShouldBreak := True;
          break;
        end
        else
        begin
          FGhbobScreenObjects.Add(AScreenObject);
        end;
      end;
      if ShouldBreak then
      begin
          break;
      end;
    end;
  end;
  
  if frmGoPhast.PhastModel.RvobIsSelected then
  begin
    ShouldBreak := False;
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.RiverObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.RiverObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        if (AScreenObject.Modflow6Obs <> nil)
          and (ogRiv in AScreenObject.Modflow6Obs.General) then
        begin
          FRivobScreenObjects.Clear;
          ShouldBreak := True;
          break;
        end
        else
        begin
          FRivobScreenObjects.Add(AScreenObject);
        end;
      end;
      if ShouldBreak then
      begin
          break;
      end;
    end;
  end;
end;

function TUndoConvertObservationsMf6.Description: string;
begin
  result := StrConvertMODFLOW2005_HOB;
end;

destructor TUndoConvertObservationsMf6.Destroy;
begin
  FHobScreenObjects.Free;
  FChobScreenObjects.Free;
  FDrobScreenObjects.Free;
  FGhbobScreenObjects.Free;
  FRivobScreenObjects.Free;
  inherited;
end;

procedure TUndoConvertObservationsMf6.DoCommand;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  FluxGroupIndex: Integer;
  FluxGroup: TFluxObservationGroup;
  FactorIndex: Integer;
  NewName: String;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowPackages.Mf6ObservationUtility.IsSelected := True;
  for ScreenObjectIndex := 0 to FHobScreenObjects.Count - 1 do
  begin
    AScreenObject := FHobScreenObjects[ScreenObjectIndex];
    AScreenObject.CreateMf6Obs;
//    AScreenObject.Modflow6Obs.Used := True;
    AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General + [ogHead];
    NewName := AScreenObject.ModflowHeadObservations.ObservationName;
    NewName := StringReplace(NewName, '-', '', [rfReplaceAll]);
    NewName := StringReplace(NewName, '+', '', [rfReplaceAll]);
    NewName := StringReplace(NewName, ':', '', [rfReplaceAll]);
    NewName := ValidName(NewName);
//    AScreenObject.Name := NewName;
    AScreenObject.Modflow6Obs.Name := NewName
  end;
  
  if FChobScreenObjects.Count > 0 then
  begin
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.HeadFluxObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.HeadFluxObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        AScreenObject.CreateMf6Obs;
//        AScreenObject.Modflow6Obs.Used := True;
        AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General + [ogCHD];
//        AScreenObject.Modflow6Obs.ChdFlowObs := True;
        AScreenObject.Modflow6Obs.Name := FluxGroup.ObservationName;
      end;
    end;
  end;
  
  if FDrobScreenObjects.Count > 0 then
  begin
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.DrainObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.DrainObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        AScreenObject.CreateMf6Obs;
//        AScreenObject.Modflow6Obs.Used := True;
        AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General + [ogDrain];
//        AScreenObject.Modflow6Obs.DrnFlowObs := True;
        AScreenObject.Modflow6Obs.Name := FluxGroup.ObservationName;
      end;
    end;
  end;

  if FGhbobScreenObjects.Count > 0 then
  begin
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.GhbObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.GhbObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        AScreenObject.CreateMf6Obs;
//        AScreenObject.Modflow6Obs.Used := True;
        AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General + [ogGHB];
//        AScreenObject.Modflow6Obs.GhbFlowObs := True;
        AScreenObject.Modflow6Obs.Name := FluxGroup.ObservationName;
      end;
    end;
  end;

  if FRivobScreenObjects.Count > 0 then
  begin
    for FluxGroupIndex := 0 to frmGoPhast.PhastModel.RiverObservations.Count -1 do
    begin
      FluxGroup := frmGoPhast.PhastModel.RiverObservations[FluxGroupIndex];
      for FactorIndex := 0 to FluxGroup.ObservationFactors.Count -1 do
      begin
        AScreenObject := FluxGroup.ObservationFactors[FactorIndex].ScreenObject as TScreenObject;
        if AScreenObject.Deleted then
        begin
          Continue;
        end;
        AScreenObject.CreateMf6Obs;
//        AScreenObject.Modflow6Obs.Used := True;
        AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General + [ogRiv];
//        AScreenObject.Modflow6Obs.RivFlowObs := True;
        AScreenObject.Modflow6Obs.Name := FluxGroup.ObservationName;
      end;
    end;
  end;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

function TUndoConvertObservationsMf6.GetShouldConvert: Boolean;
begin
  result := (FHobScreenObjects.Count > 0)
    or (FChobScreenObjects.Count > 0)
    or (FDrobScreenObjects.Count > 0)
    or (FGhbobScreenObjects.Count > 0)
    or (FRivobScreenObjects.Count > 0)
end;

procedure TUndoConvertObservationsMf6.Undo;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  procedure CheckDeleteObs;
  begin
	if AScreenObject.Modflow6Obs <> nil then
	begin
	  if not AScreenObject.Modflow6Obs.Used
//	    and not AScreenObject.Modflow6Obs.ChdFlowObs
//	    and not AScreenObject.Modflow6Obs.DrnFlowObs
//	    and not AScreenObject.Modflow6Obs.GhbFlowObs
//	    and not AScreenObject.Modflow6Obs.RivFlowObs
		then
	  begin
      AScreenObject.Modflow6Obs := nil;
	  end;
	end;
  end;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowPackages.Mf6ObservationUtility.IsSelected := FOldMf6ObsUsed;
  for ScreenObjectIndex := 0 to FHobScreenObjects.Count - 1 do
  begin
    AScreenObject := FHobScreenObjects[ScreenObjectIndex];
    AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General - [ogHead];
  end;
  for ScreenObjectIndex := 0 to FChobScreenObjects.Count - 1 do
  begin
    AScreenObject := FChobScreenObjects[ScreenObjectIndex];
    AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General - [ogCHD];
//    AScreenObject.Modflow6Obs.ChdFlowObs := False;
  end;
  for ScreenObjectIndex := 0 to FDrobScreenObjects.Count - 1 do
  begin
    AScreenObject := FDrobScreenObjects[ScreenObjectIndex];
    AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General - [ogDrain];
//    AScreenObject.Modflow6Obs.DrnFlowObs := False;
  end;
  for ScreenObjectIndex := 0 to FGhbobScreenObjects.Count - 1 do
  begin
    AScreenObject := FGhbobScreenObjects[ScreenObjectIndex];
    AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General - [ogGHB];
//    AScreenObject.Modflow6Obs.GhbFlowObs := False;
  end;
  for ScreenObjectIndex := 0 to FRivobScreenObjects.Count - 1 do
  begin
    AScreenObject := FRivobScreenObjects[ScreenObjectIndex];
    AScreenObject.Modflow6Obs.General := AScreenObject.Modflow6Obs.General - [ogRiv];
//    AScreenObject.Modflow6Obs.RivFlowObs := False;
  end;

  for ScreenObjectIndex := 0 to FHobScreenObjects.Count - 1 do
  begin
    AScreenObject := FHobScreenObjects[ScreenObjectIndex];
	CheckDeleteObs;
  end;
  for ScreenObjectIndex := 0 to FChobScreenObjects.Count - 1 do
    AScreenObject := FChobScreenObjects[ScreenObjectIndex];
  begin
    CheckDeleteObs;
  end;
  for ScreenObjectIndex := 0 to FDrobScreenObjects.Count - 1 do
  begin
    AScreenObject := FDrobScreenObjects[ScreenObjectIndex];
    CheckDeleteObs;
  end;
  for ScreenObjectIndex := 0 to FGhbobScreenObjects.Count - 1 do
  begin
    AScreenObject := FGhbobScreenObjects[ScreenObjectIndex];
    CheckDeleteObs;
  end;
  for ScreenObjectIndex := 0 to FRivobScreenObjects.Count - 1 do
  begin
    AScreenObject := FRivobScreenObjects[ScreenObjectIndex];
    CheckDeleteObs;
  end;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

{ TUndoConvertMnw2ToMaw }

constructor TUndoConvertMnw2ToMaw.Create;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  MawScreenObject: TScreenObject;
  AList: TList;
begin
  inherited;
  FMawSelected := frmGoPhast.PhastModel.ModflowPackages.MawPackage.IsSelected;
  AList := TList.Create;
  try
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted
        and (AScreenObject.ModflowMnw2Boundary <> nil)
        and AScreenObject.ModflowMnw2Boundary.Used then
      begin
        MawScreenObject := TScreenObject.Create(frmGoPhast.PhastModel);
        MawScreenObject.Assign(AScreenObject);
        MawScreenObject.name := GenerateNewRoot(AScreenObject.Name
          + '_' + AScreenObject.ModflowMnw2Boundary.WellID);
        MawScreenObject.ElevationCount := ecZero;
        MawScreenObject.ClearModflowBoundaries;
        MawScreenObject.CreateMawBoundary;
        MawScreenObject.ModflowMawBoundary.Assign(AScreenObject.ModflowMnw2Boundary);
        AList.Add(MawScreenObject);
      end;
    end;
    StoreNewScreenObjects(AList);
  finally
    AList.Free;
  end;
end;

function TUndoConvertMnw2ToMaw.Description: string;
begin
  result := StrConvertMNW2WellsT;
end;

procedure TUndoConvertMnw2ToMaw.DoCommand;
begin
  frmGoPhast.PhastModel.ModflowPackages.MawPackage.IsSelected := True;
  inherited;

end;

procedure TUndoConvertMnw2ToMaw.Redo;
begin
  frmGoPhast.PhastModel.ModflowPackages.MawPackage.IsSelected := True;
  UnDeleteNewScreenObjects;
  inherited;

end;

procedure TUndoConvertMnw2ToMaw.Undo;
begin
  frmGoPhast.PhastModel.ModflowPackages.MawPackage.IsSelected := FMawSelected;
  DeleteNewScreenObjects;
  inherited;

end;

{ TUndoSimplifyObjects }

function TUndoSimplifyObjects.Description: string;
begin
  result := 'simplify objects';
end;

procedure TUndoSimplifyObjects.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
//  TempScreenObject: TScreenObject;
//  PointCount: integer;
//  SectionIndex: Integer;
//  NextPart: Boolean;
//  NextEnd: Integer;
//  ClosedSection: boolean;
//  LastPoint: TPoint2D;
//  TempSectionIndex: integer;
//  TempVertextIndex: integer;
//  NewSection: boolean;
//  NeedToCloseSection: boolean;
//  CurrentStart: integer;
//  CurrentEnd: integer;
//  TempIndex: integer;
//  InnerVertexIndex: Integer;
//  NextStart: Integer;

begin
  frmGoPhast.CanDraw := False;
  try
    FCanDeleteVertices := True;
    for Index := 0 to FScreenObjects.Count - 1 do
    begin
      AScreenObject := FScreenObjects[Index];
      AScreenObject.SimplifyStraightEdges(MaxDeltaAngle, RequiredSpacing);

      if not AScreenObject.UpToDate then
      begin
        AScreenObject.UpToDate := True;
      end;
      AScreenObject.Invalidate;
      AScreenObject.UpToDate := True;

      for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
      begin
        AScreenObject.DataSets[DataSetIndex].Invalidate;
        AScreenObject.UpToDate := True;
      end;
    end;
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
    FShouldUpdateShowHideObjects := True;
  finally
    frmGoPhast.CanDraw := True;
  end;
  UpdateDisplay;
end;

procedure TUndoSimplifyObjects.SetMaxDeltaAngle(const Value: double);
begin
  FMaxDeltaAngle := Value;
end;

procedure TUndoSimplifyObjects.SetRequiredSpacing(const Value: Double);
begin
  FRequiredSpacing := Value;
end;

function TUndoSimplifyObjects.ShouldStoreData(
  AScreenObject: TScreenObject): Boolean;
begin
  result := AScreenObject.Selected;
end;

{ TUndoConvertUzfToUzf6 }

constructor TUndoConvertUzfToUzf6.Create;
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  FShouldUpdateShowHideObjects := True;
  FUzf6Packge := TUzfMf6PackageSelection.Create(nil);
  FUzf6Packge.Assign(frmGoPhast.PhastModel.ModflowPackages.UzfPackage);
  FScreenObjectList := TScreenObjectList.Create;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.ModflowUzfBoundary <> nil then
    begin
      FScreenObjectList.Add(AScreenObject);
    end;
  end;
end;

function TUndoConvertUzfToUzf6.Description: string;
begin
  result := StrConvertUZFToUZF6;
end;

destructor TUndoConvertUzfToUzf6.Destroy;
begin
  FUzf6Packge.Free;
  FScreenObjectList.Free;
  inherited;
end;

procedure TUndoConvertUzfToUzf6.DoCommand;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;

  frmGoPhast.PhastModel.ModflowPackages.UzfMf6Package.Assign(
    frmGoPhast.PhastModel.ModflowPackages.UzfPackage);
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;
  for ObjectIndex := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[ObjectIndex];
    AScreenObject.CreateModflowUzfMf6Boundary;
    AScreenObject.ModflowUzfMf6Boundary.Assign(AScreenObject.ModflowUzfBoundary);
  end;
  UpdateDisplay;
end;

procedure TUndoConvertUzfToUzf6.Redo;
begin
  inherited;
  DoCommand;
end;

procedure TUndoConvertUzfToUzf6.Undo;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  frmGoPhast.PhastModel.ModflowPackages.UzfMf6Package := FUzf6Packge;
  for ObjectIndex := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[ObjectIndex];
    AScreenObject.ModflowUzfMf6Boundary  := nil;
  end;
  inherited;
  UpdateDisplay;
end;

{ TUndoConvertSubAndSwtToCSub }

constructor TUndoConvertSubAndSwtToCSub.Create;
begin
  inherited;
  FShouldUpdateShowHideObjects := True;
  FOldCSubPackage := TCSubPackageSelection.Create(nil);
  FOldCSubPackage.Assign(frmGoPhast.PhastModel.ModflowPackages.CSubPackage);
//  FScreenObjectList := TScreenObjectList.Create;
end;

function TUndoConvertSubAndSwtToCSub.Description: string;
begin
  result := 'convert SUB and/or SWT to CSUB';
end;

destructor TUndoConvertSubAndSwtToCSub.Destroy;
begin
//  FScreenObjectList.Free;
  FOldCSubPackage.Free;
  FNewCSubPackage.Free;
  inherited;
end;

procedure TUndoConvertSubAndSwtToCSub.DoCommand;
var
  SubPackage: TSubPackageSelection;
  SwtPackage: TSwtPackageSelection;
  CSubPackage: TCSubPackageSelection;
  LocalModel: TPhastModel;
  LayerStructure: TLayerStructure;
  LayerGroupIndex: Integer;
  ALayerGroup: TLayerGroup;
  ANoDelayItem: TSubNoDelayBedLayerItem;
  AScreenObject: TScreenObject;
  DummyUndoCreateScreenObject: TCustomUndo;
  Grid: TCustomModelGrid;
  ModflowCSub: TCSubBoundary;
  NoDelayIndex: Integer;
  DelayIndex: Integer;
  ADelayItem: TSubDelayBedLayerItem;
  EquivNumberDataArray: TDataArray;
  WT_Index: Integer;
  WT_Item: TSwtWaterTableItem;
  ScreenObjectIndex: Integer;
  LayerCount: Integer;
  SubLayerIndex: Integer;
  LayerNumber: Integer;
  function GetInterbedItem(ModflowCSub: TCSubBoundary; Interbed: TCSubInterbed): TCSubPackageData;
  var
    IB_Index: Integer;
    CSubPackageDataItem: TCSubPackageData;
  begin
    result := nil;
    for IB_Index := 0 to ModflowCSub.CSubPackageData.Count - 1 do
    begin
      CSubPackageDataItem := ModflowCSub.CSubPackageData[IB_Index];
      if CSubPackageDataItem.Interbed = Interbed then
      begin
        result := CSubPackageDataItem;
        break;
      end;
    end;
    if result = nil then
    begin
      result := ModflowCSub.CSubPackageData.Add;
      result.Interbed := Interbed;
    end;
  end;
  procedure CreateScreenObject;
  begin
    AScreenObject := TScreenObject.CreateWithViewDirection(LocalModel,
      vdTop, DummyUndoCreateScreenObject, False);
    LocalModel.AddScreenObject(AScreenObject);
    FScreenObjectsToDelete.Add(AScreenObject);
    AScreenObject.ElevationCount := ecTwo;
  end;
  procedure AssignModflowCSub;
  begin
    AScreenObject.CreateCSubBoundary;
    ModflowCSub := AScreenObject.ModflowCSub;
  end;
  procedure AssignCellCorners;
  begin
    AScreenObject.Capacity := 5;
    AScreenObject.AddPoint(Grid.TwoDElementCorner(0,0), True);
    AScreenObject.AddPoint(Grid.TwoDElementCorner(0,Grid.RowCount), False);
    AScreenObject.AddPoint(Grid.TwoDElementCorner(Grid.ColumnCount,Grid.RowCount), False);
    AScreenObject.AddPoint(Grid.TwoDElementCorner(Grid.ColumnCount,0), False);
    AScreenObject.AddPoint(Grid.TwoDElementCorner(0,0), False);
  end;
  procedure AssignAllPoints;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    EquivNumberDataArray := LocalModel.DataArrayManager.
      GetDataSetByName(ADelayItem.EquivNumberDataArrayName);
    AScreenObject.Capacity := Grid.RowCount * Grid.ColumnCount;
    for RowIndex := 0 to Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to Grid.ColumnCount - 1 do
      begin
        if EquivNumberDataArray.RealData[0, RowIndex, ColIndex] >= 1 then
        begin
          AScreenObject.AddPoint(Grid.TwoDElementCenter(ColIndex,RowIndex), True);
        end;
      end;
    end;
  end;
  procedure AssignNoDelayProperties;
  var
    Interbed: TCSubInterbed;
    SelectedPackageData: TCSubPackageData;
  begin
    AssignModflowCSub;

    Interbed := CSubPackage.Interbeds.Add;
    Interbed.Name := 'CSUB_' + ANoDelayItem.Name;
    Interbed.InterbedType := itNoDelay;

    SelectedPackageData := GetInterbedItem(ModflowCSub, Interbed);;
    SelectedPackageData.Used := True;
    SelectedPackageData.InitialOffset :=
      ANoDelayItem.PreconsolidationHeadDataArrayName;
    SelectedPackageData.InitialInelasticSpecificStorage :=
      ANoDelayItem.InelasticSkeletalStorageCoefficientDataArrayName;
    SelectedPackageData.InitialElasticSpecificStorage :=
      ANoDelayItem.ElasticSkeletalStorageCoefficientDataArrayName;
  end;
  procedure AssignDelayProperties;
  var
    Interbed: TCSubInterbed;
    SelectedPackageData: TCSubPackageData;
  begin
    AssignModflowCSub;

    Interbed := CSubPackage.Interbeds.Add;
    Interbed.Name := 'CSUB_' + ADelayItem.Name;
    Interbed.InterbedType := itDelay;

    SelectedPackageData := GetInterbedItem(ModflowCSub, Interbed);;
    SelectedPackageData.Used := True;

    SelectedPackageData.Thickness :=
      ADelayItem.InterbedEquivalentThicknessDataArrayName;
    SelectedPackageData.EquivInterbedNumber :=
      ADelayItem.EquivNumberDataArrayName;
    SelectedPackageData.InitialInelasticSpecificStorage :=
      ADelayItem.InelasticSpecificStorageDataArrayName;
    SelectedPackageData.InitialElasticSpecificStorage :=
      ADelayItem.ElasticSpecificStorageDataArrayName;
    SelectedPackageData.DelayKv :=
      ADelayItem.VerticalHydraulicConductivityDataArrayName;
    SelectedPackageData.InitialDelayHeadOffset :=
      ADelayItem.InterbedStartingHeadDataArrayName;
  end;
  procedure AssignSwtProperties;
  var
    Interbed: TCSubInterbed;
    SelectedPackageData: TCSubPackageData;
    CSubItem: TCSubItem;
    StressPeriod: TModflowStressPeriod;
  begin
    AssignModflowCSub;

    Interbed := CSubPackage.Interbeds.Add;
    Interbed.Name := 'CSUB_' + WT_Item.Name;
    Interbed.InterbedType := itNoDelay;

    SelectedPackageData := GetInterbedItem(ModflowCSub, Interbed);;
    SelectedPackageData.Used := True;

    if SwtPackage.PreconsolidationSource = pcSpecified then
    begin
      SelectedPackageData.InitialOffset := StrInitialPreconsolida;
    end
    else
    begin
      SelectedPackageData.InitialOffset := StrInitialPreOffsets;
    end;

    SelectedPackageData.Thickness :=
      WT_Item.WaterTableCompressibleThicknessDataArrayName;

    case SwtPackage.CompressionSource of
      csCompressionReComp:
        begin
          SelectedPackageData.InitialInelasticSpecificStorage :=
            WT_Item.WaterTableRecompressionIndexDataArrayName;
          SelectedPackageData.InitialElasticSpecificStorage :=
            WT_Item.WaterTableCompressionIndexDataArrayName;
        end;
      csSpecificStorage:
        begin
          SelectedPackageData.InitialInelasticSpecificStorage :=
            WT_Item.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
          SelectedPackageData.InitialElasticSpecificStorage :=
            WT_Item.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
        end;
    end;
    SelectedPackageData.InitialPorosity := Format('%0:s / (%0:s + 1)', [
      WT_Item.WaterTableInitialVoidRatioDataArrayName]);

    CSubItem  := ModflowCSub.Values.Add as TCSubItem;
    StressPeriod := LocalModel.ModflowStressPeriods.First;
    CSubItem.StartTime := StressPeriod.StartTime;
    CSubItem.EndTime := StressPeriod.EndTime;
    CSubItem.StressOffset := '0';
      
  end;
begin
  inherited;
  LocalModel := frmGoPhast.PhastModel;
  Grid := LocalModel.ModflowGrid;
  if (Grid.RowCount = 0) or (Grid.ColumnCount = 0) then
  begin
    Exit;
  end;
  CSubPackage := LocalModel.ModflowPackages.CSubPackage;
  CSubPackage.IsSelected := True;
  SubPackage := LocalModel.ModflowPackages.SubPackage;
  SwtPackage := LocalModel.ModflowPackages.SwtPackage;
  LayerStructure := LocalModel.LayerStructure;
  if SubPackage.IsSelected then
  begin
    CSubPackage.NumberOfDelayCells := SubPackage.NumberOfNodes;

    LayerCount := 0;
    for LayerGroupIndex := 1 to LayerStructure.Count - 1 do
    begin
      ALayerGroup := LayerStructure[LayerGroupIndex];

      if ALayerGroup.SubNoDelayBedLayers.Count > 0 then
      begin

        for NoDelayIndex := 0 to ALayerGroup.SubNoDelayBedLayers.Count - 1 do
        begin
        
          ANoDelayItem := ALayerGroup.SubNoDelayBedLayers[NoDelayIndex];
          
          if ANoDelayItem.UseInAllLayers or (ALayerGroup.LayerCount = 1) then
          begin
            CreateScreenObject;
            AScreenObject.Name := GenerateNewName(
              Format(StrNoDelayInterbedss, [ALayerGroup.AquiferName]));
            AScreenObject.SetValuesOfEnclosedCells := True;
            AssignCellCorners;
            AScreenObject.LowerElevationFormula := ALayerGroup.DataArrayName;
            AScreenObject.HigherElevationFormula :=
              LayerStructure[LayerGroupIndex-1].DataArrayName;
            AssignNoDelayProperties;
          end
          else
          begin
            for SubLayerIndex := 0 to ANoDelayItem.UsedLayers.Count -1 do
            begin
              LayerNumber := LayerCount + 
                ANoDelayItem.UsedLayers[SubLayerIndex].LayerNumber;
              CreateScreenObject;
              AScreenObject.SetValuesOfEnclosedCells := True;
              AScreenObject.Name := GenerateNewName(
                Format(StrNoDelayInterbedss, [ALayerGroup.AquiferName + '_'
                  + IntToStr(LayerNumber)]));
              AssignCellCorners;
              AScreenObject.LowerElevationFormula :=
                Format('%0:s(%1:d)', [StrLayerBoundaryPosition, LayerNumber+1]);
              AScreenObject.HigherElevationFormula :=
                Format('%0:s(%1:d)', [StrLayerBoundaryPosition, LayerNumber]);
              AssignNoDelayProperties;
            end;
          end;

        end;
      end;

      if ALayerGroup.SubDelayBedLayers.Count > 0 then
      begin
        for DelayIndex := 0 to ALayerGroup.SubDelayBedLayers.Count - 1 do
        begin
          ADelayItem := ALayerGroup.SubDelayBedLayers[DelayIndex];
          if ADelayItem.UseInAllLayers or (ALayerGroup.LayerCount = 1) then
          begin
            CreateScreenObject;
            AScreenObject.Name := GenerateNewName(
              Format(StrDelayInterbed0s, [ALayerGroup.AquiferName, ADelayItem.Name]));
            AScreenObject.SetValuesOfIntersectedCells := True;
            AssignAllPoints;
            AScreenObject.LowerElevationFormula := ALayerGroup.DataArrayName;
            AScreenObject.HigherElevationFormula :=
              LayerStructure[LayerGroupIndex-1].DataArrayName;
            AssignDelayProperties;
          end
          else
          begin
            for SubLayerIndex := 0 to ADelayItem.UsedLayers.Count -1 do
            begin
              LayerNumber := LayerCount + 
                ADelayItem.UsedLayers[SubLayerIndex].LayerNumber;
              CreateScreenObject;
              AScreenObject.Name := GenerateNewName(
                Format(StrDelayInterbed0s, [ALayerGroup.AquiferName + '_'
                + IntToStr(LayerNumber), ADelayItem.Name]));
              AScreenObject.SetValuesOfIntersectedCells := True;
              AssignAllPoints;
              AScreenObject.LowerElevationFormula :=
                Format('%0:s(%1:d)', [StrLayerBoundaryPosition, LayerNumber+1]);
              AScreenObject.HigherElevationFormula :=
                Format('%0:s(%1:d)', [StrLayerBoundaryPosition, LayerNumber]);
              AssignDelayProperties;
            end;
          end;

//          AScreenObject := TScreenObject.CreateWithViewDirection(LocalModel,
//            vdTop, DummyUndoCreateScreenObject, False);
//          LocalModel.AddScreenObject(AScreenObject);
//          FScreenObjectsToDelete.Add(AScreenObject);
//          AScreenObject.ElevationCount := ecTwo;
//          AScreenObject.LowerElevationFormula := ALayerGroup.DataArrayName;
//          AScreenObject.HigherElevationFormula :=
//            LayerStructure[LayerGroupIndex-1].DataArrayName;
//          AScreenObject.Name := GenerateNewName(
//            Format(StrDelayInterbed0s, [ALayerGroup.AquiferName, ADelayItem.Name]));
//          AScreenObject.SetValuesOfIntersectedCells := True;
//
//          EquivNumberDataArray := LocalModel.DataArrayManager.
//            GetDataSetByName(ADelayItem.EquivNumberDataArrayName);
//          AScreenObject.Capacity := Grid.RowCount * Grid.ColumnCount;
//          for RowIndex := 0 to Grid.RowCount - 1 do
//          begin
//            for ColIndex := 0 to Grid.ColumnCount - 1 do
//            begin
//              if EquivNumberDataArray.RealData[0, RowIndex, ColIndex] >= 1 then
//              begin
//                AScreenObject.AddPoint(Grid.TwoDElementCenter(ColIndex,RowIndex), True);
//              end;
//            end;
//          end;
//
//          AScreenObject.CreateCSubBoundary;
//          ModflowCSub := AScreenObject.ModflowCSub;
//
//          Interbed := CSubPackage.Interbeds.Add;
//          Interbed.Name := 'CSUB_' + ADelayItem.Name;
//          Interbed.InterbedType := itNoDelay;
//
//          SelectedPackageData := GetInterbedItem(ModflowCSub, Interbed);;
//          SelectedPackageData.Used := True;
//
////          SelectedPackageData.InitialOffset :=
////            ADelayItem.PreconsolidationHeadDataArrayName;
//          SelectedPackageData.Thickness :=
//            ADelayItem.InterbedEquivalentThicknessDataArrayName;
//          SelectedPackageData.EquivInterbedNumber :=
//            ADelayItem.EquivNumberDataArrayName;
//          SelectedPackageData.InitialInelasticSpecificStorage :=
//            ADelayItem.InelasticSpecificStorageDataArrayName;
//          SelectedPackageData.InitialElasticSpecificStorage :=
//            ADelayItem.ElasticSpecificStorageDataArrayName;
////          SelectedPackageData.InitialPorosity :=
////            ADelayItem.PreconsolidationHeadDataArrayName;
//          SelectedPackageData.DelayKv :=
//            ADelayItem.VerticalHydraulicConductivityDataArrayName;
//          SelectedPackageData.InitialDelayHeadOffset :=
//            ADelayItem.InterbedStartingHeadDataArrayName;
        end;
      end;
      LayerCount := LayerCount + ALayerGroup.LayerCount;
    end;
  end;

  if SwtPackage.IsSelected then
  begin
    CSubPackage.UpdateMaterialProperties := SwtPackage.ThickResponse = trVariable;
    CSubPackage.HeadBased := SwtPackage.PreconsolidationSource = pcSpecified;
    if CSubPackage.HeadBased then
    begin
      CSubPackage.PreconsolidationHeadUsed := True;
    end;
    case SwtPackage.CompressionSource of
      csCompressionReComp:
        begin
          CSubPackage.CompressionMethod  := coRecompression;
        end;
      csSpecificStorage:
        begin
          CSubPackage.CompressionMethod  := coElasticSpecificStorage;
        end;
      else
        Assert(False);
    end;
    CSubPackage.InterbedThicknessMethod := itmThickness;
    CSubPackage.SpecifyInitialPreconsolidationStress := True;
//

    LayerCount := 0;
    for LayerGroupIndex := 1 to LayerStructure.Count - 1 do
    begin
      ALayerGroup := LayerStructure[LayerGroupIndex];
      if ALayerGroup.WaterTableLayers.Count > 0 then
      begin
//        AScreenObject := TScreenObject.CreateWithViewDirection(LocalModel,
//          vdTop, DummyUndoCreateScreenObject, False);
//        LocalModel.AddScreenObject(AScreenObject);
//        FScreenObjectsToDelete.Add(AScreenObject);
//        AScreenObject.ElevationCount := ecTwo;
//        AScreenObject.LowerElevationFormula := ALayerGroup.DataArrayName;
//        AScreenObject.HigherElevationFormula :=
//          LayerStructure[LayerGroupIndex-1].DataArrayName;
//        AScreenObject.Name := GenerateNewName(
//          Format(StrWaterTableInterbeds, [ALayerGroup.AquiferName]));
//        AScreenObject.SetValuesOfEnclosedCells := True;
//
//        AScreenObject.Capacity := 5;
//        AScreenObject.AddPoint(Grid.TwoDElementCorner(0,0), True);
//        AScreenObject.AddPoint(Grid.TwoDElementCorner(0,Grid.RowCount), False);
//        AScreenObject.AddPoint(Grid.TwoDElementCorner(Grid.ColumnCount,Grid.RowCount), False);
//        AScreenObject.AddPoint(Grid.TwoDElementCorner(Grid.ColumnCount,0), False);
//        AScreenObject.AddPoint(Grid.TwoDElementCorner(0,0), False);
//
//        AScreenObject.CreateCSubBoundary;
//        ModflowCSub := AScreenObject.ModflowCSub;

        for WT_Index := 0 to ALayerGroup.WaterTableLayers.Count - 1 do
        begin
          WT_Item :=  ALayerGroup.WaterTableLayers[WT_Index];
          if WT_Item.UseInAllLayers or (ALayerGroup.LayerCount = 1) then
          begin
            CreateScreenObject;
            AScreenObject.Name := GenerateNewName(
              Format(StrWaterTableInterbeds, [ALayerGroup.AquiferName]));
            AScreenObject.SetValuesOfEnclosedCells := True;
            AssignCellCorners;
            AScreenObject.LowerElevationFormula := ALayerGroup.DataArrayName;
            AScreenObject.HigherElevationFormula :=
              LayerStructure[LayerGroupIndex-1].DataArrayName;
            AssignSwtProperties;
          end
          else
          begin
            for SubLayerIndex := 0 to WT_Item.UsedLayers.Count -1 do
            begin
              LayerNumber := LayerCount + 
                WT_Item.UsedLayers[SubLayerIndex].LayerNumber;
              CreateScreenObject;
              AScreenObject.SetValuesOfEnclosedCells := True;
              AScreenObject.Name := GenerateNewName(
                Format(StrNoDelayInterbedss, [ALayerGroup.AquiferName + '_'
                  + IntToStr(LayerNumber)]));
              AssignCellCorners;
              AScreenObject.LowerElevationFormula :=
                Format('%0:s(%1:d)', [StrLayerBoundaryPosition, LayerNumber+1]);
              AScreenObject.HigherElevationFormula :=
                Format('%0:s(%1:d)', [StrLayerBoundaryPosition, LayerNumber]);
              AssignSwtProperties;
            end;
          end;

//          Interbed := CSubPackage.Interbeds.Add;
//          Interbed.Name := 'CSUB_' + WT_Item.Name;
//          Interbed.InterbedType := itNoDelay;
//
//          SelectedPackageData := GetInterbedItem(ModflowCSub, Interbed);;
//          SelectedPackageData.Used := True;
//
//          if SwtPackage.PreconsolidationSource = pcSpecified then
//          begin
//            SelectedPackageData.InitialOffset := StrInitialPreconsolida;
//          end
//          else
//          begin
//            SelectedPackageData.InitialOffset := StrInitialPreOffsets;
//          end;
//
//          SelectedPackageData.Thickness :=
//            WT_Item.WaterTableCompressibleThicknessDataArrayName;
////          SelectedPackageData.EquivInterbedNumber :=
////            WT_Item.EquivNumberDataArrayName;
//          case SwtPackage.CompressionSource of
//            csCompressionReComp:
//              begin
//                SelectedPackageData.InitialInelasticSpecificStorage :=
//                  WT_Item.WaterTableRecompressionIndexDataArrayName;
//                SelectedPackageData.InitialElasticSpecificStorage :=
//                  WT_Item.WaterTableCompressionIndexDataArrayName;
//              end;
//            csSpecificStorage:
//              begin
//                SelectedPackageData.InitialInelasticSpecificStorage :=
//                  WT_Item.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
//                SelectedPackageData.InitialElasticSpecificStorage :=
//                  WT_Item.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
//              end;
//          end;
//          SelectedPackageData.InitialPorosity := Format('%0:s / (%0:s + 1)', [
//            WT_Item.WaterTableInitialVoidRatioDataArrayName]);
////          SelectedPackageData.DelayKv :=
////            WT_Item.VerticalHydraulicConductivityDataArrayName;
////          SelectedPackageData.InitialDelayHeadOffset :=
////            WT_Item.InterbedStartingHeadDataArrayName;
        end;

//        CSubItem  := ModflowCSub.Values.Add as TCSubItem;
//        StressPeriod := LocalModel.ModflowStressPeriods.First;
//        CSubItem.StartTime := StressPeriod.StartTime;
//        CSubItem.EndTime := StressPeriod.EndTime;
//        CSubItem.StressOffset := '0';
      end;
      LayerCount := LayerCount + ALayerGroup.LayerCount;
    end;
  end;

  for ScreenObjectIndex := 0 to FScreenObjectsToDelete.Count - 1 do
  begin
    AScreenObject := FScreenObjectsToDelete[ScreenObjectIndex];
    AScreenObject.ModflowCSub.Loaded;
  end;
  
  LocalModel.DataArrayManager.CreateInitialDataSets;

  FNewCSubPackage := TCSubPackageSelection.Create(nil);
  FNewCSubPackage.Assign(LocalModel.ModflowPackages.CSubPackage);

  FShouldUpdateShowHideObjects := True;
  UpdateShowHideObjects;
end;

procedure TUndoConvertSubAndSwtToCSub.Redo;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowPackages.CSubPackage := FNewCSubPackage;
  FShouldUpdateShowHideObjects := True;
  ApplyDeletedStatus(False);
  UpdateShowHideObjects;
end;

procedure TUndoConvertSubAndSwtToCSub.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowPackages.CSubPackage := FOldCSubPackage;
  FShouldUpdateShowHideObjects := True;
  ApplyDeletedStatus(True);
  UpdateShowHideObjects;
end;

{ TUndoAnonymizeScreenObject }

constructor TUndoAnonymizeScreenObject.Create;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  PointStorage: TPointStorage;
begin
  inherited;
  SetPriorSelection;
  SetPostSelection;
  FOldPositions := TObjectList<TPointStorage>.Create;
  for ObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ObjectIndex];
    if (AScreenObject.ViewDirection = vdTop) and AScreenObject.Selected
      and (AScreenObject.Count = AScreenObject.SectionCount) then
    begin
      if frmGoPhast.PhastModel.ModelSelection in  SutraSelection + [msPhast] then
      begin
        if AScreenObject.EvaluatedAt = eaBlocks then
        begin
          Continue;
        end;
      end
      else
      begin
        if AScreenObject.EvaluatedAt = eaNodes then
        begin
          Continue;
        end;
      end;
      PointStorage := TPointStorage.Create;
      FOldPositions.Add(PointStorage);
      PointStorage.ScreenObject := AScreenObject;
      AScreenObject.MovePoints(PointStorage.OldLocations);
      PointStorage.NewLocations := PointStorage.OldLocations;
      SetLength(PointStorage.NewLocations, Length(PointStorage.NewLocations));
    end;
  end;
end;

function TUndoAnonymizeScreenObject.Description: string;
begin
  result := 'anonymize selected point objects';
end;

destructor TUndoAnonymizeScreenObject.Destroy;
begin
  FOldPositions.Free;
  inherited;
end;

procedure TUndoAnonymizeScreenObject.DoCommand;
var
  index: Integer;
  PointStorage: TPointStorage;
  AScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  LgrUsed: Boolean;
//  ModelIndex: Integer;
  Grid: TCustomModelGrid;
  Mesh: IMesh2D;
  PointIndex: Integer;
  APoint: TPoint2D;
  ACell: T2DTopCell;
  MeshElement: IElement2D;
  MeshNode: INode2D;
  GridIndex: Integer;
  Grids: TList;
begin
  inherited;
  Grids := TList.Create;
  try
    LgrUsed := frmGoPhast.PhastModel.LgrUsed;

    for index := 0 to FOldPositions.Count - 1 do
    begin
      PointStorage := FOldPositions[index];
      AScreenObject := PointStorage.ScreenObject;
      if LgrUsed then
      begin
        Grids.Clear;
        Grids.Add(frmGoPhast.PhastModel.Grid);
        for GridIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count -1 do
        begin
          LocalModel := frmGoPhast.PhastModel.ChildModels[GridIndex].ChildModel as TCustomModel;
          if AScreenObject.UsedModels.UsesModel(LocalModel) then
          begin
            Grids.Add(LocalModel.Grid);
          end;
        end;
      end;

      LocalModel := frmGoPhast.PhastModel;

      if LocalModel <> nil then
      begin
        Grid := LocalModel.Grid;
        if Grid <> nil then
        begin
          for PointIndex := 0 to Length(PointStorage.NewLocations) - 1 do
          begin
            APoint := PointStorage.NewLocations[PointIndex];
            if LgrUsed then
            begin
              //for GridIndex := frmGoPhast.PhastModel.LgrModel
              for GridIndex := Grids.Count-1 downto 0 do
              begin
                Grid := Grids[GridIndex];
                if Grid.InsideGrid (APoint, True) then
                begin
                  ACell := Grid.TopContainingCell(APoint, eaBlocks, True);
                  APoint := Grid.TwoDElementCenter(ACell.Col, ACell.Row);
                  PointStorage.NewLocations[PointIndex] := APoint;
                  break;
                end;
              end;
            end
            else
            begin
              APoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
              if Grid.InsideGrid (APoint, False) then
              begin
                if LocalModel.ModelSelection = msPhast then
                begin
                  ACell := Grid.TopContainingCell(APoint, eaNodes, False);
                  APoint := Grid.TwoDElementCorner(ACell.Col, ACell.Row);
                end
                else
                begin
                  ACell := Grid.TopContainingCell(APoint, eaBlocks, False);
                  APoint := Grid.TwoDElementCenter(ACell.Col, ACell.Row);
                end;
                PointStorage.NewLocations[PointIndex] := APoint;
              end;
            end;
          end;
        end
        else
        begin
          Mesh := LocalModel.Mesh3D.Mesh2DI;
          for PointIndex := 0 to Length(PointStorage.NewLocations) - 1 do
          begin
            APoint := PointStorage.NewLocations[PointIndex];
            if LocalModel.ModelSelection in SutraSelection then
            begin
              ACell := Mesh.TopContainingCellOrElement(APoint, eaNodes);
              if ACell.Col >= 0 then
              begin
                MeshNode := Mesh.NodesI2D[ACell.Col];
                APoint := MeshNode.Location;
                PointStorage.NewLocations[PointIndex] := APoint;
              end;
            end
            else
            begin
              ACell := Mesh.TopContainingCellOrElement(APoint, eaBlocks);
              if ACell.Col >= 0 then
              begin
                MeshElement := Mesh.ElementsI2D[ACell.Col];
                APoint := MeshElement.Center;
                PointStorage.NewLocations[PointIndex] := APoint;
              end;
            end;
          end;
        end;

        AScreenObject.MoveToPoints(PointStorage.NewLocations);
        UpdateScreenObject(AScreenObject);
      end;
    end;
    UpdateSelectionRectangle;
  finally
    Grids.Free;
  end;
end;

procedure TUndoAnonymizeScreenObject.Redo;
var
  index: Integer;
  PointStorage: TPointStorage;
  AScreenObject: TScreenObject;
begin
  for index := 0 to FOldPositions.Count - 1 do
  begin
    PointStorage := FOldPositions[index];
    AScreenObject := PointStorage.ScreenObject;
      AScreenObject.MoveToPoints(PointStorage.NewLocations);
    UpdateScreenObject(AScreenObject);
  end;
  UpdateSelectionRectangle;
  inherited;
end;

procedure TUndoAnonymizeScreenObject.Undo;
var
  index: Integer;
  PointStorage: TPointStorage;
  AScreenObject: TScreenObject;
begin
  inherited;
  for index := 0 to FOldPositions.Count - 1 do
  begin
    PointStorage := FOldPositions[index];
    AScreenObject := PointStorage.ScreenObject;
      AScreenObject.MoveToPoints(PointStorage.OldLocations);
    UpdateScreenObject(AScreenObject);
  end;
  UpdateSelectionRectangle
end;

{ TUndoConvertFhbToMf6 }

constructor TUndoConvertFhbToMf6.Create{(const AListOfScreenObjects: TList;
  var NewScreenObjects, OldScreenObjects: TScreenObjectEditCollection;
  var OldChildModelScreenObjects: TList)};
var
  Model: TPhastModel;
  ModflowPackages: TModflowPackages;
  NewScreenObjects: TScreenObjectEditCollection;
  OldScreenObjects: TScreenObjectEditCollection;
  AListOfScreenObjects: TList;
  OldChildModelScreenObjects: TList;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
//  OldScreenObject: TScreenObject;
  Item: TScreenObjectEditItem;
  HeadParam: TModflowTransientListParameter;
  FlowParam: TModflowTransientListParameter;
  ChdBoundary: TChdBoundary;
  ParamItem: TModflowParamItem;
  TimeIndex: Integer;
  ModflowFhbHeadBoundary: TFhbHeadBoundary;
  HeadItem: TFhbItem;
  ChdItem: TChdItem;
  PriorChdItem: TChdItem;
  WellBoundary: TMfWellBoundary;
  FhbFlowBoundary: TFhbFlowBoundary;
  FlowItem: TFhbItem;
  WellItem: TWellItem;
  PriorWellItem: TWellItem;
  NewChobObservations: TFluxObservationGroups;
  NewDrobObservations: TFluxObservationGroups;
  NewGbobObservations: TFluxObservationGroups;
  NewRvobObservations: TFluxObservationGroups;
  NewStobObservations: TFluxObservationGroups;
  NewMtsdObs: TMassFluxObs;
begin
  Model := frmGoPhast.PhastModel;
  ModflowPackages := Model.ModflowPackages;
  FOldChdUsed := ModflowPackages.ChdBoundary.IsSelected;
  FOldWellUsed := ModflowPackages.WelPackage.IsSelected;
  NewScreenObjects := TScreenObjectEditCollection.Create;
  OldScreenObjects := TScreenObjectEditCollection.Create;
  AListOfScreenObjects := TList.Create;
  OldChildModelScreenObjects := TList.Create;
  try
    NewScreenObjects.OwnScreenObject := True;
    OldScreenObjects.OwnScreenObject := True;
    HeadParam := nil;
    FlowParam := nil;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if (not AScreenObject.Deleted) then
      begin
        if ((AScreenObject.ModflowFhbHeadBoundary <> nil)
            and AScreenObject.ModflowFhbHeadBoundary.Used)
          or ((AScreenObject.ModflowFhbFlowBoundary <> nil)
            and AScreenObject.ModflowFhbFlowBoundary.Used) then
        begin
          AListOfScreenObjects.Add(AScreenObject);
          Item := OldScreenObjects.Add;
          Item.ScreenObject := TScreenObject.Create(nil);
          Item.ScreenObject.Assign(AScreenObject);

          Item := NewScreenObjects.Add;
          Item.ScreenObject := TScreenObject.Create(nil);
          Item.ScreenObject.Assign(AScreenObject);

          if (AScreenObject.ModflowFhbHeadBoundary <> nil)
            and AScreenObject.ModflowFhbHeadBoundary.Used then
          begin
            FNewChdUsed := True;
            ModflowFhbHeadBoundary := AScreenObject.ModflowFhbHeadBoundary;
            if HeadParam = nil then
            begin
              HeadParam := Model.ModflowTransientParameters.Add;
              HeadParam.ParameterType := ptCHD;
              HeadParam.Value := 1;
              HeadParam.ParameterName := 'FHB_Head';
            end;
            Item.ScreenObject.CreateChdBoundary;
            ChdBoundary := Item.ScreenObject.ModflowChdBoundary;
            ChdBoundary.Interp := mimLinearEnd;
            ParamItem := ChdBoundary.Parameters.Add;
            ParamItem.Param.Param := HeadParam;
            PriorChdItem := nil;
            for TimeIndex := 0 to ModflowFhbHeadBoundary.Values.Count - 1 do
            begin
              HeadItem := ModflowFhbHeadBoundary.Values[TimeIndex] as TFhbItem;
              ChdItem := ParamItem.Param.Add as TChdItem;
              ChdItem.StartTime := HeadItem.StartTime;
              ChdItem.EndTime := HeadItem.EndTime;
              ChdItem.StartHead := HeadItem.BoundaryValue;
              ChdItem.EndHead := HeadItem.BoundaryValue;
              if PriorChdItem <> nil then
              begin
                PriorChdItem.EndTime := HeadItem.StartTime;
                PriorChdItem.EndHead := HeadItem.BoundaryValue;
              end;
              PriorChdItem := ChdItem;
            end;
            PriorChdItem.EndTime := PriorChdItem.EndTime + 1;
          end;
          if (AScreenObject.ModflowFhbFlowBoundary <> nil)
            and AScreenObject.ModflowFhbFlowBoundary.Used then
          begin
            FNewWellUsed := True;
            FhbFlowBoundary := AScreenObject.ModflowFhbFlowBoundary;
            if FlowParam = nil then
            begin
              FlowParam := Model.ModflowTransientParameters.Add;
              FlowParam.ParameterType := ptQ;
              FlowParam.Value := 1;
              FlowParam.ParameterName := 'FHB_Flow';
            end;
            Item.ScreenObject.CreateWelBoundary;
            WellBoundary := Item.ScreenObject.ModflowWellBoundary;
            WellBoundary.Interp := mimLinear;
            ParamItem := WellBoundary.Parameters.Add;
            ParamItem.Param.Param := FlowParam;
            PriorWellItem := nil;
            for TimeIndex := 0 to FhbFlowBoundary.Values.Count - 1 do
            begin
              FlowItem := FhbFlowBoundary.Values[TimeIndex] as TFhbItem;
              WellItem := ParamItem.Param.Add as TWellItem;
              WellItem.StartTime := FlowItem.StartTime;
              WellItem.EndTime := FlowItem.EndTime;
              WellItem.PumpingRate := FlowItem.BoundaryValue;
              if PriorWellItem <> nil then
              begin
                PriorWellItem.EndTime := FlowItem.StartTime;
              end;
              PriorWellItem := WellItem;
            end;
            PriorWellItem.EndTime := PriorWellItem.EndTime + 1;
          end;
        end;
      end;
    end;
    inherited Create(AListOfScreenObjects, NewScreenObjects, OldScreenObjects, OldChildModelScreenObjects);

    NewChobObservations := TFluxObservationGroups.Create(nil);
    NewDrobObservations := TFluxObservationGroups.Create(nil);
    NewGbobObservations := TFluxObservationGroups.Create(nil);
    NewRvobObservations := TFluxObservationGroups.Create(nil);
    NewStobObservations := TFluxObservationGroups.Create(nil);
    try
      NewChobObservations.Assign(frmGoPhast.PhastModel.HeadFluxObservations);
      NewDrobObservations.Assign(frmGoPhast.PhastModel.DrainObservations);
      NewGbobObservations.Assign(frmGoPhast.PhastModel.GhbObservations);
      NewRvobObservations.Assign(frmGoPhast.PhastModel.RiverObservations);
      NewStobObservations.Assign(frmGoPhast.PhastModel.StreamObservations);
      FUndoEditFluxObservations.FillMt3dLists(NewMtsdObs);

      FUndoEditFluxObservations.AssignNewObservations(NewChobObservations, NewDrobObservations,
        NewGbobObservations, NewRvobObservations,
        NewStobObservations, NewMtsdObs);
    finally
      NewStobObservations.Free;
      NewRvobObservations.Free;
      NewGbobObservations.Free;
      NewDrobObservations.Free;
      NewChobObservations.Free;
    end;

  finally
    NewScreenObjects.Free;
    OldScreenObjects.Free;
    AListOfScreenObjects.Free;
    OldChildModelScreenObjects.Free;
  end;
end;

function TUndoConvertFhbToMf6.Description: string;
begin
  result := 'Convert FHB to CHD and WEL';
end;

destructor TUndoConvertFhbToMf6.Destroy;
begin

  inherited;
end;

procedure TUndoConvertFhbToMf6.DoCommand;
var
  ModflowPackages: TModflowPackages;
begin
  inherited;
  ModflowPackages := frmGoPhast.PhastModel.ModflowPackages;
  ModflowPackages.ChdBoundary.IsSelected := FNewChdUsed;
  ModflowPackages.WelPackage.IsSelected := FNewWellUsed;
end;

procedure TUndoConvertFhbToMf6.Undo;
var
  ModflowPackages: TModflowPackages;
begin
  inherited;
  ModflowPackages := frmGoPhast.PhastModel.ModflowPackages;
  ModflowPackages.ChdBoundary.IsSelected := FOldChdUsed;
  ModflowPackages.WelPackage.IsSelected := FOldWellUsed;

end;

//{ TUndoConvertChd }
//
//constructor TUndoConvertChd.Create;
//var
//  ScreenObjectIndex: Integer;
//  AScreenObject: TScreenObject;
//  AChdBoundary: TChdBoundary;
//begin
//  inherited Create;
//  FScreenObjects := TScreenObjectList.Create;
//  FChdBoundaries:= TObjectList<TChdBoundary>.Create;
//  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
//  begin
//    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
//    if (not AScreenObject.Deleted)
//      and (AScreenObject.ModflowChdBoundary <> nil) then
//    begin
//      if AScreenObject.ModflowChdBoundary.Used
//        and (AScreenObject.ModflowChdBoundary.Values.Count > 0) then
//      begin
//        FScreenObjects.Add(AScreenObject);
//        AChdBoundary := TChdBoundary.Create(nil, nil);
//        AChdBoundary.Assign(AScreenObject.ModflowChdBoundary);
//        FChdBoundaries.Add(AChdBoundary);
//      end;
//    end;
//  end;
//end;
//
//function TUndoConvertChd.Description: string;
//begin
//
//end;
//
//destructor TUndoConvertChd.Destroy;
//begin
//  FScreenObjects.Free;
//  FChdBoundaries.Free;
//  inherited;
//end;
//
//procedure TUndoConvertChd.DoCommand;
//const
//  Root = 'CHD_MF6';
//var
//  ScreenObjectIndex: Integer;
//  AScreenObject: TScreenObject;
//  ParamItem: TModflowParamItem;
//  ModflowTransientParameters: TModflowTransientListParameters;
//  Index: Integer;
//  NewName: string;
//begin
//  inherited;
//  ModflowTransientParameters := frmGoPhast.PhastModel.ModflowTransientParameters;
//  NewName := Root;
//  Index := 1;
//  FNewChdParam := ModflowTransientParameters.GetParamByName(NewName);
//  while FNewChdParam <> nil do
//  begin
//    NewName := Format('%0:s_%1:d',[Root, Index]);
//    Inc(Index);
//    FNewChdParam := ModflowTransientParameters.GetParamByName(NewName);
//  end;
//  FNewChdParam := ModflowTransientParameters.Add;
//  FNewChdParam.Value := 1;
//  FNewChdParam.ParameterName := NewName;
//  FNewChdParam.ParameterType := ptCHD;
//  for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
//  begin
//    AScreenObject := FScreenObjects[ScreenObjectIndex];
//    ParamItem := AScreenObject.ModflowChdBoundary.Parameters.Add;
//    ParamItem.Param.Assign(AScreenObject.ModflowChdBoundary.Values);
//    ParamItem.Param.Param := FNewChdParam;
//    AScreenObject.ModflowChdBoundary.Values.Clear;
//  end;
//end;
//
//function TUndoConvertChd.GetShouldConvert: Boolean;
//begin
//  result := (FScreenObjects.Count > 0)
//end;
//
//procedure TUndoConvertChd.Undo;
//var
//  ScreenObjectIndex: Integer;
//  AScreenObject: TScreenObject;
//begin
//  for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
//  begin
//    AScreenObject := FScreenObjects[ScreenObjectIndex];
//    AScreenObject.ModflowChdBoundary := FChdBoundaries[ScreenObjectIndex];
//  end;
//  FreeAndNil(FNewChdParam);
//  inherited;
//end;
//
end.


