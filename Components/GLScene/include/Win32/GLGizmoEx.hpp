// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGizmoEx.pas' rev: 36.00 (Windows)

#ifndef GlgizmoexHPP
#define GlgizmoexHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLScene.hpp>
#include <GLColor.hpp>
#include <GLObjects.hpp>
#include <GLVectorGeometry.hpp>
#include <GLMaterial.hpp>
#include <GLStrings.hpp>
#include <GLGeomObjects.hpp>
#include <GLBitmapFont.hpp>
#include <GLWin32Viewer.hpp>
#include <GLVectorFileObjects.hpp>
#include <GLCoordinates.hpp>
#include <GLRenderContextInfo.hpp>
#include <GLGeometryBB.hpp>
#include <GLVectorTypes.hpp>
#include <GLCanvas.hpp>
#include <GLPersistentClasses.hpp>
#include <GLScreen.hpp>
#include <GLState.hpp>
#include <GLSelection.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glgizmoex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGizmoExObjectItem;
class DELPHICLASS TGLGizmoExObjectCollection;
class DELPHICLASS TGLGizmoExActionHistoryItem;
class DELPHICLASS TGLGizmoExActionHistoryCollection;
class DELPHICLASS TGLGizmoExUIFrustrum;
class DELPHICLASS TGLGizmoExUISphere;
class DELPHICLASS TGLGizmoExUIDisk;
class DELPHICLASS TGLGizmoExUITorus;
class DELPHICLASS TGLGizmoExUIPolygon;
class DELPHICLASS TGLGizmoExUIArrowLine;
class DELPHICLASS TGLGizmoExUILines;
class DELPHICLASS TGLGizmoExUIFlatText;
class DELPHICLASS TGLGizmoEx;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExObjectItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	Glvectorgeometry::TVector FOldAutoScaling;
	Glscene::TGLBaseSceneObject* FEffectedObject;
	Glscene::TGLBaseSceneObject* FParentOldObject;
	int FIndexOldObject;
	System::UnicodeString FNameOldObject;
	bool FReturnObject;
	Glvectorgeometry::TMatrix FOldMatrix;
	Glscene::TGLBaseSceneObject* FGizmoTmpRoot;
	void __fastcall SetEffectedObject(Glscene::TGLBaseSceneObject* const Value);
	void __fastcall SetOldMatrix(const Glvectorgeometry::TMatrix &Value);
	
protected:
	void __fastcall DoUndo();
	TGLGizmoExObjectCollection* __fastcall GetParent();
	TGLGizmoEx* __fastcall GetGizmo();
	
public:
	__property Glscene::TGLBaseSceneObject* GizmoTmpRoot = {read=FGizmoTmpRoot, write=FGizmoTmpRoot};
	__fastcall virtual TGLGizmoExObjectItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoExObjectItem();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall AssignFromObject(Glscene::TGLBaseSceneObject* const AObject, bool AssignAndRemoveObj = false);
	__property Glvectorgeometry::TMatrix OldMatrix = {read=FOldMatrix, write=SetOldMatrix};
	
__published:
	__property Glscene::TGLBaseSceneObject* EffectedObject = {read=FEffectedObject, write=SetEffectedObject};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExObjectCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoExObjectItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	TGLGizmoExObjectItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoExObjectItem* const Value);
	
protected:
	TGLGizmoEx* __fastcall GetParent();
	void __fastcall DoUndo();
	
public:
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RemoveByObject(Glscene::TGLCustomSceneObject* const AObject);
	HIDESBASE TGLGizmoExObjectItem* __fastcall Add();
	__property TGLGizmoExObjectItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLGizmoExObjectCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoExObjectCollection() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExActionHistoryItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::TObject* FObject;
	TGLGizmoExObjectCollection* FGizmoObjectCollection;
	void __fastcall SetObject(System::TObject* aValue);
	void __fastcall SetGizmoObjectCollection(TGLGizmoExObjectCollection* aValue);
	
public:
	__fastcall virtual TGLGizmoExActionHistoryItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoExActionHistoryItem();
	__property System::TObject* BaseObject = {read=FObject, write=SetObject};
	__property TGLGizmoExObjectCollection* GizmoObjectCollection = {read=FGizmoObjectCollection, write=SetGizmoObjectCollection};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoExActionHistoryCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoExActionHistoryItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	int FItemIndex;
	int FItemsMaxCount;
	Glscene::TGLBaseSceneObject* FGizmoTmpRoot;
	TGLGizmoExActionHistoryItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoExActionHistoryItem* const Value);
	HIDESBASE TGLGizmoExActionHistoryItem* __fastcall Add();
	
public:
	__fastcall TGLGizmoExActionHistoryCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass);
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property int ItemIndex = {read=FItemIndex, write=FItemIndex, nodefault};
	TGLGizmoExActionHistoryItem* __fastcall Undo();
	TGLGizmoExActionHistoryItem* __fastcall Redo();
	void __fastcall AddObjects(Glselection::TGLPickList* objs);
	void __fastcall AddObject(System::TObject* obj);
	void __fastcall RemoveObjects(Glselection::TGLPickList* objs);
	__property int MaxCount = {read=FItemsMaxCount, write=FItemsMaxCount, nodefault};
	__property TGLGizmoExActionHistoryItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
	__property Glscene::TGLBaseSceneObject* GizmoTmpRoot = {read=FGizmoTmpRoot, write=FGizmoTmpRoot};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoExActionHistoryCollection() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLGizmoExVisibleInfoLabel : unsigned char { vliName, vliOperation, vliCoords };

typedef System::Set<TGLGizmoExVisibleInfoLabel, TGLGizmoExVisibleInfoLabel::vliName, TGLGizmoExVisibleInfoLabel::vliCoords> TGLGizmoExVisibleInfoLabels;

enum DECLSPEC_DENUM TInfoLabelCoordType : unsigned char { ilcChanging, ilcChangeRate };

enum DECLSPEC_DENUM TGLGizmoExAxis : unsigned char { gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ, gaXYZ };

enum DECLSPEC_DENUM TGLGizmoExSelectionRegion : unsigned char { gsrRectangular, gsrCircular, gsrFence, gsrLasso };

enum DECLSPEC_DENUM TGLGizmoExReferenceCoordinateSystem : unsigned char { rcsView, rcsLocal };

typedef System::DynamicArray<System::Types::TPoint> TGLGizmoExSelRec;

enum DECLSPEC_DENUM TGLGizmoExOperation : unsigned char { gopMove, gopRotate, gopScale, gopNone };

enum DECLSPEC_DENUM TGLGizmoExOperationMode : unsigned char { gomNone, gomSelect, gomMove, gomRotate, gomScale };

typedef void __fastcall (__closure *TGLGizmoExAcceptEvent)(System::TObject* Sender, Glselection::TGLPickList* &objs);

typedef void __fastcall (__closure *TGLGizmoExAxisSelected)(System::TObject* Sender, TGLGizmoExAxis &Axis);

enum DECLSPEC_DENUM TGLGizmoExPickMode : unsigned char { pmGetPickedObjects, pmRayCast };

class PASCALIMPLEMENTATION TGLGizmoExUIFrustrum : public Glgeomobjects::TGLFrustrum
{
	typedef Glgeomobjects::TGLFrustrum inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIFrustrum(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUIFrustrum() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIFrustrum(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLFrustrum(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUISphere : public Globjects::TGLSphere
{
	typedef Globjects::TGLSphere inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUISphere(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUISphere() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUISphere(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLSphere(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIDisk : public Glgeomobjects::TGLDisk
{
	typedef Glgeomobjects::TGLDisk inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIDisk(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUIDisk() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIDisk(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLDisk(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUITorus : public Glgeomobjects::TGLTorus
{
	typedef Glgeomobjects::TGLTorus inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUITorus(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUITorus() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUITorus(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLTorus(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIPolygon : public Glgeomobjects::TGLPolygon
{
	typedef Glgeomobjects::TGLPolygon inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIPolygon(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLPolygon.Destroy */ inline __fastcall virtual ~TGLGizmoExUIPolygon() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIPolygon(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLPolygon(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIArrowLine : public Glgeomobjects::TGLArrowLine
{
	typedef Glgeomobjects::TGLArrowLine inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIArrowLine(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoExUIArrowLine() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIArrowLine(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLArrowLine(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUILines : public Globjects::TGLLines
{
	typedef Globjects::TGLLines inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUILines(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLLines.Destroy */ inline __fastcall virtual ~TGLGizmoExUILines() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUILines(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLLines(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoExUIFlatText : public Glbitmapfont::TGLFlatText
{
	typedef Glbitmapfont::TGLFlatText inherited;
	
private:
	bool FNoZWrite;
	
public:
	__fastcall virtual TGLGizmoExUIFlatText(System::Classes::TComponent* AOwner);
	virtual void __fastcall BuildList(Glrendercontextinfo::TGLRenderContextInfo &rci);
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
public:
	/* TGLFlatText.Destroy */ inline __fastcall virtual ~TGLGizmoExUIFlatText() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoExUIFlatText(Glscene::TGLBaseSceneObject* aParentOwner) : Glbitmapfont::TGLFlatText(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoEx : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Glscene::TGLBaseSceneObject* FUIBaseGizmo;
	Glscene::TGLBaseSceneObject* FUIRootHelpers;
	Glscene::TGLBaseSceneObject* FUIRootSelect;
	Glscene::TGLBaseSceneObject* FUIRootMovement;
	Glscene::TGLBaseSceneObject* FUIRootRotate;
	Glscene::TGLBaseSceneObject* FUIRootRotateAxisLabel;
	Glscene::TGLBaseSceneObject* FUIRootScale;
	Glscene::TGLBaseSceneObject* FUIRootAxisLabel;
	Glscene::TGLBaseSceneObject* FUIRootVisibleInfoLabels;
	Glscene::TGLDirectOpenGL* FInterfaceRender;
	Glscene::TGLDirectOpenGL* FInternalRender;
	TGLGizmoExUILines* FUISelectLineX;
	TGLGizmoExUILines* FUISelectLineY;
	TGLGizmoExUILines* FUISelectLineZ;
	TGLGizmoExUIFrustrum* FUIICMovementLineX;
	TGLGizmoExUIFrustrum* FUIICMovementLineY;
	TGLGizmoExUIFrustrum* FUIICMovementLineZ;
	TGLGizmoExUIFrustrum* FUIICMovementLineXY;
	TGLGizmoExUIFrustrum* FUIICMovementLineXZ;
	TGLGizmoExUIFrustrum* FUIICMovementLineYZ;
	TGLGizmoExUIArrowLine* FUIMovementArrowX;
	TGLGizmoExUIArrowLine* FUIMovementArrowY;
	TGLGizmoExUIArrowLine* FUIMovementArrowZ;
	TGLGizmoExUILines* FUIMovementLineX;
	TGLGizmoExUILines* FUIMovementLineY;
	TGLGizmoExUILines* FUIMovementLineZ;
	TGLGizmoExUILines* FUIMovementLineXY;
	TGLGizmoExUILines* FUIMovementLineXZ;
	TGLGizmoExUILines* FUIMovementLineYZ;
	TGLGizmoExUIPolygon* FUIMovementPlaneXY;
	TGLGizmoExUIPolygon* FUIMovementPlaneXZ;
	TGLGizmoExUIPolygon* FUIMovementPlaneYZ;
	TGLGizmoExUILines* FUIRotateLineX;
	TGLGizmoExUILines* FUIRotateLineY;
	TGLGizmoExUILines* FUIRotateLineZ;
	TGLGizmoExUILines* FUIRotateLineXY;
	TGLGizmoExUILines* FUIRotateLineXZ;
	TGLGizmoExUITorus* FUIICRotateTorusX;
	TGLGizmoExUITorus* FUIICRotateTorusY;
	TGLGizmoExUITorus* FUIICRotateTorusZ;
	TGLGizmoExUITorus* FUIICRotateTorusXZ;
	TGLGizmoExUIDisk* FUIRotateDiskXY;
	TGLGizmoExUIDisk* FUIRotateDiskX;
	TGLGizmoExUIDisk* FUIRotateDiskX2;
	TGLGizmoExUIDisk* FUIRotateDiskY;
	TGLGizmoExUIDisk* FUIRotateDiskY2;
	TGLGizmoExUIDisk* FUIRotateDiskZ;
	TGLGizmoExUIDisk* FUIRotateDiskZ2;
	TGLGizmoExUILines* FUIRotateLineArrowX;
	TGLGizmoExUILines* FUIRotateLineArrowY;
	TGLGizmoExUILines* FUIRotateLineArrowZ;
	TGLGizmoExUISphere* FUIICRotateSphereXY;
	TGLGizmoExUIFlatText* FUIRotateAxisLabelX;
	TGLGizmoExUIFlatText* FUIRotateAxisLabelY;
	TGLGizmoExUIFlatText* FUIRotateAxisLabelZ;
	TGLGizmoExUISphere* FUIScaleArrowX;
	TGLGizmoExUISphere* FUIScaleArrowY;
	TGLGizmoExUISphere* FUIScaleArrowZ;
	TGLGizmoExUILines* FUIScaleLineX;
	TGLGizmoExUILines* FUIScaleLineY;
	TGLGizmoExUILines* FUIScaleLineZ;
	TGLGizmoExUILines* FUIScaleLineXY;
	TGLGizmoExUILines* FUIScaleLineYZ;
	TGLGizmoExUILines* FUIScaleLineXZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineX;
	TGLGizmoExUIFrustrum* FUIICScaleLineY;
	TGLGizmoExUIFrustrum* FUIICScaleLineZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineXY;
	TGLGizmoExUIFrustrum* FUIICScaleLineXZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineYZ;
	TGLGizmoExUIFrustrum* FUIICScaleLineXYZ;
	TGLGizmoExUIPolygon* FUIScalePlaneXY;
	TGLGizmoExUIPolygon* FUIScalePlaneXZ;
	TGLGizmoExUIPolygon* FUIScalePlaneYZ;
	TGLGizmoExUIPolygon* FUIScalePlaneXYZ;
	TGLGizmoExUIFlatText* FUIAxisLabelX;
	TGLGizmoExUIFlatText* FUIAxisLabelY;
	TGLGizmoExUIFlatText* FUIAxisLabelZ;
	TGLGizmoExUIFlatText* FUIVisibleInfoLabels;
	Glscene::TGLBaseSceneObject* FRootGizmo;
	Glscene::TGLBaseSceneObject* FRootObjects;
	Glscene::TGLBaseSceneObject* FGizmoTmpRoot;
	Glscene::TGLBaseSceneObject* FSelectedObj;
	TGLGizmoExOperation FOperation;
	TGLGizmoExOperationMode FOperationMode;
	TGLGizmoExAxis FSelAxis;
	TInfoLabelCoordType fInfoLabelCoordType;
	TGLGizmoExReferenceCoordinateSystem FReferenceCoordSystem;
	Glcolor::TGLColor* FBoundingBoxColor;
	Glcolor::TGLColor* FSelectedColor;
	Glcolor::TGLColor* FVisibleInfoLabelsColor;
	Glcolor::TGLColor* FSelectionRegionColor;
	bool FVisibleInfoLabelsColorChanged;
	bool FAutoZoom;
	bool FExcludeObjects;
	bool FExcludeClassname;
	bool FNoZWrite;
	bool FEnabled;
	float FAutoZoomFactor;
	float FZoomFactor;
	float FMoveCoef;
	float FRotationCoef;
	Glwin32viewer::TGLSceneViewer* FViewer;
	TGLGizmoExVisibleInfoLabels FVisibleVisibleInfoLabels;
	System::Classes::TStrings* FExcludeObjectsList;
	System::Classes::TStrings* FExcludeClassNameList;
	TGLGizmoExSelectionRegion FSelectionRegion;
	bool FEnableMultiSelection;
	bool FShowMultiSelecting;
	TGLGizmoExSelRec FSelectionRec;
	bool FCanAddObjToSelectionList;
	bool FCanRemoveObjFromSelectionList;
	Glselection::TGLPickList* FSelectedObjects;
	bool FAntiAliasedLines;
	bool FShowAxisLabel;
	bool FShowObjectInfos;
	bool FShowBoundingBox;
	bool FCanChangeWithChildren;
	bool moving;
	int mx;
	int my;
	System::Types::TPoint fCursorPos;
	System::Types::TPoint fLastCursorPos;
	Glvectorgeometry::TAffineVector fChangeRate;
	bool FEnableLoopCursorMoving;
	Glvectorgeometry::TVector lastMousePos;
	System::Classes::TNotifyEvent FOnUpdate;
	TGLGizmoExAcceptEvent FOnSelect;
	System::Classes::TNotifyEvent FOnOperationChange;
	System::Classes::TNotifyEvent FOnOperationModeChange;
	System::Classes::TNotifyEvent FOnSelectionLost;
	TGLGizmoExAxisSelected FOnAxisSelected;
	float FScaleCoef;
	float FGizmoThickness;
	TGLGizmoExPickMode FPickMode;
	bool FEnableHistory;
	TGLGizmoExActionHistoryCollection* FHistory;
	int FHistoryStepsCount;
	Glbitmapfont::TGLCustomBitmapFont* FLabelFont;
	void __fastcall SetRootGizmo(Glscene::TGLBaseSceneObject* const AValue);
	void __fastcall SetRootObjects(Glscene::TGLBaseSceneObject* const AValue);
	void __fastcall SetGizmoTmpRoot(Glscene::TGLBaseSceneObject* const AValue);
	void __fastcall SeTGLGizmoExVisibleInfoLabels(const TGLGizmoExVisibleInfoLabels AValue);
	void __fastcall SetBoundingBoxColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetSelectedColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetVisibleInfoLabelsColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetSelectionRegionColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetCanChangeWithChildren(bool AValue);
	void __fastcall SetAALines(bool aValue);
	void __fastcall SetInfoLabelCoordType(TInfoLabelCoordType aValue);
	void __fastcall SetReferenceCoordSystem(TGLGizmoExReferenceCoordinateSystem aValue);
	void __fastcall SetHistoryStepsCount(int aValue);
	void __fastcall SetExcludeObjectsList(System::Classes::TStrings* const AValue);
	void __fastcall SetExcludeClassNameList(System::Classes::TStrings* const AValue);
	Glvectorgeometry::TVector __fastcall MouseWorldPos(const int X, const int Y);
	bool __fastcall CheckObjectInExcludeList(Glscene::TGLBaseSceneObject* const Obj);
	bool __fastcall CheckClassNameInExcludeList(Glscene::TGLBaseSceneObject* const Obj);
	void __fastcall UpdateVisibleInfoLabels();
	void __fastcall SetGLGizmoExThickness(const float Value);
	void __fastcall ActivatingElements(Glselection::TGLPickList* PickList);
	void __fastcall InterfaceRender(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &rci);
	void __fastcall InternalRender(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &rci);
	Glselection::TGLPickList* __fastcall InternalGetPickedObjects(const int x1, const int y1, const int x2, const int y2, const int guessCount = 0x8);
	void __fastcall SetViewer(Glwin32viewer::TGLSceneViewer* const Value);
	void __fastcall SetLabelFont(Glbitmapfont::TGLCustomBitmapFont* const Value);
	void __fastcall SetSelectedObj(Glscene::TGLBaseSceneObject* const Value);
	Glscene::TGLBaseSceneObject* __fastcall GetSelectedObj();
	void __fastcall SetNoZWrite(const bool Value);
	void __fastcall SetOperation(const TGLGizmoExOperation Value);
	void __fastcall SetOperationMode(const TGLGizmoExOperationMode Value);
	void __fastcall SetAngleDisk(float aAngle);
	void __fastcall SetEnableLoopCursorMoving(const bool AValue);
	void __fastcall SetEnableMultiSelection(const bool AValue);
	void __fastcall SetSelectionRegion(const TGLGizmoExSelectionRegion AValue);
	void __fastcall SetShowAxisLabel(const bool AValue);
	void __fastcall SetShowObjectInfos(const bool AValue);
	void __fastcall SetShowBoundingBox(const bool AValue);
	void __fastcall SetAutoZoomFactor(const float AValue);
	void __fastcall SetZoomFactor(const float AValue);
	void __fastcall SetSelAxis(TGLGizmoExAxis aValue);
	void __fastcall SetPickMode(TGLGizmoExPickMode APickMode);
	void __fastcall AssignPickList(Glselection::TGLPickList* aList, bool RemoveObj = false);
	void __fastcall AddObjToSelectionList(Glscene::TGLBaseSceneObject* Obj);
	void __fastcall RemoveObjFromSelectionList(Glscene::TGLBaseSceneObject* Obj);
	void __fastcall MultiSelMouseDown(int X, int Y);
	void __fastcall MultiSelMouseUp(int X, int Y);
	void __fastcall MultiSelMouseMove(int X, int Y);
	Glselection::TGLPickList* __fastcall GetPickList();
	void __fastcall SetPickList(Glselection::TGLPickList* aValue);
	__property TGLGizmoExAxis SelAxis = {read=FSelAxis, write=SetSelAxis, nodefault};
	__property TGLGizmoExOperation Operation = {read=FOperation, write=SetOperation, nodefault};
	void __fastcall ClearSelection();
	void __fastcall SetVisible(const bool AValue);
	bool __fastcall GetVisible();
	
public:
	__fastcall virtual TGLGizmoEx(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGizmoEx();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ViewerMouseMove(const int X, const int Y);
	void __fastcall ViewerMouseDown(const int X, const int Y);
	void __fastcall ViewerMouseUp(const int X, const int Y);
	void __fastcall UpdateGizmo()/* overload */;
	virtual void __fastcall LooseSelection();
	void __fastcall UndoAdd(System::TObject* const AObject);
	void __fastcall RemoveSelectedObjects();
	TGLGizmoExActionHistoryItem* __fastcall Undo();
	TGLGizmoExActionHistoryItem* __fastcall Redo();
	__property bool CanAddObjToSelectionList = {read=FCanAddObjToSelectionList, write=FCanAddObjToSelectionList, nodefault};
	__property bool CanRemoveObjFromSelectionList = {read=FCanRemoveObjFromSelectionList, write=FCanRemoveObjFromSelectionList, nodefault};
	void __fastcall LooseCursorSelection();
	__property bool CursorSelectingRegion = {read=FShowMultiSelecting, nodefault};
	__property Glscene::TGLBaseSceneObject* RootObjects = {read=FRootObjects, write=SetRootObjects};
	__property Glscene::TGLBaseSceneObject* RootGizmo = {read=FRootGizmo, write=SetRootGizmo};
	__property Glscene::TGLBaseSceneObject* GizmoTmpRoot = {read=FGizmoTmpRoot, write=SetGizmoTmpRoot};
	
__published:
	__property Glwin32viewer::TGLSceneViewer* Viewer = {read=FViewer, write=SetViewer};
	__property Glcolor::TGLColor* BoundingBoxColor = {read=FBoundingBoxColor, write=SetBoundingBoxColor};
	__property Glcolor::TGLColor* SelectedColor = {read=FSelectedColor, write=SetSelectedColor};
	__property Glcolor::TGLColor* SelectionRegionColor = {read=FSelectionRegionColor, write=SetSelectionRegionColor};
	__property Glscene::TGLBaseSceneObject* SelectedObj = {read=GetSelectedObj, write=SetSelectedObj};
	__property Glselection::TGLPickList* SelectedObjects = {read=GetPickList, write=SetPickList};
	__property TGLGizmoExOperationMode OperationMode = {read=FOperationMode, write=SetOperationMode, default=1};
	__property bool ExcludeObjects = {read=FExcludeObjects, write=FExcludeObjects, nodefault};
	__property System::Classes::TStrings* ExcludeObjectsList = {read=FExcludeObjectsList, write=SetExcludeObjectsList};
	__property bool ExcludeClassname = {read=FExcludeClassname, write=FExcludeClassname, nodefault};
	__property System::Classes::TStrings* ExcludeClassnameList = {read=FExcludeClassNameList, write=SetExcludeClassNameList};
	__property TGLGizmoExVisibleInfoLabels VisibleInfoLabels = {read=FVisibleVisibleInfoLabels, write=SeTGLGizmoExVisibleInfoLabels, nodefault};
	__property Glcolor::TGLColor* VisibleInfoLabelsColor = {read=FVisibleInfoLabelsColor, write=SetVisibleInfoLabelsColor};
	__property bool AutoZoom = {read=FAutoZoom, write=FAutoZoom, default=1};
	__property float AutoZoomFactor = {read=FAutoZoomFactor, write=SetAutoZoomFactor};
	__property float ZoomFactor = {read=FZoomFactor, write=SetZoomFactor};
	__property float MoveCoef = {read=FMoveCoef, write=FMoveCoef};
	__property float RotationCoef = {read=FRotationCoef, write=FRotationCoef};
	__property float ScaleCoef = {read=FScaleCoef, write=FScaleCoef};
	__property bool NoZWrite = {read=FNoZWrite, write=SetNoZWrite, default=1};
	__property float GizmoThickness = {read=FGizmoThickness, write=SetGLGizmoExThickness};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property Glbitmapfont::TGLCustomBitmapFont* LabelFont = {read=FLabelFont, write=SetLabelFont, default=0};
	__property System::Classes::TNotifyEvent OnSelectionLost = {read=FOnSelectionLost, write=FOnSelectionLost};
	__property System::Classes::TNotifyEvent OnOperationChange = {read=FOnOperationChange, write=FOnOperationChange};
	__property System::Classes::TNotifyEvent OnOperationModeChange = {read=FOnOperationModeChange, write=FOnOperationModeChange};
	__property TGLGizmoExAcceptEvent OnSelect = {read=FOnSelect, write=FOnSelect};
	__property TGLGizmoExAxisSelected OnAxisSelected = {read=FOnAxisSelected, write=FOnAxisSelected};
	__property System::Classes::TNotifyEvent OnUpdate = {read=FOnUpdate, write=FOnUpdate};
	__property TGLGizmoExPickMode PickMode = {read=FPickMode, write=SetPickMode, default=0};
	__property bool EnableActionHistory = {read=FEnableHistory, write=FEnableHistory, default=1};
	__property int HistoryStepsCount = {read=FHistoryStepsCount, write=SetHistoryStepsCount, nodefault};
	__property bool EnableLoopCursorMoving = {read=FEnableLoopCursorMoving, write=SetEnableLoopCursorMoving, default=1};
	__property bool EnableMultiSelection = {read=FEnableMultiSelection, write=SetEnableMultiSelection, default=1};
	__property bool CanChangeWithChildren = {read=FCanChangeWithChildren, write=SetCanChangeWithChildren, nodefault};
	__property bool AntiAliasedLines = {read=FAntiAliasedLines, write=SetAALines, default=1};
	__property TInfoLabelCoordType InfoLabelCoordType = {read=fInfoLabelCoordType, write=SetInfoLabelCoordType, default=1};
	__property TGLGizmoExSelectionRegion SelectionRegion = {read=FSelectionRegion, write=SetSelectionRegion, default=0};
	__property bool ShowAxisLabel = {read=FShowAxisLabel, write=SetShowAxisLabel, default=1};
	__property bool ShowObjectInfos = {read=FShowObjectInfos, write=SetShowObjectInfos, default=1};
	__property bool ShowBoundingBox = {read=FShowBoundingBox, write=SetShowBoundingBox, default=1};
	__property TGLGizmoExReferenceCoordinateSystem ReferenceCoordSystem = {read=FReferenceCoordSystem, write=SetReferenceCoordSystem, default=0};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glgizmoex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGIZMOEX)
using namespace Glgizmoex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgizmoexHPP
