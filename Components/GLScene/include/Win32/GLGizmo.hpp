// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLGizmo.pas' rev: 36.00 (Windows)

#ifndef GlgizmoHPP
#define GlgizmoHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.StdCtrls.hpp>
#include <GLScene.hpp>
#include <GLPersistentClasses.hpp>
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
#include <GLState.hpp>
#include <GLSelection.hpp>
#include <GLVectorTypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glgizmo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLGizmoUndoItem;
class DELPHICLASS TGLGizmoUndoCollection;
class DELPHICLASS TGLGizmoRayCastHitData;
class DELPHICLASS TGLGizmoPickCube;
class DELPHICLASS TGLGizmoPickTorus;
class DELPHICLASS TGLGizmo;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoUndoItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FOldLibMaterialName;
	Glcoordinates::TGLCoordinates* FOldAutoScaling;
	Glscene::TGLCustomSceneObject* FEffectedObject;
	Glvectorgeometry::TMatrix FOldMatr;
	Glvectorgeometry::TMatrix FOldMatrix;
	void __fastcall SetEffectedObject(Glscene::TGLCustomSceneObject* const Value);
	void __fastcall SetOldAutoScaling(Glcoordinates::TGLCoordinates* const Value);
	void __fastcall SetOldMatrix(const Glvectorgeometry::TMatrix &Value);
	
protected:
	virtual void __fastcall DoUndo();
	TGLGizmoUndoCollection* __fastcall GetParent();
	TGLGizmo* __fastcall GetGizmo();
	
public:
	__fastcall virtual TGLGizmoUndoItem(System::Classes::TCollection* AOwner);
	__fastcall virtual ~TGLGizmoUndoItem();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall AssignFromObject(Glscene::TGLCustomSceneObject* const AObject);
	__property Glvectorgeometry::TMatrix OldMatrix = {read=FOldMatrix, write=SetOldMatrix};
	
__published:
	__property Glscene::TGLCustomSceneObject* EffectedObject = {read=FEffectedObject, write=SetEffectedObject};
	__property Glcoordinates::TGLCoordinates* OldAutoScaling = {read=FOldAutoScaling, write=SetOldAutoScaling};
	__property System::UnicodeString OldLibMaterialName = {read=FOldLibMaterialName, write=FOldLibMaterialName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoUndoCollection : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TGLGizmoUndoItem* operator[](const int Index) { return this->Items[Index]; }
	
private:
	TGLGizmoUndoItem* __fastcall GetItems(const int Index);
	void __fastcall SetItems(const int Index, TGLGizmoUndoItem* const Value);
	
protected:
	TGLGizmo* __fastcall GetParent();
	
public:
	void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall RemoveByObject(Glscene::TGLCustomSceneObject* const AObject);
	HIDESBASE TGLGizmoUndoItem* __fastcall Add();
	__property TGLGizmoUndoItem* Items[const int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TOwnedCollection.Create */ inline __fastcall TGLGizmoUndoCollection(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass) : System::Classes::TOwnedCollection(AOwner, ItemClass) { }
	
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TGLGizmoUndoCollection() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TGLGizmoElement : unsigned char { geMove, geRotate, geScale, geAxisLabel, geObjectInfos, geBoundingBox };

typedef System::Set<TGLGizmoElement, TGLGizmoElement::geMove, TGLGizmoElement::geBoundingBox> TGLGizmoElements;

enum DECLSPEC_DENUM TGLGizmoVisibleInfoLabel : unsigned char { vliName, vliOperation, vliCoords };

typedef System::Set<TGLGizmoVisibleInfoLabel, TGLGizmoVisibleInfoLabel::vliName, TGLGizmoVisibleInfoLabel::vliCoords> TGLGizmoVisibleInfoLabels;

enum DECLSPEC_DENUM TGLGizmoAxis : unsigned char { gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ };

enum DECLSPEC_DENUM TGLGizmoOperation : unsigned char { gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo, gpRotateGizmo };

typedef void __fastcall (__closure *TGLGizmoAcceptEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* &Obj, bool &Accept, Glvectorgeometry::TVector &Dimensions);

typedef void __fastcall (__closure *TGLGizmoUpdateEvent)(System::TObject* Sender, Glscene::TGLBaseSceneObject* Obj, TGLGizmoAxis Axis, TGLGizmoOperation Operation, Glvectorgeometry::TVector &Vector);

enum DECLSPEC_DENUM TGLGizmoPickMode : unsigned char { pmGetPickedObjects, pmRayCast };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLGizmoRayCastHitData : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	Glscene::TGLBaseSceneObject* Obj;
	Glvectorgeometry::TVector Point;
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLGizmoRayCastHitData() { }
	
public:
	/* TObject.Create */ inline __fastcall TGLGizmoRayCastHitData() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLGizmoPickCube : public Globjects::TGLCube
{
	typedef Globjects::TGLCube inherited;
	
public:
	/* TGLCube.Create */ inline __fastcall virtual TGLGizmoPickCube(System::Classes::TComponent* AOwner) : Globjects::TGLCube(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoPickCube() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoPickCube(Glscene::TGLBaseSceneObject* aParentOwner) : Globjects::TGLCube(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmoPickTorus : public Glgeomobjects::TGLTorus
{
	typedef Glgeomobjects::TGLTorus inherited;
	
public:
	/* TGLTorus.Create */ inline __fastcall virtual TGLGizmoPickTorus(System::Classes::TComponent* AOwner) : Glgeomobjects::TGLTorus(AOwner) { }
	
public:
	/* TGLCustomSceneObject.Destroy */ inline __fastcall virtual ~TGLGizmoPickTorus() { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLGizmoPickTorus(Glscene::TGLBaseSceneObject* aParentOwner) : Glgeomobjects::TGLTorus(aParentOwner) { }
	
};


class PASCALIMPLEMENTATION TGLGizmo : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Glscene::TGLBaseSceneObject* _GZObaseGizmo;
	Globjects::TGLCube* _GZOBoundingcube;
	Glscene::TGLBaseSceneObject* _GZOrootHelpers;
	Glscene::TGLBaseSceneObject* _GZOrootLines;
	Glscene::TGLBaseSceneObject* _GZOrootTorus;
	Glscene::TGLBaseSceneObject* _GZOrootCubes;
	Glscene::TGLBaseSceneObject* _GZORootAxisLabel;
	Glscene::TGLBaseSceneObject* _GZORootVisibleInfoLabels;
	Globjects::TGLLines* _GZOlineX;
	Globjects::TGLLines* _GZOlineY;
	Globjects::TGLLines* _GZOlineZ;
	Globjects::TGLLines* _GZOplaneXY;
	Globjects::TGLLines* _GZOplaneXZ;
	Globjects::TGLLines* _GZOplaneYZ;
	TGLGizmoPickTorus* _GZOTorusX;
	TGLGizmoPickTorus* _GZOTorusY;
	TGLGizmoPickTorus* _GZOTorusZ;
	TGLGizmoPickCube* _GZOCubeX;
	TGLGizmoPickCube* _GZOCubeY;
	TGLGizmoPickCube* _GZOCubeZ;
	Glbitmapfont::TGLFlatText* _GZOAxisLabelX;
	Glbitmapfont::TGLFlatText* _GZOAxisLabelY;
	Glbitmapfont::TGLFlatText* _GZOAxisLabelZ;
	Glbitmapfont::TGLFlatText* _GZOVisibleInfoLabels;
	Glscene::TGLBaseSceneObject* FRootGizmo;
	Glscene::TGLBaseSceneObject* FSelectedObj;
	TGLGizmoOperation FOperation;
	TGLGizmoAxis FSelAxis;
	Glcolor::TGLColor* FBoundingBoxColor;
	Glcolor::TGLColor* FSelectedColor;
	Glcolor::TGLColor* FVisibleInfoLabelsColor;
	bool FBoundingBoxColorChanged;
	bool FVisibleInfoLabelsColorChanged;
	bool FForceOperation;
	bool FForceAxis;
	bool FForceUniformScale;
	bool FAutoZoom;
	bool FExcludeObjects;
	bool FNoZWrite;
	bool FEnabled;
	float FAutoZoomFactor;
	float FZoomFactor;
	float FMoveCoef;
	float FRotationCoef;
	Glwin32viewer::TGLSceneViewer* FViewer;
	TGLGizmoElements FGizmoElements;
	TGLGizmoVisibleInfoLabels FVisibleVisibleInfoLabels;
	System::Classes::TStrings* FExcludeObjectsList;
	bool Moving;
	int Mx;
	int My;
	int Rx;
	int Ry;
	Glscene::TGLDirectOpenGL* dglEnable;
	Glscene::TGLDirectOpenGL* dglDisable;
	Glscene::TGLDirectOpenGL* dgtEnable;
	Glscene::TGLDirectOpenGL* dgtDisable;
	Glscene::TGLDirectOpenGL* dgcEnable;
	Glscene::TGLDirectOpenGL* dgcDisable;
	Glscene::TGLDirectOpenGL* dglaEnable;
	Glscene::TGLDirectOpenGL* dglaDisable;
	Glscene::TGLDirectOpenGL* dgliEnable;
	Glscene::TGLDirectOpenGL* dgliDisable;
	Glvectorgeometry::TVector LastMousePos;
	Glvectorgeometry::TVector ObjDimensions;
	TGLGizmoAcceptEvent FOnBeforeSelect;
	TGLGizmoUpdateEvent FOnBeforeUpdate;
	System::Classes::TNotifyEvent FOnSelectionLost;
	float FScaleCoef;
	float FGizmoThickness;
	TGLGizmoPickMode FPickMode;
	System::Classes::TList* FInternalRaycastHitData;
	TGLGizmoUndoCollection* FUndoHistory;
	Glbitmapfont::TGLCustomBitmapFont* FLabelFont;
	void __fastcall SetRootGizmo(Glscene::TGLBaseSceneObject* const AValue);
	void __fastcall SetGizmoElements(const TGLGizmoElements AValue);
	void __fastcall SeTGLGizmoVisibleInfoLabels(const TGLGizmoVisibleInfoLabels AValue);
	void __fastcall SetBoundingBoxColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetSelectedColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetVisibleInfoLabelsColor(Glcolor::TGLColor* const AValue);
	void __fastcall SetExcludeObjectsList(System::Classes::TStrings* const AValue);
	void __fastcall DirectGlDisable(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &Rci);
	void __fastcall DirectGlEnable(System::TObject* Sender, Glrendercontextinfo::TGLRenderContextInfo &Rci);
	Glvectorgeometry::TVector __fastcall MouseWorldPos(const int X, const int Y);
	bool __fastcall CheckObjectInExcludeList(Glscene::TGLBaseSceneObject* const Obj);
	void __fastcall UpdateVisibleInfoLabels();
	void __fastcall SetGLGizmoThickness(const float Value);
	Glselection::TGLPickList* __fastcall InternalGetPickedObjects(const int X1, const int Y1, const int X2, const int Y2, const int GuessCount = 0x8);
	void __fastcall ClearInternalRaycastHitData();
	void __fastcall SetViewer(Glwin32viewer::TGLSceneViewer* const Value);
	void __fastcall SetLabelFont(Glbitmapfont::TGLCustomBitmapFont* const Value);
	void __fastcall SetSelectedObj(Glscene::TGLBaseSceneObject* const Value);
	
public:
	System::Classes::TList* PickableObjectsWithRayCast;
	__fastcall virtual TGLGizmo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLGizmo();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ViewerMouseMove(const int X, const int Y);
	void __fastcall ViewerMouseDown(const int X, const int Y);
	void __fastcall ViewerMouseUp(const int X, const int Y);
	void __fastcall UpdateGizmo()/* overload */;
	void __fastcall UpdateGizmo(const Glvectorgeometry::TVector &NewDimensions)/* overload */;
	void __fastcall SetVisible(const bool AValue);
	Glvectorgeometry::TVector __fastcall GetPickedObjectPoint(Glscene::TGLBaseSceneObject* const Obj);
	virtual void __fastcall LooseSelection();
	void __fastcall UndoAdd(Glscene::TGLCustomSceneObject* const AObject);
	__property Glscene::TGLBaseSceneObject* RootGizmo = {read=FRootGizmo, write=SetRootGizmo};
	
__published:
	__property Glwin32viewer::TGLSceneViewer* Viewer = {read=FViewer, write=SetViewer};
	__property TGLGizmoElements GizmoElements = {read=FGizmoElements, write=SetGizmoElements, nodefault};
	__property Glcolor::TGLColor* BoundingBoxColor = {read=FBoundingBoxColor, write=SetBoundingBoxColor};
	__property Glcolor::TGLColor* SelectedColor = {read=FSelectedColor, write=SetSelectedColor};
	__property TGLGizmoAxis SelAxis = {read=FSelAxis, write=FSelAxis, nodefault};
	__property bool ForceAxis = {read=FForceAxis, write=FForceAxis, nodefault};
	__property Glscene::TGLBaseSceneObject* SelectedObj = {read=FSelectedObj, write=SetSelectedObj};
	__property TGLGizmoOperation Operation = {read=FOperation, write=FOperation, nodefault};
	__property bool ForceOperation = {read=FForceOperation, write=FForceOperation, nodefault};
	__property bool ForceUniformScale = {read=FForceUniformScale, write=FForceUniformScale, nodefault};
	__property bool ExcludeObjects = {read=FExcludeObjects, write=FExcludeObjects, nodefault};
	__property System::Classes::TStrings* ExcludeObjectsList = {read=FExcludeObjectsList, write=SetExcludeObjectsList};
	__property TGLGizmoVisibleInfoLabels VisibleInfoLabels = {read=FVisibleVisibleInfoLabels, write=SeTGLGizmoVisibleInfoLabels, nodefault};
	__property Glcolor::TGLColor* VisibleInfoLabelsColor = {read=FVisibleInfoLabelsColor, write=SetVisibleInfoLabelsColor};
	__property bool AutoZoom = {read=FAutoZoom, write=FAutoZoom, nodefault};
	__property float AutoZoomFactor = {read=FAutoZoomFactor, write=FAutoZoomFactor};
	__property float ZoomFactor = {read=FZoomFactor, write=FZoomFactor};
	__property float MoveCoef = {read=FMoveCoef, write=FMoveCoef};
	__property float RotationCoef = {read=FRotationCoef, write=FRotationCoef};
	__property float ScaleCoef = {read=FScaleCoef, write=FScaleCoef};
	__property bool NoZWrite = {read=FNoZWrite, write=FNoZWrite, nodefault};
	__property float GizmoThickness = {read=FGizmoThickness, write=SetGLGizmoThickness};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=0};
	__property Glbitmapfont::TGLCustomBitmapFont* LabelFont = {read=FLabelFont, write=SetLabelFont, default=0};
	__property TGLGizmoAcceptEvent OnBeforeSelect = {read=FOnBeforeSelect, write=FOnBeforeSelect};
	__property System::Classes::TNotifyEvent OnSelectionLost = {read=FOnSelectionLost, write=FOnSelectionLost};
	__property TGLGizmoUpdateEvent OnBeforeUpdate = {read=FOnBeforeUpdate, write=FOnBeforeUpdate};
	__property TGLGizmoPickMode PickMode = {read=FPickMode, write=FPickMode, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glgizmo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLGIZMO)
using namespace Glgizmo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlgizmoHPP
