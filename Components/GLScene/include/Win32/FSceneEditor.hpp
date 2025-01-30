// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FSceneEditor.pas' rev: 36.00 (Windows)

#ifndef FSceneEditorHPP
#define FSceneEditorHPP

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
#include <System.Win.Registry.hpp>
#include <System.ImageList.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ToolWin.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Clipbrd.hpp>
#include <DesignIntf.hpp>
#include <VCLEditors.hpp>
#include <GLScene.hpp>
#include <GLWin32Viewer.hpp>
#include <GLSceneRegister.hpp>
#include <GLStrings.hpp>
#include <FInfo.hpp>
#include <GLXCollection.hpp>
#include <GLCrossPlatform.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fsceneeditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGLSceneEditorForm;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TSetSubItemsEvent)(System::TObject* Sender);

class PASCALIMPLEMENTATION TGLSceneEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Menus::TPopupMenu* PopupMenu;
	Vcl::Menus::TMenuItem* MIAddObject;
	Vcl::Menus::TMenuItem* N1;
	Vcl::Menus::TMenuItem* MIDelObject;
	Vcl::Comctrls::TToolBar* ToolBar;
	Vcl::Actnlist::TActionList* ActionList;
	Vcl::Comctrls::TToolButton* TBAddObjects;
	Vcl::Comctrls::TToolButton* TBMoveUp;
	Vcl::Menus::TPopupMenu* PMToolBar;
	Vcl::Comctrls::TToolButton* TBDeleteObject;
	Vcl::Comctrls::TToolButton* TBMoveDown;
	Vcl::Actnlist::TAction* ACAddObject;
	Vcl::Controls::TImageList* ImageList;
	Vcl::Actnlist::TAction* ACDeleteObject;
	Vcl::Actnlist::TAction* ACMoveUp;
	Vcl::Actnlist::TAction* ACMoveDown;
	Vcl::Menus::TMenuItem* N2;
	Vcl::Menus::TMenuItem* MIMoveUp;
	Vcl::Menus::TMenuItem* MIMoveDown;
	Vcl::Actnlist::TAction* ACSaveScene;
	Vcl::Actnlist::TAction* ACLoadScene;
	Vcl::Dialogs::TOpenDialog* OpenDialog;
	Vcl::Dialogs::TSaveDialog* SaveDialog;
	Vcl::Comctrls::TToolButton* TBLoadScene;
	Vcl::Comctrls::TToolButton* TBSaveScene;
	Vcl::Actnlist::TAction* ACInfo;
	Vcl::Actnlist::TAction* ACCopy;
	Vcl::Actnlist::TAction* ACCut;
	Vcl::Actnlist::TAction* ACPaste;
	Vcl::Menus::TMenuItem* MICopy;
	Vcl::Menus::TMenuItem* MIPaste;
	Vcl::Menus::TMenuItem* MICut;
	Vcl::Comctrls::TToolButton* TBCut;
	Vcl::Comctrls::TToolButton* TBCopy;
	Vcl::Comctrls::TToolButton* TBPaste;
	Vcl::Menus::TPopupMenu* PMBehavioursToolbar;
	Vcl::Actnlist::TAction* ACAddBehaviour;
	Vcl::Menus::TMenuItem* MIAddBehaviour;
	Vcl::Menus::TMenuItem* MIAddEffect;
	Vcl::Menus::TMenuItem* MIBehaviourSeparator;
	Vcl::Actnlist::TAction* ACDeleteBehaviour;
	Vcl::Menus::TPopupMenu* BehavioursPopupMenu;
	Vcl::Menus::TMenuItem* Delete1;
	Vcl::Menus::TMenuItem* MoveUp1;
	Vcl::Menus::TMenuItem* MoveDown1;
	Vcl::Menus::TMenuItem* N4;
	Vcl::Menus::TPopupMenu* PMEffectsToolbar;
	Vcl::Actnlist::TAction* ACAddEffect;
	Vcl::Comctrls::TToolButton* TBCharacterPanels;
	Vcl::Comctrls::TToolButton* TBStayOnTop;
	Vcl::Actnlist::TAction* ACStayOnTop;
	Vcl::Comctrls::TToolButton* TBSeparator4;
	Vcl::Comctrls::TToolButton* TBExpand;
	Vcl::Actnlist::TAction* ACExpand;
	Vcl::Extctrls::TPanel* PAGallery;
	Vcl::Extctrls::TPanel* PATree;
	Vcl::Comctrls::TTreeView* Tree;
	Vcl::Comctrls::TToolButton* TBInfo;
	Vcl::Comctrls::TListView* GalleryListView;
	Vcl::Extctrls::TPanel* PABehaviours;
	Vcl::Comctrls::TToolBar* ToolBarBehaviours;
	Vcl::Comctrls::TToolButton* TBAddBehaviours;
	Vcl::Comctrls::TListView* BehavioursListView;
	Vcl::Extctrls::TPanel* PAEffects;
	Vcl::Comctrls::TListView* EffectsListView;
	Vcl::Comctrls::TToolBar* ToolBarEffects;
	Vcl::Comctrls::TToolButton* TBAddEffects;
	Vcl::Comctrls::TToolButton* TBGalleryPanel;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall TreeEditing(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node, bool &AllowEdit);
	void __fastcall TreeDragOver(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall TreeDragDrop(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall TreeChange(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node);
	void __fastcall TreeMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall TreeEnter(System::TObject* Sender);
	void __fastcall TreeMouseMove(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ACDeleteObjectExecute(System::TObject* Sender);
	void __fastcall ACMoveUpExecute(System::TObject* Sender);
	void __fastcall ACMoveDownExecute(System::TObject* Sender);
	void __fastcall ACAddObjectExecute(System::TObject* Sender);
	void __fastcall ACSaveSceneExecute(System::TObject* Sender);
	void __fastcall ACLoadSceneExecute(System::TObject* Sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	void __fastcall ACInfoExecute(System::TObject* Sender);
	void __fastcall ACCopyExecute(System::TObject* Sender);
	void __fastcall ACCutExecute(System::TObject* Sender);
	void __fastcall ACPasteExecute(System::TObject* Sender);
	void __fastcall BehavioursListViewEnter(System::TObject* Sender);
	void __fastcall EffectsListViewEnter(System::TObject* Sender);
	void __fastcall ACAddBehaviourExecute(System::TObject* Sender);
	void __fastcall DeleteBaseBehaviour(Vcl::Comctrls::TListView* ListView);
	void __fastcall PMBehavioursToolbarPopup(System::TObject* Sender);
	void __fastcall PMEffectsToolbarPopup(System::TObject* Sender);
	void __fastcall BehavioursListViewSelectItem(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, bool Selected);
	void __fastcall ACAddEffectExecute(System::TObject* Sender);
	void __fastcall PopupMenuPopup(System::TObject* Sender);
	void __fastcall TBCharacterPanelsClick(System::TObject* Sender);
	void __fastcall TreeKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ACStayOnTopExecute(System::TObject* Sender);
	void __fastcall FormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ACExpandExecute(System::TObject* Sender);
	void __fastcall TBGalleryPanelClick(System::TObject* Sender);
	
private:
	int FSelectedItems;
	Glscene::TGLScene* FScene;
	Vcl::Comctrls::TTreeNode* FObjectNode;
	Vcl::Comctrls::TTreeNode* FSceneObjects;
	Designintf::_di_IDesigner FCurrentDesigner;
	Winapi::Windows::TPoint FLastMouseDownPos;
	System::Classes::TComponent* FPasteOwner;
	Designintf::_di_IDesignerSelections FPasteSelection;
	void __fastcall ReadScene();
	void __fastcall ResetTree();
	Vcl::Comctrls::TTreeNode* __fastcall AddNodes(Vcl::Comctrls::TTreeNode* ANode, Glscene::TGLBaseSceneObject* AObject);
	void __fastcall AddObjectClick(System::TObject* Sender);
	void __fastcall AddBehaviourClick(System::TObject* Sender);
	void __fastcall AddEffectClick(System::TObject* Sender);
	void __fastcall SetObjectsSubItems(Vcl::Menus::TMenuItem* parent);
	void __fastcall SetXCollectionSubItems(Vcl::Menus::TMenuItem* parent, Glxcollection::TXCollection* XCollection, TSetSubItemsEvent Event);
	void __fastcall SetBehavioursSubItems(Vcl::Menus::TMenuItem* parent, Glxcollection::TXCollection* XCollection);
	void __fastcall SetEffectsSubItems(Vcl::Menus::TMenuItem* parent, Glxcollection::TXCollection* XCollection);
	void __fastcall OnBaseSceneObjectNameChanged(System::TObject* Sender);
	bool __fastcall IsValidClipBoardNode();
	bool __fastcall IsPastePossible();
	void __fastcall ShowBehaviours(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowEffects(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowGallery(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall ShowBehavioursAndEffects(Glscene::TGLBaseSceneObject* BaseSceneObject);
	void __fastcall EnableAndDisableActions();
	bool __fastcall CanPaste(Glscene::TGLBaseSceneObject* obj, Glscene::TGLBaseSceneObject* destination);
	void __fastcall CopyComponents(System::Classes::TComponent* Root, const Designintf::_di_IDesignerSelections Components);
	void __fastcall MethodError(System::Classes::TReader* Reader, const System::UnicodeString MethodName, void * &Address, bool &Error);
	bool __fastcall PasteComponents(System::Classes::TComponent* AOwner, System::Classes::TComponent* AParent, const Designintf::_di_IDesignerSelections Components);
	void __fastcall ReaderSetName(System::Classes::TReader* Reader, System::Classes::TComponent* Component, System::UnicodeString &Name);
	void __fastcall ComponentRead(System::Classes::TComponent* Component);
	System::UnicodeString __fastcall UniqueName(System::Classes::TComponent* Component);
	void __fastcall TreeEdited(System::TObject* Sender, Vcl::Comctrls::TTreeNode* Node, System::UnicodeString &S);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	void __fastcall SetScene(Glscene::TGLScene* Scene, Designintf::_di_IDesigner Designer);
public:
	/* TCustomForm.Create */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.CreateScaledNew */ inline __fastcall virtual TGLSceneEditorForm(System::Classes::TComponent* AOwner, int ADPI, int Dummy) : Vcl::Forms::TForm(AOwner, ADPI, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TGLSceneEditorForm() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGLSceneEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Int8 SCENE_SELECTED = System::Int8(0x0);
static _DELPHI_CONST System::Int8 BEHAVIOURS_SELECTED = System::Int8(0x1);
static _DELPHI_CONST System::Int8 EFFECTS_SELECTED = System::Int8(0x2);
extern DELPHI_PACKAGE TGLSceneEditorForm* __fastcall GLSceneEditorForm(void);
extern DELPHI_PACKAGE void __fastcall ReleaseGLSceneEditorForm(void);
}	/* namespace Fsceneeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FSCENEEDITOR)
using namespace Fsceneeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FSceneEditorHPP
