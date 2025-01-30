// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GR32_Containers.pas' rev: 36.00 (Windows)

#ifndef GR32_ContainersHPP
#define GR32_ContainersHPP

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
#include <System.RTLConsts.hpp>
#include <GR32.hpp>
#include <System.SysUtils.hpp>
#include <GR32_LowLevel.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gr32_containers
{
//-- forward type declarations -----------------------------------------------
struct TPointerBucketItem;
struct TPointerBucket;
class DELPHICLASS TPointerMap;
class DELPHICLASS TPointerMapIterator;
class DELPHICLASS TRectList;
class DELPHICLASS TClassList;
struct TLinkedNode;
class DELPHICLASS TLinkedList;
//-- type declarations -------------------------------------------------------
typedef void * *PPItem;

typedef void * PItem;

typedef void * *PPData;

typedef void * PData;

typedef TPointerBucketItem *PPointerBucketItem;

struct DECLSPEC_DRECORD TPointerBucketItem
{
public:
	PItem Item;
	PData Data;
};


typedef System::DynamicArray<TPointerBucketItem> TPointerBucketItemArray;

struct DECLSPEC_DRECORD TPointerBucket
{
public:
	int Count;
	TPointerBucketItemArray Items;
};


typedef System::StaticArray<TPointerBucket, 256> TPointerBucketArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPointerMap : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	PData operator[](PItem Item) { return this->Data[Item]; }
	
private:
	TPointerBucketArray FBuckets;
	int FCount;
	
protected:
	PData __fastcall GetData(PItem Item);
	void __fastcall SetData(PItem Item, const PData Data);
	bool __fastcall Exists(void * Item, /* out */ int &BucketIndex, /* out */ int &ItemIndex);
	virtual PData __fastcall Delete(int BucketIndex, int ItemIndex);
	
public:
	__fastcall virtual ~TPointerMap();
	PPData __fastcall Add(PItem NewItem)/* overload */;
	PPData __fastcall Add(PItem NewItem, /* out */ bool &IsNew)/* overload */;
	PPData __fastcall Add(PItem NewItem, PData NewData)/* overload */;
	PPData __fastcall Add(PItem NewItem, PData NewData, /* out */ bool &IsNew)/* overload */;
	PData __fastcall Remove(PItem Item);
	void __fastcall Clear();
	bool __fastcall Contains(PItem Item);
	bool __fastcall Find(PItem Item, /* out */ PPData &Data);
	__property PData Data[PItem Item] = {read=GetData, write=SetData/*, default*/};
	__property int Count = {read=FCount, nodefault};
public:
	/* TObject.Create */ inline __fastcall TPointerMap() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPointerMapIterator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TPointerMap* FSrcPointerMap;
	PItem FItem;
	PData FData;
	int FCurBucketIndex;
	int FCurItemIndex;
	
public:
	__fastcall TPointerMapIterator(TPointerMap* SrcPointerMap);
	bool __fastcall Next();
	__property PItem Item = {read=FItem};
	__property PData Data = {read=FData};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TPointerMapIterator() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<System::Types::TRect, 67108863> TPolyRects;

typedef TPolyRects *PPolyRects;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TRectList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	Gr32::PRect operator[](int Index) { return this->Items[Index]; }
	
private:
	PPolyRects FList;
	int FCount;
	int FCapacity;
	
protected:
	Gr32::PRect __fastcall Get(int Index);
	virtual void __fastcall Grow();
	void __fastcall SetCapacity(int NewCapacity);
	void __fastcall SetCount(int NewCount);
	
public:
	__fastcall virtual ~TRectList();
	int __fastcall Add(const Gr32::TRect &Rect);
	virtual void __fastcall Clear();
	void __fastcall Delete(int Index);
	void __fastcall Exchange(int Index1, int Index2);
	int __fastcall IndexOf(const Gr32::TRect &Rect);
	void __fastcall Insert(int Index, const Gr32::TRect &Rect);
	void __fastcall Move(int CurIndex, int NewIndex);
	int __fastcall Remove(const Gr32::TRect &Rect);
	void __fastcall Pack();
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property Gr32::PRect Items[int Index] = {read=Get/*, default*/};
	__property PPolyRects List = {read=FList};
public:
	/* TObject.Create */ inline __fastcall TRectList() : System::TObject() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClassList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	System::TClass operator[](int Index) { return this->Items[Index]; }
	
protected:
	System::TClass __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, System::TClass AClass);
	
public:
	HIDESBASE int __fastcall Add(System::TClass AClass);
	HIDESBASE System::TClass __fastcall Extract(System::TClass Item);
	HIDESBASE int __fastcall Remove(System::TClass AClass);
	HIDESBASE int __fastcall IndexOf(System::TClass AClass);
	HIDESBASE System::TClass __fastcall First();
	HIDESBASE System::TClass __fastcall Last();
	System::TClass __fastcall Find(System::UnicodeString AClassName);
	void __fastcall GetClassNames(System::Classes::TStrings* Strings);
	HIDESBASE void __fastcall Insert(int Index, System::TClass AClass);
	__property System::TClass Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TClassList() { }
	
public:
	/* TObject.Create */ inline __fastcall TClassList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

typedef TLinkedNode *PLinkedNode;

struct DECLSPEC_DRECORD TLinkedNode
{
public:
	PLinkedNode Prev;
	PLinkedNode Next;
	void *Data;
};


typedef void __fastcall (*TIteratorProc)(PLinkedNode Node, int Index);

typedef void __fastcall (__closure *TFreeDataEvent)(void * Data);

class PASCALIMPLEMENTATION TLinkedList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FCount;
	PLinkedNode FHead;
	PLinkedNode FTail;
	TFreeDataEvent FOnFreeData;
	
protected:
	virtual void __fastcall DoFreeData(void * Data);
	
public:
	__fastcall virtual ~TLinkedList();
	PLinkedNode __fastcall Add();
	void __fastcall Remove(PLinkedNode Node);
	int __fastcall IndexOf(PLinkedNode Node);
	PLinkedNode __fastcall GetNode(int Index);
	void __fastcall Exchange(PLinkedNode Node1, PLinkedNode Node2);
	void __fastcall InsertBefore(PLinkedNode Node, PLinkedNode NewNode);
	void __fastcall InsertAfter(PLinkedNode Node, PLinkedNode NewNode);
	void __fastcall Clear();
	void __fastcall IterateList(TIteratorProc CallBack);
	__property PLinkedNode Head = {read=FHead, write=FHead};
	__property PLinkedNode Tail = {read=FTail, write=FTail};
	__property int Count = {read=FCount, write=FCount, nodefault};
	__property TFreeDataEvent OnFreeData = {read=FOnFreeData, write=FOnFreeData};
public:
	/* TObject.Create */ inline __fastcall TLinkedList() : System::TObject() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Byte BUCKET_MASK = System::Byte(0xff);
static _DELPHI_CONST System::Word BUCKET_COUNT = System::Word(0x100);
extern DELPHI_PACKAGE void __fastcall SmartAssign(System::Classes::TPersistent* Src, System::Classes::TPersistent* Dst, System::Typinfo::TTypeKinds TypeKinds = (System::Typinfo::TTypeKinds() << System::TTypeKind::tkInteger << System::TTypeKind::tkChar << System::TTypeKind::tkEnumeration << System::TTypeKind::tkFloat << System::TTypeKind::tkString << System::TTypeKind::tkSet << System::TTypeKind::tkClass << System::TTypeKind::tkWChar << System::TTypeKind::tkLString << System::TTypeKind::tkWString << System::TTypeKind::tkVariant << System::TTypeKind::tkArray << System::TTypeKind::tkRecord << System::TTypeKind::tkInterface << System::TTypeKind::tkInt64 << System::TTypeKind::tkDynArray << System::TTypeKind::tkUString << System::TTypeKind::tkClassRef << System::TTypeKind::tkPointer << System::TTypeKind::tkProcedure << System::TTypeKind::tkMRecord ));
extern DELPHI_PACKAGE void __fastcall Advance(PLinkedNode &Node, int Steps = 0x1);
}	/* namespace Gr32_containers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GR32_CONTAINERS)
using namespace Gr32_containers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GR32_ContainersHPP
