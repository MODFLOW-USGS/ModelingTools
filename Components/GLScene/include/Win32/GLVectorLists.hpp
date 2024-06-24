// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLVectorLists.pas' rev: 35.00 (Windows)

#ifndef GlvectorlistsHPP
#define GlvectorlistsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <GLVectorTypes.hpp>
#include <GLVectorGeometry.hpp>
#include <GLPersistentClasses.hpp>

//-- user supplied -----------------------------------------------------------

namespace Glvectorlists
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TBaseList;
class DELPHICLASS TBaseVectorList;
class DELPHICLASS TAffineVectorList;
class DELPHICLASS TVectorList;
class DELPHICLASS TTexPointList;
class DELPHICLASS TIntegerList;
class DELPHICLASS TSingleList;
class DELPHICLASS TDoubleList;
class DELPHICLASS TByteList;
class DELPHICLASS TQuaternionList;
struct T4ByteData;
class DELPHICLASS T4ByteList;
class DELPHICLASS TLongWordList;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TBaseListOption : unsigned char { bloExternalMemory, bloSetCountResetsMemory };

typedef System::Set<TBaseListOption, TBaseListOption::bloExternalMemory, TBaseListOption::bloSetCountResetsMemory> TBaseListOptions;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseList : public Glpersistentclasses::TPersistentObject
{
	typedef Glpersistentclasses::TPersistentObject inherited;
	
private:
	int FCount;
	int FCapacity;
	int FGrowthDelta;
	Glvectorgeometry::TByteVector *FBufferItem;
	TBaseListOptions FOptions;
	unsigned FRevision;
	System::UnicodeString FTagString;
	
protected:
	Glvectorgeometry::TByteVector *FBaseList;
	int FItemSize;
	void __fastcall SetCount(int Val);
	virtual void __fastcall SetCapacity(int NewCapacity);
	Glvectorgeometry::PByteVector __fastcall BufferItem();
	bool __fastcall GetSetCountResetsMemory();
	void __fastcall SetSetCountResetsMemory(const bool Val);
	virtual void __fastcall ReadItemsData(System::Classes::TReader* AReader);
	virtual void __fastcall WriteItemsData(System::Classes::TWriter* AWriter);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* AFiler);
	
public:
	__fastcall virtual TBaseList();
	__fastcall virtual ~TBaseList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	void __fastcall AddNulls(unsigned nbVals);
	void __fastcall InsertNulls(int Index, unsigned nbVals);
	void __fastcall AdjustCapacityToAtLeast(const int size);
	int __fastcall DataSize();
	void __fastcall UseMemory(void * rangeStart, int rangeCapacity);
	void __fastcall Flush();
	void __fastcall Clear();
	void __fastcall Delete(int Index);
	void __fastcall DeleteItems(int Index, unsigned nbVals);
	void __fastcall Exchange(int index1, int index2);
	void __fastcall Move(int curIndex, int newIndex);
	void __fastcall Reverse();
	__property int Count = {read=FCount, write=SetCount, nodefault};
	__property int Capacity = {read=FCapacity, write=SetCapacity, nodefault};
	__property int GrowthDelta = {read=FGrowthDelta, write=FGrowthDelta, nodefault};
	__property bool SetCountResetsMemory = {read=GetSetCountResetsMemory, write=SetSetCountResetsMemory, nodefault};
	__property System::UnicodeString TagString = {read=FTagString, write=FTagString};
	__property unsigned Revision = {read=FRevision, write=FRevision, nodefault};
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseList(Glpersistentclasses::TVirtualReader* reader) : Glpersistentclasses::TPersistentObject(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TBaseVectorList : public TBaseList
{
	typedef TBaseList inherited;
	
protected:
	Glvectorgeometry::PFloatVector __fastcall GetItemAddress(int Index);
	
public:
	DYNAMIC void __fastcall WriteToFiler(Glpersistentclasses::TVirtualWriter* writer);
	DYNAMIC void __fastcall ReadFromFiler(Glpersistentclasses::TVirtualReader* reader);
	virtual void __fastcall GetExtents(/* out */ Glvectortypes::TVector3f &min, /* out */ Glvectortypes::TVector3f &max);
	Glvectortypes::TVector3f __fastcall Sum();
	virtual void __fastcall Normalize();
	float __fastcall MaxSpacing(TBaseVectorList* list2);
	virtual void __fastcall Translate(const Glvectortypes::TVector3f &delta)/* overload */;
	virtual void __fastcall Translate(TBaseVectorList* const delta)/* overload */;
	virtual void __fastcall TranslateInv(TBaseVectorList* const delta)/* overload */;
	virtual void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor) = 0 ;
	void __fastcall AngleLerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
	void __fastcall AngleCombine(TBaseVectorList* const list1, float intensity);
	virtual void __fastcall Combine(TBaseVectorList* const list2, float factor);
	__property Glvectorgeometry::PFloatVector ItemAddress[int Index] = {read=GetItemAddress};
public:
	/* TBaseList.Create */ inline __fastcall virtual TBaseVectorList() : TBaseList() { }
	/* TBaseList.Destroy */ inline __fastcall virtual ~TBaseVectorList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TBaseVectorList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAffineVectorList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Glvectortypes::TVector3f operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TAffineVectorArray *FList;
	
protected:
	Glvectortypes::TVector3f __fastcall Get(int Index);
	void __fastcall Put(int Index, const Glvectortypes::TVector3f &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TAffineVectorList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Glvectortypes::TVector3f &item)/* overload */;
	int __fastcall Add(const Glvectortypes::TVector4f &item)/* overload */;
	void __fastcall Add(const Glvectortypes::TVector3f &i1, const Glvectortypes::TVector3f &i2)/* overload */;
	void __fastcall Add(const Glvectortypes::TVector3f &i1, const Glvectortypes::TVector3f &i2, const Glvectortypes::TVector3f &i3)/* overload */;
	int __fastcall Add(const Glvectortypes::TVector2f &item)/* overload */;
	int __fastcall Add(const Glvectorgeometry::TTexPoint &item)/* overload */;
	int __fastcall Add(const float X, const float Y)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z)/* overload */;
	int __fastcall Add(const int X, const int Y, const int Z)/* overload */;
	int __fastcall AddNC(const int X, const int Y, const int Z)/* overload */;
	int __fastcall Add(const Glvectorgeometry::PIntegerVector xy, const int Z)/* overload */;
	int __fastcall AddNC(const Glvectorgeometry::PIntegerVector xy, const int Z)/* overload */;
	void __fastcall Add(TAffineVectorList* const list)/* overload */;
	void __fastcall Push(const Glvectortypes::TVector3f &Val);
	Glvectortypes::TVector3f __fastcall Pop();
	void __fastcall Insert(int Index, const Glvectortypes::TVector3f &item);
	int __fastcall IndexOf(const Glvectortypes::TVector3f &item);
	int __fastcall FindOrAdd(const Glvectortypes::TVector3f &item);
	__property Glvectortypes::TVector3f Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PAffineVectorArray List = {read=FList};
	virtual void __fastcall Translate(const Glvectortypes::TVector3f &delta)/* overload */;
	HIDESBASE void __fastcall Translate(const Glvectortypes::TVector3f &delta, int base, int nb)/* overload */;
	void __fastcall TranslateItem(int Index, const Glvectortypes::TVector3f &delta);
	void __fastcall TranslateItems(int Index, const Glvectortypes::TVector3f &delta, int nb);
	void __fastcall CombineItem(int Index, const Glvectortypes::TVector3f &vector, const float f);
	void __fastcall TransformAsPoints(const Glvectortypes::TMatrix4f &matrix);
	void __fastcall TransformAsVectors(const Glvectortypes::TMatrix4f &matrix)/* overload */;
	void __fastcall TransformAsVectors(const Glvectortypes::TMatrix3f &matrix)/* overload */;
	virtual void __fastcall Normalize();
	virtual void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
	void __fastcall Scale(float factor)/* overload */;
	void __fastcall Scale(const Glvectortypes::TVector3f &factors)/* overload */;
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TAffineVectorList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TAffineVectorList(Glpersistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  Translate(TBaseVectorList* const delta){ TBaseVectorList::Translate(delta); }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TVectorList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Glvectortypes::TVector4f operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TVectorArray *FList;
	
protected:
	Glvectortypes::TVector4f __fastcall Get(int Index);
	void __fastcall Put(int Index, const Glvectortypes::TVector4f &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TVectorList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Glvectortypes::TVector4f &item)/* overload */;
	int __fastcall Add(const Glvectortypes::TVector3f &item, float w)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z, const float w)/* overload */;
	void __fastcall Add(const Glvectortypes::TVector3f &i1, const Glvectortypes::TVector3f &i2, const Glvectortypes::TVector3f &i3, float w)/* overload */;
	int __fastcall AddVector(const Glvectortypes::TVector3f &item)/* overload */;
	int __fastcall AddPoint(const Glvectortypes::TVector3f &item)/* overload */;
	int __fastcall AddPoint(const float X, const float Y, const float Z = 0.000000E+00f)/* overload */;
	void __fastcall Push(const Glvectortypes::TVector4f &Val);
	Glvectortypes::TVector4f __fastcall Pop();
	int __fastcall IndexOf(const Glvectortypes::TVector4f &item);
	int __fastcall FindOrAdd(const Glvectortypes::TVector4f &item);
	int __fastcall FindOrAddPoint(const Glvectortypes::TVector3f &item);
	void __fastcall Insert(int Index, const Glvectortypes::TVector4f &item);
	__property Glvectortypes::TVector4f Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PVectorArray List = {read=FList};
	virtual void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TVectorList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TVectorList(Glpersistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTexPointList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Glvectorgeometry::TTexPoint operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TTexPointArray *FList;
	
protected:
	Glvectorgeometry::TTexPoint __fastcall Get(int Index);
	void __fastcall Put(int Index, const Glvectorgeometry::TTexPoint &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TTexPointList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall IndexOf(const Glvectorgeometry::TTexPoint &item);
	int __fastcall FindOrAdd(const Glvectorgeometry::TTexPoint &item);
	int __fastcall Add(const Glvectorgeometry::TTexPoint &item)/* overload */;
	int __fastcall Add(const Glvectortypes::TVector2f &item)/* overload */;
	int __fastcall Add(const float texS, const float Text)/* overload */;
	int __fastcall Add(const int texS, const int Text)/* overload */;
	int __fastcall AddNC(const int texS, const int Text)/* overload */;
	int __fastcall Add(const Glvectorgeometry::PIntegerVector texST)/* overload */;
	int __fastcall AddNC(const Glvectorgeometry::PIntegerVector texST)/* overload */;
	void __fastcall Push(const Glvectorgeometry::TTexPoint &Val);
	Glvectorgeometry::TTexPoint __fastcall Pop();
	void __fastcall Insert(int Index, const Glvectorgeometry::TTexPoint &item);
	__property Glvectorgeometry::TTexPoint Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PTexPointArray List = {read=FList};
	HIDESBASE void __fastcall Translate(const Glvectorgeometry::TTexPoint &delta);
	void __fastcall ScaleAndTranslate(const Glvectorgeometry::TTexPoint &scale, const Glvectorgeometry::TTexPoint &delta)/* overload */;
	void __fastcall ScaleAndTranslate(const Glvectorgeometry::TTexPoint &scale, const Glvectorgeometry::TTexPoint &delta, int base, int nb)/* overload */;
	virtual void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TTexPointList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TTexPointList(Glpersistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TIntegerList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	int operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TIntegerVector *FList;
	
protected:
	int __fastcall Get(int Index);
	void __fastcall Put(int Index, const int item);
	virtual void __fastcall SetCapacity(int newCapacity);
	
public:
	__fastcall virtual TIntegerList();
	virtual void __fastcall Assign(System::Classes::TPersistent* src);
	int __fastcall Add(const int item)/* overload */;
	int __fastcall AddNC(const int item)/* overload */;
	void __fastcall Add(const int i1, const int i2)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3)/* overload */;
	void __fastcall Add(TIntegerList* const AList)/* overload */;
	void __fastcall Push(const int Val);
	int __fastcall Pop();
	void __fastcall Insert(int Index, const int item);
	void __fastcall Remove(const int item);
	int __fastcall IndexOf(int item);
	__property int Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PIntegerVector List = {read=FList};
	void __fastcall AddSerie(int aBase, int aDelta, int aCount);
	void __fastcall AddIntegers(const System::PInteger First, int n)/* overload */;
	void __fastcall AddIntegers(TIntegerList* const aList)/* overload */;
	void __fastcall AddIntegers(const int *anArray, const int anArray_High)/* overload */;
	int __fastcall MinInteger();
	int __fastcall MaxInteger();
	void __fastcall Sort();
	void __fastcall SortAndRemoveDuplicates();
	int __fastcall BinarySearch(const int Value)/* overload */;
	int __fastcall BinarySearch(const int Value, bool returnBestFit, bool &found)/* overload */;
	int __fastcall AddSorted(const int Value, const bool ignoreDuplicates = false);
	void __fastcall RemoveSorted(const int Value);
	void __fastcall Offset(int delta)/* overload */;
	void __fastcall Offset(int delta, const int base, const int nb)/* overload */;
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TIntegerList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TIntegerList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<float, 134217728> TSingleArrayList;

typedef TSingleArrayList *PSingleArrayList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSingleList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	float operator[](int Index) { return this->Items[Index]; }
	
private:
	TSingleArrayList *FList;
	
protected:
	float __fastcall Get(int Index);
	void __fastcall Put(int Index, const float item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TSingleList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const float item)/* overload */;
	void __fastcall Add(const float i1, const float i2)/* overload */;
	void __fastcall AddSingles(const System::PSingle First, int n)/* overload */;
	void __fastcall AddSingles(const float *anArray, const int anArray_High)/* overload */;
	void __fastcall Push(const float Val);
	float __fastcall Pop();
	void __fastcall Insert(int Index, const float item);
	__property float Items[int Index] = {read=Get, write=Put/*, default*/};
	__property PSingleArrayList List = {read=FList};
	void __fastcall AddSerie(float aBase, float aDelta, int aCount);
	void __fastcall Offset(float delta)/* overload */;
	void __fastcall Offset(TSingleList* const delta)/* overload */;
	void __fastcall Scale(float factor);
	void __fastcall Sqr();
	void __fastcall Sqrt();
	float __fastcall Sum();
	float __fastcall Min();
	float __fastcall Max();
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TSingleList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TSingleList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<double, 134217728> TDoubleArrayList;

typedef TDoubleArrayList *PDoubleArrayList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDoubleList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	double operator[](int Index) { return this->Items[Index]; }
	
private:
	TDoubleArrayList *FList;
	
protected:
	double __fastcall Get(int Index);
	void __fastcall Put(int Index, const double item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TDoubleList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const double item);
	void __fastcall Push(const double Val);
	double __fastcall Pop();
	void __fastcall Insert(int Index, const double item);
	__property double Items[int Index] = {read=Get, write=Put/*, default*/};
	__property PDoubleArrayList List = {read=FList};
	void __fastcall AddSerie(double aBase, double aDelta, int aCount);
	void __fastcall Offset(double delta)/* overload */;
	void __fastcall Offset(TDoubleList* const delta)/* overload */;
	void __fastcall Scale(double factor);
	void __fastcall Sqr();
	void __fastcall Sqrt();
	double __fastcall Sum();
	float __fastcall Min();
	float __fastcall Max();
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TDoubleList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TDoubleList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TByteList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	System::Byte operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TByteVector *FList;
	
protected:
	System::Byte __fastcall Get(int Index);
	void __fastcall Put(int Index, const System::Byte item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TByteList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const System::Byte item);
	void __fastcall Insert(int Index, const System::Byte item);
	__property System::Byte Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PByteVector List = {read=FList};
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TByteList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TByteList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TQuaternionList : public TBaseVectorList
{
	typedef TBaseVectorList inherited;
	
public:
	Glvectorgeometry::TQuaternion operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TQuaternionArray *FList;
	
protected:
	Glvectorgeometry::TQuaternion __fastcall Get(int Index);
	void __fastcall Put(int Index, const Glvectorgeometry::TQuaternion &item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual TQuaternionList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const Glvectorgeometry::TQuaternion &item)/* overload */;
	int __fastcall Add(const Glvectortypes::TVector3f &item, float w)/* overload */;
	int __fastcall Add(const float X, const float Y, const float Z, const float W)/* overload */;
	void __fastcall Push(const Glvectorgeometry::TQuaternion &Val);
	Glvectorgeometry::TQuaternion __fastcall Pop();
	int __fastcall IndexOf(const Glvectorgeometry::TQuaternion &item);
	int __fastcall FindOrAdd(const Glvectorgeometry::TQuaternion &item);
	void __fastcall Insert(int Index, const Glvectorgeometry::TQuaternion &item);
	__property Glvectorgeometry::TQuaternion Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PQuaternionArray List = {read=FList};
	virtual void __fastcall Lerp(TBaseVectorList* const list1, TBaseVectorList* const list2, float lerpFactor);
	virtual void __fastcall Combine(TBaseVectorList* const list2, float factor);
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TQuaternionList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TQuaternionList(Glpersistentclasses::TVirtualReader* reader) : TBaseVectorList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,1)
struct DECLSPEC_DRECORD T4ByteData
{
	
private:
	struct DECLSPEC_DRECORD _T4ByteData__1
	{
	public:
		System::StaticArray<System::Byte, 4> Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__2
	{
	public:
		int Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__3
	{
	public:
		unsigned Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__4
	{
	public:
		float Value;
	};
	
	
	struct DECLSPEC_DRECORD _T4ByteData__5
	{
	public:
		System::StaticArray<System::Word, 2> Value;
	};
	
	
	
	
public:
	union
	{
		struct 
		{
			_T4ByteData__5 Word;
		};
		struct 
		{
			_T4ByteData__4 Float;
		};
		struct 
		{
			_T4ByteData__3 UInt;
		};
		struct 
		{
			_T4ByteData__2 Int;
		};
		struct 
		{
			_T4ByteData__1 Bytes;
		};
		
	};
};
#pragma pack(pop)


typedef System::StaticArray<T4ByteData, 134217728> T4ByteArrayList;

typedef T4ByteArrayList *P4ByteArrayList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION T4ByteList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	T4ByteData operator[](int Index) { return this->Items[Index]; }
	
private:
	T4ByteArrayList *FList;
	
protected:
	T4ByteData __fastcall Get(int Index);
	void __fastcall Put(int Index, const T4ByteData item);
	virtual void __fastcall SetCapacity(int NewCapacity);
	
public:
	__fastcall virtual T4ByteList();
	virtual void __fastcall Assign(System::Classes::TPersistent* Src);
	int __fastcall Add(const T4ByteData item)/* overload */;
	void __fastcall Add(const float i1)/* overload */;
	void __fastcall Add(const float i1, const float i2)/* overload */;
	void __fastcall Add(const float i1, const float i2, const float i3)/* overload */;
	void __fastcall Add(const float i1, const float i2, const float i3, const float i4)/* overload */;
	void __fastcall Add(const int i1)/* overload */;
	void __fastcall Add(const int i1, const int i2)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3)/* overload */;
	void __fastcall Add(const int i1, const int i2, const int i3, const int i4)/* overload */;
	void __fastcall Add(const unsigned i1)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3, const unsigned i4)/* overload */;
	void __fastcall Add(T4ByteList* const AList)/* overload */;
	void __fastcall Push(const T4ByteData Val);
	T4ByteData __fastcall Pop();
	void __fastcall Insert(int Index, const T4ByteData item);
	__property T4ByteData Items[int Index] = {read=Get, write=Put/*, default*/};
	__property P4ByteArrayList List = {read=FList};
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~T4ByteList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall T4ByteList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLongWordList : public TBaseList
{
	typedef TBaseList inherited;
	
public:
	unsigned operator[](int Index) { return this->Items[Index]; }
	
private:
	Glvectorgeometry::TLongWordVector *FList;
	
protected:
	unsigned __fastcall Get(int Index);
	void __fastcall Put(int Index, const unsigned item);
	virtual void __fastcall SetCapacity(int newCapacity);
	
public:
	__fastcall virtual TLongWordList();
	virtual void __fastcall Assign(System::Classes::TPersistent* src);
	int __fastcall Add(const unsigned item)/* overload */;
	int __fastcall AddNC(const unsigned item)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2)/* overload */;
	void __fastcall Add(const unsigned i1, const unsigned i2, const unsigned i3)/* overload */;
	void __fastcall Add(TLongWordList* const AList)/* overload */;
	void __fastcall Push(const unsigned Val);
	unsigned __fastcall Pop();
	void __fastcall Insert(int Index, const unsigned item);
	void __fastcall Remove(const unsigned item);
	unsigned __fastcall IndexOf(int item);
	__property unsigned Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Glvectorgeometry::PLongWordVector List = {read=FList};
	void __fastcall AddLongWords(const System::PLongWord First, int n)/* overload */;
	void __fastcall AddLongWords(TLongWordList* const aList)/* overload */;
	void __fastcall AddLongWords(const unsigned *anArray, const int anArray_High)/* overload */;
public:
	/* TBaseList.Destroy */ inline __fastcall virtual ~TLongWordList() { }
	
public:
	/* TPersistentObject.CreateFromFiler */ inline __fastcall TLongWordList(Glpersistentclasses::TVirtualReader* reader) : TBaseList(reader) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall QuickSortLists(int startIndex, int endIndex, TSingleList* refList, System::Classes::TList* objList)/* overload */;
extern DELPHI_PACKAGE void __fastcall QuickSortLists(int startIndex, int endIndex, TSingleList* refList, TBaseList* objList)/* overload */;
extern DELPHI_PACKAGE void __fastcall FastQuickSortLists(int startIndex, int endIndex, TSingleList* const refList, Glpersistentclasses::TPersistentObjectList* const objList);
}	/* namespace Glvectorlists */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLVECTORLISTS)
using namespace Glvectorlists;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlvectorlistsHPP
