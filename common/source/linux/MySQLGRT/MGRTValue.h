/*
 *  MGRTValue.h
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/8/10.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#ifndef __MGRTVALUE_H__
#define __MGRTVALUE_H__

#ifdef __APPLE__
#include <MySQLGRT/myx_grt_public_interface.h>
#else
#include "myx_grt_public_interface.h"
#endif

class MGRTValue 
{
  MYX_GRT_VALUE *_value;
  
public:
  MGRTValue() : _value(0) { };
  MGRTValue(const MGRTValue &value) { _value= value.grtValue(); if (_value) myx_grt_value_retain(_value); };
  MGRTValue(MYX_GRT_VALUE *value) : _value(value) { if (_value) myx_grt_value_retain(_value); };
  MGRTValue(int value) { _value= myx_grt_value_from_int(value); };
  MGRTValue(float value) { _value= myx_grt_value_from_real(value); };
  MGRTValue(double value) { _value= myx_grt_value_from_real(value); };
  MGRTValue(const char *value) { _value= myx_grt_value_from_string(value); };
  ~MGRTValue() { if (_value) myx_grt_value_release(_value); };
  
  void release() { if (_value) myx_grt_value_release(_value); };
  void retain() { if (_value) myx_grt_value_retain(_value); };
  
  MGRTValue copy() { return MGRTValue(myx_grt_value_dup(_value)); };
  
  void assign(const MGRTValue &nvalue) { release(); if (nvalue.isValid()) _value= myx_grt_value_retain(nvalue.grtValue()); else _value= NULL; };
  
  static MGRTValue createList(MYX_GRT_VALUE_TYPE contentType=MYX_ANY_VALUE, const char *contentStruct=0)
  {
    return MGRTValue(myx_grt_list_new(contentType, contentStruct));
  }
  static MGRTValue createTypedDict(MYX_GRT_VALUE_TYPE contentType=MYX_ANY_VALUE, const char *contentStruct=0)
  {
    return MGRTValue(myx_grt_dict_new_typed(contentType, contentStruct));
  }
  
  static MGRTValue createObject(MYX_GRT *grt, const char *structName, const char *name=NULL, const MGRTValue &owner=MGRTValue())
  {
    MYX_GRT_VALUE *value= myx_grt_dict_new_obj(grt, structName, name, NULL, 
                                               owner.isValid() ? (owner.type()==MYX_STRING_VALUE ? owner.asString() : owner.dictId()) : NULL);
    MGRTValue tmp(value);
    tmp.release();
    return tmp;
  }

  static MGRTValue refObject(MYX_GRT *grt, const char *rid)
  {
    return MGRTValue(myx_grt_reference_cache_lookup(grt, rid)); 
  }

  static MGRTValue refObject(MYX_GRT *grt, const MGRTValue &rid)
  {
    if (rid.isValid())
      return MGRTValue(myx_grt_reference_cache_lookup(grt, rid.asString()));
    else
      return MGRTValue();
  }
  
  static MGRTValue fromGlobal(MYX_GRT *grt, const char *path)
  {
    return MGRTValue(myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), path));
  }
  
  static MGRTValue fromFile(MYX_GRT *grt, const char *file)
  {
    return MGRTValue(myx_grt_retrieve_from_file(grt, file));
  }

  inline void print(MYX_GRT *grt)
  {
    myx_grt_value_print(grt, _value);
  }
    
  inline void invalidate() { release(); _value= 0; };

  inline bool isValid() const { return _value != 0; };
  inline MYX_GRT_VALUE_TYPE type() const { return myx_grt_value_get_type(_value); };
  inline const char *typeName() const { return myx_get_value_type_as_string(type()); };
  inline MYX_GRT_VALUE_TYPE listContentType() const { return myx_grt_list_content_get_type(_value); };
  inline bool isKindOf(MYX_GRT *grt, const char *struct_name) const { return myx_grt_struct_is_or_inherits_from(grt, contentStruct(), struct_name); };
  inline const char *contentStruct() const { return myx_grt_dict_struct_get_name(_value); };
  inline const char *listContentStruct() const { return myx_grt_list_content_get_struct_name(_value); };

  inline bool inheritsFrom(MYX_GRT *grt, const char *gstruct) { return myx_grt_dict_struct_is_or_inherits_from(grt, _value, gstruct); };

  inline MYX_GRT_STRUCT_MEMBER *structMember(MYX_GRT *grt, const char *name) {
    MYX_GRT_STRUCT *gstruct= myx_grt_struct_get(grt, contentStruct());
    if (gstruct)
      return myx_grt_struct_get_member_by_name(grt, gstruct, name, 1);
    return 0;
  }
  inline bool memberIsRef(MYX_GRT *grt, const char *name) { 
    if (type() == MYX_DICT_VALUE)
      return myx_grt_struct_member_get_is_ref(structMember(grt, name)); 
    return false; 
  }
  inline const char *contentStructOfMember(MYX_GRT *grt, const char *member) const {
    MYX_GRT_STRUCT *gstruct= myx_grt_struct_get(grt, contentStruct());
    if (gstruct)
    {
      MYX_GRT_STRUCT_MEMBER *smember= myx_grt_struct_get_member_by_name(grt, gstruct, member, 1);
      if (smember)
        return myx_grt_struct_member_get_content_struct_name(smember);
    }
    return 0;
  };
  
  inline unsigned int count() const { 
    switch (type())
    {
      case MYX_DICT_VALUE: return myx_grt_dict_item_count(_value); 
      case MYX_LIST_VALUE: return myx_grt_list_item_count(_value);
      default: return 0;
    }
  };
    
  inline unsigned int countComplex() const {
    return myx_grt_dict_item_count_complex(_value);
  };
    
  inline MGRTValue listItem(const char *ident) { 
    unsigned int i, c= myx_grt_list_item_count(_value);
    for (i= 0; i < c; i++)
    {
      MYX_GRT_VALUE *item= myx_grt_list_item_get(_value, i); 
      if (strcmp(myx_grt_dict_id_item_as_string(item), ident)==0)
        return MGRTValue(item);
    }
    return MGRTValue();
  };
  inline MGRTValue listItemNamed(const char *name) { return MGRTValue(myx_grt_list_item_get_by_object_name(_value, name)); };

  inline void complexDictItemByIndex(unsigned int index, const char *&key, MGRTValue &value) { key= myx_grt_dict_item_key_by_index_complex(_value, index); value= MGRTValue(myx_grt_dict_item_value_by_index_complex(_value, index)); };
  inline void dictItemByIndex(unsigned int index, const char *&key, MGRTValue &value) { MYX_GRT_VALUE *gvalue; myx_grt_dict_item_by_index(_value, index, &key, &gvalue); value= MGRTValue(gvalue); };

  inline const char *dictId() const { return myx_grt_dict_id_item_as_string(_value); };

  inline bool compareObject(const MGRTValue &other) { const char *id1, *id2; if (type() == MYX_STRING_VALUE) id1= asString(); else id1= dictId(); if (other.type() == MYX_STRING_VALUE) id2= other.asString(); else id2= other.dictId(); if (!id1 || !id2) return false; return strcmp(id1, id2)==0; };
    
  MGRTValue &operator = (const MGRTValue &other) { if (_value) myx_grt_value_release(_value); _value= (other.isValid() ? myx_grt_value_retain(other.grtValue()) : 0); return *this; };

  inline bool isSet(const char *key) const { return myx_grt_dict_item_get_value(_value, key) != NULL; };
  
  inline void set(const char *key, int value) { myx_grt_dict_item_set_value_from_int(_value, key, value); };
  inline void set(const char *key, unsigned int value) { myx_grt_dict_item_set_value_from_int(_value, key, value); };
  inline void set(const char *key, const char *value) { myx_grt_dict_item_set_value_from_string(_value, key, value); };
  inline void set(const char *key, float value) { myx_grt_dict_item_set_value_from_real(_value, key, value); };
  inline void set(const char *key, MGRTValue &value) { myx_grt_dict_item_set_value(_value, key, value.grtValue()); };
  inline void set(const char *key, MYX_GRT_VALUE *value) { myx_grt_dict_item_set_value(_value, key, value); };

  inline MGRTValue getValue(const char *key) const { MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(_value, key); return MGRTValue(value); };
  inline int get(const char *key, int deflt=0) const { MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(_value, key); if (value) return myx_grt_value_as_int(value); return deflt; };
  inline const char* get(const char *key, const char *deflt=NULL) const { MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(_value, key); if (value) return myx_grt_value_as_string(value); return deflt; };

  inline MGRTValue &append(const MGRTValue &value) { myx_grt_list_item_add(_value, value.grtValue()); return *this; };
  inline void remove(int index) { myx_grt_list_item_del(_value, index); };
  inline void remove(const char *key) { myx_grt_dict_item_del(_value, key); };
  inline void replace(int index, const MGRTValue &value) { myx_grt_list_item_set(_value, index, value.grtValue()); };
  inline void clear() { if (type() == MYX_LIST_VALUE) myx_grt_list_clear(_value); };
  
  inline MYX_GRT_VALUE *grtValue() const { return _value; };
  
  MGRTValue operator [](unsigned int index) const  { return MGRTValue(myx_grt_list_item_get(_value, index)); };
  MGRTValue operator [](int index) const  { return MGRTValue(myx_grt_list_item_get(_value, index)); };
  MGRTValue operator [](const char *key) const  { return MGRTValue(myx_grt_dict_item_get_value(_value, key)); };

  operator MYX_GRT_VALUE*() const { return _value; };
  operator const char *() const { return myx_grt_value_as_string(_value); };
  operator int() const { return myx_grt_value_as_int(_value); };
  operator unsigned int() const { return myx_grt_value_as_int(_value); };
  operator float() const { return myx_grt_value_as_real(_value); };

  inline double asDouble() const { return myx_grt_value_as_real(_value); };
  inline int asInt() const { return myx_grt_value_as_int(_value); };
  inline const char* asString() const { return myx_grt_value_as_string(_value); };
};

#endif 
