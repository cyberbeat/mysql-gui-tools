/* Copyright (c) 2004, 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */


/**
 * @file  myx_grt_struct.c
 * @brief GRT struct handling.
 *
 * See also: <a href="../grt.html#GRTStruct">GRT Structs</a>
 */

#include "myx_grt_private.h"

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <assert.h>
#include <ctype.h>
#include <myx_xml_util_functions.h>
#include <myx_util_functions.h>
#include <myx_shared_util_functions.h>


//#define DEBUGVAL

#ifdef DEBUGVAL
#define VDEBUG(args) g_message args
#else
#define VDEBUG(args)
#endif


static void free_struct_contents(MYX_GRT_STRUCT *gstruct)
{
  unsigned int i;

  g_free(gstruct->name);
  g_free(gstruct->parent_struct_name);
  g_free(gstruct->desc);
  g_free(gstruct->bridge);

  for (i= 0; i < gstruct->members_num; i++)
  {
    MYX_GRT_STRUCT_MEMBER *member= gstruct->members + i;

    g_free(member->name);
    g_free(member->caption);
    g_free(member->desc);
    g_free(member->default_value);
    g_free(member->overrides);
  }
  g_free(gstruct->members);
}

/** 
 ****************************************************************************
 * @brief Frees all memory associated to a GRT struct.
 *
 * @param gstruct GRT struct to free.
 *
 * @return 0
 ****************************************************************************/
int myx_grt_struct_free(MYX_GRT_STRUCT *gstruct)
{
  free_struct_contents(gstruct);
  g_free(gstruct);

  return 0;
}



/** 
 ****************************************************************************
 * @brief Frees a MYX_GRT_STRUCTS object and everything inside it..
 *
 * @param gstructs
 *
 * @return 0
 ****************************************************************************/
int myx_grt_structs_free(MYX_GRT_STRUCTS *gstructs)
{
  unsigned int i;

  for (i= 0; i < gstructs->structs_num; i++)
    free_struct_contents(gstructs->structs+i);
  g_free(gstructs->structs);
  g_free(gstructs);

  return 0;
}



/** 
 ****************************************************************************
 * @brief Register a Struct in the GRT.
 *
 *   Adds the specified struct object to the GRT. The struct needs to be
 * registered in the GRT before it can be used anywhre. If the struct has a parent,
 * either the parent_name can be submitted or, if that is NULL, the \c parent in
 * the \a gstruct must be a pointer to a struct previously registered in the GRT.
 * The struct will be copied, therefore \a gstruct may be freed later.
 *
 * @param grt GRT object
 * @param gstruct
 * @param parent_name name of the parent struct
 *
 * @return MYX_GRT_NO_ERROR or MYX_GRT_DUPLICATE_ENTRY, others
 ****************************************************************************/
MYX_GRT_ERROR myx_grt_struct_register(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct)
{
  MYX_GRT_STRUCT copy;
  unsigned int i;
  
  g_return_val_if_fail(grt!=NULL, MYX_GRT_INTERNAL_ERROR);
  g_return_val_if_fail(gstruct!=NULL, MYX_GRT_INTERNAL_ERROR);
  g_return_val_if_fail(gstruct->name!=NULL, MYX_GRT_INTERNAL_ERROR);

  for (i= 0; i < grt->structs_num; i++)
  {
    if (strcmp(grt->structs[i].name, gstruct->name)==0)
      return MYX_GRT_DUPLICATE_ENTRY;
  }

  copy.name= g_strdup(gstruct->name);
  if (gstruct->parent_struct_name)
    copy.parent_struct_name= g_strdup(gstruct->parent_struct_name);
  else
    copy.parent_struct_name= NULL;

  copy.members_num= gstruct->members_num;
  copy.members= g_new0(MYX_GRT_STRUCT_MEMBER, gstruct->members_num);
  for (i= 0; i < copy.members_num; i++)
  {
    copy.members[i].name= g_strdup(gstruct->members[i].name);
    copy.members[i].value_type= gstruct->members[i].value_type;
    copy.members[i].struct_name= g_strdup(gstruct->members[i].struct_name);

    copy.members[i].content_type= gstruct->members[i].content_type;
    copy.members[i].content_struct_name= g_strdup(gstruct->members[i].content_struct_name);
  }

  grt->structs= vec_insert_resize(grt->structs, sizeof(MYX_GRT_STRUCT),
                                  &grt->structs_num, grt->structs_num,
                                  &copy);
  
  return MYX_GRT_NO_ERROR;
}


/** 
 ****************************************************************************
 * @brief Returns the Struct object with the given name.
 *
 * @param grt  GRT environment object.
 * @param name  name of the struct.
 *
 * @return The named struct object or NULL if its not found.
 ****************************************************************************/
MYX_GRT_STRUCT *myx_grt_struct_get(MYX_GRT *grt, const char *name)
{
  unsigned int i;
  
  g_return_val_if_fail(grt!=NULL, NULL);
  g_return_val_if_fail(name!=NULL, NULL);
  
  for (i= 0; i < grt->structs_num; i++)
  {
    if (strcmp(grt->structs[i].name, name)==0)
      return grt->structs+i;
  }

  return NULL;
}


/** 
 ****************************************************************************
 * @brief Checks whether a struct inherits in some way from another one.
 *
 * @param grt GRT environment object.
 * @param struct_name  Name of the struct to check.
 * @param parent_name  Name of a struct possibly higher in the hierarchy of
 *   \a struct_name
 *
 * @return 1 if struct_name inherits in some way from parent_name
 * @return 0 if not
 * @return -1 if there's some error (such as one of the named structs not existing)
 ****************************************************************************/
int myx_grt_struct_inherits_from(MYX_GRT *grt, const char *struct_name, const char *parent_name)
{
  MYX_GRT_STRUCT *gstruct;
  
  g_return_val_if_fail(grt!=NULL, -1);
  g_return_val_if_fail(struct_name!=NULL, -1);
  g_return_val_if_fail(parent_name!=NULL, -1);

  gstruct= myx_grt_struct_get(grt, struct_name);
  
  if (!gstruct)
    return -1;
  
  while (gstruct->parent_struct_name)
  {
    if (strcmp(gstruct->parent_struct_name, parent_name)==0)
      return 1;

    if (!(gstruct= myx_grt_struct_get(grt, gstruct->parent_struct_name)))
      return -1;
  }
  
  return 0;
}

/** 
 ****************************************************************************
 * @brief Checks whether a struct is the same as the given one or if it 
 * inherits in some way from another one.
 *
 * @param grt GRT environment object.
 * @param struct_name  Name of the struct to check.
 * @param parent_name  Name of a struct possibly higher in the hierarchy of
 *   \a struct_name
 *
 * @return 1 if struct_name inherits in some way from parent_name
 * @return 0 if not
 * @return -1 if there's some error (such as one of the named structs not existing)
 ****************************************************************************/
int myx_grt_struct_is_or_inherits_from(MYX_GRT *grt, const char *struct_name, const char *parent_name)
{
  if (struct_name && struct_name[0] && (strcmp2(struct_name, parent_name) == 0))
    return 1;
  else
    return myx_grt_struct_inherits_from(grt, struct_name, parent_name);
}


/** 
 ****************************************************************************
 * @brief Gets the name of the struct.
 *
 * @param gstruct  
 *
 * @return Name of the struct. Do not free it.
 ****************************************************************************/
const char *myx_grt_struct_get_name(MYX_GRT_STRUCT *gstruct)
{
  g_return_val_if_fail(gstruct != NULL, NULL);
  
  return gstruct->name;
}


/** 
 ****************************************************************************
 * @brief Gets the name of the parent struct.
 *
 * @param gstruct  
 *
 * @return Name of the parent struct. Do not free it.
 ****************************************************************************/
const char * myx_grt_struct_get_parent_name(MYX_GRT_STRUCT *gstruct)
{
  g_return_val_if_fail(gstruct != NULL, NULL);
  
  return gstruct->parent_struct_name;
}


/** 
 ****************************************************************************
 * @brief Gets the caption of the struct. 
 * 
 * If there is none, the caption from a parent struct is taken and the string 
 *   "child" is added
 *
 * @param grt the GRT environment
 * @param gstruct the GRT struct
 * @param inherited_caption pointer to a integer that will be set to 0 or 1 
 *   whether the caption was taken from a parent struct
 *
 * @return Caption of the struct. Do not free it.
 ****************************************************************************/
const char * myx_grt_struct_get_caption(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, int *inherited_caption)
{
  g_return_val_if_fail(gstruct != NULL, NULL);

  *inherited_caption= 0;
  
  if ((gstruct->caption) && (gstruct->caption[0]))
    return gstruct->caption;
  else
  {
    *inherited_caption= 1;

    while (gstruct->parent_struct_name)
    {
      if (!(gstruct= myx_grt_struct_get(grt, gstruct->parent_struct_name)))
        return "";

      if ((gstruct->caption) && (gstruct->caption[0]))
        return gstruct->caption;
    }

    return "";
  }
}


/** 
 ****************************************************************************
 * @brief Gets the description of the struct. 
 *
 * If there is none, the description is taken from a parent struct
 *
 * @param grt the GRT environment
 * @param gstruct the GRT struct
 *
 * @return Description of the struct. Do not free it.
 ****************************************************************************/
const char * myx_grt_struct_get_desc(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct)
{
  g_return_val_if_fail(gstruct != NULL, NULL);
  
  if ((gstruct->desc) && (gstruct->desc[0]))
    return gstruct->desc;
  else
  {
    while (gstruct->parent_struct_name)
    {
      if (!(gstruct= myx_grt_struct_get(grt, gstruct->parent_struct_name)))
        return "";

      if ((gstruct->desc) && (gstruct->desc[0]))
        return gstruct->desc;
    }

    return "";
  }
}

/** 
 ****************************************************************************
 * @brief Returns the bridge module that is responsible to handle this struct. 
 *
 * The function performs a lookup of the module that is supposed to handle
 * dicts of this struct and returns it.
 *
 * @param grt the GRT environment
 * @param gstruct the GRT struct
 *
 * @return The module that is responsible to handle the struct or NULL
 ****************************************************************************/
MYX_GRT_MODULE * myx_grt_struct_get_bridge(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct)
{
  g_return_val_if_fail(gstruct != NULL, NULL);

  return myx_grt_module_get(grt, gstruct->bridge);
}

/** 
 ****************************************************************************
 * @brief Returns the name of the bridge module that is responsible to handle 
 * this strcut. 
 *
 * @param gstruct the GRT struct
 *
 * @return The name of the module that is responsible to handle the struct or 
 * NULL if this struct is not handled by a bridge
 ****************************************************************************/
const char * myx_grt_struct_get_bridge_name(MYX_GRT_STRUCT *gstruct)
{
  g_return_val_if_fail(gstruct != NULL, NULL);

  return gstruct->bridge;
}

/** 
 ****************************************************************************
 * @brief Return number of members in the struct.
 *
 * @param gstruct
 *
 * @return Number of members.
 ****************************************************************************/
unsigned int myx_grt_struct_get_member_count(MYX_GRT_STRUCT *gstruct)
{
  if (gstruct == NULL)
    g_return_val_if_fail(gstruct != NULL, 0);
  
  return gstruct->members_num;
}

MYX_GRT_STRUCT_MEMBER * myx_grt_struct_get_member_by_index(MYX_GRT_STRUCT *gstruct, unsigned int index)
{
  g_return_val_if_fail(gstruct != NULL, NULL);
  g_return_val_if_fail(index < gstruct->members_num, NULL);

  return gstruct->members + index;
}

MYX_GRT_STRUCT_MEMBER * myx_grt_struct_get_member_by_name(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, 
                                                          const char *name, int check_parent_structs)
{
  unsigned int i;

  for (i= 0; i<myx_grt_struct_get_member_count(gstruct); i++)
  {
    MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_index(gstruct, i);

    if (strcmp2(name, member->name) == 0)
      return member;
  }

  if ((check_parent_structs == 1) && (gstruct->parent_struct_name) && (gstruct->parent_struct_name[0]))
  {
    MYX_GRT_STRUCT *parent_struct= myx_grt_struct_get(grt, gstruct->parent_struct_name);

    if (parent_struct)
      return myx_grt_struct_get_member_by_name(grt, parent_struct, name, 1);
  }

  return NULL;
}

/** 
 ****************************************************************************
 * @brief Return total number of members in the struct, including inherited members
 *   or finds the member with the given index
 *
 * @param grt
 * @param gstruct
 * @param excluding_struct_name
 * @param member_cache
 * @param member
 * @param index set this to greater or equal 0 to set the member pointer to the correct member
 *
 * @return Number of members, including inherited members
 ****************************************************************************/
int grt_struct_get_member_count_total(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, const char *excluding_struct_name,
                                      unsigned int index, MYX_GRT_STRUCT_MEMBER **member, GHashTable *member_cache)
{
  unsigned int count= 0;
  const char *parent_struct_name = myx_grt_struct_get_parent_name(gstruct);
  GHashTable *l_member_cache= NULL;

  int free_l_member_cache= 0;
  unsigned int i;

  if (member_cache)
  {
    l_member_cache= member_cache;
  }
  else
  {
    l_member_cache= g_hash_table_new(g_str_hash, g_str_equal);
    free_l_member_cache= 1;
  }

  for (i= 0; i < myx_grt_struct_get_member_count(gstruct); i++)
  {
    MYX_GRT_STRUCT_MEMBER *struct_member= myx_grt_struct_get_member_by_index(gstruct, i);
    MYX_GRT_STRUCT_MEMBER *current_member= NULL;
    const char *member_name= myx_grt_struct_get_member_name(struct_member);
      
    // do not perform a lookup when the l_member_cache has just been created
    if (!free_l_member_cache)
      current_member= g_hash_table_lookup(l_member_cache, member_name);

    // check if this member has already been added because it was overloaded
    if (current_member)
    {
      //if it is, do nothing since the member was already added before
    }
    else
    {
      //if not add the member to the member cache
      g_hash_table_insert(l_member_cache, (gpointer) member_name, (gpointer) struct_member);

      if (index == count)
      {
        *member= struct_member;
        return ++count;
      }

      count++;
    }
  }

  // loop recursive over all parent structs and sum the member count
  if (parent_struct_name)
  {
    if ( (!excluding_struct_name) || (strcmp2(parent_struct_name, excluding_struct_name) != 0) )
      count+= grt_struct_get_member_count_total(grt,
        myx_grt_struct_get(grt, myx_grt_struct_get_parent_name(gstruct)), excluding_struct_name,
          index - count, member, l_member_cache);
  }
  
  if (free_l_member_cache)
    g_hash_table_destroy(l_member_cache);

  return count;
}

/** 
 ****************************************************************************
 * @brief Return total number of members in the struct, including inherited members
 *
 * @param gstruct
 *
 * @return Number of members, including inherited members
 ****************************************************************************/
int myx_grt_struct_get_member_count_total(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct)
{
  return grt_struct_get_member_count_total(grt, gstruct, NULL, -1, NULL, NULL);

  /*unsigned int count= myx_grt_struct_get_member_count(gstruct);

  // loop recursive over all parent structs and sum the member count
  if (myx_grt_struct_get_parent_name(gstruct))
    count+= myx_grt_struct_get_member_count_total(grt,
      myx_grt_struct_get(grt, myx_grt_struct_get_parent_name(gstruct)));
  
  return count;*/
}

/** 
 ****************************************************************************
 * @brief Return total number of members in the struct, including inherited 
 *   members but stop looking up the struct hierachie at the given struct
 *
 * @param gstruct
 *
 * @return Number of members, including inherited members but stopping lookup
 *   with the given struct
 ****************************************************************************/
int myx_grt_struct_get_member_count_total_excluding_struct(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, const char *excluding_struct_name)
{
  return grt_struct_get_member_count_total(grt, gstruct, excluding_struct_name, -1, NULL, NULL);
}

MYX_GRT_STRUCT_MEMBER * myx_grt_struct_get_member_by_index_total(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, int index)
{
  MYX_GRT_STRUCT_MEMBER *member= NULL;

  grt_struct_get_member_count_total(grt, gstruct, NULL, index, &member, NULL);

  if (member)
    return member;
  else
  {
    grt_struct_get_member_count_total(grt, gstruct, NULL, index, &member, NULL);
    return NULL;
  }
}



/** 
 ****************************************************************************
 * @brief Returns name of member.
 *
 * @param member  
 *
 * @return Name of the member.
 ****************************************************************************/
const char *myx_grt_struct_get_member_name(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, NULL);

  return member->name;
}

/** 
 ****************************************************************************
 * @brief Returns the default value of the member.
 *
 * @param member  
 *
 * @return Default value of the member.
 ****************************************************************************/
const char * myx_grt_struct_get_member_default(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, NULL);

  return member->default_value;
}

/** 
 ****************************************************************************
 * @brief Returns the caption of the member.
 *
 * @param member  
 *
 * @return Caption of the member.
 ****************************************************************************/
const char * myx_grt_struct_get_member_caption(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, const char *member_name, int check_parent_structs)
{
  MYX_GRT_STRUCT_MEMBER *member;
  g_return_val_if_fail(gstruct!=NULL, NULL);

  member= myx_grt_struct_get_member_by_name(grt, gstruct, member_name, 1);

  if (member)
  {
    if ( (!member->caption) && (check_parent_structs) && (gstruct->parent_struct_name) )
    {
      MYX_GRT_STRUCT *parent_struct= myx_grt_struct_get(grt, gstruct->parent_struct_name);

      return myx_grt_struct_get_member_caption(grt, parent_struct, member_name, 1);
    }
    else
      return member->caption;
  }
  else
    return NULL;
}

/** 
 ****************************************************************************
 * @brief Returns the description of the member.
 *
 * @param member  
 *
 * @return Description of the member.
 ****************************************************************************/
const char * myx_grt_struct_get_member_desc(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, NULL);

  return member->desc;
}


/** 
 ****************************************************************************
 * @brief Gets the type of the struct member.
 *
 * @param member The member  
 * 
 * @return The type of the struct member or -1 on error.
 ****************************************************************************/
MYX_GRT_VALUE_TYPE myx_grt_struct_member_get_type(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, -1);

  return member->value_type;
}

/** 
 ****************************************************************************
 * @brief Gets the content type of the struct member.
 *
 * @param member The member  
 * 
 * @return The content type of the struct member or -1 on error.
 ****************************************************************************/
MYX_GRT_VALUE_TYPE myx_grt_struct_member_get_content_type(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, -1);

  return member->content_type;
}

/** 
 ****************************************************************************
 * @brief Gets the struct_name of the struct member.
 *
 * @param member The member
 * 
 * @return The struct_name of the struct member or NULL on error.
 ****************************************************************************/
const char * myx_grt_struct_member_get_struct_name(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, NULL);
  g_return_val_if_fail(member->value_type == MYX_DICT_VALUE, NULL);

  return member->struct_name;
}

/** 
 ****************************************************************************
 * @brief Gets the struct_name of the struct member.
 *
 * @param member The member
 * 
 * @return The struct_name of the struct member or NULL on error.
 ****************************************************************************/
const char * myx_grt_struct_member_get_content_struct_name(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, NULL);
  g_return_val_if_fail((member->value_type == MYX_DICT_VALUE || 
    member->value_type == MYX_LIST_VALUE ||
    member->value_type == MYX_STRING_VALUE), NULL);

  return member->content_struct_name;
}

unsigned int myx_grt_struct_member_get_is_ref(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, 0);

  return member->is_ref;
}

const char * myx_grt_struct_member_get_content_struct_name_overridden(MYX_GRT_STRUCT_MEMBER *member)
{
  g_return_val_if_fail(member!=NULL, 0);

  if (member->overrides)
    return member->overrides;
  else
    return member->content_struct_name;
}


static int validate_dict(MYX_GRT *grt, 
                         MYX_GRT_STRUCT *gstruct, MYX_GRT_VALUE *dict, 
                         const char **validated_items,
                         int strict)
{
  unsigned int i, j;
  
  VDEBUG(("validating against struct %s", gstruct->name));

  // Check if all struct members are present in the dict
  for (i= 0; i<gstruct->members_num; i++)
  {
    int found, ok;
    MYX_GRT_STRUCT_MEMBER *member= gstruct->members + i;
    
    found= 0;
    // Check whether a member with this name was already validated
    for (j= 0; j < dict->value.d->items_num; j++)
    {
      if (validated_items[j] && strcmp(validated_items[j], member->name)==0)
      {
        found= 1;
        break;
      }
    }
    if (found)
      continue;

    ok= 0;
    for (j= 0; j<dict->value.d->items_num; j++)
    {
      MYX_GRT_DICT_ITEM *item= dict->value.d->items + j;

      //Check for the same name
      if (strcmp(member->name, item->key) == 0)
      {
        found= 1;
        // Check for the same type
        if (member->value_type == item->value->type)
        {
          // lists
          if (member->value_type == MYX_LIST_VALUE)
          {
            // Check the list's content_type
            if (member->content_type == item->value->value.l->content_type)
            {
              // If it is a list of structs, check content_struct_name also
              if (member->content_type != MYX_DICT_VALUE ||
                  strcmp3(member->content_struct_name, item->value->value.l->content_struct_name)==0)
              {
                ok= 1;
              }
              else // mismatch
              {
                VDEBUG(("mismatch of list content struct, member:%s (%s != %s)",
                       member->name,
                       member->value_struct_name, 
                       item->value->value.l->content_struct_name));
              }
            }
            else // list content type mismatch
            {
              VDEBUG(("mismatch of list content type, member:%s (%i != %i)", 
                     member->name,
                     member->value_content_type, 
                     item->value->value.l->content_type));
            }
          } 
          // dicts
          else if (member->value_type == MYX_DICT_VALUE)
          {
            // Check struct name
            if (!member->struct_name)
            {
              ok= 1;
            }
            else if (!item->value->value.d->struct_name ||
                     strcmp(member->struct_name, item->value->value.d->struct_name) == 0)
            {
              ok= myx_grt_dict_struct_validate(grt, item->value, member->struct_name, strict);
            }
            else
            {
              VDEBUG(("struct mismatch for a member dictionary:%s (%s!=%s)",
                     member->name,
                     member->value_struct_name, 
                     item->value->value.d->struct_name));
            }
          }
          else
          {
            ok= 1;
          }
        }
        else // the member has a different type from what's expected
        {
          VDEBUG(("type mismatch of member %s: (%i != %i)", member->name,
                 member->value_type, item->value->type));
        }

        // record that we already checked this item
        validated_items[j]= item->key;

        break;
      }
    }

    if (found)
    {
      if (!ok)
        return 0;
      else 
        VDEBUG(("member %s ok", member->name));
    }
  }
  return 1;
}


/** 
 ****************************************************************************
 * @brief Validates a dict value against a struct.
 *
 * @param grt
 * @param dict  GRT dict value to validate.
 * @param struct_name name of the struct to use for validation
 * @param strict  1 if members not declared in the struct should be accepted
 *    0 if they should be rejected.
 *
 * @return 1 if the value was correctly validated, 0 if not. If any of the
 *    structs is not registered in the GRT, it will result in a validation
 *    error. A warning message will be printed.
 ****************************************************************************/
int myx_grt_dict_struct_validate(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name, int strict)
{
  MYX_GRT_STRUCT *gstruct;
  const char **validated_items;
  int ok;

  g_return_val_if_fail(grt!=NULL, 0);
  g_return_val_if_fail(struct_name!=NULL, 0);
  g_return_val_if_fail(dict!=NULL, 0);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, 0);

  //if the struct name holds an empty string, it's ok
  if (!struct_name[0])
    return 1;
  

  gstruct= myx_grt_struct_get(grt, struct_name);
  if (!gstruct)
  {
    g_warning("invalid struct '%s' during validation", struct_name);
    return 0;
  }
  
  
  // This array will contain the list of all items in the dict
  // that were already validated. It's used to make sure that
  // overriden values are not checked twice.
  validated_items= g_new0(const char*, dict->value.d->items_num);

  do 
  {
    if (!validate_dict(grt, gstruct, dict, validated_items, strict))
    {
      g_free((char*)validated_items);
      return 0;
    }

    if (gstruct->parent_struct_name)
    {
      gstruct= myx_grt_struct_get(grt, gstruct->parent_struct_name);
      if (!gstruct)
      {
        g_free((char*)validated_items);
        g_warning("invalid struct '%s' during validation", struct_name);
        return 0;
      }
    }
    else
      gstruct= NULL;
  } while (gstruct);

  
  ok= 1;
  // If strict is set, check if there are any keys that are not in the struct
  if (strict)
  {
    unsigned int i;
    
    // Check by looking at the validated_items list. If there's
    // space left in the array, it means that some item was not validated
    // in the previous step.
    for (i= 0; i < dict->value.d->items_num; i++)
    {
      if (!validated_items[i])
      {
        ok= 0;
        break;
      }
    }
  }

  g_free((char*)validated_items);

  return 1;
}




static void struct_to_xml(MYX_GRT *grt, xmlNodePtr parent, MYX_GRT_STRUCT *gstruct)
{
  unsigned int i;
  xmlNodePtr node;  
  xmlNodePtr child_structs_node= NULL;

  node= xmlNewTextChild(parent, NULL, (xmlChar*)"gstruct", NULL);
  xmlNewProp(node, (xmlChar*)"name", (xmlChar*)gstruct->name);

  if ((gstruct->parent_struct_name) && (gstruct->parent_struct_name[0]))
    xmlNewProp(node, (xmlChar*)"parent", (xmlChar*)gstruct->parent_struct_name);

  if ((gstruct->caption) && (gstruct->caption[0]))
    xmlNewProp(node, (xmlChar*)"caption", (xmlChar*)gstruct->caption);

  if ((gstruct->desc) && (gstruct->desc[0]))
    xmlNewProp(node, (xmlChar*)"desc", (xmlChar*)gstruct->desc);

  if ((gstruct->bridge) && (gstruct->bridge[0]))
    xmlNewProp(node, (xmlChar*)"bridge", (xmlChar*)gstruct->bridge);


  if (gstruct->members_num>0)
  {
    xmlNodePtr members_node= xmlNewTextChild(node, NULL, (xmlChar*)"members", NULL);

    for (i= 0; i<gstruct->members_num; i++)
    {
      MYX_GRT_STRUCT_MEMBER *member= gstruct->members + i;
      xmlNodePtr member_node= xmlNewTextChild(members_node, NULL, (xmlChar*)"member", NULL);
      xmlNewProp(member_node, (xmlChar*)"name", (xmlChar*)member->name);

      switch (member->value_type)
      {
      case MYX_ANY_VALUE:
        break;
      case MYX_INT_VALUE:
        xmlNewProp(member_node, (xmlChar*)"type", (xmlChar*)"int");
        break;
      case MYX_REAL_VALUE:
        xmlNewProp(member_node, (xmlChar*)"type", (xmlChar*)"real");
        break;
      case MYX_STRING_VALUE:
        xmlNewProp(member_node, (xmlChar*)"type", (xmlChar*)"string");

        if (member->is_ref)
          xmlNewProp(member_node, (xmlChar*)"option", (xmlChar*)"ref");
        if (member->struct_name)
          xmlNewProp(member_node, (xmlChar*)"content-struct-name", (xmlChar*)member->content_struct_name);

        break;
      case MYX_LIST_VALUE:
        xmlNewProp(member_node, (xmlChar*)"type", (xmlChar*)"list");

        if (member->is_ref)
          xmlNewProp(member_node, (xmlChar*)"option", (xmlChar*)"ref");

        break;
      case MYX_DICT_VALUE:
        xmlNewProp(member_node, (xmlChar*)"type", (xmlChar*)"dict");

        if (member->struct_name)
          xmlNewProp(member_node, (xmlChar*)"struct-name", (xmlChar*)member->struct_name);

        break;
      }

      // store typed list / dicts infos
      if ((member->value_type == MYX_LIST_VALUE) || (member->value_type == MYX_DICT_VALUE))
      {
        xmlNewProp(member_node, (xmlChar*)"content-type", (xmlChar*)myx_get_value_type_as_string(member->content_type));

        if (member->content_struct_name)
          xmlNewProp(member_node, (xmlChar*)"content-struct-name", (xmlChar*)member->content_struct_name);
      }

      if ((member->default_value) && (member->default_value[0]))
        xmlNewProp(member_node, (xmlChar*)"default", (xmlChar*)member->default_value);

      if ((member->overrides) && (member->overrides[0]))
        xmlNewProp(member_node, (xmlChar*)"overrides", (xmlChar*)member->overrides);

      
      if ((member->caption) && (member->caption[0]))
        xmlNewProp(member_node, (xmlChar*)"caption", (xmlChar*)member->caption);

      if ((member->desc) && (member->desc[0]))
        xmlNewProp(member_node, (xmlChar*)"desc", (xmlChar*)member->desc);
    }
  }

  /* if grt == NULL, then don't dump the children */
  if (grt)
  {
    //Loop over all inherited
    for (i= 0; i < grt->structs_num; i++)
    {
      if (strcmp2(grt->structs[i].parent_struct_name, gstruct->parent_struct_name)==0)
      {
        if (!child_structs_node)
          child_structs_node= xmlNewTextChild(node, NULL, (xmlChar*)"child_structs", NULL);
        
        struct_to_xml(grt, child_structs_node, grt->structs+i);
      }
    }
  }
}

/*
 ****************************************************************************
 * @brief Loads a GRT struct tree from a XML file.
 *
 * Static function, used by myx_grt_struct_to_xml. If destination is a pointer
 * to NULL, new memory is allocated. Otherwise it has to point to an already 
 * allocated block of memory
 * 
 * @param node node to parse
 * @param destination pointer a to where loaded struct should be placed
 *
 * @return The parsed MYX_GRT_STRUCT
 ****************************************************************************/
static MYX_GRT_STRUCT *xml_to_struct(xmlNodePtr node, MYX_GRT_STRUCTS **gstructs)
{
  xmlChar *node_property= xmlGetProp(node, (xmlChar*)"name");
  xmlNodePtr child_node;
  unsigned int gstruct_index;

  if (xmlStrcmp(node->name, (xmlChar*)"gstruct") != 0)
  {
    g_warning("[XML parser] Node '%s': 'gstruct' expected.", node->name);
    return NULL;
  }

  if (!node_property)
  {
    g_warning("[XML parser] Node '%s' does not have a name property.", node->name);
    return NULL;
  }

  if (!*gstructs)
  {
    *gstructs= g_malloc0(sizeof(MYX_GRT_STRUCTS));
    gstruct_index= 0;
    (*gstructs)->structs_num= 1;
    (*gstructs)->structs= g_malloc0(sizeof(MYX_GRT_STRUCT));
  }
  else
  {
    gstruct_index= (*gstructs)->structs_num;
    (*gstructs)->structs_num++;
    (*gstructs)->structs= g_realloc((*gstructs)->structs, sizeof(MYX_GRT_STRUCT) * (*gstructs)->structs_num);
  }

// use this for convenience, because getting a pointer to the struct array is 
// not good, as the gstructs->structs array may change while parsing a child
// struct recursively
#define GSTRUCT ((*gstructs)->structs+gstruct_index)

  memset(GSTRUCT, 0, sizeof(MYX_GRT_STRUCT));

  GSTRUCT->name= g_strdup((char*)node_property);
  xmlFree(node_property);
  
  node_property= xmlGetProp(node, (xmlChar*)"parent");
  GSTRUCT->parent_struct_name= g_strdup((char*)node_property);
  xmlFree(node_property);

  node_property= xmlGetProp(node, (xmlChar*)"caption");
  GSTRUCT->caption= g_strdup((char*)node_property);
  xmlFree(node_property);

  node_property= xmlGetProp(node, (xmlChar*)"desc");
  GSTRUCT->desc= g_strdup((char*)node_property);
  xmlFree(node_property);

  node_property= xmlGetProp(node, (xmlChar*)"bridge");
  GSTRUCT->bridge= g_strdup((char*)node_property);
  xmlFree(node_property);


  // read in members and child_structs
  child_node= node->children;
  while (child_node)
  {
    // members
    if (xmlStrcmp(child_node->name, (xmlChar*)"members")==0)
    {
      xmlNodePtr member_node= child_node->children;
      MYX_GRT_STRUCT_MEMBER *member;

      GSTRUCT->members_num= get_child_count(child_node, (xmlChar*)"member");
      GSTRUCT->members= g_malloc0(sizeof(MYX_GRT_STRUCT_MEMBER)*GSTRUCT->members_num);

      member= GSTRUCT->members;
      while (member_node)
      {
        if (xmlStrcmp(member_node->name, (xmlChar*)"member")==0)
        {
          xmlChar *node_type= xmlGetProp(member_node, (xmlChar*)"type");
          
          member->name= g_strdup((char*)xmlGetProp(member_node, (xmlChar*)"name"));

          node_property= xmlGetProp(member_node, (xmlChar*)"default");
          member->default_value= g_strdup((char*)node_property);
          xmlFree(node_property);

          node_property= xmlGetProp(member_node, (xmlChar*)"overrides");
          member->overrides= g_strdup((char*)node_property);
          xmlFree(node_property);

          node_property= xmlGetProp(member_node, (xmlChar*)"caption");
          member->caption= g_strdup((char*)node_property);
          xmlFree(node_property);

          node_property= xmlGetProp(member_node, (xmlChar*)"desc");
          member->desc= g_strdup((char*)node_property);
          xmlFree(node_property);
          
          if (xmlStrcmp(node_type, (xmlChar*)"int")==0)
            member->value_type= MYX_INT_VALUE;
          else 
            if (xmlStrcmp(node_type, (xmlChar*)"real") == 0)
              member->value_type= MYX_REAL_VALUE;
            else 
              if (xmlStrcmp(node_type, (xmlChar*)"string")==0)
              {
                xmlChar *option= xmlGetProp(member_node, (xmlChar*)"option");
                xmlChar *content_struct_name= xmlGetProp(member_node, (xmlChar*)"content-struct-name");

                member->value_type= MYX_STRING_VALUE;
                if (strcmp2((char*)option, "ref") == 0)
                  member->is_ref= 1;
                if (content_struct_name)
                  member->content_struct_name= g_strdup((char*)content_struct_name);

                if (option)
                  xmlFree(option);
                if (content_struct_name)
                  xmlFree(content_struct_name);
              }
              else 
                if (xmlStrcmp(node_type, (xmlChar*)"list") == 0)
                {
                  xmlChar *option= xmlGetProp(member_node, (xmlChar*)"option");

                  member->value_type= MYX_LIST_VALUE;

                  if (strcmp2((char*)option, "ref") == 0)
                    member->is_ref= 1;

                  if (option)
                    xmlFree(option);
                }
                else 
                  if (xmlStrcmp(node_type, (xmlChar*)"dict")==0)
                  {
                    xmlChar *struct_name= xmlGetProp(member_node, (xmlChar*)"struct-name");

                    member->value_type= MYX_DICT_VALUE;

                    if (struct_name) 
                      member->struct_name= g_strdup((char*)struct_name);

                    if (struct_name)
                      xmlFree(struct_name);
                  }
                  else
                    g_warning("[XML parser] Node '%s' contains member '%s' with unknown type '%s'.", 
                      GSTRUCT->name, member->name, node_type);

          // lists and dicts can be typed
          if ((member->value_type == MYX_LIST_VALUE) || (member->value_type == MYX_DICT_VALUE)) 
          {
            xmlChar *content_type= xmlGetProp(member_node, (xmlChar*)"content-type");
            xmlChar *content_struct_name= xmlGetProp(member_node, (xmlChar*)"content-struct-name");
            MYX_GRT_ERROR error= MYX_GRT_NO_ERROR;

            if (content_type)
              member->content_type= myx_get_value_type_from_string((char*)content_type, &error);
            if (error != MYX_GRT_NO_ERROR)
              g_warning("[XML parser] Node '%s' contains member '%s' with unknown type '%s'.", 
                GSTRUCT->name, member->name, content_type);

            if (content_struct_name)
              member->content_struct_name= g_strdup((char*)content_struct_name);

            if (content_type)
              xmlFree(content_type);
            if (content_struct_name)
              xmlFree(content_struct_name);
          }
          member++;
        }
        member_node= member_node->next;
      }
    }
    // child_structs
    else if (xmlStrcmp(child_node->name, (xmlChar*)"child_structs")==0)
    {
      xmlNodePtr child_struct_node= child_node->children;

      while (child_struct_node)
      {
        if (xmlStrcmp(child_struct_node->name, (xmlChar*)"gstruct")==0)
        {
          MYX_GRT_STRUCT *child_struct;

          child_struct= xml_to_struct(child_struct_node, gstructs);
          if (child_struct)
          {
            // override if the child already had a parent value set
            if (child_struct->parent_struct_name != NULL)
              g_free(child_struct->parent_struct_name);
            // assign parent of the child to ourselves
            child_struct->parent_struct_name= g_strdup(GSTRUCT->name);
          }
        }

        child_struct_node= child_struct_node->next;
      }
    }

    child_node= child_node->next;
  }

  return GSTRUCT;
#undef GSTRUCT
}


/** 
 ****************************************************************************
 * @brief Generates a XML representation of the given struct.
 *
 * Generates a string containing a XML representation of the struct, suitable
 * for storing in a file. The
 * 
 * @param grt
 * @param gstruct
 * @param include_children whether the include children structs of \a gstruct
 *      in the XML
 *
 * @return A newly allocated string containing the XML or NULL on error.
 *  Must be freed.
 ****************************************************************************/
char *myx_grt_struct_to_xml(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, 
                            int include_children)
{  
  xmlDocPtr doc;
  xmlNodePtr root, child;
  xmlChar *buffer= NULL;
  int size;
  
  g_return_val_if_fail(gstruct!=NULL, NULL);
  
  doc= xmlNewDoc((xmlChar*)"1.0");
  doc->children= root= xmlNewDocRawNode(doc, NULL, (xmlChar*)"gstructs", NULL);

  child= xmlNewTextChild(root, NULL, (xmlChar*)"gstruct", NULL);
  
  struct_to_xml(include_children ? grt : NULL, child, gstruct);

  xmlDocDumpFormatMemory(doc, &buffer, &size, 1);

  xmlFreeDoc(doc);

  return (char*)buffer;
}


/** 
 ****************************************************************************
 * @brief Reconstruct a struct from its XML representation.
 *
 * @param str  a string containing XML data.
 * @param size  length of string
 *
 * @return A new XML struct.
 ****************************************************************************/
MYX_GRT_STRUCTS *myx_grt_struct_from_xml(const char *str, size_t size)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  MYX_GRT_STRUCTS *gstructs= g_malloc(sizeof(MYX_GRT_STRUCTS));

  g_return_val_if_fail(str!=NULL, NULL);

  doc= xmlParseMemory(str, (int)size);
  
  if (!doc)
  {
    g_warning("Could not parse XML data");
    return NULL;
  }

  gstructs= NULL;
  root= xmlDocGetRootElement(doc);
  if (root && xmlStrcmp(root->name, (xmlChar*)"gstructs")==0)
  {
    root= root->children;
    while (root)
    {
      if (xmlStrcmp(root->name, (xmlChar*)"gstruct")==0)
      {
        xml_to_struct(root, &gstructs);
        break;
      }
    }
  }

  xmlFreeDoc(doc);
  
  return gstructs;
}


/** 
 ****************************************************************************
 * @brief Saves a list of structs to a XML file.
 * 
 * This will save the structs in a XML file as a flat list of structs, ie.
 * \b not as a tree of structs with their respective children.
 *
 * @param gstructs  A structs object with the list of structs to save.
 * @param filename  Name of the file to store the data.
 *
 * @return MYX_GRT_NO_ERROR, MYX_GRT_CANT_OPEN_FILE 
 * @see myx_grt_struct_load_list
 ****************************************************************************/
MYX_GRT_ERROR myx_grt_struct_save_list(MYX_GRT_STRUCTS *gstructs, const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  int res;
  unsigned int i;

  g_return_val_if_fail(gstructs!=NULL, MYX_GRT_INTERNAL_ERROR);
  g_return_val_if_fail(filename!=NULL, MYX_GRT_INTERNAL_ERROR);

  doc= xmlNewDoc((xmlChar*)"1.0");
  doc->children= root= xmlNewDocRawNode(doc, NULL, (xmlChar*)"gstructs", NULL);

  for (i= 0; i < gstructs->structs_num; i++)
  {
    struct_to_xml(NULL, root, gstructs->structs+i);
  }
  res= myx_xmlSaveFile(filename, doc);

  xmlFreeDoc(doc);
  
  return res == -1 ? MYX_GRT_CANT_OPEN_FILE : MYX_GRT_NO_ERROR;
}


/** 
 ****************************************************************************
 * @brief Loads a list of GRT structs.
 *
 * Loads a list of structs from a file created with \c myx_grt_struct_save_list
 * into a structs object.
 * 
 * @param filename  Name of file that contains a list of GRT structs. 
 * @param error  Error that occured during load
 *
 * @return A new allocated list of structs
 * @see myx_grt_struct_save_list
 *****************************************************************************/
MYX_GRT_STRUCTS * myx_grt_struct_load_list(const char *filename, MYX_GRT_ERROR *error)
{
  MYX_GRT_STRUCTS *gstructs= g_new0(MYX_GRT_STRUCTS, 1);
  xmlDocPtr doc;
  xmlNodePtr root;

  *error= MYX_GRT_NO_ERROR;

  if (!filename)
  {
    *error= MYX_GRT_CANT_OPEN_FILE;
    return NULL;
  }
  
  if (!(doc= myx_xmlParseFile(filename)))
  {
    *error= MYX_GRT_CANT_OPEN_FILE;
    return NULL;
  }

  root= xmlDocGetRootElement(doc);
  if (root && xmlStrcmp(root->name, (xmlChar*)"gstructs")==0)
  {
    xmlNodePtr child= root->children;

    while (child)
    {
      if (xmlStrcmp(child->name, (xmlChar*)"gstruct")==0)
        xml_to_struct(child, &gstructs);
      child= child->next;
    }
  }
  xmlFreeDoc(doc);

  return gstructs;
}


/**
 ****************************************************************************
 * @brief Loads a XML file containing struct definitions and register them.
 *
 * If any of the structs in the file can't be registered for having a 
 * duplicate name, the whole operation will fail and none of the structs
 * will be registered.
 * 
 * @param grt  
 * @param filename File created with myx_grt_struct_save_list
 *
 * @return MYX_GRT_NO_ERROR if all structs could be loaded. 
 * @return MYX_GRT_DUPLICATE_ENTRY if any of the structs were duplicate.
 * @return MYX_GRT_CANT_OPEN_FILE
 *****************************************************************************/
MYX_GRT_ERROR myx_grt_struct_load_and_register(MYX_GRT *grt, const char *filename)
{
  MYX_GRT_STRUCTS *strlist= NULL;
  MYX_GRT_ERROR err;
  unsigned int i;

  g_return_val_if_fail(grt!=NULL, MYX_GRT_INTERNAL_ERROR);
  g_return_val_if_fail(filename!=NULL, MYX_GRT_INTERNAL_ERROR);

  strlist= myx_grt_struct_load_list(filename, &err);
  if (err != MYX_GRT_NO_ERROR)
    return err;

  if (strlist)
  {    
    for (i= 0; i < strlist->structs_num; i++)
    {
      if (myx_grt_struct_get(grt, strlist->structs[i].name))
      {
        myx_grt_structs_free(strlist);
        return MYX_GRT_DUPLICATE_ENTRY;
      }
    }

    for (i= 0; i < strlist->structs_num; i++)
    {      
      grt->structs= vec_insert_resize(grt->structs, sizeof(MYX_GRT_STRUCT),
                                      &grt->structs_num, grt->structs_num,
                                      strlist->structs+i);
    }
    g_free(strlist->structs);
    g_free(strlist);
  }
  
  return MYX_GRT_NO_ERROR;
}

#define grt_lgpl "/*\n" \
" Generic Runtime Library (GRT)\n" \
" Copyright (C) 2005 MySQL AB\n\n" \
" This library is free software; you can redistribute it and/or\n" \
" modify it under the terms of the GNU Lesser General Public\n" \
" License as published by the Free Software Foundation; either\n" \
" version 2.1 of the License, or (at your option) any later version.\n\n" \
" This library is distributed in the hope that it will be useful,\n" \
" but WITHOUT ANY WARRANTY; without even the implied warranty of\n" \
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n" \
" Lesser General Public License for more details.\n\n" \
" You should have received a copy of the GNU Lesser General Public\n" \
" License along with this library; if not, write to the Free Software\n" \
" Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA\n" \
" */\n\n"

// -----------------------------------------------------------------------------------------------------------------------
// Java Class
// -----------------------------------------------------------------------------------------------------------------------

const char *java_class_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  grt_lgpl
  "package %package_name%;\n\n"
  "public class %struct_name% extends %struct_parent_name% {\n\n"
  "%constructor[1]%"
  "%getter_setter_definiton.loop_int_members[2]%"
  "%getter_setter_definiton.loop_real_members[2]%"
  "%getter_setter_definiton.loop_string_members[2]%"
  "%getter_setter_definiton.loop_dict_members[3]%"
  "%getter_setter_definiton.loop_list_members[4]%"
  "%getter_setter_definiton.loop_REF_members[5]%"
  "%getter_setter_definiton.loop_REFLIST_members[6]%"
  "    public StringBuffer getGrtXmlMembers(StringBuffer xml) {\n"
  "        super.getGrtXmlMembers(xml);\n\n"
  "%get_xml_member.loop_int_members[7]%"
  "%get_xml_member.loop_real_members[8]%"
  "%get_xml_member.loop_string_members[9]%"
  "%get_xml_member.loop_list_members[10]%"
  "%get_xml_member.loop_dict_members[11]%"
  "%get_xml_member.loop_REF_members[12]%"
  "%get_xml_member.loop_REFLIST_members[13]%"
  "        return xml;\n"
  "    }\n"
  "}\n",

  // [1] -----------------------------------------------------------------
  "    protected %struct_name%() {\n"
  "    }\n\n"
  "    public %struct_name%(Object obj) {\n"
  "        super();\n\n"
  "        if ((obj == null) || (obj instanceof com.mysql.grt.GrtObject)) {\n"
  "            data = new %struct_name%DataLocal(this);\n\n"
  "            data.setOwner((com.mysql.grt.GrtObject) obj);\n"
  "            com.mysql.grt.Grt.getInstance().addToObjectCache(this);\n"
  "        } else if (obj instanceof String) {\n"
  "            data = new %struct_name%DataGlobal((String) obj);\n"
  "        }\n"
  "    }\n\n",

  // [2] -----------------------------------------------------------------
  "    public %member_type% get%member_name_1st_uc%() {\n"
  "        return ((%struct_name%Data) data).get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public %member_type% set%member_name_1st_uc%(%member_type% %member_name%) {\n"
  "        ((%struct_name%Data) data).set%member_name_1st_uc%(%member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [3] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%() {\n"
  "        return ((%struct_name%Data) data).get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%) {\n"
  "        ((%struct_name%Data) data).set%member_name_1st_uc%(%member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [4] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%() {\n"
  "        return ((%struct_name%Data) data).get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%) {\n"
  "        ((%struct_name%Data) data).set%member_name_1st_uc%(%member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",


  // [5] ----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%() {\n"
  "        return ((%struct_name%Data) data).get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%) {\n"
  "        ((%struct_name%Data) data).set%member_name_1st_uc%(%member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public String get%member_name_1st_uc%ById() {\n"
  "        return ((%struct_name%Data) data).get%member_name_1st_uc%ById();\n"
  "    }\n\n"
  "    public void set%member_name_1st_uc%ById(String %member_name%) {\n"
  "        ((%struct_name%Data) data).set%member_name_1st_uc%ById(%member_name%);\n"
  "    }\n\n",

  // [6] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%() {\n"
  "        return ((%struct_name%Data) data).get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%) {\n"
  "        ((%struct_name%Data) data).set%member_name_1st_uc%(%member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [7] ----------------------------------------------------------------
  "            xml.append(\"<value type=\\\"int\\\" key=\\\"%member_name%\\\">\"\n"
  "                    + Integer.toString(get%member_name_1st_uc%()) + \"</value>\\n\");\n\n",

  // [8] ----------------------------------------------------------------
  "            xml.append(\"<value type=\\\"real\\\" key=\\\"%member_name%\\\">\"\n"
  "                    + Double.toString(get%member_name_1st_uc%()) + \"</value>\\n\");\n\n",

  // [9] ----------------------------------------------------------------
  "            if (get%member_name_1st_uc%() != null) {\n"
  "                xml.append(\"<value type=\\\"string\\\" key=\\\"%member_name%\\\">\"\n"
  "                        + com.mysql.grt.Grt.escapeStringForXml(get%member_name_1st_uc%()) + \"</value>\\n\");\n"
  "            }\n\n",

  // [10] ----------------------------------------------------------------
  "            if (get%member_name_1st_uc%() != null) {\n"
  "                get%member_name_1st_uc%().getGrtXml(xml, \" key=\\\"%member_name%\\\"\");\n"
  "            }\n\n",

  // [11] ----------------------------------------------------------------
  "            if (get%member_name_1st_uc%() != null) {\n"
  "                get%member_name_1st_uc%().getGrtXml(xml, \" key=\\\"%member_name%\\\"\");\n"
  "            }\n\n",

  // [12] ----------------------------------------------------------------
  "            if (get%member_name_1st_uc%() != null) {\n"
  "                xml.append(\"<value type=\\\"string\\\" option=\\\"ref\\\" key=\\\"%member_name%\\\">\"\n"
  "                        + get%member_name_1st_uc%ById() + \"</value>\\n\");\n"
  "            }\n\n",

  // [13] ----------------------------------------------------------------
  "            if (get%member_name_1st_uc%() != null) {\n"
  "                get%member_name_1st_uc%().getGrtRefXml(xml, \" key=\\\"%member_name%\\\"\");\n"
  "            }\n\n"
};
static const unsigned int java_class_templates_count= sizeof(java_class_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Java Class Data Interface
// -----------------------------------------------------------------------------------------------------------------------

const char *java_class_data_interface_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  grt_lgpl
  "package %package_name%;\n\n"
  "public interface %struct_name%Data extends %struct_parent_name%Data {\n\n"
  "%getter_setter_definiton.loop_int_members[1]%"
  "%getter_setter_definiton.loop_real_members[1]%"
  "%getter_setter_definiton.loop_string_members[1]%"
  "%getter_setter_definiton.loop_dict_members[2]%"
  "%getter_setter_definiton.loop_list_members[3]%"
  "%getter_setter_definiton.loop_REF_members[4]%"
  "%getter_setter_definiton.loop_REFLIST_members[5]%"
  "}\n",

  // [1] -----------------------------------------------------------------
  "    public %member_type% get%member_name_1st_uc%();\n\n"
  "    public %member_type% set%member_name_1st_uc%(%member_type% %member_name%);\n\n",

  // [2] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%();\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%);\n\n",

  // [3] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%();\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%);\n\n",

  // [4] ----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%();\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%);\n\n"
  "    public String get%member_name_1st_uc%ById();\n\n"
  "    public void set%member_name_1st_uc%ById(String %member_name%);\n\n",

  // [5] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%();\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%);\n\n"
};
static const unsigned int java_class_data_interface_templates_count= sizeof(java_class_data_interface_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Java Class Data Local
// -----------------------------------------------------------------------------------------------------------------------

const char *java_class_data_local_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  grt_lgpl
  "package %package_name%;\n\n"
  "public class %struct_name%DataLocal extends %struct_parent_name%DataLocal implements %struct_name%Data {\n\n"
  "%field_definition.loop_int_members[1]%"
  "%field_definition.loop_real_members[1]%"
  "%field_definition.loop_string_members[1]%"
  "%field_definition_list.loop_list_members[2]%"
  "%field_definition_dict.loop_dict_members[3]%"
  "%field_definition_dict.loop_REF_members[1]%"
  "%field_definition_dict.loop_REFLIST_members[4]%"
  "    public %struct_name%DataLocal(com.mysql.grt.GrtObject grtObject) {\n"
  "        super(grtObject);\n"
  "    }\n\n"
  "%getter_setter_definiton.loop_int_members[5]%"
  "%getter_setter_definiton.loop_real_members[5]%"
  "%getter_setter_definiton.loop_string_members[5]%"
  "%getter_setter_definiton.loop_dict_members[6]%"
  "%getter_setter_definiton.loop_list_members[7]%"
  "%getter_setter_definiton.loop_REF_members[8]%"
  "%getter_setter_definiton.loop_REFLIST_members[9]%"
  "}\n",

  // [1] -----------------------------------------------------------------
  "    private %member_type% %member_name%;\n\n",

  // [2] -----------------------------------------------------------------
  "    private com.mysql.grt.%member_struct_name%List %member_name%;\n\n",

  // [3] -----------------------------------------------------------------
  "    private com.mysql.grt.%member_struct_name% %member_name%;\n\n",

  // [4] -----------------------------------------------------------------
  "    private com.mysql.grt.%member_struct_name%List %member_name%;\n\n",

  // [5] -----------------------------------------------------------------
  "    public %member_type% get%member_name_1st_uc%() {\n"
  "        return %member_name%;\n"
  "    }\n\n"
  "    public %member_type% set%member_name_1st_uc%(%member_type% %member_name%) {\n"
  "        this.%member_name% = %member_name%;\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [6] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%() {\n"
  "        if (%member_name% == null)\n"
  "            %member_name% = new com.mysql.grt.%member_struct_name%(this.grtObject);\n"
  "        return %member_name%;\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%) {\n"
  "        this.%member_name% = %member_name%;\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [7] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%() {\n"
  "        if (%member_name% == null)\n"
  "            %member_name% = new com.mysql.grt.%member_struct_name%List(this.grtObject);\n"
  "        return %member_name%;\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%) {\n"
  "        this.%member_name% = %member_name%;\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [8] ----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%() {\n"
  "        return (com.mysql.grt.%member_struct_name%) com.mysql.grt.Grt.getInstance().getObjectByRefId(%member_name%);\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%) {\n"
  "        this.%member_name% = %member_name%.get_id();\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public String get%member_name_1st_uc%ById() {\n"
  "        return %member_name%;\n"
  "    }\n\n"
  "    public void set%member_name_1st_uc%ById(String %member_name%) {\n"
  "        this.%member_name% = %member_name%;\n"
  "    }\n\n",

  // [9] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%() {\n"
  "        if (%member_name% == null)\n"
  "            %member_name% = new com.mysql.grt.%member_struct_name%List(this.grtObject);\n"
  "        return %member_name%;\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%) {\n"
  "        this.%member_name% = %member_name%;\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n"
};
static const unsigned int java_class_data_local_templates_count= sizeof(java_class_data_local_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Java Class Data Global
// -----------------------------------------------------------------------------------------------------------------------

const char *java_class_data_global_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  grt_lgpl
  "package %package_name%;\n\n"
  "public class %struct_name%DataGlobal extends %struct_parent_name%DataGlobal implements %struct_name%Data {\n\n"
  "    public %struct_name%DataGlobal(String globalObjectPath) {\n"
  "        super(globalObjectPath);\n"
  "    }\n\n"
  "%getter_setter_definiton.loop_int_members[1]%"
  "%getter_setter_definiton.loop_real_members[1]%"
  "%getter_setter_definiton.loop_string_members[1]%"
  "%getter_setter_definiton.loop_dict_members[2]%"
  "%getter_setter_definiton.loop_list_members[3]%"
  "%getter_setter_definiton.loop_REF_members[4]%"
  "%getter_setter_definiton.loop_REFLIST_members[5]%"
  "}\n",

  // [1] -----------------------------------------------------------------
  "    public %member_type% get%member_name_1st_uc%() {\n"
  "        return getGrtGlobalAs%getter_setter_type%(\"%member_name%\");\n"
  "    }\n\n"
  "    public %member_type% set%member_name_1st_uc%(%member_type% %member_name%) {\n"
  "        setGrtGlobalFrom%getter_setter_type%(\"%member_name%\", %member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [2] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%() {\n"
  "        return (com.mysql.grt.%member_struct_name%) getGrtGlobalAsObject(\"%member_name%\");\n\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%) {\n"
  "        setGrtGlobalFromObject(\"%member_name%\", %member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [3] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%() {\n"
  "        return (com.mysql.grt.%member_struct_name%List) getGrtGlobalAsObject(\"%member_name%\");\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%) {\n"
  "        setGrtGlobalFromObject(\"%member_name%\", %member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [4] ----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name% get%member_name_1st_uc%() {\n"
  "        return (com.mysql.grt.%member_struct_name%) com.mysql.grt.Grt.getInstance().getObjectByRefId(\n"
  "                getGrtGlobalAsString(\"%member_name%\"));\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name% set%member_name_1st_uc%(com.mysql.grt.%member_struct_name% %member_name%) {\n"
  "        setGrtGlobalFromString(\"%member_name%\", %member_name%.get_id());\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public String get%member_name_1st_uc%ById() {\n"
  "        return getGrtGlobalAsString(\"%member_name%\");\n"
  "    }\n\n"
  "    public void set%member_name_1st_uc%ById(String %member_name%) {\n"
  "        setGrtGlobalFromString(\"%member_name%\", %member_name%);\n"
  "    }\n\n",

  // [5] -----------------------------------------------------------------
  "    public com.mysql.grt.%member_struct_name%List get%member_name_1st_uc%() {\n"
  "        return (com.mysql.grt.%member_struct_name%List) getGrtGlobalAsObject(\"%member_name%\");\n"
  "    }\n\n"
  "    public com.mysql.grt.%member_struct_name%List set%member_name_1st_uc%(com.mysql.grt.%member_struct_name%List %member_name%) {\n"
  "        setGrtGlobalFromObject(\"%member_name%\", %member_name%);\n\n"
  "        return get%member_name_1st_uc%();\n"
  "    }\n\n"
};
static const unsigned int java_class_data_global_templates_count= sizeof(java_class_data_global_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Java Class List
// -----------------------------------------------------------------------------------------------------------------------

const char *java_class_template_list = 
  grt_lgpl
  "package %package_name%;\n\n"
  "import com.mysql.grt.*;\n\n"
  "public class %struct_name%List extends GrtList {\n\n"
  "    public %struct_name%List() {\n"
  "        super();\n"
  "    }\n\n"
  "    public %struct_name%List(com.mysql.grt.GrtObject owner) {\n"
  "        super(owner);\n"
  "    }\n\n"
  "    public %struct_name%List(String contentStructName) {\n"
  "        super(contentStructName);\n"
  "    }\n\n"
  "    public %struct_name%List(String contentStructName, String globalObjectPath) {\n"
  "        super(contentStructName, globalObjectPath);\n"
  "    }\n\n"
  "    public %struct_name% add(%struct_name% item) {\n"
  "        return (%struct_name%) super.addObject(item);\n"
  "    }\n\n"
  "    // TODO: this is a hack and should be replaced with a ObjectRefList\n"
  "    public %struct_name% get(int index) {\n"
  "        Object obj = super.getObject(index);\n\n"
  "        if (obj.getClass() == String.class)\n"
  "            return (%struct_name%) Grt.getInstance().getObjectByRefId((String) obj);\n"
  "        else\n"
  "            return (%struct_name%) obj;\n"
  "    }\n\n"
  "    /*public %struct_name% get(int index) {\n"
  "        return (%struct_name%) super.getObject(index);\n"
  "    }*/\n\n"
  "    public %struct_name% getItemByName(String name) {\n"
  "        int index = getIndexOfName(name);\n\n"
  "        if (index > -1)\n"
  "            return get(index);\n"
  "        else\n"
  "            return null;\n"
  "    }\n"
  "    public String getContentType() {\n"
  "        return \"dict\";\n"
  "    }\n\n"
  "    public String getContentStructName() {\n"
  "        return \"%content_struct_name%\";\n"
  "    }\n\n"
  "}\n";

// -----------------------------------------------------------------------------------------------------------------------
// Java Class HashMap
// -----------------------------------------------------------------------------------------------------------------------

const char *java_class_template_hashmap = 
  grt_lgpl
  "package %package_name%;\n\n"
  "import com.mysql.grt.*;\n\n"
  "public class %struct_name%HashMap extends GrtHashMap {\n\n"
  "    public %struct_name%HashMap() {\n"
  "        super();\n"
  "    }\n\n"
  "    public %struct_name%HashMap(com.mysql.grt.GrtObject owner) {\n"
  "        super(owner);\n"
  "    }\n\n"
  "    public %struct_name%HashMap(String contentStructName) {\n"
  "        super(contentStructName);\n"
  "    }\n\n"
  "    public %struct_name%HashMap(String contentStructName, String globalObjectPath) {\n"
  "        super(contentStructName, globalObjectPath);\n"
  "    }\n\n"
  "    public %struct_name% add(String key, %struct_name% item) {\n"
  "        return (%struct_name%) super.addObject(key, item);\n"
  "    }\n\n"
  "    public %struct_name% get(String key) {\n"
  "        return (%struct_name%) super.getObject(key);\n"
  "    }\n\n"
  "    public String getContentType() {\n"
  "        return \"dict\";\n"
  "    }\n\n"
  "    public String getContentStructName() {\n"
  "        return \"%content_struct_name%\";\n"
  "    }\n\n"
  "}\n";





// -----------------------------------------------------------------------------------------------------------------------
// Php Class
// -----------------------------------------------------------------------------------------------------------------------

const char *php_class_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  "<?php\n"
  grt_lgpl
  "class %struct_name% extends %struct_parent_name% {\n\n"
  "%constructor[1]%"
  "%getter_setter_definiton.loop_int_members[2]%"
  "%getter_setter_definiton.loop_real_members[2]%"
  "%getter_setter_definiton.loop_string_members[2]%"
  "%getter_setter_definiton.loop_dict_members[3]%"
  "%getter_setter_definiton.loop_list_members[4]%"
  "%getter_setter_definiton.loop_REF_members[5]%"
  "%getter_setter_definiton.loop_REFLIST_members[6]%"
  "    public function getGrtXmlMembers() {\n"
  "        $xml = parent::getGrtXmlMembers();\n\n"
  "%get_xml_member.loop_int_members[7]%"
  "%get_xml_member.loop_real_members[8]%"
  "%get_xml_member.loop_string_members[9]%"
  "%get_xml_member.loop_list_members[10]%"
  "%get_xml_member.loop_dict_members[11]%"
  "%get_xml_member.loop_REF_members[12]%"
  "%get_xml_member.loop_REFLIST_members[13]%"
  "        return $xml;\n"
  "    }\n"
  "}\n"
  "?>\n",

  // [1] -----------------------------------------------------------------
  "    public function __construct($obj) {\n"
  "        if (is_string($obj)) {\n"
  "            $this->data = new %struct_name%DataGlobal($obj);\n"
  "        } else if (($obj == null) || ($obj instanceof com_mysql_grt_GrtObject)) {\n"
  "            $this->data = new %struct_name%DataLocal($this);\n\n"
  "            $this->data->setOwner($obj);\n"
  "            Grt::getInstance()->addToObjectCache($this);\n"
  "        }\n"
  "    }\n\n",

  // [2] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return $this->data->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        return $this->data->set%member_name_1st_uc%($%member_name%);\n"
  "    }\n\n",

  // [3] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return $this->data->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        return $this->data->set%member_name_1st_uc%($%member_name%);\n"
  "    }\n\n",

  // [4] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return $this->data->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        return $this->data->set%member_name_1st_uc%($%member_name%);\n"
  "    }\n\n",


  // [5] ----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return $this->data->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        return $this->data->set%member_name_1st_uc%($%member_name%);\n"
  "    }\n\n"
  "    public function get%member_name_1st_uc%ById() {\n"
  "        return $this->data->get%member_name_1st_uc%ById();\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%ById($%member_name%) {\n"
  "        $this->data->set%member_name_1st_uc%ById($%member_name%);\n"
  "    }\n\n",

  // [6] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return $this->data->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        return $this->data->set%member_name_1st_uc%($%member_name%);\n"
  "    }\n\n",

  // [7] ----------------------------------------------------------------
  "            $xml .= \"<value type=\\\"int\\\" key=\\\"%member_name%\\\">\"\n"
  "                    . $this->data->get%member_name_1st_uc%() . \"</value>\\n\";\n\n",

  // [8] ----------------------------------------------------------------
  "            $xml .= \"<value type=\\\"real\\\" key=\\\"%member_name%\\\">\"\n"
  "                    . $this->data->get%member_name_1st_uc%() . \"</value>\\n\";\n\n",

  // [9] ----------------------------------------------------------------
  "            if ($this->data->get%member_name_1st_uc%() != null) {\n"
  "                $xml .= \"<value type=\\\"string\\\" key=\\\"%member_name%\\\">\"\n"
  "                        . Grt::escapeStringForXml($this->data->get%member_name_1st_uc%()) . \"</value>\\n\";\n"
  "            }\n\n",

  // [10] ----------------------------------------------------------------
  "            if ($this->data->get%member_name_1st_uc%() != null) {\n"
  "                $xml .= $this->data->get%member_name_1st_uc%()->getGrtXml(\" key=\\\"%member_name%\\\"\");\n"
  "            }\n\n",

  // [11] ----------------------------------------------------------------
  "            if ($this->data->get%member_name_1st_uc%() != null) {\n"
  "                $xml .= $this->data->get%member_name_1st_uc%()->getGrtXml(\" key=\\\"%member_name%\\\"\");\n"
  "            }\n\n",

  // [12] ----------------------------------------------------------------
  "            if ($this->data->get%member_name_1st_uc%() != null) {\n"
  "                $xml .= \"<value type=\\\"string\\\" option=\\\"ref\\\" key=\\\"%member_name%\\\">\"\n"
  "                        . $this->data->get%member_name_1st_uc%ById() . \"</value>\\n\";\n"
  "            }\n\n",

  // [13] ----------------------------------------------------------------
  "            if ($this->data->get%member_name_1st_uc%() != null) {\n"
  "                $xml .= $this->data->get%member_name_1st_uc%()->getGrtXmlKeyTag(\" key=\\\"%member_name%\\\"\");\n"
  "            }\n\n"
};
static const unsigned int php_class_templates_count= sizeof(php_class_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Php Class Data Interface
// -----------------------------------------------------------------------------------------------------------------------

const char *php_class_data_interface_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  "<?php\n"
  grt_lgpl
  "interface %struct_name%Data extends %struct_parent_name%Data {\n\n"
  "%getter_setter_definiton.loop_int_members[1]%"
  "%getter_setter_definiton.loop_real_members[1]%"
  "%getter_setter_definiton.loop_string_members[1]%"
  "%getter_setter_definiton.loop_dict_members[2]%"
  "%getter_setter_definiton.loop_list_members[3]%"
  "%getter_setter_definiton.loop_REF_members[4]%"
  "%getter_setter_definiton.loop_REFLIST_members[5]%"
  "}\n"
  "?>\n",

  // [1] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%();\n\n"
  "    public function set%member_name_1st_uc%($%member_name%);\n\n",

  // [2] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%();\n\n"
  "    public function set%member_name_1st_uc%($%member_name%);\n\n",

  // [3] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%();\n\n"
  "    public function set%member_name_1st_uc%($%member_name%);\n\n",

  // [4] ----------------------------------------------------------------
  "    public function get%member_name_1st_uc%();\n\n"
  "    public function set%member_name_1st_uc%($%member_name%);\n\n"
  "    public function get%member_name_1st_uc%ById();\n\n"
  "    public function set%member_name_1st_uc%ById($%member_name%);\n\n",

  // [5] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%();\n\n"
  "    public function set%member_name_1st_uc%($%member_name%);\n\n"
};
static const unsigned int php_class_data_interface_templates_count= sizeof(php_class_data_interface_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Php Class Data Local
// -----------------------------------------------------------------------------------------------------------------------

const char *php_class_data_local_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  "<?php\n"
  grt_lgpl
  "class %struct_name%DataLocal extends %struct_parent_name%DataLocal implements %struct_name%Data {\n\n"
  "%field_definition.loop_int_members[1]%"
  "%field_definition.loop_real_members[1]%"
  "%field_definition.loop_string_members[1]%"
  "%field_definition_list.loop_list_members[2]%"
  "%field_definition_dict.loop_dict_members[3]%"
  "%field_definition_dict.loop_REF_members[1]%"
  "%field_definition_dict.loop_REFLIST_members[4]%"
  "    function __construct(com_mysql_grt_GrtObject $grtObject) {\n"
  "        parent::__construct($grtObject);\n"
  "    }\n\n"
  "%getter_setter_definiton.loop_int_members[5]%"
  "%getter_setter_definiton.loop_real_members[5]%"
  "%getter_setter_definiton.loop_string_members[5]%"
  "%getter_setter_definiton.loop_dict_members[6]%"
  "%getter_setter_definiton.loop_list_members[7]%"
  "%getter_setter_definiton.loop_REF_members[8]%"
  "%getter_setter_definiton.loop_REFLIST_members[9]%"
  "}\n"
  "?>\n",

  // [1] -----------------------------------------------------------------
  "    private $%member_name%;\n\n",

  // [2] -----------------------------------------------------------------
  "    private $%member_name%;\n\n",

  // [3] -----------------------------------------------------------------
  "    private $%member_name%;\n\n",

  // [4] -----------------------------------------------------------------
  "    private $%member_name%;\n\n",

  // [5] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return $this->%member_name%;\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        $this->%member_name% = $%member_name%;\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [6] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        if ($this->%member_name% == null)\n"
  "            $this->%member_name% = new %member_struct_name%(%constructor_params%);\n"
  "        return $this->%member_name%;\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        $this->%member_name% = $%member_name%;\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [7] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        if ($this->%member_name% == null)\n"
  "            $this->%member_name% = new GrtObjectList('com_mysql_grt_%member_struct_name%', '');\n"
  "        return $this->%member_name%;\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        $this->%member_name% = $%member_name%;\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [8] ----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return Grt::getInstance()->getObjectByRefId($this->%member_name%);\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        $this->%member_name% = $%member_name%->get_id();\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function get%member_name_1st_uc%ById() {\n"
  "        return $this->%member_name%;\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%ById($%member_name%) {\n"
  "        $this->%member_name% = $%member_name%;\n"
  "    }\n\n",

  // [9] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        if ($this->%member_name% == null)\n"
  "            $this->%member_name% = new GrtObjectList('com_mysql_grt_%member_struct_name%', '');\n"
  "        return %member_name%;\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        $this->%member_name% = $%member_name%;\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n"
};
static const unsigned int php_class_data_local_templates_count= sizeof(php_class_data_local_templates)/sizeof(char*);



// -----------------------------------------------------------------------------------------------------------------------
// Php Class Data Global
// -----------------------------------------------------------------------------------------------------------------------

const char *php_class_data_global_templates[] = 
{ 
  // [0] -----------------------------------------------------------------
  "<?php\n"
  grt_lgpl
  "class %struct_name%DataGlobal extends %struct_parent_name%DataGlobal implements %struct_name%Data {\n\n"
  "    function __construct($globalObjectPath) {\n"
  "        parent::__construct($globalObjectPath);\n"
  "    }\n\n"
  "%getter_setter_definiton.loop_int_members[1]%"
  "%getter_setter_definiton.loop_real_members[1]%"
  "%getter_setter_definiton.loop_string_members[1]%"
  "%getter_setter_definiton.loop_dict_members[2]%"
  "%getter_setter_definiton.loop_list_members[3]%"
  "%getter_setter_definiton.loop_REF_members[4]%"
  "%getter_setter_definiton.loop_REFLIST_members[5]%"
  "}\n"
  "?>\n",

  // [1] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return parent::getGrtGlobalAs%getter_setter_type%(\"%member_name%\");\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        parent::setGrtGlobalFrom%getter_setter_type%(\"%member_name%\", $%member_name%);\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [2] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return parent::getGrtGlobalAsObject(\"%member_name%\");\n\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        parent::setGrtGlobalFromObject(\"%member_name%\", $%member_name%);\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [3] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return parent::getGrtGlobalAsObject(\"%member_name%\");\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        parent::setGrtGlobalFromObject(\"%member_name%\", $%member_name%);\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n",

  // [4] ----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return Grt::getInstance()->getObjectByRefId(\n"
  "                parent::getGrtGlobalAsString(\"%member_name%\"));\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        parent::setGrtGlobalFromString(\"%member_name%\", $%member_name%->get_id());\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n"
  "    public function get%member_name_1st_uc%ById() {\n"
  "        return parent::getGrtGlobalAsString(\"%member_name%\");\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%ById($%member_name%) {\n"
  "        parent::setGrtGlobalFromString(\"%member_name%\", $%member_name%);\n"
  "    }\n\n",

  // [5] -----------------------------------------------------------------
  "    public function get%member_name_1st_uc%() {\n"
  "        return parent::getGrtGlobalAsObject(\"%member_name%\");\n"
  "    }\n\n"
  "    public function set%member_name_1st_uc%($%member_name%) {\n"
  "        parent::setGrtGlobalFromObject(\"%member_name%\", $%member_name%);\n\n"
  "        return $this->get%member_name_1st_uc%();\n"
  "    }\n\n"
};
static const unsigned int php_class_data_global_templates_count= sizeof(php_class_data_global_templates)/sizeof(char*);

// Regex to filter template refrences
// %(\w+)(\.loop_(\w+)_members){0,1}\[(\d+)\]%
//   0: complete match
//   1: caption of reference
//   2: -
//   3: member type to loop (no loop if empty)
//   4: reference number
#define FILTER_TEMPLATE_REFERENCES_REGEX "%(\\w+)(\\.loop_(\\w+)_members){0,1}\\[(\\d+)\\]%"

char * process_template_references(const char *input, const char **temps, void *loop_user_data,
                                   char * (*process_loop) (const char *input, const char *loop_caption, const char *loop_type, void *user_data))
{
  pcre *pcre_exp;
  const char *error_str;
  int erroffset, offset;
  int matched[60], rc;
  char *output= g_strdup(input);

  pcre_exp= pcre_compile(FILTER_TEMPLATE_REFERENCES_REGEX, PCRE_CASELESS, &error_str, &erroffset, NULL);
  if (!pcre_exp)
    return NULL;

  offset= 0;
  while((rc= pcre_exec(pcre_exp, NULL, input, (int)strlen(input), offset, 0, matched, sizeof(matched)/sizeof(*matched)))>=0)
  {
    const char *ref_nr;
    const char *loop_caption;
    const char *loop_type;
    const char *complete_match;
    unsigned int ref_index;
    char *ref_txt;
    char *new_out;

    pcre_get_substring(input, matched, rc, 0, &complete_match);
    pcre_get_substring(input, matched, rc, 1, &loop_caption);
    pcre_get_substring(input, matched, rc, 3, &loop_type);
    pcre_get_substring(input, matched, rc, 4, &ref_nr);

    ref_index= atoi(ref_nr);

    if ((loop_type) && (loop_type[0]))
      ref_txt= process_loop(temps[ref_index], loop_caption, loop_type, loop_user_data);
    else
      ref_txt= g_strdup(temps[ref_index]);

    new_out= process_template_references(ref_txt, temps, loop_user_data, *process_loop);
    g_free(ref_txt);

    output= str_g_replace(output, complete_match, new_out);
    g_free(new_out);

    pcre_free_substring(complete_match);
    pcre_free_substring(loop_type);
    pcre_free_substring(ref_nr);

    //Move offset
    offset= matched[1];
  }

  return output;
}

char * myx_get_struct_export_java_process_member_loop(const char *input, const char *loop_caption, const char *loop_type, void *user_data)
{
  MYX_GRT_STRUCT *gstruct= (MYX_GRT_STRUCT *)user_data;
  char *output= g_strdup("");
  unsigned int i;

  for (i= 0; i<gstruct->members_num; i++)
  {
    MYX_GRT_STRUCT_MEMBER *member= gstruct->members+i;
    const char *member_java_type;
    const char *getter_setter_type;
    const char *member_type_as_string= myx_get_value_type_as_string(member->value_type);

    unsigned int loop_type_is_ref= (strcmp2(loop_type, "REF") == 0);
    unsigned int member_type_is_string= (strcmp2(member_type_as_string, "string") == 0);
    unsigned int loop_type_is_reflist= (strcmp2(loop_type, "REFLIST") == 0);
    unsigned int member_type_is_list= (strcmp2(member_type_as_string, "list") == 0);

    char *member_name_1st_uc;

    // do not generate any get_xml code for overloading members
    // since the get_xml code will be in the parent class
    if ((strcmp2(loop_caption, "get_xml_member") == 0) && 
      (member->overrides))
      continue;

    // If this is a overloaded member do not define it in the sub-class
    if (member->overrides && member->overrides[0] && (strcmp2(loop_caption, "field_definition") == 0))
      continue;

    member_name_1st_uc= g_strdup(member->name);
    member_name_1st_uc[0]= toupper(member_name_1st_uc[0]);

    if (((strcmp2(loop_type, member_type_as_string) == 0) && 
          (!loop_type_is_ref) && (!loop_type_is_reflist) && (!member->is_ref)) ||  // Regular type
      (member_type_is_string && loop_type_is_ref && member->is_ref) ||  //REF type
      (member_type_is_list && loop_type_is_reflist && member->is_ref))  //REFLIST type
    {
      const char *member_struct_name;
      char *dict_member_struct_name= NULL;

      if (member->value_type == MYX_DICT_VALUE)
      {
        if ( (member->content_type == MYX_ANY_VALUE) && (member->struct_name) )
          member_struct_name= member->struct_name;
        else if (member->content_type == MYX_ANY_VALUE)
          member_struct_name= "GrtHashMap";
        else if (member->content_type == MYX_STRING_VALUE)
          member_struct_name= "GrtStringHashMap";
        else if (member->content_type == MYX_INT_VALUE)
          member_struct_name= "GrtIntHashMap";
        else if (member->content_type == MYX_REAL_VALUE)
          member_struct_name= "GrtRealHashMap";
        else if (member->content_type == MYX_LIST_VALUE)
          member_struct_name= "GrtHashMap";
        else if (member->content_type == MYX_DICT_VALUE)
        {
          //dict_member_struct_name= g_strdup_printf("com.mysql.grt.%s", member->content_struct_name);
          //member_struct_name= dict_member_struct_name;
          member_struct_name= member->content_struct_name;
        }
      }
      else
        member_struct_name= member->content_struct_name;


      // Map simple GRT types to Java types
      switch (member->value_type) 
      {
        case MYX_INT_VALUE:
          member_java_type= "int";
          getter_setter_type = "Int";
          break;
        case MYX_REAL_VALUE:
          member_java_type= "double";
          getter_setter_type = "Real";
          break;
        case MYX_STRING_VALUE:
          member_java_type= "String";
          getter_setter_type = "String";
          break;
        default:
          member_java_type= "";
          getter_setter_type = "";
      }

      // If this is a overloaded member and this is a getter_setter_definiton loop
      // replace the overloaded member_struct_name with the original one
      // since Java 1.4 cannot handle overloaded functions with different return type
      if ((member->overrides) && (member->overrides[0]))  
        member_struct_name= member->overrides;

      // If this is a string list, set the empty member_struct_name to "GrtString" so it results in "GrtStringList"
      if (!member_struct_name && member_type_is_list && (member->content_type == MYX_STRING_VALUE))
        member_struct_name= "GrtString";
      else if (!member_struct_name && member_type_is_list && (member->content_type == MYX_REAL_VALUE))
        member_struct_name= "GrtReal";
      else if (!member_struct_name && member_type_is_list && (member->content_type == MYX_INT_VALUE))
        member_struct_name= "GrtInt";

      output= str_g_append_and_free(output, 
        str_g_replace(
            str_g_replace(
              str_g_replace(
                str_g_replace(
                  str_g_replace(g_strdup(input), "%member_name%", member->name),
                "%getter_setter_type%", getter_setter_type),
              "%member_name_1st_uc%", member_name_1st_uc),
            "%member_type%", member_java_type),
          "%member_struct_name%", member_struct_name));

      g_free(dict_member_struct_name);
    }

    g_free(member_name_1st_uc);
  }

  return output;
}


char * myx_get_struct_export_php_process_member_loop(const char *input, const char *loop_caption, const char *loop_type, void *user_data)
{
  MYX_GRT_STRUCT *gstruct= (MYX_GRT_STRUCT *)user_data;
  char *output= g_strdup("");
  unsigned int i;

  for (i= 0; i<gstruct->members_num; i++)
  {
    MYX_GRT_STRUCT_MEMBER *member= gstruct->members+i;
    const char *member_java_type;
    const char *getter_setter_type;
    const char *member_type_as_string= myx_get_value_type_as_string(member->value_type);

    unsigned int loop_type_is_ref= (strcmp2(loop_type, "REF") == 0);
    unsigned int member_type_is_string= (strcmp2(member_type_as_string, "string") == 0);
    unsigned int loop_type_is_reflist= (strcmp2(loop_type, "REFLIST") == 0);
    unsigned int member_type_is_list= (strcmp2(member_type_as_string, "list") == 0);

    char *member_name_1st_uc;

    // do not generate any get_xml code for overloading members
    // since the get_xml code will be in the parent class
    if ((strcmp2(loop_caption, "get_xml_member") == 0) && 
      (member->overrides))
      continue;

    member_name_1st_uc= g_strdup(member->name);
    member_name_1st_uc[0]= toupper(member_name_1st_uc[0]);


    if (((strcmp2(loop_type, member_type_as_string) == 0) && 
          (!loop_type_is_ref) && (!loop_type_is_reflist) && (!member->is_ref)) ||  // Regular type
      (member_type_is_string && loop_type_is_ref && member->is_ref) ||  //REF type
      (member_type_is_list && loop_type_is_reflist && member->is_ref))  //REFLIST type
    {
      const char *member_struct_name;
      char *dict_member_struct_name= NULL;

      //GrtHashList as a constructor with different parameters than normal objects in PHP
      const char *constructor_params= "'', ''";

      if (member->value_type == MYX_DICT_VALUE)
      {
        if ( (member->content_type == MYX_ANY_VALUE) && (member->struct_name) )
        {
          dict_member_struct_name= str_g_replace(
            g_strdup_printf("com_mysql_grt_%s", member->struct_name), ".", "_");
          member_struct_name= dict_member_struct_name;

          //Set constructor parameter for objects
          constructor_params= "this->grtObject";
        }
        else if (member->content_type == MYX_ANY_VALUE)
          member_struct_name= "GrtHashMap";
        else if (member->content_type == MYX_STRING_VALUE)
          member_struct_name= "GrtStringHashMap";
        else if (member->content_type == MYX_INT_VALUE)
          member_struct_name= "GrtIntHashMap";
        else if (member->content_type == MYX_REAL_VALUE)
          member_struct_name= "GrtRealHashMap";
        else if (member->content_type == MYX_LIST_VALUE)
          member_struct_name= "GrtHashMap";
        else if (member->content_type == MYX_DICT_VALUE)
        {
          dict_member_struct_name= str_g_replace(
            g_strdup_printf("com_mysql_grt_%s", member->content_struct_name), ".", "_");
          member_struct_name= dict_member_struct_name;

          constructor_params= "this->grtObject";
        }
      }
      else
      {
        if (member->content_struct_name) 
        {
          dict_member_struct_name= str_g_replace(g_strdup(member->content_struct_name), ".", "_");
          member_struct_name= dict_member_struct_name;
        }
        else
          member_struct_name= NULL;
      }


      // Map simple GRT types to Java types
      switch (member->value_type) 
      {
        case MYX_INT_VALUE:
          member_java_type= "int";
          getter_setter_type = "Int";
          break;
        case MYX_REAL_VALUE:
          member_java_type= "double";
          getter_setter_type = "Real";
          break;
        case MYX_STRING_VALUE:
          member_java_type= "String";
          getter_setter_type = "String";
          break;
        default:
          member_java_type= "";
          getter_setter_type = "";
      }

      // If this is a overloaded member and this is a getter_setter_definiton loop
      // replace the overloaded member_struct_name with the original one
      // since Java 1.4 cannot handle overloaded functions with different return type
      if ((member->overrides) && (member->overrides[0]))  
        member_struct_name= member->overrides;

      // If this is a string list, set the empty member_struct_name to "GrtString" so it results in "GrtStringList"
      if (!member_struct_name && member_type_is_list && (member->content_type == MYX_STRING_VALUE))
        member_struct_name= "GrtString";
      else if (!member_struct_name && member_type_is_list && (member->content_type == MYX_REAL_VALUE))
        member_struct_name= "GrtReal";
      else if (!member_struct_name && member_type_is_list && (member->content_type == MYX_INT_VALUE))
        member_struct_name= "GrtInt";

      output= str_g_append_and_free(output, 
        str_g_replace(
            str_g_replace(
              str_g_replace(
                str_g_replace(
                  str_g_replace(
                    str_g_replace(g_strdup(input), "%member_name%", member->name),
                  "%constructor_params%", constructor_params),
                "%getter_setter_type%", getter_setter_type),
              "%member_name_1st_uc%", member_name_1st_uc),
            "%member_type%", member_java_type),
          "%member_struct_name%", member_struct_name));

      g_free(dict_member_struct_name);
    }

    g_free(member_name_1st_uc);
  }

  return output;
}

/**
 ****************************************************************************
 * @brief Exports a given MYX_GRT_STRUCTS list as classes for Java
 *
 * This function will generate classes based on MYX_GRT_STRUCTS that can be used 
 * to write GRT functions in Java
 * 
 * @param gstructs The list of MYX_GRT_STRUCTS
 * @param package_name The package name without the prefix "com.mysql.grt." that will be added automatically
 * @param output_path The directory the files will be written to
 *
 * @return MYX_GRT_NO_ERROR if not error occurs
 * @return MYX_GRT_CANT_OPEN_FILE
 *****************************************************************************/
MYX_GRT_ERROR myx_grt_struct_export_java_classes(MYX_GRT_STRUCTS *gstructs, 
                                                 const char *package_name, const char *output_path)
{
  unsigned int i;
  char *package_string= g_strdup_printf("com.mysql.grt.%s", package_name);

  for (i= 0; i<gstructs->structs_num; i++)
  {
    MYX_GRT_STRUCT *gstruct= gstructs->structs+i;
    FILE *f;
    char *struct_name;
    char *parent_struct_name= g_strdup_printf("com.mysql.grt.%s", gstruct->parent_struct_name);
    char *class_filename;
    char *class_data_interface_filename;
    char *class_data_local_filename;
    char *class_data_global_filename;
    char *list_filename;
    char *hashmap_filename;
    char *class_string= 
      process_template_references(java_class_templates[0], java_class_templates, gstruct,
        *myx_get_struct_export_java_process_member_loop);
    char *class_data_interface_string= 
      process_template_references(java_class_data_interface_templates[0], java_class_data_interface_templates, gstruct,
        *myx_get_struct_export_java_process_member_loop);
    char *class_data_local_string=
      process_template_references(java_class_data_local_templates[0], java_class_data_local_templates, gstruct,
        *myx_get_struct_export_java_process_member_loop);
    char *class_data_global_string=
      process_template_references(java_class_data_global_templates[0], java_class_data_global_templates, gstruct,
        *myx_get_struct_export_java_process_member_loop);
    char *list_string;
    char *hashmap_string;

    char *dot_pos= g_strrstr(gstruct->name, ".")+1;

    if (dot_pos)
      struct_name= g_strdup(dot_pos);
    else
      struct_name= g_strdup(gstruct->name);
    

    // ----------------------------------------------------------------------------------------------------------------
    // Set filenames
    class_filename= g_strdup_printf("%s%s.java", output_path, struct_name);
    class_data_interface_filename= g_strdup_printf("%s%sData.java", output_path, struct_name);
    class_data_local_filename= g_strdup_printf("%s%sDataLocal.java", output_path, struct_name);
    class_data_global_filename= g_strdup_printf("%s%sDataGlobal.java", output_path, struct_name);
    list_filename= g_strdup_printf("%s%sList.java", output_path, struct_name);
    hashmap_filename= g_strdup_printf("%s%sHashMap.java", output_path, struct_name);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class
    class_string= 
      str_g_replace(
        str_g_replace(
          str_g_replace(
            class_string, "%package_name%", package_string),
          "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class Data Interface
    class_data_interface_string= 
      str_g_replace(
        str_g_replace(
          str_g_replace(
            class_data_interface_string, "%package_name%", package_string),
          "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_data_interface_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_data_interface_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class Data Local
    class_data_local_string= 
      str_g_replace(
        str_g_replace(
          str_g_replace(
            class_data_local_string, "%package_name%", package_string),
          "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_data_local_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_data_local_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class Data Global
    class_data_global_string= 
      str_g_replace(
        str_g_replace(
          str_g_replace(
            class_data_global_string, "%package_name%", package_string),
          "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_data_global_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_data_global_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create List Classes
    list_string= str_g_replace(
        str_g_replace(
          str_g_replace(
            g_strdup(java_class_template_list), "%package_name%", package_string),
          "%content_struct_name%", gstruct->name),
        "%struct_name%", struct_name);

    f= myx_fopen(list_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", list_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create HashMap Classes
    hashmap_string= str_g_replace(
        str_g_replace(
          str_g_replace(
            g_strdup(java_class_template_hashmap), "%package_name%", package_string),
          "%content_struct_name%", gstruct->name),
        "%struct_name%", struct_name);

    f= myx_fopen(hashmap_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", hashmap_string);
    fclose(f);


    // ----------------------------------------------------------------------------------------------------------------
    // free strings
    g_free(struct_name);
    g_free(parent_struct_name);
    g_free(class_filename);
    g_free(class_data_interface_filename);
    g_free(class_data_local_filename);
    g_free(class_data_global_filename);
    g_free(list_filename);
    g_free(hashmap_filename);
    g_free(class_string);
    g_free(class_data_interface_string);
    g_free(class_data_local_string);
    g_free(class_data_global_string);
    g_free(list_string);
    g_free(hashmap_string);
  }

  g_free(package_string);

  return MYX_GRT_NO_ERROR;
}

/**
 ****************************************************************************
 * @brief Exports a given MYX_GRT_STRUCTS list as classes for Php
 *
 * This function will generate classes based on MYX_GRT_STRUCTS that can be used 
 * to write GRT functions in Php
 * 
 * @param gstructs The list of MYX_GRT_STRUCTS
 * @param package_name The package name without the prefix "com.mysql.grt." that will be added automatically
 * @param output_path The directory the files will be written to
 *
 * @return MYX_GRT_NO_ERROR if not error occurs
 * @return MYX_GRT_CANT_OPEN_FILE
 *****************************************************************************/
MYX_GRT_ERROR myx_grt_struct_export_php_classes(MYX_GRT_STRUCTS *gstructs, const char *output_path)
{
  unsigned int i;

  for (i= 0; i<gstructs->structs_num; i++)
  {
    MYX_GRT_STRUCT *gstruct= gstructs->structs+i;
    FILE *f;
    char *struct_name= str_g_replace(g_strdup_printf("com_mysql_grt_%s", gstruct->name), ".", "_");
    char *struct_file_name;
    char *parent_struct_name= str_g_replace(g_strdup_printf("com_mysql_grt_%s", gstruct->parent_struct_name), ".", "_");
    char *class_filename;
    char *class_data_interface_filename;
    char *class_data_local_filename;
    char *class_data_global_filename;
    char *class_string= 
      process_template_references(php_class_templates[0], php_class_templates, gstruct,
        *myx_get_struct_export_php_process_member_loop);
    char *class_data_interface_string= 
      process_template_references(php_class_data_interface_templates[0], php_class_data_interface_templates, gstruct,
        *myx_get_struct_export_php_process_member_loop);
    char *class_data_local_string=
      process_template_references(php_class_data_local_templates[0], php_class_data_local_templates, gstruct,
        *myx_get_struct_export_php_process_member_loop);
    char *class_data_global_string=
      process_template_references(php_class_data_global_templates[0], php_class_data_global_templates, gstruct,
        *myx_get_struct_export_php_process_member_loop);
    
    char *dot_pos= g_strrstr(gstruct->name, ".")+1;

    if (dot_pos)
      struct_file_name= g_strdup(dot_pos);
    else
      struct_file_name= g_strdup(gstruct->name);

    // ----------------------------------------------------------------------------------------------------------------
    // Set filenames
    class_filename= g_strdup_printf("%s%s.php", output_path, struct_file_name);
    class_data_interface_filename= g_strdup_printf("%s%sData.php", output_path, struct_file_name);
    class_data_local_filename= g_strdup_printf("%s%sDataLocal.php", output_path, struct_file_name);
    class_data_global_filename= g_strdup_printf("%s%sDataGlobal.php", output_path, struct_file_name);

    g_free(struct_file_name);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class
    class_string= 
      str_g_replace(
        str_g_replace(
          class_string, "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class Data Interface
    class_data_interface_string= 
      str_g_replace(
        str_g_replace(
          class_data_interface_string, "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_data_interface_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_data_interface_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class Data Local
    class_data_local_string= 
      str_g_replace(
        str_g_replace(
          class_data_local_string, "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_data_local_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_data_local_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // Create Class Data Global
    class_data_global_string= 
      str_g_replace(
        str_g_replace(
          class_data_global_string, "%struct_name%", struct_name),
        "%struct_parent_name%", parent_struct_name);

    f= myx_fopen(class_data_global_filename, "w");
    if (!f)
      return MYX_GRT_CANT_OPEN_FILE;

    fprintf(f, "%s", class_data_global_string);
    fclose(f);

    // ----------------------------------------------------------------------------------------------------------------
    // free strings
    g_free(struct_name);
    g_free(parent_struct_name);
    g_free(class_filename);
    g_free(class_data_interface_filename);
    g_free(class_data_local_filename);
    g_free(class_data_global_filename);
    g_free(class_string);
    g_free(class_data_interface_string);
    g_free(class_data_local_string);
    g_free(class_data_global_string);
  }

  return MYX_GRT_NO_ERROR;
}


static struct {
  MYX_ICON_TYPE type;
  const char *suffix;
} type_suffixes[]= {
  {MYX_IT_SMALL, "16x16"},
  {MYX_IT_STANDARD, "24x24"},
  {MYX_IT_MANY_STANDARD, "many_32x32"},
  {0, NULL}
};

typedef struct {
  unsigned int length;
  char data[1];
} DATA_PIECE;

char * myx_grt_struct_get_icon_path(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, MYX_ICON_TYPE icon_type)
{
  int i;
  const char *suffix= "";

  for (i= 0; type_suffixes[i].suffix; i++)
  {
    if (type_suffixes[i].type == icon_type)
    {
      suffix= type_suffixes[i].suffix;
      break;
    }
  }

  return g_strdup_printf("%s.%s.png", myx_grt_struct_get_name(gstruct), suffix);
}

const char * myx_grt_struct_get_icon(MYX_GRT *grt, const char *source_path, MYX_GRT_STRUCT *gstruct,
                                    MYX_ICON_TYPE icon_type, unsigned int *length)
{
  char *fn;
  const char *suffix= "";
  int i;
  DATA_PIECE *data;
  char *icon_data= NULL;

  *length= 0;
  
  g_return_val_if_fail(grt != NULL, NULL);
  g_return_val_if_fail(source_path != NULL, NULL);
  g_return_val_if_fail(gstruct != NULL, NULL);
  g_return_val_if_fail(length != NULL, NULL);
  
  for (i= 0; type_suffixes[i].suffix; i++)
  {
    if (type_suffixes[i].type == icon_type)
    {
      suffix= type_suffixes[i].suffix;
      break;
    }
  }

  fn= g_strdup_printf("%s.%s.png", myx_grt_struct_get_name(gstruct), suffix);

  data= g_hash_table_lookup(grt->struct_icon_cache, fn);
  if (data)
  {
    icon_data= (char*)data->data;
    *length= data->length;
  }
  else
  {
    char *path= g_build_filename(source_path, fn, NULL);

    if (!g_file_get_contents(path, &icon_data, (gsize*)length, NULL))
    {
      if (gstruct->parent_struct_name)
      {
        MYX_GRT_STRUCT *parent= myx_grt_struct_get(grt, gstruct->parent_struct_name);
        if (parent)
          icon_data= (char*)myx_grt_struct_get_icon(grt, source_path, parent, icon_type, length);
        else
          icon_data= NULL;
      }
      else
        icon_data= NULL;
    }
    else
    {
      data= g_malloc0(sizeof(DATA_PIECE)+*length);
      data->length= *length;
      memcpy(data->data, icon_data, *length);

      g_hash_table_insert(grt->struct_icon_cache, g_strdup(fn), data);
    }
    g_free(path);
  }
  g_free(fn);

  return icon_data;
}
