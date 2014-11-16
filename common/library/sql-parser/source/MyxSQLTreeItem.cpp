#include "MyxSQLTreeItem.h"

MyxSQLTreeItem::SubItemList MyxSQLSimpleTreeItem::_empty_list;

MYX_PUBLIC_FUNC std::ostream& operator << (std::ostream& os, const MyxSQLTreeItem& item)
{
  if(item.get_value()[0] != '\0')
  {
    os << "<elem name='" << item.get_name() << "' value='" << item.get_value() << "'>";
  }
  else
  {
    os << "<elem name='" << item.get_name() << "'>";
  }

  if(item.get_subitems()->size() > 0) 
  {
    for(MyxSQLTreeItem::SubItemList::const_iterator it= item.get_subitems()->begin(); it != item.get_subitems()->end(); it++)
    {
      const MyxSQLTreeItem* p_subitem= *it;
      os << *p_subitem;
    }
  }
  os << "</elem>";
  return os;
}

// path should be in the form name/name/name
const MyxSQLTreeItem *MyxSQLTreeItem::get_subitem_by_path(const char *path) const
{
  size_t len= strlen(path);
  char *t= strcpy(new char[len+1], path);
  char *p= t;
  const MyxSQLTreeItem *retval= this;

  while(retval != NULL) 
  {
    char *n= p;
    while((*n != '\0') && (*n != '/')) 
    {
      n++;
    }
    if(n == p)
    {
      break;
    }
    *n= '\0';
    retval= retval->get_subitem_by_name(p);
    p= n+1;
    if((size_t)(p - t) >= len) 
    {
      break;
    }
  }

  delete[] t;
  return retval;
}

const MyxSQLTreeItem *MyxSQLTreeItem::get_subitem_by_name(const char *name, int position) const
{
  for(MyxSQLTreeItem::SubItemList::const_iterator it = _subitems->begin(); it != _subitems->end(); it++) 
  {
    MyxSQLTreeItem *item = *it;
    if(item->name_equals(name)) 
    {
      if(position == 0) {
        return item;
      } else {
        position--;
      }
    }
  }
  return NULL;
}

const MyxSQLTreeItem *MyxSQLTreeItem::rget_subitem_by_name(const char *name, int position) const
{
  for(MyxSQLTreeItem::SubItemList::reverse_iterator it = _subitems->rbegin(); it != _subitems->rend(); it++) 
  {
    MyxSQLTreeItem *item = *it;
    if(item->name_equals(name)) 
    {
      if(position == 0) {
        return item;
      } else {
        position--;
      }
    }
  }
  return NULL;
}

char * MyxSQLTreeItem::get_subitems_as_string(const char *delim) const
{
  const char *current_delim= "";
  std::string to;
  for(MyxSQLTreeItem::SubItemList::const_iterator it = _subitems->begin(); it != _subitems->end(); it++) 
  {
    MyxSQLTreeItem *subitem = *it;
    if(subitem->get_subitems()->size() > 0) 
    {
      char *s= subitem->get_subitems_as_string(delim);
      to += current_delim;
      current_delim= delim;
      to += s;
      delete[] s;
    }
    else
    {
      to += current_delim;
      current_delim= delim;
      to += subitem->get_value();
    }
  }
  return strcpy(new char[to.length()+1], to.c_str());
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
// MyxSchemaTreeItemHelperBase members

bool MyxSchemaTreeItemHelperBase::check1word(MyxSQLTreeItem *tree, const char *w1)
{
  return tree->name_equals(w1);
}

bool MyxSchemaTreeItemHelperBase::check2words(MyxSQLTreeItem *tree, const char *w1, const char *w2)
{
  if(!tree->name_equals(w1))
  {
    return false;
  }
  const MyxSQLTreeItem *obj= *tree->get_subitems()->begin();
  if(obj != NULL)
  {
    return obj->name_equals(w2);
  }
  return false;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
// MyxCreateTableTreeItem members

bool MyxCreateSchemaTreeItem::check(MyxSQLTreeItem *tree)
{
  return check2words(tree, "create", "database");
}

MyxCreateSchemaTreeItem::MyxCreateSchemaTreeItem(MyxSQLTreeItem *tree)
: _item(tree), schema_name(0)
{
  const MyxSQLTreeItem *ident;
  ident= _item->rget_subitem_by_name("ident");
  if(ident == NULL)
  {
    ident= _item->rget_subitem_by_name("quoted_ident");
  }
  schema_name= ident->get_value();
}

MyxCreateSchemaTreeItem::~MyxCreateSchemaTreeItem()
{
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
// MyxCreateTableTreeItem members

bool MyxCreateTableTreeItem::check(MyxSQLTreeItem *tree)
{
  return check2words(tree, "create", "table");
}

MyxCreateTableTreeItem::MyxCreateTableTreeItem(MyxSQLTreeItem *tree)
: _item(tree), schema_name(0), table_name(0), table_engine(0), row_format(0), next_auto_inc(0), def_collation(0), delay_key_write(0), 
  comment(0), temporary(false), def_charset(0), insert_method(0), data_directory(0), index_directory(0), raid_type(0), raid_chunks(0),
  raid_chunksize(0), pack_keys(0), avg_row_length(0), min_rows(0), max_rows(0), checksum(0)
{
  const MyxSQLTreeItem *table_ident= _item->rget_subitem_by_name("table_ident");
  if(table_ident->get_subitems()->size() == 3)   // ident.ident
  {
    schema_name= (*table_ident->get_subitems()->begin())->get_value();
  } 
  table_name= (*table_ident->get_subitems()->rbegin())->get_value();

  // process table attributes
  const MyxSQLTreeItem *options= _item->rget_subitem_by_name("create_table_options");
  if(options != NULL)
  {
    for(MyxSQLTreeItem::SubItemList::const_iterator it = options->get_subitems()->begin(); it != options->get_subitems()->end(); it++)
    {
      const MyxSQLTreeItem *option= *it;
      MyxSQLTreeItem::SubItemList::const_iterator parts= option->get_subitems()->begin();
      const MyxSQLTreeItem *option1= *parts++;
      const MyxSQLTreeItem *option2= *parts++;
      const MyxSQLTreeItem *option3= *parts++;

      if(option1->name_equals("engine") || option1->name_equals("type"))
      {
        table_engine= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("row_format")) 
      {
        row_format= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("auto_increment")) 
      {
        next_auto_inc= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if((option1->name_equals("default") && option2->name_equals("character") && option3->name_equals("set")) 
        || (option1->name_equals("default") && option2->name_equals("charset")))
      {
        def_charset= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("default") && option2->name_equals("collation")) 
      {
        def_collation= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("insert_method")) 
      {
        insert_method= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("delay_key_write")) 
      {
        delay_key_write= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("comment")) 
      {
        comment= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("data") && option2->name_equals("directory")) 
      {
        data_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("index") && option2->name_equals("directory")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("raid_type")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("raid_chunks")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("raid_chunksize")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("pack_keys")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("avg_row_length")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("min_rows")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("max_rows")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
      if(option1->name_equals("checksum")) 
      {
        index_directory= (*option->get_subitems()->rbegin())->get_value();
        continue;
      }
    }
  }
  
  // process columns
  const MyxSQLTreeItem *columns= _item->rget_subitem_by_name("field_list");
  for(MyxSQLTreeItem::SubItemList::const_iterator it = columns->get_subitems()->begin(); it != columns->get_subitems()->end(); it++)
  {
    const MyxSQLTreeItem *coldef= (*it);
    if(!coldef->name_equals("column_def")) 
    {
      continue;
    }
    _columns.push_back(Column(coldef));
  }

  // process indices and foreign keys
  const MyxSQLTreeItem *indices= _item->rget_subitem_by_name("field_list");
  for(MyxSQLTreeItem::SubItemList::const_iterator it = indices->get_subitems()->begin(); it != indices->get_subitems()->end(); it++)
  {
    const MyxSQLTreeItem *keydef= (*it);
    if(!keydef->name_equals("key_def")) 
    {
      continue;
    }
    if(keydef->get_subitem_by_name("foreign"))
    {
      _fks.push_back(ForeignKey(keydef, &_columns));
    }
    else
    {
      _indices.push_back(Index(keydef, &_columns));
    }
  }
}

MyxCreateTableTreeItem::~MyxCreateTableTreeItem()
{
}

MyxCreateTableTreeItem::Column::Column(const MyxSQLTreeItem *column)
: _column(column), column_name(0), datatype_name(0), default_value(0), default_value_null(false), precision(0), scale(0), nullable(true),
  length(0), charset_name(0), collation_name(0), explicit_params(0), auto_increment(0), comment(0), unsigned_flag(false), zerofill_flag(false), 
  binary_flag(false), ascii_flag(false), unicode_flag(false)
{
  const MyxSQLTreeItem *field_spec= column->get_subitem_by_name("field_spec");
  const MyxSQLTreeItem *ident= field_spec->get_subitem_by_path("field_ident/ident");
  if(ident == NULL) 
  {
    ident= field_spec->get_subitem_by_path("field_ident/quoted_ident");
  }
  if(ident != NULL)
  {
    column_name= ident->get_value();
  }
  
  // field type details
  const MyxSQLTreeItem *type= field_spec->get_subitem_by_name("type");
  if(type != NULL)
  {
    datatype_name= type->get_subitems()->front()->get_value();
    const MyxSQLTreeItem *in_braces;
    if((in_braces= type->get_subitem_by_name("opt_len")) != NULL)
    {
      precision = atoi(in_braces->get_subitem_by_name("num")->get_value());
      scale = 0;
    } 
    else if((in_braces= type->get_subitem_by_name("float_options")) != NULL)
    {
      precision = atoi(in_braces->get_subitem_by_name("num", 0)->get_value());
      scale = atoi(in_braces->get_subitem_by_name("num", 1)->get_value());
    }
    else if((in_braces= type->get_subitem_by_name("string_list")) != NULL)  // enum or set
    {
      explicit_params= in_braces->get_subitems_as_string();
    }
    else if((in_braces= type->get_subitem_by_name("num")) != NULL) // character types
    {
      length = atoi(in_braces->get_value());
    }

    const MyxSQLTreeItem *opt_binary= type->get_subitem_by_name("opt_binary");
    if(opt_binary != NULL)
    {
      if(opt_binary->get_subitem_by_name("charset") != NULL)
      {
        charset_name= opt_binary->get_subitem_by_name("ident")->get_value();
      }
      if(opt_binary->get_subitem_by_name("binary") != NULL)
      {
        binary_flag= true;
      }
      if(opt_binary->get_subitem_by_name("ascii") != NULL)
      {
        ascii_flag= true;
      }
      if(opt_binary->get_subitem_by_name("unicode") != NULL)
      {
        unicode_flag= true;
      }
    }

    const MyxSQLTreeItem *field_opt_list= type->get_subitem_by_name("field_opt_list");
    if(field_opt_list != NULL)
    {
      if(field_opt_list->get_subitem_by_name("unsigned") != NULL)
      {
        unsigned_flag= true;
      }
      if(field_opt_list->get_subitem_by_name("zerofill") != NULL)
      {
        zerofill_flag= true;
      }
    }
  }
  
  // field type options
  const MyxSQLTreeItem *options= field_spec->get_subitem_by_name("opt_attribute_list");
  if(options != NULL)
  {
    for(MyxSQLTreeItem::SubItemList::const_iterator it= options->get_subitems()->begin(); it != options->get_subitems()->end(); it++)
    {
      const MyxSQLTreeItem *attr= (*it);
      if(!attr->name_equals("attribute")) 
      {
        continue;
      }
      MyxSQLTreeItem::SubItemList::const_iterator attr_iter= attr->get_subitems()->begin();
      const MyxSQLTreeItem *attr1= *attr_iter;
      attr_iter++;
      const MyxSQLTreeItem *attr2= *attr_iter;
      attr_iter++;

      if(attr1 && attr2 && attr1->name_equals("default")) 
      {
        if(attr2->name_equals("signed_literal") && attr2->get_subitem_by_path("literal/null"))
        {
          default_value_null= true;  
        }
        default_value= attr2->get_subitems_as_string();
        continue;
      }
      if(attr1 && attr2 && attr1->name_equals("not") && attr2->name_equals("null")) 
      {
        nullable= false;
        continue;
      }
      if(attr1 && attr2 && attr1->name_equals("collate")) 
      {
        collation_name= attr2->get_value();
        continue;
      }
      if(attr1 && attr1->name_equals("auto_increment"))
      {
        auto_increment= 1;
      }
    }
  }
}

MyxCreateTableTreeItem::Column::~Column()
{
}

MyxCreateTableTreeItem::Index::Index(const MyxSQLTreeItem *keydef, const ColumnList *tablecols)
: _index(keydef), index_name(0), primary(false), unique(false), index_type(0)
{
  const MyxSQLTreeItem *keyname;
  if((keyname= keydef->get_subitem_by_path("field_ident/ident")) != NULL)
  {
    index_name= keyname->get_value();
  }
  if(keydef->get_subitem_by_path("constraint_key_type/primary"))
  {
    primary= true;
  }
  if(keydef->get_subitem_by_path("constraint_key_type/unique"))
  {
    unique= true;
  }
  
  const MyxSQLTreeItem *keyalg= keydef->get_subitem_by_name("keyalg");
  const MyxSQLTreeItem *keytype;

  if(keyalg != NULL)
  {
    if(((keytype= keyalg->get_subitem_by_name("btree")) != 0) 
      || ((keytype= keyalg->get_subitem_by_name("rtree")) != 0) 
      || ((keytype= keyalg->get_subitem_by_name("hash")) != 0))
    {
      index_type= keytype->get_value();
    }
  }
  if(((keytype= keydef->get_subitem_by_path("key_type/spatial")) != 0)
    || ((keytype= keydef->get_subitem_by_path("key_type/fulltext")) != 0))
  {
    index_type= keytype->get_value();
  }

  // process index columns
  const MyxSQLTreeItem *keycols= _index->rget_subitem_by_name("key_list");
  for(MyxSQLTreeItem::SubItemList::const_iterator it = keycols->get_subitems()->begin(); it != keycols->get_subitems()->end(); it++)
  {
    MyxSQLTreeItem::SubItemList::const_iterator dirit= it;
    const MyxSQLTreeItem *coldef= (*it);
    if(!coldef->name_equals("key_part")) 
    {
      continue;
    }
    bool desc= false;
    if(dirit != keycols->get_subitems()->end())
    {
      const MyxSQLTreeItem *dir= (*++dirit);
      desc= (dir->name_equals("order_dir") && dir->value_equals("desc"));
    }
    _columns.push_back(IndexColumn(coldef, desc, tablecols));
  }
}

MyxCreateTableTreeItem::IndexColumn::IndexColumn(const MyxSQLTreeItem *coldef, bool desc, const ColumnList *tablecols)
: length(0), descend(desc), ref_column(0), stored_function(0), comment(0)
{
  const MyxSQLTreeItem *len= coldef->get_subitem_by_name("num");
  if(len != NULL)
  {
    length= atoi(len->get_value());
  }
  const MyxSQLTreeItem *name= coldef->get_subitem_by_name("ident");
  if(name == NULL)
  {
    name= coldef->get_subitem_by_name("quoted_ident");
  }
  for(MyxCreateTableTreeItem::ColumnList::const_iterator it= tablecols->begin(); it != tablecols->end(); it++)
  {
    const MyxCreateTableTreeItem::Column& c= *it;
    if(strcmp(c.get_column_name(), name->get_value()) == 0)
    {
      ref_column= &c;
      break;
    }
  }
}

MyxCreateTableTreeItem::ForeignKey::ForeignKey(const MyxSQLTreeItem *fk, const ColumnList *tablecols)
: _fk(fk), fk_name(0), delete_rule(0), update_rule(0), ref_schema_name(0), ref_table_name(0)
{
  const MyxSQLTreeItem *name= fk->get_subitem_by_path("field_ident/ident");
  if(name != NULL)
  {
    fk_name= name->get_value();
  }
  const MyxSQLTreeItem *ref= fk->get_subitem_by_name("references");
  const MyxSQLTreeItem *on_del_list= ref->get_subitem_by_path("opt_ref_list/opt_on_delete_list");
  for(MyxSQLTreeItem::SubItemList::const_iterator it= on_del_list->get_subitems()->begin(); it != on_del_list->get_subitems()->end(); it++)
  {
    const MyxSQLTreeItem *item= *it;
    if(item->name_equals("opt_on_delete_item") && item->get_subitem_by_name("delete") && item->get_subitem_by_name("delete_option"))
    {
      delete_rule= item->get_subitem_by_name("delete_option")->get_subitems_as_string();
      continue;
    }
    if(item->name_equals("opt_on_delete_item") && item->get_subitem_by_name("update") && item->get_subitem_by_name("delete_option"))
    {
      update_rule= item->get_subitem_by_name("delete_option")->get_subitems_as_string();
      continue;
    }
  }
  const MyxSQLTreeItem *reftable= ref->get_subitem_by_name("table_ident");
  if(reftable != NULL)
  {
    if(reftable->get_subitems()->size() > 1) 
    {
      ref_schema_name= reftable->get_subitem_by_name("ident")->get_value();
    }
    ref_table_name= reftable->rget_subitem_by_name("ident")->get_value();
  }

  // own columns
  const MyxSQLTreeItem *keycols= fk->rget_subitem_by_name("key_list");
  for(MyxSQLTreeItem::SubItemList::const_iterator it = keycols->get_subitems()->begin(); it != keycols->get_subitems()->end(); it++)
  {
    const MyxSQLTreeItem *keypart= *it;
    if(!keypart->name_equals("key_part"))
    {
      continue;
    }
    const MyxSQLTreeItem *colname= keypart->get_subitem_by_name("ident");
    if(colname == NULL)
    {
      colname= keypart->get_subitem_by_name("quoted_ident");
    }
    if(colname != NULL)
    {
      const char *n= colname->get_value();
      for(ColumnList::const_iterator colit= tablecols->begin(); colit != tablecols->end(); colit++)
      {
        const Column &c= *colit;
        if(strcmp(c.get_column_name(), n) == 0) 
        {
          _columns.push_back(c);
          break;
        }
      }
    }
  }

  // ref columns
  const MyxSQLTreeItem *keyrefs= ref->get_subitem_by_path("opt_ref_list/ref_list");
  for(MyxSQLTreeItem::SubItemList::const_iterator it = keyrefs->get_subitems()->begin(); it != keyrefs->get_subitems()->end(); it++)
  {
    MyxSQLTreeItem *keyref= *it;
    if(!keyref->name_equals("ident"))
    {
      continue;
    }
    _ref_columns.push_back(strcpy(new char[strlen(keyref->get_value())+1], keyref->get_value()));
  }
}

bool MyxCreateRoutineTreeItem::check(MyxSQLTreeItem *tree)
{
  return check2words(tree, "create", "procedure") || check2words(tree, "create", "function");
}

MyxCreateRoutineTreeItem::MyxCreateRoutineTreeItem(MyxSQLTreeItem *tree)
: _item(tree), routine_type(0), routine_name(0), schema_name(0), return_type(0)
{
  const MyxSQLTreeItem *name= _item->get_subitem_by_name("sp_name");
  if(name != NULL)
  {
    if(name->get_subitems()->size() == 3)   // ident.ident
    {
      schema_name= (*name->get_subitems()->begin())->get_value();
    } 
    routine_name= (*name->get_subitems()->rbegin())->get_value();
  }
  if(_item->get_subitem_by_name("procedure"))
  {
    routine_type= "procedure";
    const MyxSQLTreeItem *params= tree->get_subitem_by_name("sp_pdparams");
    if(params != NULL)
    {
      for(MyxSQLTreeItem::SubItemList::const_iterator it= params->get_subitems()->begin(); it != params->get_subitems()->end(); it++)
      {
        const MyxSQLTreeItem *param= *it;
        if(!param->name_equals("sp_pdparam"))
        {
          continue;
        }
        _params.push_back(RoutineParam(param));
      }
    }
  }
  else
  {
    return_type= _item->get_subitem_by_path("create_function_tail/type")->get_subitems_as_string();
    routine_type= "function";
    const MyxSQLTreeItem *params= tree->get_subitem_by_path("create_function_tail/sp_fdparams");
    if(params != NULL)
    {
      for(MyxSQLTreeItem::SubItemList::const_iterator it= params->get_subitems()->begin(); it != params->get_subitems()->end(); it++)
      {
        const MyxSQLTreeItem *param= *it;
        if(!param->name_equals("sp_fdparam"))
        {
          continue;
        }
        _params.push_back(RoutineParam(param));
      }
    }
  }
}

MyxCreateRoutineTreeItem::~MyxCreateRoutineTreeItem()
{
}

MyxCreateRoutineTreeItem::RoutineParam::RoutineParam(const MyxSQLTreeItem *param)
: _param(param), param_name(0), param_type(0), param_dir(0)
{
  const MyxSQLTreeItem *name= param->get_subitem_by_name("ident");
  if(name != NULL)
  {
    param_name= name->get_value();
  }
  const MyxSQLTreeItem *type= param->get_subitem_by_name("type");
  if(type != NULL)
  {
    param_type= type->get_subitems_as_string();
  }
  if(param->get_subitem_by_name("in"))
  {
    param_dir= "in";
  }
  if(param->get_subitem_by_name("out"))
  {
    param_dir= "out";
  }
  if(param->get_subitem_by_name("inout"))
  {
    param_dir= "inout";
  }
}

bool MyxCreateViewTreeItem::check(MyxSQLTreeItem *tree)
{
  return check2words(tree, "create", "view");
}

MyxCreateViewTreeItem::MyxCreateViewTreeItem(MyxSQLTreeItem *tree)
: _item(tree), schema_name(0), view_name(0), query(0), read_only(false), with_check(false)
{
  const MyxSQLTreeItem *name= _item->get_subitem_by_name("table_ident");
  if(name != NULL)
  {
    if(name->get_subitems()->size() == 3)   // ident.ident
    {
      schema_name= (*name->get_subitems()->begin())->get_value();
    } 
    view_name= (*name->get_subitems()->rbegin())->get_value();
  }
  //query= _item->get_subitem_by_name("select_view_init")->get_subitems_as_string();
  with_check= (_item->get_subitem_by_path("check_option/with") != NULL);
}

MyxCreateViewTreeItem::~MyxCreateViewTreeItem()
{
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
// MyxUseSchemaTreeItem members

bool MyxUseSchemaTreeItem::check(MyxSQLTreeItem *tree)
{
  return check1word(tree, "use");
}

MyxUseSchemaTreeItem::MyxUseSchemaTreeItem(MyxSQLTreeItem *tree)
: _item(tree), schema_name(0)
{
  const MyxSQLTreeItem *ident;
  ident= _item->rget_subitem_by_name("ident");
  if(ident == NULL)
  {
    ident= _item->rget_subitem_by_name("quoted_ident");
  }
  schema_name= ident->get_value();
}

MyxUseSchemaTreeItem::~MyxUseSchemaTreeItem()
{
}

extern "C" 
{

void *new_simple_tree_item(const void *name, const void *value)
{
  return new MyxSQLSimpleTreeItem(static_cast<const char *>(name), static_cast<const char *>(value));
}

void *new_tree_item(const void *name, const void *value, void *list)
{
  return new MyxSQLTreeItem(static_cast<const char *>(name), static_cast<const char *>(value), static_cast<MyxSQLTreeItem::SubItemList *>(list));
}

void *new_tree_item_list()
{
  return new MyxSQLTreeItem::SubItemList;
}

extern void delete_tree_item(void *tree_item)
{
  MyxSQLTreeItem* item= static_cast<MyxSQLTreeItem *>(tree_item);
  delete item;
}

void tree_item_list_add(void *tree_item_list, void *tree_item)
{
  if(tree_item == 0)
  {
    return;
  }
  MyxSQLTreeItem::SubItemList *list= static_cast<MyxSQLTreeItem::SubItemList *>(tree_item_list);
  MyxSQLTreeItem* item= static_cast<MyxSQLTreeItem *>(tree_item);
  list->push_back(item);
}

void *new_tree_item_list_reuse(void *tree_item_from)
{
  MyxSQLTreeItem* item_from= static_cast<MyxSQLTreeItem *>(tree_item_from);
  item_from->del_list(false);
  return const_cast<MyxSQLTreeItem::SubItemList *>(item_from->get_subitems());
}

void tree_item_list_add_all(void *tree_item_list, void *tree_item)
{
  MyxSQLTreeItem::SubItemList *list= static_cast<MyxSQLTreeItem::SubItemList *>(tree_item_list);
  MyxSQLTreeItem* item_from= static_cast<MyxSQLTreeItem *>(tree_item);
  item_from->del_list(false);
  list->insert(list->end(), item_from->get_subitems()->begin(), item_from->get_subitems()->end());
}

const void *tree_item_get_subitem_by_name(const void *tree_item, const char *name, int pos)
{
  const MyxSQLTreeItem* item= static_cast<const MyxSQLTreeItem *>(tree_item);
  return item->get_subitem_by_name(name, pos);
}

const char *tree_item_get_name(const void *tree_item)
{
  const MyxSQLTreeItem* item= static_cast<const MyxSQLTreeItem *>(tree_item);
  return item->get_name();
}

const char *tree_item_get_value(const void *tree_item)
{
  const MyxSQLTreeItem* item= static_cast<const MyxSQLTreeItem *>(tree_item);
  return item->get_value();
}

void tree_item_dump_xml_to_file(const void *tree_item, const char *filename)
{
  const MyxSQLTreeItem* item= static_cast<const MyxSQLTreeItem *>(tree_item);
  std::ofstream os(filename);
  os << *item;
}

} // extern "C"

