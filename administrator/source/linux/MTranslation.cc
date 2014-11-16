

#include "MTranslation.h"



MTranslation::MTranslation()
    : _trans(0)
{
}


MTranslation::~MTranslation()
{
  if (_trans)
      myx_free_trans(_trans);
}


bool MTranslation::open(const std::string &file,
                        const std::string &app_file,
                        const std::string &lang)
{
  MYX_LIB_ERROR err;
  
  _trans= myx_init_trans(file.c_str(), app_file.c_str(), lang.c_str(), &err);
  
  return _trans!=0;
}


Glib::ustring MTranslation::get(const char *group,
                                const char *keyword, bool general)
{
  if (general)
    return myx_tg(_trans, group, keyword, keyword);
  else
    return myx_t(_trans, group, keyword, keyword);
}


Glib::ustring MTranslation::get(const std::string &group, 
                                const std::string &keyword, bool general)
{
  if (general)
    return myx_tg(_trans, group.c_str(), keyword.c_str(), keyword.c_str());
  else
    return myx_t(_trans, group.c_str(), keyword.c_str(), keyword.c_str());
}


Glib::ustring MTranslation::get(const Glib::ustring &group, 
                                const Glib::ustring &keyword, bool general)
{
  if (general)
    return myx_tg(_trans, group.c_str(), keyword.c_str(), keyword.c_str());
  else
    return myx_t(_trans, group.c_str(), keyword.c_str(), keyword.c_str());
}

