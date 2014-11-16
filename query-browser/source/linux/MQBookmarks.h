/* Copyright (C) 2004 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

#ifndef _MQBOOKMARKS_H_
#define _MQBOOKMARKS_H_

#include <gtkmm/tooltips.h>
#include <gtkmm/menu.h>
#include "myx_qb_public_interface.h"

class MGGladeXML;
class MQNewBookmarkDialog;
class MQBookmarkBrowser;

class MQBookmarks : public Glib::ObjectBase {
  public:
    class Bookmark {
      public:
        Glib::ustring name;
        virtual ~Bookmark() {};
        virtual bool is_group()= 0;
    };
    class BookmarkGroup : public Bookmark {
      public:
        std::list<Bookmark*> items;
        virtual ~BookmarkGroup() 
        {
          for (std::list<Bookmark*>::iterator i=items.begin(); i!=items.end(); ++i)
            delete *i;
        }

        virtual bool is_group() { return true; };
        
        BookmarkGroup *find_group(const Glib::ustring &group)
        {
          if (name == group)
            return this;

          for (std::list<Bookmark*>::const_iterator iter= items.begin();
               iter != items.end(); ++iter)
          {
            if (((BookmarkGroup*)*iter)->is_group())
            {
              BookmarkGroup *g= ((BookmarkGroup*)*iter)->find_group(group);
              if (g)
                return g;
            }
          }
          return 0;
        }
    };
    class BookmarkItem : public Bookmark {
      public:
        Glib::ustring query;

        Glib::ustring catalog;
        Glib::ustring schema;

        unsigned int access_count;
        time_t ctime;
        time_t mtime;
        time_t atime;

        void touch() { time(&atime); };
        virtual bool is_group() { return false; };
    };

    typedef sigc::slot1<void,BookmarkItem*> OpenBookmarkSlot;
    typedef sigc::signal0<void> BookmarksChangedSignal;

  protected:
    MGGladeXML *_addxml;

    BookmarkGroup *_root_group;

    MQNewBookmarkDialog *_add_dialog;
    
    MQBookmarkBrowser *_browser;
    
    Gtk::Tooltips _tips;

    BookmarksChangedSignal _signal_changed;

    std::list<Glib::ustring> get_bookmark_folder_list();

    BookmarkGroup *load_group(MYX_BOOKMARK_GROUP *group);
    BookmarkItem *load_item(MYX_BOOKMARK *group);

    void store_group(BookmarkGroup *bgroup, MYX_BOOKMARK_GROUP *group);
    void store_item(BookmarkItem *item, MYX_BOOKMARK *bmk);
    void load();
    
    void add_bookmark_group_to_menu(Gtk::Menu *menu, BookmarkGroup *group,
                                    const OpenBookmarkSlot &slot);
  public:
    MQBookmarks(MGGladeXML *add_xml);
    virtual ~MQBookmarks();

    void save();

    void update_bookmark_menu(Gtk::Menu *menu, int index,
                              const OpenBookmarkSlot &slot);

    void interactive_add(const Glib::ustring &catalog,
                         const Glib::ustring &schema,
                         const Glib::ustring &query);
    
    BookmarkGroup *get_bookmarks() { return _root_group; };
    
    MQBookmarkBrowser *get_browser() { return _browser; };

    BookmarkGroup *create_group(BookmarkGroup *group, const Glib::ustring &name);
    bool delete_item(Bookmark *bm, BookmarkGroup *group= 0);

    void bookmarks_changed();
    
    BookmarksChangedSignal signal_changed() { return _signal_changed; };
};


#endif /* _MQBOOKMARKS_H_ */
