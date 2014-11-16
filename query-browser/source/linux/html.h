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

#ifndef _HTML_H_
#define _HTML_H_

// quick&dirty wrapper for gtkhtml

extern "C" {
#include "config.h"
#ifndef WITH_GTKHTML2
#include <gtkhtml/gtkhtml.h>
#else
#include <libgtkhtml/util/htmlstream.h>
#endif
};
#include <gtkmm/layout.h>

#ifdef WITH_GTKHTML2
typedef struct _HtmlView GtkHTML;
typedef struct _HtmlViewClass GtkHTMLClass;
#else
typedef struct _GtkHTML GtkHTML;
typedef struct _GtkHTMLClass GtkHTMLClass;
#endif

namespace Gtk {

class HTML_Class;
  
class HTML : public Gtk::Layout {
  public:
    typedef HTML CppObjectType;
    typedef HTML_Class CppClassType;
//    typedef GtkHTML BaseObjectType;
//    typedef GtkHTMLClass BaseClassType;
    virtual ~HTML();
    
#ifdef WITH_GTKHTML2
    typedef HtmlStream* Stream;
#else
    typedef struct _GtkHTMLStream* Stream;
#endif
    

    typedef sigc::signal3<void,HTML*,const std::string&,Stream> URLRequestedSignal;
    
  private:
    friend class HTML_Class;
    static CppClassType html_class_;

    std::list<std::string> _search_path;
    
    URLRequestedSignal _signal_url_requested;
    
    // noncopyable
    HTML(const HTML&);
    HTML& operator=(const HTML&);

    void setup();
    
#ifdef WITH_GTKHTML2
    static void url_requested(GtkHTML *html, const char *url, Stream stream,
                              gpointer data);
#else
    static void url_requested(GtkHTML *html, const char *url, 
                              Stream handle, gpointer data);
#endif
  protected:
    explicit HTML(const Glib::ConstructParams& construct_params);
    explicit HTML(GtkHTML* castitem);
    
    std::string find_file(const std::string &file);

    HTML();
  public:
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;

    GtkHTML*       gobj()       { return reinterpret_cast<GtkHTML*>(gobject_); }
                                                                              
    const GtkHTML* gobj() const { return reinterpret_cast<GtkHTML*>(gobject_); }

  public:

    static HTML *create();
    
    void add_search_path(const std::string &path);
    
    void load_empty();
    void load_from_string(const Glib::ustring &str);
    
    bool jump_to_anchor(const Glib::ustring &str);
    
    Stream begin();
    void write(Stream stream, const Glib::ustring &str);
    void write(Stream stream, const char *data, unsigned int size);
    void end(Stream stream);

    URLRequestedSignal signal_url_requested() { return _signal_url_requested; };
};

}


/*
namespace Glib
{
#ifdef WITH_GTKHTML2
  Gtk::HTML* wrap(GtkHTML* object, bool take_copy = false);
#else
  Gtk::HTML* wrap(GtkHTML* object, bool take_copy = false);
#endif
}
*/

void html_init();


#endif /* _HTML_H_ */
