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

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "html.h"
#include <gtkmm/private/layout_p.h>
#include <glibmm/class.h>

#ifdef WITH_GTKHTML2
#define class klass
#include <libgtkhtml/gtkhtml.h>
#undef class

#define gtk_html_get_type() html_view_get_type()
#endif


namespace Glib 
{
Gtk::HTML* wrap(GtkHTML* object, bool take_copy)
{
  return dynamic_cast<Gtk::HTML *> (Glib::wrap_auto ((GObject*)(object), take_copy));
}
}


namespace Gtk {
  
class HTML_Class : public Glib::Class
{
public:
  typedef HTML CppObjectType;
  typedef GtkHTML BaseObjectType;
  typedef GtkHTMLClass BaseClassType;
  typedef Gtk::Layout_Class CppClassParent;
  typedef GtkLayoutClass BaseClassParent;
                                                                              
  friend class HTML;
                                                                              
  const Glib::Class& init();
                                                                              
  static void class_init_function(void* g_class, void* class_data);
                                                                              
  static Glib::ObjectBase* wrap_new(GObject*);
                                                                              
protected:

};

  


const Glib::Class& HTML_Class::init()
{
  if(!gtype_)
  {
    class_init_func_ = &HTML_Class::class_init_function;

    CppClassParent::CppObjectType::get_type();

    register_derived_type(gtk_html_get_type());
  }
                                                                              
  return *this;
}


void HTML_Class::class_init_function(void* g_class, void* class_data)
{
  BaseClassType *const klass = static_cast<BaseClassType*>(g_class);
  CppClassParent::class_init_function(klass, class_data);
}


Glib::ObjectBase* HTML_Class::wrap_new(GObject* o)
{
  return manage(new HTML((GtkHTML*)(o)));
}

  
//----------------------------------------------------------------------


HTML::HTML(const Glib::ConstructParams& construct_params)
:
  Gtk::Layout(construct_params)
{
  setup();
}


HTML::HTML(GtkHTML* castitem)
:
  Gtk::Layout((GtkLayout*)(castitem))
{
  setup();
}
                                                                              
HTML::~HTML()
{
  destroy_();
}
                                                                              
HTML::CppClassType HTML::html_class_; // initialize static member

GType HTML::get_type()
{
  return html_class_.init().get_type();
}
                                                                              
GType HTML::get_base_type()
{
  return gtk_html_get_type();
}

  
HTML::HTML()
:
  Glib::ObjectBase(), //Mark this class as gtkmmproc-generated, rather than a custom class, to allow vfunc optimisations.
  Gtk::Layout(Glib::ConstructParams(html_class_.init()))
{
  setup();
}

  
HTML *HTML::create()
{
#ifdef WITH_GTKHTML2
  return Glib::wrap(HTML_VIEW(html_view_new()), false);
#else
  return Glib::wrap(GTK_HTML(gtk_html_new()), false);
#endif
}


#ifdef WITH_GTKHTML2
static void link_clicked(HtmlDocument *doc, const gchar *url, gpointer data);
#endif


void HTML::setup()
{
#ifdef WITH_GTKHTML2
  HtmlDocument *doc= html_document_new();
  html_view_set_document(gobj(), doc);
  g_signal_connect(doc, "request_url", G_CALLBACK(url_requested), this);
  g_signal_connect(doc, "link_clicked", G_CALLBACK(link_clicked), this);
  g_object_unref(G_OBJECT(doc));
#else
  g_signal_connect(gobj(), "url_requested", G_CALLBACK(url_requested), this);
#endif
}
  

void HTML::add_search_path(const std::string &path)
{
  if (std::find(_search_path.begin(), _search_path.end(), path) == _search_path.end())
    _search_path.push_back(path);
}

  
void HTML::load_empty()
{
#ifdef WITH_GTKHTML2
  html_document_clear(gobj()->document);
#else
  gtk_html_load_empty(gobj());
#endif
}
 
  
void HTML::load_from_string(const Glib::ustring &str)
{
#ifdef WITH_GTKHTML2
  HtmlDocument *doc= gobj()->document;

  if (html_document_open_stream(doc, "text/html")) 
  {
    html_document_write_stream(doc, str.data(), str.bytes());
    html_document_close_stream(doc);
  }
#else
  gtk_html_load_from_string(gobj(), str.data(), str.size());
#endif
}

  
  
HTML::Stream HTML::begin()
{
#ifdef WITH_GTKHTML2
  html_document_open_stream(gobj()->document, "text/html");
  return NULL;
#else
  return gtk_html_begin(gobj());
#endif
}

  
void HTML::write(HTML::Stream stream, const Glib::ustring &str)
{
#ifdef WITH_GTKHTML2
  html_document_write_stream(HTML_VIEW(gobj())->document, str.data(), str.bytes());
#else
  gtk_html_write(gobj(), stream, str.data(), str.size());
#endif
}

  
void HTML::write(HTML::Stream stream, const char *data, unsigned int size)
{
#ifdef WITH_GTKHTML2
  html_document_write_stream(HTML_VIEW(gobj())->document, data, size);
#else
  gtk_html_write(gobj(), stream, data, size);
#endif
}

  
void HTML::end(HTML::Stream stream)
{
#ifdef WITH_GTKHTML2
  html_document_close_stream(HTML_VIEW(gobj())->document);
#else
  gtk_html_end(gobj(), stream, GTK_HTML_STREAM_OK);
#endif
}

  
bool HTML::jump_to_anchor(const Glib::ustring &str)
{
#ifdef WITH_GTKHTML2
  html_view_jump_to_anchor(gobj(), str.c_str());
  return true;
#else
  return gtk_html_jump_to_anchor(gobj(), str.c_str());
#endif
}
//

std::string HTML::find_file(const std::string &file)
{
  for (std::list<std::string>::const_iterator iter= _search_path.begin();
       iter != _search_path.end(); ++iter)
  {
    std::string path= Glib::build_filename(*iter, file);

    if (access(path.c_str(), F_OK)==0)
      return path;
  }
  return "";
}
  

static guint get_file_size(const char *path)
{
  struct stat buf;

  if (stat(path, &buf) < 0) {
    return 0;
  }

  return buf.st_size;
}


#ifdef WITH_GTKHTML2
static void link_clicked(HtmlDocument *doc, const char *url, gpointer data)
{
  HTML *me= (HTML*)data;

  if (*url == '#')
  {
    me->jump_to_anchor(url+1);
  }
}
#endif

void HTML::url_requested(GtkHTML *html, const char *url, 
                         Stream handle, gpointer data)
{
  HTML *me= (HTML*)data;

  if (!url) return;
  std::string path= me->find_file(url);
  int fd;

  if (!path.empty() && (fd= ::open(path.c_str(), O_RDONLY))>=0)
  {
    guint size= get_file_size(path.c_str());
    char *buffer= 0;
    
    if (size <= 0) return;

    buffer= (char*)g_malloc(size);
    size= read(fd, buffer, size);

#ifdef WITH_GTKHTML2
    html_stream_write(handle, buffer, size);
#else
    gtk_html_write(html, handle, buffer, size);
#endif
    g_free(buffer);
  }
  else
  {
#ifdef WITH_GTKHTML2
    me->_signal_url_requested.emit(me, url, NULL);
#else
    me->_signal_url_requested.emit(me, url, handle);
#endif
  }
}
  
  

  
}; // namespace Gtk



void html_init()
{
  Glib::wrap_register(gtk_html_get_type(), &Gtk::HTML_Class::wrap_new);

  Gtk::HTML::get_type();
}


