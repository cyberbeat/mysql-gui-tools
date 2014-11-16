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

#include "MGCodeEditor.h"

#include <gtksourceview/gtksourcelanguagesmanager.h>
#include <cstring>



MGCodeEditor::MGCodeEditor()
  : _pc_marker(0)
{
  Gdk::Color bg_color, fg_color;
  
  _srcv= gtk_source_view_new();
  g_object_ref(G_OBJECT(_srcv));

  _buffer= GTK_SOURCE_BUFFER(gtk_text_view_get_buffer(GTK_TEXT_VIEW(_srcv)));
  
  _wrapper= Glib::wrap(GTK_TEXT_VIEW(_srcv));
  
  _swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _swin.set_shadow_type(Gtk::SHADOW_IN);
  _swin.add(*_wrapper);
  _wrapper->show();
  
  gtk_source_view_set_show_line_markers(GTK_SOURCE_VIEW(_srcv), TRUE);
  gtk_source_view_set_compose_line_markers(GTK_SOURCE_VIEW(_srcv), FALSE);
  
  bg_color= Gdk::Color("#ffa6a6");
  _wrapper->get_colormap()->alloc_color(bg_color);
  fg_color= Gdk::Color("#bd5952");
  _wrapper->get_colormap()->alloc_color(fg_color);
  gtk_source_view_set_marker_highlight_colors(GTK_SOURCE_VIEW(_srcv), "breakpoint",
                                              fg_color.gobj(), bg_color.gobj());

  bg_color= Gdk::Color("#a6a6ff");
  _wrapper->get_colormap()->alloc_color(bg_color);
  fg_color= Gdk::Color("#5952bd");
  _wrapper->get_colormap()->alloc_color(fg_color);
  gtk_source_view_set_marker_highlight_colors(GTK_SOURCE_VIEW(_srcv), "pc",
                                              fg_color.gobj(), bg_color.gobj());
}


MGCodeEditor::~MGCodeEditor()
{
  // protected in older versions of gtkmm
  //_wrapper->unparent();
  gtk_widget_unparent(GTK_WIDGET(_wrapper->gobj()));
  delete _wrapper;
}


// cheat a bit because the gtksourceview api is extremely limited
extern "C" {
  GtkSourceLanguage *
    _gtk_source_language_new_from_file(const gchar *filename,
                                       GtkSourceLanguagesManager *lm);
};

void MGCodeEditor::set_dirty(bool dirty)
{
  if (dirty != this->dirty)
  {
    this->dirty= dirty;

    _changed_signal.emit(dirty);
  }
}

bool MGCodeEditor::get_dirty()
{
  return this->dirty= dirty;
}

void MGCodeEditor::set_language(const std::string &mime)
{
  static GtkSourceLanguagesManager *lm= 0;
  GtkSourceLanguage *lang;

  extern std::string get_app_file(const std::string &file);

  if (mime.empty())
  {
    gtk_source_buffer_set_highlight(_buffer, FALSE);
  }
  else
  {
    if (!lm)
      lm= gtk_source_languages_manager_new();

    lang= _gtk_source_language_new_from_file(get_app_file("mysql.lang").c_str(), lm);

    gtk_source_buffer_set_highlight(_buffer, TRUE);
  
    if (lang)
    {
      gtk_source_buffer_set_language(_buffer, lang);
      g_object_unref(G_OBJECT(lang));
    }
  }
}


Gtk::Widget *MGCodeEditor::get_widget()
{
  return &_swin;
}


void MGCodeEditor::begin_no_undo()
{
  gtk_source_buffer_begin_not_undoable_action(_buffer);
}


void MGCodeEditor::end_no_undo()
{
  gtk_source_buffer_end_not_undoable_action(_buffer);
}


void MGCodeEditor::set_show_line_numbers(bool flag)
{
  gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(_srcv), flag);
}


void MGCodeEditor::set_line_pointer(int line)
{
  if (line < 0)
  {
    if (_pc_marker)
      gtk_source_buffer_delete_marker(_buffer, _pc_marker);
    _pc_marker= NULL;
  }
  else
  {
    Gtk::TextIter iter= _wrapper->get_buffer()->get_iter_at_line(line);

    if (!_pc_marker)
    {
      _pc_marker= gtk_source_buffer_create_marker(_buffer, "pc", "pc",
                                                  iter.gobj());
      gtk_source_marker_set_priority(_pc_marker, 100);
    }
    else
      gtk_source_buffer_move_marker(_buffer, _pc_marker, iter.gobj());
    
    _wrapper->scroll_to_iter(iter, 0.1);
  }
  _wrapper->queue_draw();
}


void MGCodeEditor::clear_statement_markers()
{
  for (std::map<int,GtkSourceMarker*>::const_iterator iter= _st_markers.begin();
       iter != _st_markers.end(); ++iter)
  {
    if (!gtk_text_mark_get_deleted(GTK_TEXT_MARK(iter->second)))
      gtk_source_buffer_delete_marker(_buffer, iter->second);
    else
      g_object_unref(iter->second);
  }
  _st_markers.clear();
}


void MGCodeEditor::remove_statement_marker(int line)
{
  std::map<int,GtkSourceMarker*>::iterator iter= _st_markers.find(line);

  if (iter != _st_markers.end())
  {
    if (!gtk_text_mark_get_deleted(GTK_TEXT_MARK(iter->second)))
      gtk_source_buffer_delete_marker(_buffer, iter->second);
    else
      g_object_unref(iter->second);
    _st_markers.erase(iter);
  }
}


void MGCodeEditor::add_statement_marker(int line)
{
  GtkSourceMarker *marker;
  Gtk::TextIter iter;
  
  iter= _wrapper->get_buffer()->get_iter_at_line(line);
  marker= gtk_source_buffer_create_marker(_buffer, NULL, "statement", iter.gobj());
  g_object_ref(marker);
  _st_markers[line] = marker;
}


void MGCodeEditor::clear_breakpoints()
{
  GtkSourceMarker *marker;
  
  for (marker= gtk_source_buffer_get_first_marker(_buffer);
       marker != NULL; marker= gtk_source_marker_next(marker))
  {
    if (strcmp(gtk_source_marker_get_marker_type(marker),"breakpoint")==0)
    {
      gtk_source_buffer_delete_marker(_buffer, marker);
    }
  }
}


void MGCodeEditor::toggle_breakpoint()
{
  GtkSourceMarker *marker;
  Gtk::TextIter iter= _wrapper->get_buffer()->get_iter_at_mark(_wrapper->get_buffer()->get_insert());
  bool was_on;

again:
  was_on= false;
  for (marker= gtk_source_buffer_get_first_marker(_buffer);
       marker != NULL; marker= gtk_source_marker_next(marker))
  {
    GtkTextIter titer;

    if (strcmp(gtk_source_marker_get_marker_type(marker),"breakpoint")==0)
    {
      gtk_source_buffer_get_iter_at_marker(_buffer, &titer, marker);
      if (gtk_text_iter_get_line(&titer) == iter.get_line())
      {
        was_on= true;
        gtk_source_buffer_delete_marker(_buffer, marker);
        break;
      }
    }
  }
  
  if (!was_on)
  {
    bool ok;
    bool retry= false;

    while (!(ok= is_statement_start(iter.get_line()))
           && iter.backward_line()) retry= true;

    if (retry)
      goto again;
    
    if (ok)
    {
      marker= gtk_source_buffer_create_marker(_buffer, NULL, "breakpoint",
                                              iter.gobj());
      gtk_source_marker_set_priority(marker, 10);
    }
  }
  _wrapper->queue_draw();
}


void MGCodeEditor::set_gutters(const Glib::RefPtr<Gdk::Pixbuf> &pc,
                               const Glib::RefPtr<Gdk::Pixbuf> &breakpoint,
                               const Glib::RefPtr<Gdk::Pixbuf> &statement)
{
  gtk_source_view_set_marker_pixbuf(GTK_SOURCE_VIEW(_srcv),
                                    "breakpoint", breakpoint->gobj());
  gtk_source_view_set_marker_pixbuf(GTK_SOURCE_VIEW(_srcv),
                                    "pc", breakpoint->gobj());
  gtk_source_view_set_marker_pixbuf(GTK_SOURCE_VIEW(_srcv),
                                    "statement", statement->gobj());
  _pc_pixbuf= pc;
  _statement_pixbuf= statement;
  _breakpoint_pixbuf= breakpoint;
}


bool MGCodeEditor::is_statement_start(int line)
{
  return false;
}


void MGCodeEditor::invalidate_lines(int begin, int end)
{
  GdkRectangle rect;
  Gtk::TextIter iter;
  int y, height;
  
  iter= _wrapper->get_buffer()->get_iter_at_line(begin);
  _wrapper->get_line_yrange(iter, y, height);

  rect.x= 0;
  rect.width= 1024;
  rect.y= y;
  
  if (end < begin)
    end= begin;
  
  iter= _wrapper->get_buffer()->get_iter_at_line(end);
  _wrapper->get_line_yrange(iter, y, height);
  
  rect.height= (y+height) - rect.y;
}


bool MGCodeEditor::has_breakpoint(int line)
{
  Gtk::TextIter iter= _wrapper->get_buffer()->get_iter_at_line(line);
  
  return has_breakpoint(iter);
}


bool MGCodeEditor::has_breakpoint(const Gtk::TextIter &iter)
{
  GtkSourceMarker *marker;
  int line= iter.get_line();
  Gtk::TextIter iterc= iter;

  marker= gtk_source_buffer_get_next_marker(_buffer, iterc.gobj());

  while (marker)
  {
    GtkTextIter titer;

    gtk_source_buffer_get_iter_at_marker(_buffer, &titer, marker);

    if (gtk_text_iter_get_line(&titer)!=line)
      break;

    if (strcmp(gtk_source_marker_get_marker_type(marker),"breakpoint")==0)
      return true;
    
    marker= gtk_source_marker_next(marker);
  }

  return false;
}
