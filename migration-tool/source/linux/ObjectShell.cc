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

#include <gtkmm.h>
#include "ObjectShell.h"
#include "GRTEnvironment.h"
#include "myg_gtkutils.h"
#include "myg_utils.h"

#define _(s) s


ObjectShell::ObjectShell(GRTEnvironment *env)
  : _env(env)
{  
  _xml= new MGGladeXML("../../res/linux/objshell.glade");

  Gtk::TreeView *tree;
  Gtk::TreeViewColumn *column;
  
  tree= _xml->get_tree("object_tree");
  _otree= Gtk::TreeStore::create(_otcols);
  
  column= new Gtk::TreeView::Column(_("Object Tree"));
  column->pack_start(_otcols.icon, false);
  column->pack_start(_otcols.text);
  std::vector<Gtk::CellRenderer*> rends= column->get_cell_renderers();
  column->clear_attributes(*((Gtk::CellRendererText*)rends[1]));
  column->add_attribute(((Gtk::CellRendererText*)rends[1])->property_markup(),
                        _otcols.text);
  tree->append_column(*Gtk::manage(column));

  tree->set_model(_otree);
  
  tree->get_selection()->signal_changed().connect(SigC::slot(*this,&ObjectShell::object_selected));
  
  
  tree= _xml->get_tree("inspector_tree");
  _ilist= Gtk::ListStore::create(_oicols);

  tree->append_column(_("Parameter"), _oicols.name);
  tree->append_column(_("Value"), _oicols.value);
  tree->get_column(0)->set_resizable();

  tree->set_model(_ilist);

  _xml->get_text("shell_text")->signal_key_press_event().connect(SigC::slot(*this,&ObjectShell::shell_key_press), false);
  
  env->set_shell_output_handler(SigC::slot(*this,&ObjectShell::print_shell));
  
  put_prompt();
}


void ObjectShell::object_selected()
{
  Gtk::TreeIter iter= _xml->get_tree("object_tree")->get_selection()->get_selected();
  Gtk::TreeRow row;
  Gtk::TreeIter iiter;
  Gtk::TreeRow irow;

  _ilist->clear();
  
  if (iter)
  {
    row= *iter;

    switch ((int)row[_otcols.type])
    {
    case 'D':
      break;
    case 'C':
      {
        MYX_GRT_OBJ *obj= (MYX_GRT_OBJ*)(void*)row[_otcols.object];
        unsigned int count= _env->get_object_var_count(obj, false);
        for (unsigned int i= 0; i < count; i++)
        {
          MYX_GRT_VAR *var= _env->get_object_var(obj, i, false);
          
          iiter= _ilist->append();
          irow= *iiter;
          
          irow[_oicols.name]= var->var_name;
          irow[_oicols.value]= _env->get_object_var_value(obj, var->var_name);
        }
        break;
      }
    }
  }
}

void ObjectShell::show()
{
  ((Gtk::Paned*)_xml->get_widget("vpaned"))->set_position(350);
  ((Gtk::Paned*)_xml->get_widget("top_paned"))->set_position(400);
  
  _xml->get_widget("shell_window")->show();
  
  _xml->get_text("shell_text")->grab_focus();
  
  _env->update_objects();

  build_object_tree();
}


void ObjectShell::build_object_tree()
{
  build_object_tree(_env->get_env()->root_dict, Gtk::TreeIter());
}



void ObjectShell::build_object_tree(MYX_GRT_OBJ *obj, const Gtk::TreeIter &parent)
{
  unsigned int count;
  Gtk::TreeRow prow;
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  MYX_GRT_OBJ_REFLIST *objl;

  objl= _env->get_object_reference_list(obj, "dicObjects");
  if (objl)
  {
    unsigned int objlc= _env->get_object_reference_list_count(objl);
    for (unsigned int i= 0; i < objlc; i++)
    {
      MYX_GRT_OBJ *nobj= _env->get_object_reference_list_obj(objl, i);
  
      if (nobj)
      {
        if(_env->object_implements_interface(nobj, "MyxObjectDict"))
        {
          if (parent)
          {
            prow= *parent;
            iter= _otree->append(prow.children());
          }
          else
            iter= _otree->append();
          row= *iter;
          row[_otcols.icon]= PIXCACHE->load("folder_closed_14x14.png");
          row[_otcols.text]= _env->get_object_var_value(nobj, "name");
          row[_otcols.type]= 'D';
          row[_otcols.object]= nobj;

          build_object_tree(nobj, iter);
        }
        else
        {
          if (parent)
          {
            prow= *parent;
            iter= _otree->append(prow.children());
          }
          else
            iter= _otree->append();
          row= *iter;
          row[_otcols.icon]= PIXCACHE->load("grt_class_14x14.png");
          row[_otcols.text]= _env->get_object_var_value(nobj, "name");
          row[_otcols.type]= 'C';
          row[_otcols.object]= nobj;

          count= _env->get_object_func_count(nobj, _publicOnly);
          for (unsigned int j= 0; j < count; j++)
          {
            MYX_GRT_FUNC *func= _env->get_object_func(nobj, j, _publicOnly);
            Gtk::TreeIter fiter= _otree->append(row.children());
            Gtk::TreeRow frow= *fiter;

            frow[_otcols.icon]= PIXCACHE->load("grt_public_member_14x14.png");
            frow[_otcols.text]=
              _env->get_func_modifiers(func)+" "+
              uswrapf(myx_grt_get_func_return_type_signature(func))+" "+
              _env->get_func_name(func)+
              myx_grt_get_func_param_type_signature(func);
            frow[_otcols.type]= 'C';
            frow[_otcols.object]= nobj;
          }
          /*
          count= _env->get_object_var_count(obj, true);
          for (unsigned int j= 0; j < count; j++)
          {
            MYX_GRT_VAR *var= _env->get_object_var(obj, j, true);
            Gtk::TreeIter fiter= _otree->append(row.children());
            Gtk::TreeRow frow= *fiter;

            frow[_otcols.icon]= PIXCACHE->load("grt_inherited_public_member_14x14.png");
            frow[_otcols.type]= 'V';
            frow[_otcols.text]= _env->get_var_name(var);
            
            build_object_tree(nobj, iter);
          }*/
        }
      }
    }
  }
}


void ObjectShell::put_prompt()
{
  Glib::RefPtr<Gtk::TextBuffer> buffer= _xml->get_text("shell_text")->get_buffer();
  char *prompt= myx_grt_shell_get_prompt(_env->get_env());
  if (!buffer->end().starts_line())
    buffer->insert(buffer->end(), "\n");
  buffer->insert(buffer->end(), prompt);
  Gtk::TextIter end= buffer->end();
  g_free(prompt);
  
  end= buffer->end();
  end.backward_char();
  buffer->place_cursor(end);
  _xml->get_text("shell_text")->scroll_to_iter(end, 0.0);
  
  Glib::RefPtr<Gtk::TextBuffer::Mark> mark;
  mark= buffer->get_mark("cmdstart");
  if (mark)
    buffer->move_mark(mark, end);
  else
    buffer->create_mark("cmdstart", end, true);
  
}


void ObjectShell::print_shell(const Glib::ustring &text)
{
  Glib::RefPtr<Gtk::TextBuffer> buffer= _xml->get_text("shell_text")->get_buffer();

  buffer->insert(buffer->end(), text);
  buffer->place_cursor(buffer->end());
  Gtk::TextIter end= buffer->end();
  _xml->get_text("shell_text")->scroll_to_iter(end, 0.0);
}


bool ObjectShell::shell_key_press(GdkEventKey *ev)
{
  Glib::RefPtr<Gtk::TextBuffer> buffer= _xml->get_text("shell_text")->get_buffer();

  if (ev->keyval == GDK_Return)
  {
    Glib::ustring cmd;
    cmd= buffer->get_text(buffer->get_iter_at_mark(buffer->get_mark("cmdstart")), buffer->end());
    
    if (!cmd.empty())
    {
      buffer->insert(buffer->end(), "\n");

      if (_env->execute_shell_command(cmd) == MYX_GRT_SHELL_COMMAND_EXIT)
      {
        exit(1);
      }
      _history.push_back(cmd);
    }
    _history_index= 0;
    put_prompt();
    return true;
  }
  else if (ev->keyval == GDK_Up || ev->keyval == GDK_Down)
  {
    Glib::ustring text;
    if (ev->keyval == GDK_Up)
      _history_index++;
    else 
      _history_index--;
    if (_history_index < 0)
    {
      text= "";
      _history_index= 0;
    }
    else
      text= _history[_history_index];
    buffer->erase(buffer->get_iter_at_mark(buffer->get_mark("cmdstart")), buffer->end());
    buffer->insert(buffer->get_iter_at_mark(buffer->get_mark("cmdstart")), text);

    return true;
  }

  return false;
}
