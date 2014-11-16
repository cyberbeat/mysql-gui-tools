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

#include "myqb.h"
#include "myg_gtkutils.h"
#include "MQResultTab.h"

#include <gtkmm/scrollbar.h>

#include "MGGladeXML.h"

#include "MQResultSetView.h"
#include "html.h"


MQResultTab::MQResultTab()
  : MQBaseTab(), _toolbar_xml(0), _toolmenu_xml(0), 
    _topBox(false, 0), _paned(0)
{
  set_icon(PIXCACHE->load("tabsheet_icon_resultset.png"));
  
  add(_topPaned);
  _topPaned.show();

  _topBoxScroll.set_shadow_type(Gtk::SHADOW_NONE);
  _topBoxScroll.set_policy(Gtk::POLICY_NEVER, Gtk::POLICY_AUTOMATIC);
  
  _topPaned.pack2(_topBoxScroll, true, true);
  _topBoxScroll.add(_topBox);
  _topBoxScroll.show();
  _topBox.show();
  
  ((Gtk::Viewport*)_topBoxScroll.get_child())->set_shadow_type(Gtk::SHADOW_NONE);
}


MQResultTab::~MQResultTab()
{
  unsigned int i;
  
  for (i= 0; i < _rs_items.size(); i++)
    delete _rs_items[i];
  
  delete _toolbar_xml;
  delete _toolmenu_xml;
}
  

bool MQResultTab::tab_will_close()
{
  MQResultSetView *rset;
  unsigned int i;
  
  for (i= 0; i < _rs_items.size(); i++)
  {
    rset= _rs_items[i]->rs;
    if (rset->is_busy())
      rset->cancel_query();
  }
  
  // unparent the query area so that it wont be destroyed
  if (_topPaned.get_child1())
    _topPaned.remove(*_topPaned.get_child1());
  
  return true;
}


void MQResultTab::set_top_widget(Gtk::Widget *w)
{
  if (_topPaned.get_child1() && _topPaned.get_child1()!=w)
    _topPaned.remove(*_topPaned.get_child1());

  if (w)
  {
    w->reference();
    if (w->get_parent())
      ((Gtk::Container*)w->get_parent())->remove(*w);
    _topPaned.pack1(*w, true, true);
    w->unreference();
  }
}


static Gtk::Paned *find_last_paned(Gtk::Paned *paned)
{
  Gtk::Paned *p2;
  
  p2= (Gtk::Paned*)paned->get_child2();
  if (!p2)
    return paned;
  else
    return find_last_paned(p2);
}


Gtk::Paned *MQResultTab::find_paned_with(MQResultSetView *rs)
{
  unsigned int i;
  RSItem *item= 0;
  
  for (i= 0; i < _rs_items.size(); i++)
  {
    if (_rs_items[i]->rs == rs)
    {
      item= _rs_items[i];
      break;
    }
  }  
  g_assert(item!=NULL);
  
  if (item->rs == rs)
    return item->paned;
  return NULL;
}


void MQResultTab::rotate_paned(bool vertical)
{
  Gtk::Widget *ch1= _paned->get_child1();
  Gtk::Widget *ch2= _paned->get_child2();
  Gtk::Box *parent= (Gtk::Box*)_paned->get_parent();
  Gtk::Paned *npaned;
  RSItem *item;

  for (unsigned int i= 0; _rs_items.size(); i++)
  {
    if (_rs_items[i]->paned == _paned)
    {
      item= _rs_items[i];
      break;
    }
  }

  if (vertical)
  {
    _topBoxScroll.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_NEVER);
    npaned= Gtk::manage(new Gtk::HPaned());
  }
  else
  {
    _topBoxScroll.set_policy(Gtk::POLICY_NEVER, Gtk::POLICY_AUTOMATIC);
    npaned= Gtk::manage(new Gtk::VPaned());
  }

  if (ch1) 
  {
    ch1->reference(); 
    _paned->remove(*ch1);
    npaned->pack1(*ch1, true, true);
    ch1->show();
  }
  if (ch2) 
  {
    ch2->reference();
    _paned->remove(*ch2);
    npaned->pack2(*ch2, true, true);
    ch2->show();
  }
  parent->remove(*_paned);
  parent->pack_end(*npaned, true, true);

  npaned->show();
  _paned= npaned;

  item->paned= npaned;
}


void MQResultTab::add_resultset(MQResultSetView *rsv, bool vertical)
{
  RSItem *item= new RSItem;
  Gtk::Paned *paned;
  
  if (_rs_items.size()==1)
  {
    _vertical= vertical;
    if (vertical)
      _rs_items[0]->rs->set_compact_editbar(true);

    rotate_paned(_vertical);
  }
  else if (_rs_items.size()>1 && vertical!=_vertical)
  {
    g_warning("Splitting resultset in inconsistent direction");
  }

  if (_vertical)
    paned= Gtk::manage(new Gtk::HPaned());
  else
    paned= Gtk::manage(new Gtk::VPaned());

  item->rs= rsv;
  item->paned= paned;
  item->frame.add(*rsv);
  item->frame.set_shadow_type(Gtk::SHADOW_IN);
  paned->pack1(item->frame, true, true);
  
  item->frame.show();
  paned->show();

  rsv->signal_activate().connect(sigc::mem_fun(*this,&MQResultTab::activated));

  if (_rs_items.empty()) // first one
  {    
    _paned= paned;

    _topBox.pack_end(*paned, true, true);
  }
  else
  {
    Gtk::Paned *parent= find_last_paned(_paned);
    
    parent->pack2(*paned, true, true);
  }
  _rs_items.push_back(item);
  
  if (_vertical && _rs_items.size() > 1)
    rsv->set_compact_editbar(true);

  rsv->show();

  rsv->signal_editable_changed().connect(sigc::mem_fun(*this,&MQResultTab::update_cmp_toolbar));
  rsv->signal_row_changed().connect(sigc::mem_fun(*this,&MQResultTab::row_changed));
}


void MQResultTab::remove_resultset(MQResultSetView *rsview)
{
  Gtk::Paned *parent;
  
  g_return_if_fail(_paned!=0);
  
  parent= find_paned_with(rsview);

  // link the next paned to the previous
  if (parent->get_child2())
  {
    Gtk::Paned *child= (Gtk::Paned*)parent->get_child2();
    Gtk::Paned *grand_parent= (Gtk::Paned*)parent->get_parent();
    child->reference();
    parent->remove(*child);
    
    if (parent == _paned)
    {
      _paned= child;
      _topBox.remove(*parent);
      _topBox.pack_end(*_paned, true, true);
      _paned->show();
    }
    else
    {
      grand_parent->remove(*parent);
      grand_parent->pack2(*child, true, true);
    }
    child->show();
    child->unreference();
  }
  else
  {
    Gtk::Paned *grand_parent= (Gtk::Paned*)parent->get_parent();

    if (parent == _paned)
    {
      g_warning("not supposed to reach here");
      _paned= 0;
      _topBox.remove(*parent);
    }
    else
    {
      grand_parent->remove(*parent);
    }
  }

  for (std::vector<RSItem*>::iterator iter= _rs_items.begin();
       iter != _rs_items.end(); ++iter)
  {
    if ((*iter)->rs == rsview)
    {
      delete *iter;
      _rs_items.erase(iter);
      break;
    }
  }

  if (_rs_items.size()==1)
    get(0)->set_compact_editbar(false);
  
  if (_toolbar_xml)
  {
    cmp_close();
  }
}


int MQResultTab::get_index(MQResultSetView *rsview)
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs == rsview)
      return i;
  return -1;
}


bool MQResultTab::contains_rsview(MQResultSetView *rsview)
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs == rsview)
      return true;
  return false;
}


void MQResultTab::set_label(MQResultSetView *rs, const Glib::ustring &text)
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs == rs)
    {
      _rs_items[i]->label= text;
      break;
    }

  if (get_active() == rs)
    _label.set_text(text);
}

    
MQResultSetView *MQResultTab::get_active()
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs->get_active())
      return _rs_items[i]->rs;

  if (_rs_items.size()>0)
    g_warning("no active");
  return 0;
}


void MQResultTab::activated(MQResultSetView *sender)
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
  {
    if (_rs_items[i]->rs != sender)
      _rs_items[i]->rs->set_active(false);
  }
}


void MQResultTab::set_query_text(MQResultSetView *rs, const Glib::ustring &text)
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs == rs)
    {
      _rs_items[i]->query= text;
      break;
    }
}


Glib::ustring MQResultTab::get_query_text(MQResultSetView *rs)
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs == rs)
    {
      return _rs_items[i]->query;
    }
  return "";
}


bool MQResultTab::is_busy()
{
  for (unsigned int i= 0; i < _rs_items.size(); i++)
    if (_rs_items[i]->rs->is_busy())
      return true;
  return false;
}


void MQResultTab::scrolled(MQResultSetView *rs)
{
  double value= rs->get_scrollwin()->get_vscrollbar()->get_value();
  
  for (unsigned int i= 0; i < _rs_items.size(); i++)
  {
    if (_rs_items[i]->rs != rs)
      _rs_items[i]->rs->get_scrollwin()->get_vscrollbar()->set_value(value);
  }
}


void MQResultTab::set_sync_scrolling(bool flag)
{
  g_return_if_fail(_rs_items.size()>1);
  
  if (flag)
  {
    for (unsigned int i= 0; i < _rs_items.size(); i++)
    {
      _rs_items[i]->scroll_con=
        _rs_items[i]->rs->get_scrollwin()->get_vscrollbar()->signal_value_changed().connect(sigc::bind<MQResultSetView*>(sigc::mem_fun(*this,&MQResultTab::scrolled),_rs_items[i]->rs));
    }
  }
  else
  {
    for (unsigned int i= 0; i < _rs_items.size(); i++)
      _rs_items[i]->scroll_con.disconnect();
  }
}


void MQResultTab::show_compare_toolbar()
{
  if (!_toolbar_xml)
  {
    Gtk::Widget *toolbar;
    static struct {
      CompareAction action;
      const char *widget;
      int menu;
    } setup_data[]= {
      { CopyToLeft, "rcopyrow", 0 },
      { CopyToRight, "lcopyrow", 0 },
      { CopyAllToLeft, "copy_from_right", 1 },
      { CopyAllToRight, "copy_from_left", 1 },
      { DeleteFromLeft, "delete_left", 1 },
      { DeleteFromRight, "delete_right", 1 },

      { CopyToLeft, NULL, 0 }
    };
    
    _toolbar_xml= new MGGladeXML(get_app_file("compare_toolbar.glade"), "compare_toolbar");
    _toolmenu_xml= new MGGladeXML(get_app_file("compare_toolbar.glade"), "actions_menu");

    toolbar= _toolbar_xml->get_widget("compare_toolbar");

    _topBox.pack_start(*toolbar, false, false);
    toolbar->show();

    _toolbar_xml->get_button("actions")->signal_clicked().connect(sigc::mem_fun(*this,&MQResultTab::cmp_actions));
    _toolbar_xml->get_button("close")->signal_clicked().connect(sigc::mem_fun(*this,&MQResultTab::cmp_close));

    _toolbar_xml->get_button("next")->signal_clicked().connect(sigc::mem_fun(*this,&MQResultTab::cmp_go_next));
    _toolbar_xml->get_button("previous")->signal_clicked().connect(sigc::mem_fun(*this,&MQResultTab::cmp_go_back));
    
    for (unsigned int i= 0; setup_data[i].widget; i++)
    {
      if (setup_data[i].menu)
        ((Gtk::MenuItem*)_toolmenu_xml->get_widget(setup_data[i].widget))->signal_activate().connect(sigc::bind<CompareAction>(sigc::mem_fun(*this,&MQResultTab::cmp_do_action),setup_data[i].action));
      else
        _toolbar_xml->get_button(setup_data[i].widget)->signal_clicked().connect(sigc::bind<CompareAction>(sigc::mem_fun(*this,&MQResultTab::cmp_do_action),setup_data[i].action));
    }
    
    update_cmp_toolbar();
  }
}


MQResultSetView *MQResultTab::get_not_active()
{
  if (get(0)==get_active())
    return get(1);
  else
    return get(0);
}


void MQResultTab::row_changed(MQResultSetView *sender)
{
  if (_toolbar_xml)
  {
    Gtk::TreePath path= sender->get_selected_row_path();
    if (!path.empty())
    {
      if (get(0)==sender)
        get(1)->set_selected_row_path(path);
      else
        get(0)->set_selected_row_path(path);
    }
  }
  update_cmp_toolbar();
}


void MQResultTab::update_cmp_toolbar()
{
  if (_toolbar_xml)
  {
    Gtk::TreePath path= get(0)->get_selected_row_path();

    bool flag= false;
    if (get(0)->is_editable())
    {
      if (!path.empty())
      {
        MYX_RS_ROW *row= get(1)->get_selected_row();
        
        if (row && row->fields && row->diff!=0)
          flag= true;
      }
     
      _toolmenu_xml->get_widget("delete_left")->set_sensitive(true);
    }
    else
      _toolmenu_xml->get_widget("delete_left")->set_sensitive(false);
    _toolmenu_xml->get_widget("copy_from_right")->set_sensitive(flag);
    _toolbar_xml->get_widget("rcopyrow")->set_sensitive(flag);

    flag= false;
    if (get(1)->is_editable())
    {
      if (!path.empty())
      {
        MYX_RS_ROW *row= get(0)->get_selected_row();
        
        if (row && row->fields && row->diff!=0)
          flag= true;
      }
      
      _toolmenu_xml->get_widget("delete_right")->set_sensitive(true);
    }
    else
      _toolmenu_xml->get_widget("delete_right")->set_sensitive(false);
    _toolmenu_xml->get_widget("copy_from_left")->set_sensitive(flag);
    _toolbar_xml->get_widget("lcopyrow")->set_sensitive(flag);
  }
}


void MQResultTab::cmp_close()
{
  if (_toolbar_xml)
  {
    _toolbar_xml->get_widget("compare_toolbar")->reference();
    _topBox.remove(*_toolbar_xml->get_widget("compare_toolbar"));

    delete _toolbar_xml;
    _toolbar_xml= 0;
    
    delete _toolmenu_xml;
    _toolmenu_xml= 0;
  }
}


void MQResultTab::cmp_get_menu_pos(int &x, int &y, bool &push_in)
{
  int yy;
  _toolbar_xml->get_widget("actions")->get_window()->get_origin(x,y);
  _toolbar_xml->get_widget("actions")->translate_coordinates(*get_toplevel(),
                                                             x,y, x,yy);
  y+= _toolbar_xml->get_widget("actions")->get_height();
  y+=2;
  x-=2;
}


void MQResultTab::cmp_actions()
{
  Gtk::Menu *menu= (Gtk::Menu*)_toolmenu_xml->get_widget("actions_menu");
  
  menu->popup(sigc::mem_fun(*this,&MQResultTab::cmp_get_menu_pos),0, 0);
}




bool MQResultTab::copy_row_contents(MQResultSetView *from_rs,
                                    MQResultSetView *to_rs,
                                    const Gtk::TreePath &path)
{
  Glib::RefPtr<MGResultSetModel> from_model= from_rs->get_model();
  Glib::RefPtr<MGResultSetModel> to_model= to_rs->get_model();
  Gtk::TreeIter from_iter= from_model->get_iter(path);
  Gtk::TreeIter to_iter= to_model->get_iter(path);
  MYX_RESULTSET *resultset;
  MYX_RS_ROW *from_row, *to_row;
  
  if (!from_iter || !to_iter)
    return false;

  from_row= from_model->get_iter_row(from_iter);
  if (!from_row->fields)
    return false;
  
  if (from_row->diff == 0)
    return false;

  to_row= to_model->get_iter_row(to_iter);

  resultset= from_model->get_resultset();

  bool copied= false;
  
  for (unsigned int i= 0; i < resultset->columns_num; i++)
  {
    gpointer fdata, tdata;
    size_t fsize, tsize;

    if (!from_model->get_value(from_iter, i*2, fdata, fsize))
    {
      fdata= NULL;
      fsize= 0;
    }
    if (!to_model->get_value(to_iter, i*2, tdata, tsize))
    {
      tdata= NULL;
      tsize= 0;
    }

    if (fdata != tdata && 
        (fsize != tsize || memcmp(fdata, tdata, fsize)!=0))
    {
      to_model->set_value(to_iter, i*2, fdata, fsize);      
      copied= true;
    }
  }

  return copied;
}


void MQResultTab::copy_all_rows(MQResultSetView *from_rs,
                                MQResultSetView *to_rs)
{
  Gtk::TreePath path;
  guint count= 0;
  
  path.push_back(0);
  
  unsigned int last= from_rs->get_resultset()->rows_num;

  for (unsigned int i= 0; i < last; i++)
  {
    if (copy_row_contents(from_rs, to_rs, path))
      count++;

    path.next();
  }
}


void MQResultTab::delete_all_rows(MQResultSetView *from_rs,
                                  MQResultSetView *that_are_missing_in_rs)
{
  MYX_RESULTSET *fromrs= from_rs->get_resultset();
  MYX_RESULTSET *tors= that_are_missing_in_rs->get_resultset();
  Glib::RefPtr<MGResultSetModel> from_model= from_rs->get_model();
  guint count= 0;

  for (unsigned int i= 0; i < fromrs->rows_num; i++)
  {
    MYX_RS_ROW *frow= fromrs->rows+i;
    MYX_RS_ROW *trow= tors->rows+i;

    if (frow->fields && !trow->fields)
    {
      Gtk::TreePath path;
      path.push_back(i);
      from_model->erase(from_model->get_iter(path));
      count++;
    }
  }
}


void MQResultTab::cmp_do_action(CompareAction action)
{
  switch (action)
  {
  case CopyToLeft:
    copy_row_contents(get(1), get(0), get(0)->get_selected_row_path());
    break;
  case CopyToRight:
    copy_row_contents(get(0), get(1), get(0)->get_selected_row_path());
    break;
  case CopyAllToLeft:
    copy_all_rows(get(1), get(0));
    break;
  case CopyAllToRight:
    copy_all_rows(get(0), get(1));
    break;
  case DeleteFromLeft:
    delete_all_rows(get(0), get(1));
    break;
  case DeleteFromRight:
    delete_all_rows(get(1), get(0));
    break;
  }
  
  get(0)->queue_draw();
  get(1)->queue_draw();
}


void MQResultTab::cmp_go_next()
{
  Gtk::TreePath path;

  if (get_active()->find_next_diff())
  {
    path= get_active()->get_selected_row_path();
    get_not_active()->set_selected_row_path(path);
  }
  else
  {
    if (!prefs.dont_beep)
      Gdk::Display::get_default()->beep();
  }
}

void MQResultTab::cmp_go_back()
{
  Gtk::TreePath path;

  if (get_active()->find_previous_diff())
  {
    path= get_active()->get_selected_row_path();
    get_not_active()->set_selected_row_path(path);
  }
  else
  {
    if (!prefs.dont_beep)
      Gdk::Display::get_default()->beep();
  }
}


//======================================================================

MQHelpTab::MQHelpTab()
{
  _html= Gtk::manage(Gtk::HTML::create());
  _swin.add(*_html);
  _html->add_search_path(get_app_file(""));
  _html->show();
  _swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _swin.set_shadow_type(Gtk::SHADOW_IN);

  set_icon(PIXCACHE->load("tabsheet_icon_inlinehelp.png"));

  add(_swin);
  _swin.show();
}


void MQHelpTab::show_help(const std::string &id, const std::string &file)
{
  if (file.find('/')!=std::string::npos)
    _html->add_search_path(file.substr(0, file.rfind('/')));

  if (_loaded_file!=file)
  {
    _html->load_from_string(Glib::file_get_contents(file));
    _loaded_file= file;
  }

  if (!id.empty())
    _html->jump_to_anchor(id);
}

