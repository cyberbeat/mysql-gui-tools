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

#include "MGCharsetPickDialog.h"
#include <gtkmm/stock.h>
#include "myg_gtkutils.h"
#include "mygpriv.h"

static void dummy()
{
}


MGCharsetPickDialog::MGCharsetPickDialog()
  : Gtk::Dialog(_("Select Encoding of File"), true, true),
   _hbox(false, 12), _detect(_("Auto Detect"))
{
  _message.set_label(_("Please pick the correct character set encoding for the file."));
  
  get_vbox()->set_spacing(12);
  get_vbox()->pack_start(_message, false, false);
  get_vbox()->pack_start(_hbox, false, false);
  _hbox.pack_start(_option, true, true);
  _hbox.pack_start(_detect, false, false);
  _hbox.set_border_width(8);
  
  _detect.signal_clicked().connect(sigc::mem_fun(*this,&MGCharsetPickDialog::autodetect));
  
  Gtk::Menu *menu= Gtk::manage(new Gtk::Menu());
  _option.set_menu(*menu);
  myg_menu_add(*menu, "utf8", sigc::ptr_fun(dummy), "utf8");
  myg_menu_add(*menu, "latin1", sigc::ptr_fun(dummy), "latin1");
  myg_menu_add(*menu, "sjis", sigc::ptr_fun(dummy), "sjis");
  myg_menu_add(*menu, "EUC-JP", sigc::ptr_fun(dummy), "EUC-JP");
  myg_menu_add(*menu, "big5", sigc::ptr_fun(dummy), "big5");
  myg_menu_add(*menu, "cp850", sigc::ptr_fun(dummy), "cp850");
  myg_menu_add(*menu, "cp866", sigc::ptr_fun(dummy), "cp866");
  myg_menu_add(*menu, "latin2", sigc::ptr_fun(dummy), "latin2");
  myg_menu_add(*menu, "latin5", sigc::ptr_fun(dummy), "latin5");
  myg_menu_add(*menu, "latin7", sigc::ptr_fun(dummy), "latin7");
  myg_menu_add(*menu, "hebrew", sigc::ptr_fun(dummy), "hebrew");
  myg_menu_add(*menu, "tis620", sigc::ptr_fun(dummy), "tis620");
  myg_menu_add(*menu, "euckr", sigc::ptr_fun(dummy), "euckr");
  myg_menu_add(*menu, "gb2312", sigc::ptr_fun(dummy), "gb2312");
  myg_menu_add(*menu, "greek", sigc::ptr_fun(dummy), "greek");
  myg_menu_add(*menu, "cp1250", sigc::ptr_fun(dummy), "cp1250");
  myg_menu_add(*menu, "gbk", sigc::ptr_fun(dummy), "gbk");
  myg_menu_add(*menu, "macroman", sigc::ptr_fun(dummy), "macroman");
  myg_menu_add(*menu, "cp1251", sigc::ptr_fun(dummy), "cp1251");
  myg_menu_add(*menu, "cp1256", sigc::ptr_fun(dummy), "cp1256");
  myg_menu_add(*menu, "cp1257", sigc::ptr_fun(dummy), "cp1257");
  myg_menu_add(*menu, "HP-ROMAN8", sigc::ptr_fun(dummy), "HP-ROMAN8");
  myg_menu_add(*menu, "KOI8-R", sigc::ptr_fun(dummy), "KOI8-R");
  myg_menu_add(*menu, "US-ASCII", sigc::ptr_fun(dummy), "US-ASCII");
  myg_menu_add(*menu, "KOI8-U", sigc::ptr_fun(dummy), "KOI8-U");
  myg_menu_add(*menu, "ARMSCII-8", sigc::ptr_fun(dummy), "ARMSCII-8");
  myg_menu_add(*menu, "UCS-2", sigc::ptr_fun(dummy), "UCS-2");
  myg_menu_add(*menu, "MACCENTRALEUROPE", sigc::ptr_fun(dummy), "MACCENTRALEUROPE");

  _option.set_history(0);
  
  add_button(Gtk::Stock::CANCEL, 0);
  add_button(Gtk::Stock::OK, 1);

  get_vbox()->show_all();
  
  _detect.hide();
}


void MGCharsetPickDialog::autodetect()
{
  if (_detect_func)
  {
    Glib::ustring chs;
    int index;
    
    if (_detect_func(chs))
    {      
      index= myg_menu_get_index_with_id(*_option.get_menu(), chs);
      if (index < 0)
        myg_show_info(_("Could not detect encoding of file."));
      else
        _option.set_history(index);
    }
    else
      myg_show_info(_("Could not detect encoding of file."));
  }
}


void MGCharsetPickDialog::set_detect_function(const sigc::slot1<bool,Glib::ustring&> &func)
{
  _detect_func= func;
  _detect.show();
}

    
Glib::ustring MGCharsetPickDialog::show(const Glib::ustring &message)
{  
  _message.set_text(message);

  if (run() == 1)
  {
    Glib::ustring chs;
    
    chs= myg_menu_get_id_at_index(*_option.get_menu(), _option.get_history());

    return chs;
  }
  return "";
}
