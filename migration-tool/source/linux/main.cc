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

#include "ObjectShell.h"
#include "GRTEnvironment.h"
#include "myg_gtkutils.h"
#include "MigrationTool.h"


int main(int argc, char **argv)
{
  Gtk::Main main(argc, argv);

  if (getenv("wait_on_start"))
  {
    g_message("wait %i", getpid());
    sleep(5);
  }
  GRTEnvironment env;

  PIXCACHE->add_search_path("../../images/icons/png");

  
  MigrationTool mt;
  

  
  
  if (env.init_jni("../../source/java")!=0)
    g_message("Error initializing JNI");
  
  {
    //ObjectShell shell(&env);
    
    //shell.show();
    
    main.run();
  }

  return 0;
}
