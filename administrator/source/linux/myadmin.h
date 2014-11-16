/* Copyright (C) 2003, 2004 MySQL AB

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


#ifndef _MYADMIN_H_
#define _MYADMIN_H_

#include "config.h"

#include "util.h"
#include "MAPreferences.h"


#include <libintl.h>

#ifdef ENABLE_NLS
# define _(s) gettext(s)
# define N_(s) s
#else
# define N_(s) s
# define _(s) s
#endif


#ifdef HAVE_INTTYPES
#include <inttypes.h>
#endif

#ifndef HAVE_STRTOLL
// HP-UX doesn't define strtoll but does __strtoll
# ifdef HAVE___STRTOLL
#  define strtoll __strtoll
# endif
#endif


extern MAPreferences prefs;


#define MY_NAME "MySQL Administrator"

#define COPYRIGHT_NOTE "Copyright (c) MySQL AB 2004, 2005"

#define PATH_ENV "MA_DIR"

extern std::string get_prefix();
extern std::string get_glade_file(const std::string &file);
extern std::string get_app_file(const std::string &file);

/*
#define GLADE_DATADIR DATADIR"/administrator"
#define APP_DATADIR DATADIR"/administrator"
#define COMMON_DATADIR DATADIR"/common"
*/

#define GLADE_MAINWINDOW_FILE "main_window.glade"

#define GLADE_BACKUP_FILE "backup.glade"
#define GLADE_RESTORE_FILE "restore.glade"
#define GLADE_CATALOGS_FILE "catalogs.glade"
#define GLADE_HEALTH_FILE "health.glade"
#define GLADE_REPLICATIONSTATUS_FILE "replication_status.glade"
#define GLADE_SERVERINFORMATION_FILE "server_information.glade"
#define GLADE_SERVERCONNECTIONS_FILE "server_connections.glade"
#define GLADE_SERVERLOGS_FILE "server_logs.glade"
#define GLADE_SERVICECONTROL_FILE "service_control.glade"
#define GLADE_STARTUPPARAMETERS_FILE "startup_parameters.glade"
#define GLADE_USERADMINISTRATION_FILE "user_administration.glade"

/* system specific paths */
#define DEFAULT_INNODB_DATADIR "/var/lib/mysql"

#define DEFAULT_MY_CNF_PATH "/etc/mysql/my.cnf"
#define MY_CNF_PATH_TRY_GUESS

#define DEFAULT_MYSQLD_PID_FILE "/var/lib/mysqld/mysqld.pid"
#define MYSQLD_PID_FILE_TRY_GUESS

#define DEFAULT_ERR_LOG_FILE "/var/log/mysql/mysql.err"
#define DEFAULT_LOG_FILE "/var/log/mysql/mysql.log"
#define DEFAULT_SLOW_LOG_FILE "/var/lib/mysql/"

#endif /* _MYADMIN_H_ */
