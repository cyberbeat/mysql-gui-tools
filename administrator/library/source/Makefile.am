
noinst_LIBRARIES = libadmin.a

INCLUDES= -Wsign-compare -Wall \
	 -I../include @MYSQL_CFLAGS@ \
	-I$(top_srcdir)/../common/source/linux\
	-I$(top_srcdir)/../common/library/base-library/include\
	-I$(top_srcdir)/../common/library/utilities/shared_include\
	-I$(top_srcdir)/../common/library/utilities/include\
	@PCRE_CFLAGS@\
	@GLIB_CFLAGS@


libadmin_a_SOURCES=\
	myx_admin_library.c\
	myx_backup.c\
	myx_processlist.c\
	myx_log_files.c\
	myx_process_mysql_cnf_file.c\
	myx_user_management.c\
	myx_xml_gui_description.c\
	myx_xml_variables_listing.c\
	myx_xml_health.c\
	myx_replication.c
