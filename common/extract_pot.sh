#!/bin/sh

FILES="glade_files.c source/linux/*c library/source/*c"

python ../mysql-gui-common/tools/po_glade.py res/linux/*.glade > glade_files.c

xgettext --default-domain=mysql-gui-common\
	--add-comments --keyword=_ --keyword=N_ $FILES \
	-ores/linux/po/mysql-gui-common.po

