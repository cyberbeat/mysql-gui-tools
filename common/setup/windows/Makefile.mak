###############################################################
# Makefile for building the msi-installation-file
#
# This Makefile works with MS NMake 7.10 as shipped with MS
#   Visual++ Studio.net Standard 2003. (at least)
# There are known problems with Borland Make Version 5.2 that
#   is shipped with Delphi 7.
#
# You need Wix and Msival2 installed.
# Wix: wix.sourceforge.net
# Msival2: part of the platform sdk which can be
#          downloaded from MS
#
###############################################################
# The variable SOURCE_DIR needs to be adjusted by the user
# before executing this makescript
# example:
# Set SOURCE_DIR="..\..\bin\windows"
###############################################################
CANDLE =  "../../../win-external-libs/setup/wix/candle" -nologo
LIGHT= "../../../win-external-libs/setup/wix/light" -nologo
MSIVAL = "../../../win-external-libs/setup/MsiVal2\msival2.exe"
ALL_CUB = "../../../win-external-libs/setup/MsiVal2\darice.cub"
MERGE_CUB = "../../../win-external-libs/setup/MsiVal2\mergemod.cub"
COMMON_GUI=source

all: mysql_gui_tools.msi

mysql_gui_tools.msi: mysql_gui_tools.wixobj mysql_gui_tools_fragment.wixobj
	$(LIGHT) -b $(SOURCE_DIR)  $** -out $@

mysql_gui_tools.wixobj: mysql_gui_tools.xml $(COMMON_GUI)\mysql_common_ui.inc
	$(CANDLE) mysql_gui_tools.xml -out $@ -dLICENSE_TYPE=$(LICENSE_TYPE)

mysql_gui_tools_fragment.wixobj: mysql_gui_tools_fragment.xml
	$(CANDLE) $** -out $@ -dLICENSE_TYPE=$(LICENSE_TYPE)

clean:
	del *.wixobj 2> nul
	del mysql_gui_tools.msi 2> nul

#ignore error-codes in test
#.IGNORE:
test:
	$(MSIVAL) mysql_gui_tools.msi $(ALL_CUB) -f
