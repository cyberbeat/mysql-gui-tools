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

#ifndef __MYX_GRT_MYSQL_REVENG_SCRIPT_H_
#define __MYX_GRT_MYSQL_REVENG_SCRIPT_H_

#ifdef __cplusplus
extern "C" {
#endif

MYX_GRT_VALUE *re_script_get_version(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *re_script_get_schemata(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *reverse_engineer_script(MYX_GRT_VALUE *param, void *data);
MYX_GRT_VALUE *re_script_get_character_sets(MYX_GRT_VALUE *param, void *data);

static MYX_GRT_BUILTIN_FUNCTION functions_reverse_engineer_script[]= {
  {"getVersion::", re_script_get_version },
  {"getSchemata::", re_script_get_schemata },
  {"reverseEngineerScript::", reverse_engineer_script },
  {"getCharacterSets::", re_script_get_character_sets },
};

static MYX_GRT_BUILTIN_MODULE grt_module_reverse_engineer_mysql_script= {
  "ReverseEngineeringMysqlScript",
  NULL,
  sizeof(functions_reverse_engineer_script)/sizeof(MYX_GRT_BUILTIN_FUNCTION),
  functions_reverse_engineer_script
};

#ifdef __cplusplus
}
#endif

#endif

