/* Copyright (C) 2005 MySQL AB

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA */

#ifndef myx_util_h
#define myx_util_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  #define __LCC__
  //#define _WINDOWS
  #define HAVE_UNICODE_PCRE
#endif

// Definition of a MySQL identifier:
// Single character definitions.
#ifdef HAVE_UNICODE_PCRE
  #define PCRE_IDENTIFIER_START "\\pL|_"
  #define PCRE_IDENTIFIER_PART "\\pL|\\pN|_"
  // If quoted then allowed are all letters, numbers, marks, currency sign, punctuation and the white space.
  // This leaves out: line and paragraph separators, symbol modifiers (like grave accent, aka. back tick), surrogate
  // private, control and not assigned chars/code points.
  #define PCRE_ANY_CHAR "\\pL|\\pN|\\pM|\\p{Sc}|\\p{Sm}|\\p{So}|\\pP|\\p{Zs}"
#else
  #define PCRE_IDENTIFIER_START "\\w|_"
  #define PCRE_IDENTIFIER_PART "\\w|_|[0-9]"
  #define PCRE_ANY_CHAR "[^`\"]"
#endif

// Identifier definitions, both simple (for unquoted ids) and complete (for quoted ids).
#define SIMPLE_ID_PCRE "(?:(?:" PCRE_IDENTIFIER_START")(?:" PCRE_IDENTIFIER_PART ")*)"
#define COMPLETE_ID_PCRE "(?:(?:" PCRE_ANY_CHAR ")+)"
#define COMPLETE_ID_PCRE_NO_QUOTE_CHAR "(?:(?:(?!\")(?:" PCRE_ANY_CHAR "))+)"
#define COMPLETE_ID_PCRE_NO_BACK_TICK "(?:(?:(?!`)(?:" PCRE_ANY_CHAR "))+)"

// Single IDs, both quoted and unquoted.
#define IDENTIFIER_PCRE "`" COMPLETE_ID_PCRE_NO_BACK_TICK "`|\"" COMPLETE_ID_PCRE_NO_QUOTE_CHAR "\"|" SIMPLE_ID_PCRE
#define IDENTIFIER_IGNORE_PCRE "(?:" IDENTIFIER_PCRE ")"

// Qualified IDs.
#define QUALIFIED_IDENTIFIER_PCRE "(?:(" IDENTIFIER_PCRE ")\\.)?(" IDENTIFIER_PCRE ")"
#define QUALIFIED_IDENTIFIER_IGNORE_PCRE "(?:(?:" IDENTIFIER_PCRE ")\\.)?(?:" IDENTIFIER_PCRE ")"

#include "myx_util_public_interface.h"
#include "myx_util_functions.h"
#include "myx_shared_util_functions.h"

/*
 * Enums
 */


/*
 * Structs
 */


/*
 * Functions
 */

#endif

