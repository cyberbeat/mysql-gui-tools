{
	Delphi Scintilla Interface Components
	Copyright (C) 2004,2005 Jan Martin Pettersen (hdalis)

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later
	version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free
	Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}

{Notice: This is here because of some intention to eventually get these
controls to support more than Borland C++ Builder 6,Delphi 7, and
Delphi 2005 (Win32 Mode).

Drop me a hint of how (if you you had to) modify the code to get it to
work.

I'm also interested in the package source files you've created for that
version, or other contributions of code.
}
//{$IFDEF BORLAND}
	{$IFDEF VER170}
		{$DEFINE COMPILER9}
		{$DEFINE DELPHI9}
		{$DEFINE DELPHICOMPILER9}
	{$ENDIF}

	{$IFDEF VER160}
		{$DEFINE COMPILER8}
		{$DEFINE DELPHI8}
		{$DEFINE DELPHICOMPILER8}
	{$ENDIF}

	{$IFDEF VER150}
		{$DEFINE COMPILER7}
		{$IFDEF BCB}
			{$DEFINE BCB7}
		{$ELSE}
			{$DEFINE DELPHI7}
			{$DEFINE DELPHICOMPILER7}
		{$ENDIF}
	{$ENDIF}

	{$IFDEF VER140}
		{$DEFINE COMPILER6}
		{$IFDEF BCB}
			{$DEFINE BCB6}
		{$ELSE}
			{$DEFINE DELPHI6}
			{$DEFINE DELPHICOMPILER6}
		{$ENDIF}
	{$ENDIF}

	{$IFDEF VER130}
		{$DEFINE COMPILER5}
		{$IFDEF BCB}
			{$DEFINE BCB5}
		{$ELSE}
			{$DEFINE DELPHI5}
			{$DEFINE DELPHICOMPILER5}
		{$ENDIF}
	{$ENDIF}

	{$IFDEF BCB}
		{$DEFINE CPPBUILDER}
		{$DEFINE BCBCOMPILER}
	{$ELSE ~BCB}
		{$DEFINE DELPHI}
		{$DEFINE DELPHICOMPILER}
	{$ENDIF ~BCB}

{$Ifdef COMPILER5}
  {$Define COMPILER5_UP}
{$Endif}

{$Ifdef COMPILER6}
  {$Define COMPILER5_UP}
  {$Define COMPILER6_UP}
{$Endif}

{$Ifdef COMPILER7}
  {$Define COMPILER5_UP}
  {$Define COMPILER6_UP}
  {$Define COMPILER7_UP}
{$Endif}
{$Ifdef COMPILER8}
  {$Define COMPILER5_UP}
  {$Define COMPILER6_UP}
  {$Define COMPILER7_UP}
  {$Define COMPILER8_UP}
{$Endif}
{$Ifdef COMPILER9}
  {$Define COMPILER5_UP}
  {$Define COMPILER6_UP}
  {$Define COMPILER7_UP}
  {$Define COMPILER8_UP}
  {$Define COMPILER9_UP}
{$Endif}

//If USENEWEXTENSIONMAPPER is defined, uses the new fileextensionmapper rather than the old
//The old is by the way, very much deprecated, and will disappear in v0.20
{$define USENEWEXTENSIONMAPPER}

{.$Define USENEWLOADLEXER}

{
  If you want the more specialized LoadLexerLibrary in TScintilla then remove the dot above.
}
//Newabbrews is not done yet, and isn't tested.
{.$Define NEWABBREVS}

