//CE_Desc_Include(helpdescriptions.txt)
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
	Software Foundatifon, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
  Author  : hdalis (hdalis@users.sourceforge.net)
       $Id: SciDetectUtils.pas,v 1.5 2004/11/13 04:29:50 hdalis Exp $
  History : 29/09/2004 Initial Release
            13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                       generate the help.
                       The TExtensionMapper is moved to SciPropertyMgr.pas
            24/10/2004 Removed the TLexerNamesMap class, and the LexerMapper function
                       these are now part of the TSciLanguageManager class (functions AddLanguage,
                       DupLanguage,RemoveLanguage,FillListWithLanguages)

}
{.$WEAKPACKAGEUNIT ON}
{$Include SciCommonDef.Inc}
unit SciDetectUtils;
interface
function GetWordChars(const highlighter : String) : String;
implementation
uses SciLexer,SysUtils,Windows;

function GetWordChars(const highlighter : String) : String;
begin
	if highlighter='' then Result:=sci_alphachars+sci_numericchars+sci_accentedchars+'_'
	else if AnsiCompareText('PERL',highlighter)=0 then Result:=sci_alphachars+sci_numericchars+'_$@%&'
	else if (AnsiCompareText('PHP',highlighter)=0) then
    Result:=sci_alphachars+sci_numericchars+'_-$'
	else if (AnsiCompareText('CSS',highlighter)=0) or (AnsiCompareText('HTML',highlighter)=0) or (AnsiCompareText('HTML',highlighter)=0) then
    Result:=sci_alphachars+sci_numericchars+'_-'
	else if (AnsiCompareText('XML',highlighter)=0) or (AnsiCompareText('WML',highlighter)=0) then Result:=sci_alphachars+sci_numericchars+'_-'
	else if AnsiCompareText('LUA',highlighter)=0 then Result:=sci_alphachars+sci_numericchars+'_%'
	else if (AnsiCompareText('Ave',highlighter)=0) or (AnsiCompareText('C++/C',highlighter)=0) or
  (AnsiCompareText('Resource',highlighter)=0) or (AnsiCompareText('IDL',highlighter)=0) or
  (AnsiCompareText('Java',highlighter)=0) or (AnsiCompareText('JavaScript',highlighter)=0) or
  (AnsiCompareText('C#',highlighter)=0) or (AnsiCompareText('POV',highlighter)=0) or
  (AnsiCompareText('Specman',highlighter)=0)  then Result:=sci_alphachars+sci_numericchars+'_#'
	else if AnsiCompareText('Baan',highlighter)=0 then Result:=sci_alphachars+sci_numericchars+'_#.$'
	else if AnsiCompareText('Forth',highlighter)=0 then Result:=sci_alphachars+sci_numericchars+'%-'
	else if (AnsiCompareText('Lisp',highlighter)=0) or (AnsiCompareText('Scheme',highlighter)=0) then Result:=sci_alphachars+sci_numericchars+'_-<>.#+@$%^&=*!?'
	else if AnsiCompareText('lot',highlighter)=0 then Result:=sci_alphachars+sci_numericchars
	else if AnsiCompareText('nnCronTab',highlighter)=0 then Result:=sci_alphachars+sci_numericchars+'%-'
	else if AnsiCompareText('Verilog',highlighter)=0 then Result:=sci_alphachars+sci_numericchars+'_`$#'
	else
		Result:='_'+sci_alphachars+sci_numericchars;
end;

end.
