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
  Author: hdalis (hdalis@users.sourceforge.net)
  $Id: scilangfiller.pas,v 1.5 2004/11/13 04:29:51 hdalis Exp $
  Purpose: Functions for filling default languages or a null language in the designer.
  History: 29/09/2004 Initial Release
           17/10/2004 Added property editor for the Lexer property of
                      TSciLangItem
           30/10/2004 Added TSciLangSettings record, and the AddIt function has
                      a new parameter of this type to define the commenting style and other things
                      for the languages.
					 19/11/2004 Now you can select exactly what languages to add.
					 29/01/2005 Added PHP as a language in it's own right, it is almost exactly the same as
											the HTML languagedefinition, but for some of the other components it's an
											advantage to have the PHP separated somewhat.
           12/04/2005 Now when we add languages we check if the style/keywordlist exists, and if so
                      then don't overwrite/or add double up.
}

{ TODO : Find another way to add the languages from the selectionform. Redesign the selectionform. }
{$Include SciCommonDef.Inc}
unit Scilangfiller;
interface
uses  SciSupport,ScintillaLanguageManager;

procedure FillLanguages(langlist : TSciLangList;const usejustnull : Boolean=false;const stylesonly : Boolean=false);
implementation
uses SysUtils, Classes,Graphics, SciWhatToFillUnit,Controls;

type
TSciLangSettings = Record
  CommentBoxStart,CommentBoxMiddle,CommentBoxEnd,CommentBlock : String;
  CommentAtLineStart : Boolean;
  CommentStreamStart,CommentStreamEnd : String;
  AssignmentOperator,EndOfStatementOperator : String;
  NumStyleBits : Integer;  
end;


{$Include ScintillaLanguageStyles.inc}

procedure FillLanguages(langlist : TSciLangList;const usejustnull : Boolean;const stylesonly : Boolean);
var
frm : TTSciWhatToFillForm;

procedure AddIt(const langname : String;KeywordRecs : array of TSciLangKeywords;xStyles : array of TSciLangStyle;xLangSettings : TSciLangSettings;
const Lexer : String='');
	var
		i : Integer;
		itm : TSciLangItem;
	begin
    itm:=langlist.Find(langname);
    if (itm<>nil) and (itm.Lexer='null') then
    begin
      if Lexer='' then itm.Lexer:=langname
      else itm.Lexer:=Lexer;
    end;
    if itm=nil then
    begin
		  itm :=TSciLangItem(langlist.Add);
      //tmprem itm.LanguageStr:=langname;
      itm.Name:=langname;
      if Lexer='' then itm.Lexer:=langname
      else
        itm.Lexer:=Lexer;
    end;
    itm.CommentBoxStart:=xLangSettings.CommentBoxStart;
    itm.CommentBoxMiddle:=xLangSettings.CommentBoxMiddle;
    itm.CommentBoxEnd:=xLangSettings.CommentBoxEnd;
    itm.CommentBlock:=xLangSettings.CommentBlock;
    itm.CommentStreamStart:=xLangSettings.CommentStreamStart;
    itm.CommentStreamEnd:=xLangSettings.CommentStreamEnd;
    itm.CommentAtLineStart:=xLangSettings.CommentAtLineStart;
    itm.AssignmentOperator:=xLangSettings.AssignmentOperator;
    itm.EndOfStatementOperator:=xLangSettings.EndOfStatementOperator;
    itm.NumStyleBits:=xLangSettings.NumStyleBits;
		try
			if stylesonly=false then itm.KeyWords.BeginUpdate;
			itm.Styles.BeginUpdate;
			for i:=Low(DefaultStyles) to High(DefaultStyles) do
      begin
        if itm.Styles.HasStyle(DefaultStyles[i].StyleNumber)=False then
          TSciStyle(itm.Styles.Add).AssignRec(DefaultStyles[i]);
      end;
			if stylesonly=false then
			begin
        for i := Low(KeywordRecs) to High(KeywordRecs) do
        begin
          if itm.Keywords.HasList(KeywordRecs[i].KeywordListNumber)=False then
            TSciKeyWords(itm.Keywords.Add).AssignRec(KeywordRecs[i]);
        end;
			end;
			for i := Low(xStyles) to High(xStyles) do
      begin
        if itm.Styles.HasStyle(xStyles[i].StyleNumber)=False then
				  TSciStyle(itm.Styles.Add).AssignRec(xStyles[i]);
      end;
		finally
			itm.Styles.EndUpdate;
			if stylesonly=false then
			  itm.KeyWords.EndUpdate;
		end;
	end;

begin
	try
		langlist.BeginUpdate;
    if (usejustnull=True) then
      langlist.Clear;
		AddIt('null',[],[],cpplanguagesettings);
		if (usejustnull=False) then
    begin
      frm:=TTSciWhatToFillForm.Create(nil);

      if frm.ShowModal=mrOk then
      begin
        if frm.c0.Checked=True then AddIt('C++/C',CPPKeywords,CPPStyles,cpplanguagesettings,'cpp');
        if frm.c5.Checked=True then AddIt('cppnocase',CPPNoCaseKeywords,CPPStyles,cpplanguagesettings);
        if frm.c27.Checked=True then AddIt('C#',CSKeywords,CPPStyles,cpplanguagesettings,'cpp');
        if frm.c1.Checked=True then AddIt('Java',JavaKeywords,CPPStyles,cpplanguagesettings,'cpp');
        if frm.c2.Checked=True then AddIt('JavaScript',JavaScriptKeywords,CPPStyles,cpplanguagesettings,'cpp');
        if frm.c3.Checked=True then AddIt('Resource',ResourceKeywords,CPPStyles,cpplanguagesettings,'cpp');
        if frm.c4.Checked=True then AddIt('IDL',IDLKeywords,CPPStyles,cpplanguagesettings,'cpp');

        if frm.c6.Checked=True then AddIt('Pascal',PascalKeywords,PascalStyles,pascallanguagesettings,'pascal');
        if frm.c7.Checked=True then AddIt('VB',VBKeywords,VBStyles,vblanguagesettings,'vb');
        if frm.c8.Checked=True then AddIt('VBScript',VBKeywords,VBStyles,vblanguagesettings,'vbscript');
        if frm.c16.Checked=True then AddIt('PowerBasic',PowerBasicKeywords,PowerBasicStyles,vblanguagesettings,'powerbasic');

        if frm.c9.Checked=True then AddIt('Python',PythonKeywords,PythonStyles,pythonlanguagesettings,'python');
        if frm.c10.Checked=True then AddIt('Ruby',RubyKeywords,PythonStyles,pythonlanguagesettings,'ruby');
        if frm.c11.Checked=True then AddIt('CSS',CSSKeywords,CSSStyles,csslanguagesettings,'css');
        if frm.c12.Checked=True then AddIt('PERL',PerlKeywords,PerlStyles,pythonlanguagesettings,'perl');
        if frm.c13.Checked=True then AddIt('HTML',HTMLKeywords,HTMLStyles,htmllanguagesettings,'hypertext');
        if frm.c37.Checked=True then AddIt('PHP',HTMLKeywords,HTMLStyles,htmllanguagesettings,'hypertext');
        if frm.c24.Checked=True then AddIt('WML',WMLKeywords,WMLStyles,htmllanguagesettings,'xml');
        if frm.c14.Checked=True then AddIt('XML',XMLKeywords,XMLStyles,htmllanguagesettings,'xml');
        if frm.c36.Checked=True then AddIt('VXML',VXMLKeywords,XMLStyles,htmllanguagesettings,'xml');

        if frm.c15.Checked=True then AddIt('SQL',SQLKeywords,SQLStyles,sqllanguagesettings,'sql');
        if frm.c54.Checked=True then AddIt('MSSQL',MSSQLKeywords,MSSQLStyles,sqllanguagesettings,'mssql');
        if frm.c17.Checked=True then AddIt('TCL/TK',TCLKeywords,TCLStyles,tcllanguagesettings,'tcl');

        if frm.c18.Checked=True then AddIt('Batch',BatKeywords,BatStyles,batlanguagesettings,'batch');
        if frm.c19.Checked=True then AddIt('Properties',[],PropsStyles,pythonlanguagesettings,'props');
        if frm.c20.Checked=True then AddIt('Makefile',[],MakeStyles,pythonlanguagesettings,'makefile');
        if frm.c21.Checked=True then AddIt('Diff',[],DiffStyles,pythonlanguagesettings,'diff');
        if frm.c22.Checked=True then AddIt('Apache Config',ConfKeywords,ConfStyles,pythonlanguagesettings,'conf');
        if frm.c30.Checked=True then AddIt('ErrorList',[],ErrorListStyles,cpplanguagesettings,'errorlist');

        if frm.c23.Checked=True then AddIt('LUA',LuaKeywords,LuaStyles,lualanguagesettings,'lua');
        if frm.c25.Checked=True then AddIt('ADA',AdaKeywords,AdaStyles,adalanguagesettings,'ada');
        if frm.c26.Checked=True then AddIt('nnCronTab',NncrontabKeywords,NncrontabStyles,nncrontablanguagesettings,'nncrontab');

        if frm.c28.Checked=True then AddIt('Lisp',LispKeywords,LispStyles,lisplanguagesettings,'lisp');
        if frm.c29.Checked=True then AddIt('Scheme',SchemeKeywords,LispStyles,lisplanguagesettings,'lisp');

        if frm.c31.Checked=True then AddIt('MMixal',MMixalKeywords,MMixalStyles,genericlanguagesettings,'mmixal');
        if frm.c33.Checked=True then AddIt('AutoIt_3',AutoItKeywords,AutoItStyles,autoitlanguagesettings,'au3');
        if frm.c38.Checked=True then AddIt('TEX',TexKeywords,TexStyles,genericlanguagesettings,'tex');
        if frm.c39.Checked=True then AddIt('ADA',AdaKeywords,AdaStyles,adalanguagesettings,'ada');
        if frm.c40.Checked=True then AddIt('APDL',APDLKeywords,APDLStyles,genericlanguagesettings,'apdl');
        if frm.c41.Checked=True then AddIt('ASM',APDLKeywords,ASMStyles,asmlanguagesettings,'asm');
        if frm.c42.Checked=True then AddIt('Clarion',CLWKeywords,CLWStyles,genericlanguagesettings,'clw');
        if frm.c43.Checked=True then AddIt('Clarion_Nocase',CLWKeywords,CLWStyles,genericlanguagesettings,'clwnocase');
        if frm.c44.Checked=True then AddIt('Avenue',AveKeywords,AveStyles,avelanguagesettings,'ave');
        if frm.c45.Checked=True then AddIt('Bullant',BullantKeywords,BullantStyles,bullantlanguagesettings,'bullant');
        if frm.c46.Checked=True then AddIt('Baan',BaanKeywords,BaanStyles,genericlanguagesettings,'baan');
        if frm.c47.Checked=True then AddIt('Eiffel',EiffelKeywords,EiffelStyles,genericlanguagesettings,'eiffel');
        if frm.c48.Checked=True then AddIt('Eiffel_kw',EiffelKeywords,EiffelStyles,genericlanguagesettings,'eiffelkw');
        if frm.c49.Checked=True then AddIt('EScript',EScriptKeywords,EScriptStyles,cpplanguagesettings,'escript');

        if frm.c50.Checked=True then AddIt('Forth',ForthKeywords,ForthStyles,cpplanguagesettings,'forth');
        if frm.c51.Checked=True then AddIt('Fortran',FortranKeywords,FortranStyles,fortranlanguagesettings,'fortran');
        if frm.c52.Checked=True then AddIt('Fortran77',FortranKeywords,FortranStyles,fortranlanguagesettings,'f77');

        if frm.c34.Checked=True then AddIt('VHDL',VHDLKeywords,VHDLStyles,vhdllanguagesettings,'vhdl');
        if frm.c35.Checked=True then AddIt('ASN1',ASN1Keywords,ASN1Styles,genericlanguagesettings,'asn1');
        if frm.c53.Checked=True then AddIt('KIX',KIXKeywords,KIXStyles,asmlanguagesettings,'kix');
        if frm.c32.Checked=True then AddIt('LaTeX',[],LatexStyles,genericlanguagesettings,'latex');
        if frm.c55.Checked=True then AddIt('Haskell',[],HaskellStyles,cpplanguagesettings,'haskell');
        if frm.c55.Checked=True then AddIt('Objective_Caml',Objective_CamlKeywords,Objective_CamlStyles,camllanguagesettings,'caml');

        if frm<>nil then frm.Free;
      end;
			//AddIt('batch',[],[]);
		end;
  finally
		langlist.EndUpdate;
  end;
end;

end.
