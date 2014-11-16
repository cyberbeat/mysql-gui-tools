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
  Author : hdalis
      $Id: scilexermod.pas,v 1.5 2004/11/13 04:29:51 hdalis Exp $
  History: 29/09/2004 Initial Release
           13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                      generate the help.
           13/10/2004 Changed the ViewWSpace from boolean to sciWSMode, to allow all settings.
					 17/10/2004 Removed LoadSettingsFromStream and SaveSettingsToStream, unnessecary now.
           29/10/2004 Added The TSciHotSpot subclass to manage Active hotspot settings (TScintilla.ActiveHotspotOpts).
           30/10/2004 Added CommentBox and CommentBlock functions.
           10/11/2004 CommentBox and CommentStream now takes a boolean parameter 'includecommentcharsinselection'
                      that if it's True it selects the commenting strings also, if not then the commenting
                      strings are not selected.
           11/11/2004 Added the FlipVars function.
					            Updated the helpdescriptions in helpdescriptions.txt
					 18/11/2004 Changed the TSciSynLexer to TSciLanguageManager, and changed all properties etc to reflect that.
					            became more logical eventually.
					            Added a keyboardmanager, is more easy to connect other components that also
											want a keyboad/calltipclick that way.
											The TSciLexerAuto component no longer exists. When you need autocomplete drop a
											TSciAutoComplete component on the form, and set the editor property.
											Or for Calltips drop a TSciCallTips component.
           19/11/2004 Made a miniversion of this component, TScintillaMemo.. This is the immediate
                      ancestor for TScintilla now. Separated in two files, SciLexerMemo.pas and SciLexerMod.Pas
                      All commentfunctions now uses SetTarget* and ReplaceTarget, instead of SetSel and ReplaceSel.
                      It seems faster to do it that way.
           08/02/2005 Added a LoadLexerLibrary override.. Until now you could load all libraries you wanted, but
                      they didn't appear in the languagemanager.. Now they do..

}

{$Include SciCommonDef.Inc}
unit SciLexerMod;
interface
uses Classes, Windows, Controls, Messages, SysUtils, Graphics,SciLexer,ScintillaLanguageManager, SciKeyBindings,
SciSupport,SciLexerMemo,SciControllerHandler;

type
  sciFoldDrawFlag=(sciBoxIfExpanded,sciAboveIfExpanded,sciAboveIfNotExpanded,sciBelowIfExpanded,sciBelowIfNotExpanded,sciHexLevels);
  sciFoldDrawFlags=set of sciFoldDrawFlag;

  // Display whitespace?
	sciMarkerType =(sciMarkCircle=0,sciMarkBox=1,sciMarkPlusMinus=2,sciMarkArrows=3);
  sciCodeFoldingFlag=(foldFold,foldCompact,foldComment,foldPreprocessor,foldCommentPython,foldQuotesPython,
  foldAtElse,foldHTML,foldHTMLPreProcessor);
  sciCodeFoldingFlags=set of sciCodeFoldingFlag;

	TScintilla =class;



TScintilla = class(TScintillaMemo)
  private
		fLanguageManager : TSciLanguageManager;
		FFoldMarkerType : sciMarkerType;
    FCodeFoldingFlags : sciCodeFoldingFlags;
    FFoldDrawFlags : sciFoldDrawFlags;
		FBraceHilite : Boolean;
{$Ifdef USENEWLOADLEXER}
    FLibrariesLoaded : TStrings;
{$Endif}
		procedure SetFoldMarkerType(const Value : sciMarkerType);
		procedure SetMarkers(const Value : sciMarkerType);
    procedure SetLanguageManager(const Value : TSciLanguageManager);
    procedure SetCodeFoldingFlags(const Value : sciCodeFoldingFlags);
    procedure SetFoldDrawFlags(const Value : sciFoldDrawFlags);

	protected
		procedure BoxComment(start_comment,middle_comment,end_comment : String;includecommentcharsinselection : Boolean=True);
		procedure BlockComment(comment : String;commentatlinestart : Boolean);
		procedure StreamComment(start_comment,end_comment : String;includecommentcharsinselection : Boolean=True);
protected
		procedure Expand(var line: Integer; doExpand : Boolean;force: Boolean = false; visLevels: Integer = 0; level : Integer=-1);
    procedure ProcessBraces; virtual;
		procedure doSciUpdateUI;override;
		procedure doSciMarginClick(const modifiers : LongInt; const position : LongInt; const margin : LongInt);override;
		procedure doSciModified(const position : LongInt; const modificationType : LongInt; text : PChar;const length : LongInt; const linesAdded : LongInt; const line : LongInt;const foldLevelNow : LongInt; const foldLevelPrev : LongInt);override;
		procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
		procedure Loaded; override;
public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CopyFrom(Source: TScintillaBase); override;
		procedure FoldAll;
		//Used for brace matching and highlighting
		procedure FindMatchingBracePosition(var braceAtCaret : Integer;var braceOpposite : Integer; var IsInside : boolean; sloppy : boolean=true);
		procedure GoMatchingBrace(Select : boolean; Sloppy : boolean); //Move carret to matching brace
    procedure CommentBox(includecommentcharsinselection : Boolean=True);
    procedure CommentBlock;
    procedure CommentStream(includecommentcharsinselection : Boolean=True);
    procedure ExportToHTML(Stream: TStream; Title : string;UseCurrentEOLMode : Boolean=True);
    procedure FlipVars(const equalsign : String='=';spacebeforeandaftereq : Boolean=True;const endstatement : String=';');
{$Ifdef USENEWLOADLEXER}
    procedure LoadLexerLibrary(path : String);
    property  LibrariesLoaded : TStrings read FLibrariesLoaded;
{$Endif}
published
		//Enable or disable brace highlighting
		property BraceHilite : boolean read FBraceHilite write FBraceHilite;
		//Enable or disable code folding if supported by the highlighter
    property Folding : sciCodeFoldingFlags read FCodeFoldingFlags write SetCodeFoldingFlags;
		property FoldMarkerType : sciMarkerType read FFoldMarkerType write SetFoldMarkerType;
		//Get or set the Syntax highlighter
		property LanguageManager : TSciLanguageManager read fLanguageManager write SetLanguageManager;
    property FoldDrawFlags : sciFoldDrawFlags read FFoldDrawFlags write SetFoldDrawFlags;
    property KeyCommands;
end;


TScintillaAuto=TScintilla;

implementation
uses Math,sciUtils,SciResLang;

const
  DefaultLinenumbersWidth = 4;


constructor TScintilla.Create(AOwner : TComponent);
begin
	inherited;
{$Ifdef USENEWLOADLEXER}
  FLibrariesLoaded:=nil;
{$Endif}
  FBraceHilite := True;
	FFoldMarkerType :=sciMarkBox;
  fLanguageManager := TSciLanguageManager.Create(Self);
  FFoldDrawFlags:=[sciBelowIfNotExpanded];
  FCodeFoldingFlags:=[foldCompact,foldComment,foldPreprocessor,foldAtElse,foldHTML,foldHTMLPreProcessor];
end;

destructor TScintilla.Destroy;
begin
{$Ifdef USENEWLOADLEXER}
  if assigned(FLibrariesLoaded) then FLibrariesLoaded.Free;
{$Endif}
  fLanguageManager.Free;
  inherited;
end;

procedure TScintilla.CopyFrom(Source: TScintillaBase);
begin
	inherited;
	if Source is TScintilla then
	begin
		BraceHilite :=TScintilla(Source).BraceHilite;
		Folding:=TScintilla(Source).Folding;
		FoldMarkerType:=TScintilla(Source).FoldMarkerType;
		LanguageManager.Assign(TScintilla(Source).LanguageManager);
    FoldDrawFlags:=TScintilla(Source).FoldDrawFlags;
	end;
end;

procedure TScintilla.CommentStream(includecommentcharsinselection : Boolean);
var
  itm : TSciLangItem;
begin
  itm:=LanguageManager.LanguageList.Find(LanguageManager.SelectedLanguage);
  if itm<>nil then
  begin
   if (itm.CommentStreamStart='') or (itm.CommentStreamEnd='') then
   begin
     raise Exception.CreateRes(@sNoStartOrEndCommentDefined);
   end;
    StreamComment(itm.CommentStreamStart,itm.CommentStreamEnd,includecommentcharsinselection);
  end else
    StreamComment('/*','*/',includecommentcharsinselection);


end;

procedure TScintilla.CommentBox(includecommentcharsinselection : Boolean);
var
  itm : TSciLangItem;
begin
  itm:=LanguageManager.LanguageList.Find(LanguageManager.SelectedLanguage);
  if itm<>nil then
  begin
    if (itm.CommentBoxStart='') or (itm.CommentBoxMiddle='') or (itm.CommentBoxEnd='') then
    begin
      raise Exception.CreateRes(@sNoStartMiddleOrEndCommentDefined);
    end;
    BoxComment(itm.CommentBoxStart,itm.CommentBoxMiddle,itm.CommentBoxEnd,includecommentcharsinselection);
  end else
    BoxComment('/*','*','*/',includecommentcharsinselection);
end;
procedure TScintilla.CommentBlock;
var
  itm : TSciLangItem;
begin
  itm:=LanguageManager.LanguageList.Find(LanguageManager.SelectedLanguage);
  if itm<>nil then
  begin
   if (itm.CommentBlock='') then
   begin
     raise Exception.CreateRes(@sNoOneLineCommentDefined);
   end;

    BlockComment(itm.CommentBlock,itm.CommentAtLineStart);
  end else
    BlockComment('//',True);
end;

procedure TScintilla.BlockComment(comment : String;commentatlinestart : Boolean);
var
  caretPosition,
  selectionStart,
  selectionEnd,
  selStartLine,
  selEndLine,
  firstSelLineStart,numlines,i,lineStart,lineIndent,lineEnd,comment_length,curbuflen  : LongInt;
  move_caret : Boolean;
  linebuf,long_comment : String;
begin
  comment:=comment+' ';
  long_comment  :=comment;
  curbuflen:=0;
  comment_length:=Length(comment);
  selectionStart:=GetSelectionStart;
  selectionEnd  :=GetSelectionEnd;
  caretPosition :=GetCurrentPos;
  move_caret    :=(caretPosition<selectionEnd);
  selStartLine  :=LineFromPosition(selectionStart);
  selEndLine    :=LineFromPosition(selectionEnd);
  numlines      :=selEndLine-selStartLine;
  firstSelLineStart:=PositionFromLine(selStartLine);
  if (numlines>0) and (selectionEnd=PositionFromLine(selEndLine)) then Dec(selEndLine);
  BeginUndoAction;
  for i:=selStartLine to selEndLine do
  begin
    lineStart:=PositionFromLine(i);
    lineIndent:=lineStart;
    lineEnd:=GetLineEndPosition(i);
    if (lineEnd-lineIndent)>=curbuflen then
    begin
      curbuflen:=lineEnd-lineIndent;
      SetLength(linebuf,curbuflen+1);
    end;
		if (commentatlinestart=True) then
			GetRange(lineIndent, lineEnd, PChar(linebuf))
		else
    begin
			lineIndent := GetLineIndentPosition(i);
			GetRange(lineIndent, lineEnd, PChar(linebuf));
		end;
    if Length(linebuf)<1 then Continue;
    // Insert comment removal here.
    if CompareMem(PChar(linebuf),PChar(comment),comment_length-1) then
    begin
      if CompareMem(PChar(linebuf),PChar(long_comment),comment_length) then
      begin
        SetTargetStart(lineIndent);
        SetTargetEnd(lineIndent+comment_length);
        ReplaceTarget(-1,PChar(''));
        if (i=selStartLine) then
          Dec(selectionStart,comment_length);
        Dec(selectionEnd,comment_length);
        Continue;
      end else
      begin
        SetTargetStart(lineIndent);
        SetTargetEnd(lineIndent+(comment_length-1));

        ReplaceTarget(-1,PChar(''));
        if (i=selStartLine) then
          Dec(selectionStart,comment_length-1);
        Dec(selectionEnd,comment_length-1);
        Continue;
      end;
    end;
    Inc(selectionEnd,Length(comment));
    InsertText(lineIndent,PChar(long_comment));
  end;
  if (selectionStart<firstSelLineStart) then
  begin
    if (selectionStart >= (selectionEnd - (Length(comment)- 1))) then
      selectionEnd := firstSelLineStart;
    selectionStart := firstSelLineStart;
  end;
  if move_caret=True then
  begin
    GotoPos(selectionEnd);
    SetCurrentPos(selectionStart);
  end else
    SetSel(selectionStart,selectionEnd);
  EndUndoAction;
end;

procedure TScintilla.BoxComment(start_comment,middle_comment,end_comment : String;includecommentcharsinselection : Boolean);
var
  caretPosition,selectionStart,selectionEnd,selStartLine,selEndLine,numlines,lineStart,lineEnd,i,
  start_comment_length,middle_comment_length,end_comment_length,maxCommentLength,whitespace_length,eollen  : Integer;
  move_caret : Boolean;
  linebuf,whitespace : String;
  tempstring : PChar;
  hasselection : Boolean;
begin

  SetLength(linebuf,1000);
  whitespace    :=' ';
  start_comment :=start_comment+whitespace;
  middle_comment:=middle_comment+whitespace;
  selectionStart:=GetSelectionStart;
  selectionEnd  :=GetSelectionEnd;
  caretPosition :=GetCurrentPos;
  hasselection  :=(selectionStart-selectionEnd)<>0;
  move_caret    :=(caretPosition<selectionEnd);
  selStartLine  :=LineFromPosition(selectionStart);
  selEndLine    :=LineFromPosition(selectionEnd);
  numlines      :=selEndLine-selStartLine;
  if (numlines>1) and (selectionEnd=PositionFromLine(selEndLine)) then
  begin
    Dec(selEndLine);
    Dec(numlines);
    selectionEnd:=GetLineEndPosition(selEndLine);
  end;
  if numlines>1 then // More than one line, adjust the selection to the start of selstartline, and end of selendline.
  begin
    selectionStart:=PositionFromLine(selStartLine);
    selectionEnd:=GetLineEndPosition(selEndLine);
    SetSelectionStart(selectionStart);
    SetSelectionEnd(selectionEnd);
  end;
  start_comment_length:=Length(start_comment);
  middle_comment_length:=Length(middle_comment);
  end_comment_length:=Length(end_comment);
  maxCommentLength:=start_comment_length;
  whitespace_length:=Length(whitespace);
  if middle_comment_length>maxCommentLength then
    maxCommentLength:=middle_comment_length;
  if (end_comment_length+whitespace_length)>maxCommentLength then
    maxCommentLength:=end_comment_length+whitespace_length;
  GetMem(tempString,maxCommentLength+1);
  case EOLStyle of
    eolCRLF: begin
               eollen:=2;
             end;
     eolCR:  begin
               eollen:=1;
             end;
    eolLF:   begin
               eollen:=1;
             end;
          else eollen:=2;
  end;

  BeginUndoAction; // Start undoable operation.
  lineStart:=PositionFromLine(selStartLine);
  GetRange(lineStart, lineStart + start_comment_length, PChar(tempString));
  tempString[start_comment_length+1]:=#0;
  if start_comment<>tempString then // If it's not a start_comment, add it.
  begin
    InsertText(lineStart,PChar(start_comment));
    if numlines>1 then //More than one line selected
    begin
      Inc(selectionEnd,start_comment_length);
      if (includecommentcharsinselection=False) then Inc(selectionStart,start_comment_length);
    end else
    begin
      Inc(caretPosition,start_comment_length);
      if (hasselection=True) then
      begin
        Inc(selectionStart,start_comment_length);
        Inc(selectionEnd,start_comment_length);
      end;
    end;
  end else // If it's a start_comment, remove it.
  begin
    SetTargetStart(lineStart);
    SetTargetEnd(lineStart+start_comment_length);
    ReplaceTarget(-1,'');
    if numlines>1 then // More than one line selected
    begin
      Dec(selectionEnd,start_comment_length);
    end else
    begin
      Dec(caretPosition,start_comment_length);
      if (hasselection=True) then
      begin
      Dec(selectionStart,start_comment_length);
      Dec(selectionEnd,start_comment_length);
      end;
    end;
  end;
  if numlines<=1 then // One line or less selected.
  begin
    lineEnd:=GetLineEndPosition(selEndLine);
    GetRange(lineEnd - end_comment_length, lineEnd, PChar(tempString));
    tempString[end_comment_length+1] := #0;
    if (end_comment <> tempString) then // If it's not a end comment, add it.
    begin
      InsertText(lineEnd,PChar(whitespace+end_comment));
    end else // If it's a end comment, remove it.
    begin
      SetTargetStart(lineEnd-end_comment_length-whitespace_length);
      SetTargetEnd(lineEnd);
      ReplaceTarget(-1,'');
    end;
  end else // More than one line selected.
  begin
    for i:=(selStartLine+1) to (selEndLine-1) do
    begin
      lineStart:=PositionFromLine(i);
      GetRange(lineStart, lineStart + middle_comment_length, PChar(tempString));
      tempString[middle_comment_length+1] := #0;
      if (middle_comment <> tempString) then // If it's not a middle comment, add it.
      begin
        InsertText(lineStart,PChar(middle_comment));
        Inc(selectionEnd,middle_comment_length);
      end else // If it's a middle comment, remove it.
      begin
        SetTargetStart(lineStart);
        SetTargetEnd(lineStart+middle_comment_length);
        ReplaceTarget(-1,'');
        Dec(selectionEnd,middle_comment_length);
      end;
    end;
    lineStart:=PositionFromLine(selEndLine);
    GetRange(lineStart, lineStart + end_comment_length, PChar(tempString));
    tempString[end_comment_length+1] := #0;
    if (end_comment <> tempString) then // If it's not a end comment, add it.
    begin
      GetRange(lineStart, lineStart + middle_comment_length, PChar(tempString));
      tempString[middle_comment_length+1] := #0;
      if (middle_comment <> tempString) then
      begin
        InsertText(lineStart,PChar(middle_comment));
        Inc(selectionEnd,middle_comment_length);
      end else
      begin
        SetTargetStart(lineStart);
        SetTargetEnd(lineStart+middle_comment_length);
        ReplaceTarget(-1,'');
        Dec(selectionEnd,middle_comment_length);
      end;
      lineStart:=PositionFromLine(selEndLine+1);
      GetRange(lineStart, lineStart + end_comment_length, PChar(tempString));
      tempString[end_comment_length+1] := #0;
      if (end_comment <> tempString) then // If it's not a end comment, add it.
      begin
        InsertText(lineStart,PChar(end_comment));
        if (includecommentcharsinselection=True) then Inc(selectionEnd,end_comment_length);

        case EOLStyle of // Add the correct linefeed, and increment selectionend
          eolCRLF: begin
                     InsertText(lineStart+end_comment_length,PChar(CrLf));
                   end;
          eolCR:   begin
                     InsertText(lineStart+end_comment_length,PChar(#13));
                   end;
          eolLF:   begin
                     InsertText(lineStart+end_comment_length,PChar(#10));
                   end;
        end;
        if (includecommentcharsinselection=True) then Inc(selectionEnd,eollen);
      end else // If it's a end comment, remove it.
      begin
        SetTargetStart(lineStart);
        SetTargetEnd(lineStart++end_comment_length+eollen);
        ReplaceTarget(-1,'');
        if (includecommentcharsinselection=True) then Dec(selectionEnd,end_comment_length);
        if (includecommentcharsinselection=True) then Dec(selectionEnd,eollen); // Decrement with the length of the correct linefeed
      end;
    end else // If it's a end comment, remove it.
    begin
      SetTargetStart(lineStart);
      SetTargetEnd(lineStart+end_comment_length+eollen);
      ReplaceTarget(-1,'');
      Dec(selectionEnd,end_comment_length+eollen);
    end;
    EndUndoAction; // End undoable operation.
  end;

  if (move_caret=True) then
  begin
    GotoPos(selectionEnd);
    SetCurrentPos(selectionStart);
  end else if (numlines=0) then // If it wasn't selected anything, just move the caret to the corresponding previous position after the line was commented/not commented.
  begin
    if hasselection=True then
      SetSel(selectionStart,selectionEnd)
    else
      GotoPos(caretPosition);
  end else
  SetSel(selectionStart,selectionEnd);
  if assigned(tempString) then FreeMem(tempString);
end;

procedure TScintilla.StreamComment(start_comment,end_comment : String;includecommentcharsinselection : Boolean);
var
caretPosition,selectionStart,selectionEnd,lineEnd,current,end_comment_length,start_comment_length,startword,endword,start_counter,end_counter,selLine,lineIndent  : Integer;
move_caret : Boolean;
linebuf,whitespace : String;
temp : PChar;
begin
  whitespace :=' ';
	start_comment :=start_comment+ whitespace;
	whitespace :=whitespace+end_comment;
	end_comment := whitespace;
  start_comment_length:=Length(start_comment);
  end_comment_length:=Length(end_comment);
  selectionStart:=GetSelectionStart;
  selectionEnd  :=GetSelectionEnd;
  caretPosition :=GetCurrentPos;
  move_caret    :=(caretPosition<selectionEnd);
  if selectionEnd-selectionStart<=0 then
  begin
    selLine:=LineFromPosition(selectionStart);
    lineIndent:=GetLineIndentPosition(selLine);
    lineEnd:=GetLineEndPosition(selLine);
    if RangeIsAllWhitespace(lineIndent,lineEnd)=True then
    begin
      Exit;
    end;
    lineBuf:=GetLineS;
    SetLength(lineBuf,1000);
    current:=GetCaretInLine;
    if (CharPos(WordChars,AnsiChar(lineBuf[current]))=0) then
    begin
      Exit;
    end;
    start_counter:=0;
    end_counter:=0;
    startword:=current;
    endword:=current;
		while ((startword > 0) and (CharPos(WordChars,AnsiChar(lineBuf[startword]))<>0)) do
    begin
			Inc(start_counter);
			Dec(startword);
		end;
    if startword=current then
    begin
      Exit;
    end;
		while ((linebuf[endword + 1] <> #0) and (CharPos(WordChars,AnsiChar(lineBuf[endword + 1]))<>0)) do
    begin
			Inc(end_counter);
			Inc(endword);
		end;
    Dec(selectionStart,start_counter);
    Inc(selectionEnd,end_counter);
  end else
  begin
    GetMem(temp,Max(start_comment_length,end_comment_length)+1);
    GetRange(selectionStart, selectionStart + start_comment_length, temp);
    temp[start_comment_length+1]:=#0;
    if temp=start_comment then
    begin
      GetRange(selectionEnd - end_comment_length, selectionEnd, temp);
      temp[end_comment_length+1]:=#0;
      if temp=end_comment then
      begin
        BeginUndoAction;
        SetTargetStart(selectionStart);
        SetTargetEnd(selectionStart+start_comment_length);
        ReplaceTarget(-1,'');
        Dec(selectionEnd,start_comment_length);
        SetTargetStart(selectionEnd-end_comment_length);
        SetTargetEnd(selectionEnd);
        ReplaceTarget(-1,'');
        Dec(selectionEnd,end_comment_Length);
        EndUndoAction;
        if temp<>nil then FreeMem(temp);
        if move_caret=True then
        begin
          GotoPos(selectionEnd);
          SetCurrentPos(selectionStart);
        end else
        begin
          SetSel(selectionStart,selectionEnd);
        end;
        Exit;
      end;
    end;
    if temp<>nil then FreeMem(temp);
  end;

  BeginUndoAction;
  InsertText(selectionStart,PChar(start_comment));
  Inc(selectionEnd,start_comment_length);
  if (includecommentcharsinselection=False) then Inc(selectionStart,start_comment_length);
  InsertText(selectionEnd,PChar(end_comment));
  if (includecommentcharsinselection=True) then Inc(selectionEnd,end_comment_length);
  if move_caret=True then
  begin
    GotoPos(selectionEnd);
    SetCurrentPos(selectionStart);
  end else
  begin
    SetSel(selectionStart,selectionEnd);
  end;
  EndUndoAction;
end;

procedure TScintilla.CMFontChanged(var Message: TMessage);
begin
  inherited CMFontChanged(Message);
  LanguageManager.Update;
end;

procedure TScintilla.CMColorChanged(var Message: TMessage);
begin
  inherited CMColorChanged(Message);
  LanguageManager.Update;
end;

procedure TScintilla.Loaded;
begin
	inherited;
	LanguageManager.Update;
end;

procedure TScintilla.doSciModified(const position : LongInt; const modificationType : LongInt; text : PChar;
const length : LongInt; const linesAdded : LongInt; const line : LongInt; const foldLevelNow : LongInt; const foldLevelPrev : LongInt);
var
tmpline : LongInt;
begin
	tmpline:=line;
	if (modificationType and SC_MOD_CHANGEFOLD) <> 0 then
  begin
		if (foldLevelNow and SC_FOLDLEVELHEADERFLAG) <> 0 then
    begin
			if (foldLevelPrev and SC_FOLDLEVELHEADERFLAG) = 0 then
				SetFoldExpanded(line, True);
		end else if (foldLevelPrev and SC_FOLDLEVELHEADERFLAG) <> 0 then
    begin
			if not GetFoldExpanded(line) then
				// Removing the fold from one that has been contracted so should expand
				// otherwise lines are left invisible with no way to make them visible
				Expand(tmpline, true, false, 0, foldLevelPrev);
		end;
    end;
	inherited doSciModified(position,modificationType,text,length,linesAdded,tmpline,foldLevelNow,foldLevelPrev);
end;

procedure TScintilla.doSciMarginClick(const modifiers : LongInt; const position : LongInt; const margin : LongInt);
var
LineClick,
LevelClick : Integer;
begin
	if (foldFold in FCodeFoldingFlags) and (Margin = 2) then
		begin
		LineClick := LineFromPosition(Position);
		if (Modifiers and SCMOD_SHIFT <> 0) and (Modifiers and SCMOD_CTRL <> 0) then
			FoldAll
		else begin
			LevelClick := GetFoldLevel(LineClick);
			if (LevelClick and SC_FOLDLEVELHEADERFLAG) <> 0 then
      begin
				if (modifiers and SCMOD_SHIFT) <> 0 then
        begin
					// Ensure all children visible
					SetFoldExpanded(LineClick, True);
					Expand(lineClick, true, true, 100, levelClick);
				end else if (modifiers and SCMOD_CTRL) <> 0 then
        begin
					if GetFoldExpanded(lineClick) then
          begin
						// Contract this line and all children
						SetFoldExpanded(LineClick, False);
						Expand(lineClick, false, true, 0, levelClick);
					end else begin
						// Expand this line and all children
						SetFoldExpanded(LineClick, True);
						Expand(lineClick, true, true, 100, levelClick);
					end;
				end else begin
					// Toggle this line
					ToggleFold(LineClick);
				end;
			end;
		end;
	end;
	//if assigned(FOnmarginclick) then FOnmarginclick(Self, modifiers, position, margin);
	inherited doSciMarginClick(modifiers,position,margin);
end;

procedure TScintilla.SetFoldMarkerType(const Value : sciMarkerType);
begin
	if Value<>FFoldMarkerType then
	begin
		FFoldMarkerType:=Value;
		SetMarkers(Value);
	end;
end;

procedure TScintilla.SetMarkers(const Value : sciMarkerType);
begin
	case Value of
		sciMarkBox:
			begin
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_BOXMINUS);
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_BOXPLUS);
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_VLINE);
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_LCORNER);
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_BOXPLUSCONNECTED);
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_BOXMINUSCONNECTED);
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_TCORNER);
			end;
		sciMarkCircle:
			begin
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_CIRCLEMINUS);
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_CIRCLEPLUS);
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_VLINE);
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_LCORNERCURVE);
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_CIRCLEPLUSCONNECTED);
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_CIRCLEMINUSCONNECTED);
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_TCORNERCURVE);
			end;
		sciMarkPlusMinus:
			begin
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_MINUS);
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_PLUS);
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY);
			end;
		sciMarkArrows:
			begin
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_ARROWDOWN);
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_ARROW);
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY);
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY);
			end;
	end;
end;

procedure TScintilla.SetLanguageManager(const Value: TSciLanguageManager);
begin
  fLanguageManager.Assign(Value);
end;

procedure TScintilla.Expand(var line: Integer; doExpand, force: Boolean;
	visLevels, level: Integer);
Var
  lineMaxSubord,LevelLine : Integer;
begin
  lineMaxSubord :=  GetLastChild(line, level and SC_FOLDLEVELNUMBERMASK);
  Inc(Line);
  while (line <= lineMaxSubord) do begin
    if force then
    begin
      if visLevels > 0 then
        ShowLines(line, line)
      else
        HideLines(line, line)
    end else begin
      if doExpand then
        ShowLines(line, line)
    end;
    LevelLine := level;
    if LevelLine = -1 then
      LevelLine := GetFoldLevel(line);
    if (levelLine and SC_FOLDLEVELHEADERFLAG) <> 0 then
    begin
      if force then
      begin
        if visLevels > 1 then
          SetFoldExpanded(line, True)
        else
          SetFoldExpanded(line, False);
        Expand(line, doExpand, force, visLevels - 1);
      end else begin
        if doExpand then
        begin
          if not GetFoldExpanded(line) then
            SetFoldExpanded(line, True);
          Expand(line, true, force, visLevels - 1);
        end else
          Expand(line, false, force, visLevels - 1);
      end;
    end else
      Inc(Line);
	end;
end;

procedure TScintilla.FoldAll;
Var
  MaxLine,LineSeek,Line,Level,LineMaxSubord : Integer;
  Expanding : Boolean;
begin
  Colourise(0, -1);
  MaxLine := GetLineCount;
  Expanding := True;
  for LineSeek := 0 to MaxLine -1 do begin
    if (GetFoldLevel(LineSeek) and  SC_FOLDLEVELHEADERFLAG) > 0 then
    begin
      Expanding := not GetFoldExpanded(LineSeek);
      break;
    end;
  end;
  Line := 0;
  While Line < MaxLine do begin
    level := GetFoldLevel(line);
    if ((level and SC_FOLDLEVELHEADERFLAG) <> 0) and
		        (SC_FOLDLEVELBASE = (level and SC_FOLDLEVELNUMBERMASK)) then
    begin
      if expanding then
      begin
        SetFoldExpanded(line, True);
        Expand(line, true, false, 0, level);
        Dec(Line);
      end else begin
        LineMaxSubord := GetLastChild(line, -1);
        SetFoldExpanded(line, False);
        if LineMaxSubord > Line then
          HideLines(line+1, lineMaxSubord);
      end;
    end;
		Inc(Line);
  end;
end;


procedure TScintilla.doSciUpdateUI;
begin
 if FBraceHilite then ProcessBraces;
	inherited doSciUpdateUI;
end;

(**
 * Find if there is a brace next to the caret, checking before caret first, then
 * after caret. If brace found also find its matching brace.
 * @return @c true if inside a bracket pair.
 *)

procedure TScintilla.FindMatchingBracePosition(var braceAtCaret : Integer;
             var braceOpposite : Integer; var IsInside : boolean; sloppy : boolean=true);
Var
  IsAfter,ColonMode : boolean;
  CharBefore, CharAfter: Char;
  LineStart, LineMaxSubord,CaretPos :Integer;
begin
  IsInside := False;
  CaretPos := GetCurrentPos;
  BraceAtCaret := -1;
  BraceOpposite := -1;
  CharBefore := #0;
  if CaretPos > 0 then
    CharBefore := Char(GetCharAt(CaretPos -1));
  // Priority goes to character before caret
	if (CharBefore <> #0) and  (Pos(CharBefore, '[](){}') > 0)  then
		BraceAtCaret := caretPos - 1;

  ColonMode := False;
  if (GetLexer = SCLEX_PYTHON) and (CharBefore = ':') then
  begin
		BraceAtCaret := caretPos - 1;
		ColonMode := true;
  end;

  IsAfter := True;
  If (Sloppy and (BraceAtCaret < 0)) then
  begin
    // No brace found so check other side
    CharAfter := Char(GetCharAt(CaretPos));
  	if (CharAfter <> #0) and  (Pos(CharAfter, '[](){}') > 0)  then
    begin
	  	BraceAtCaret := CaretPos;
      IsAfter := False;
    end;
    if (GetLexer = SCLEX_PYTHON) and (CharAfter = ':') then
    begin
      BraceAtCaret := caretPos;
      ColonMode := true;
    end;
  end;

  if BraceAtCaret >= 0 then
  begin
    if ColonMode then
    begin
			LineStart := LineFromPosition(BraceAtCaret);
      LineMaxSubord := GetLastChild(LineStart, -1);
      BraceOpposite := GetLineEndPosition(LineMaxSubord);
    end else
      BraceOpposite := BraceMatch(BraceAtCaret);

    if BraceOpposite > BraceAtCaret then
      IsInside := IsAfter
    else
      IsInside := not IsAfter;
  end;
end;

procedure TScintilla.GoMatchingBrace(Select: boolean; Sloppy : boolean);
Var
  BraceAtCaret,BraceOpposite : Integer;
  IsInside : Boolean;
begin
  FindMatchingBracePosition(braceAtCaret, braceOpposite, IsInside, Sloppy);
  // Convert the character positions into caret positions based on whether
	// the caret position was inside or outside the braces.
  if BraceOpposite >= 0 then
  begin
    if IsInside then
    begin
      if braceOpposite > braceAtCaret then
        Inc(braceAtCaret)
      else
        Inc(braceOpposite);
    end else begin
      if braceOpposite > braceAtCaret then
        Inc(braceOpposite)
      else
        Inc(braceAtCaret);
    end;

		EnsureRangeVisible(braceOpposite, braceOpposite);
		if Select then
			SetSel(braceAtCaret, braceOpposite)
		else
			SetSel(BraceOpposite, BraceOpposite);
  end;
end;


procedure TScintilla.ProcessBraces;
Var
  ColumnAtCaret, ColumnOpposite,lineStart, indentPos, indentSize,indentPosNext, columnAtCaretNext,BraceAtCaret,BraceOpposite : Integer;
  IsInside : boolean;
	chBrace : char;
begin
  FindMatchingBracePosition(BraceAtCaret, BraceOpposite, IsInside, True);
	if ((BraceAtCaret <> -1) and (BraceOpposite = -1)) then
  begin
		BraceBadLight(braceAtCaret);
    Self.SetHighlightGuide(0);
	end else begin
		chBrace := Char(GetCharAt(BraceAtCaret));
    BraceHighlight(braceAtCaret, braceOpposite);
		columnAtCaret := GetColumn(BraceAtCaret);
    columnOpposite := GetColumn(BraceOpposite);
    if chBrace = ':' then
    begin
			lineStart := LineFromPosition(braceAtCaret);
			indentPos := GetLineIndentPosition(lineStart);
			indentPosNext := GetLineIndentPosition(lineStart + 1);
			columnAtCaret := GetColumn(indentPos);
			columnAtCaretNext := GetColumn(indentPosNext);
			indentSize := Self.GetIndent;
			if (columnAtCaretNext - indentSize > 1) then
				columnAtCaret := columnAtCaretNext - indentSize;
			if (columnOpposite = 0) then	// If the final line of the structure is empty
				columnOpposite := columnAtCaret;
    end;
    Self.SetHighlightGuide(Min(columnAtCaret, columnOpposite));
  end;
end;

procedure TScintilla.ExportToHTML(Stream: TStream; Title : string;UseCurrentEOLMode : Boolean);
Var
  StyleIsUsed  : array [0..STYLE_MAX+1] of boolean;
  lang : TSciLangItem;
  eolstr : String;

  procedure WriteS(const S : String);
  begin
    Stream.Write(S[1], Length(S));
  end;

  function ColorToHTMLColor(Color : TColor): string;
  var
    TempS : string;
  begin
    if Color<0 then
      Color:=ColorToRGB(Color);
    FmtStr(Result, '%s%.6x', ['#', Color]);
    // Now swap Blue and Red
    TempS := Result;
    Result[2] := TempS[6];
    Result[3] := TempS[7];
    Result[6] := TempS[2];
    Result[7] := TempS[3];
  end;

  procedure WriteStyle(const Style : TSciStyle);
  begin
    with Style do begin
      if not StyleIsUsed[StyleNumber] then exit;
      WriteS('.S'+IntToStr(StyleNumber)+ ' {'+eolstr);
      if FontName <> '' then
        WriteS(#9'font-family:' + FontName + ';'+eolstr);
      if FontSize <> 0 then
        WriteS(#9'font-size:' + IntToStr(Fontsize)+ 'pt' + ';'+eolstr);
      if fsItalic in FontStyles then
        WriteS(#9'font-style: italic;'+eolstr)
      else
        WriteS(#9'font-style: normal;'+eolstr);
      if fsBold in FontStyles then
        WriteS(#9'font-weight: 700;'+eolstr)
      else
        WriteS(#9'font-weight: 400;'+eolstr);
      if fsUnderline in FontStyles then
         WriteS(#9'text-decoration:underline;'+eolstr)
      else
         WriteS(#9'text-decoration:none;'+eolstr);
      if ForeColor<>clDefault then
        WriteS(#9'color: ' +  ColorToHTMLColor(ForeColor) +';'+eolstr);
      if BackColor<>clDefault then
        WriteS(#9'background: ' +  ColorToHTMLColor(BackColor) +';'+eolstr);
			WriteS('}'+eolstr);
    end;
  end;

Var
  i, itab,TabSize,Style, StyleCurrent, LengthDoc : Integer;
  ch : char;
begin
  if UseCurrentEOLMode=True then
  begin
  case EOLStyle of
  eolCRLF: eolstr:=CrLf;
  eolCR:eolstr:=#13;
  eolLF:eolstr:=#10;
  end;
  end else eolstr:=CrLf;

  Colourise(0,-1);
  TabSize := TabWidth;
  if TabSize = 0 then
    TabSize := 4;
  lang:=LanguageManager.LanguageList.Find(LanguageManager.SelectedLanguage);
  LengthDoc := GetLength;
  for i := 0 to STYLE_MAX + 1 do
    StyleIsUsed[i] := false;
  // check the used styles
  for i := 0 to LengthDoc do
    StyleIsUsed[GetStyleAt(i)] := true;
  StyleIsUsed[STYLE_DEFAULT] := true;

  WriteS('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "DTD/xhtml1-strict.dtd">'+eolstr);
  WriteS('<html xmlns="http://www.w3.org/1999/xhtml">'+eolstr);
  WriteS('<head>'+eolstr);
  WriteS('<title>'+Title+'</title>'+eolstr);
  WriteS('<meta name="GENERATOR" content="Delphi Scintilla Interface Components - delphisci.sourceforge.net"/>'+eolstr);
  if UseUnicode then
    WriteS('<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>' +eolstr);
  WriteS('<style type="text/css">'+eolstr);

  // Write default style
  WriteS('span {'+eolstr);
  WriteS(#9'font-family:' + Font.Name + ';'+eolstr);
  WriteS(#9'font-size:' + ToStr(Font.Size)+ 'pt' + ';'+eolstr);
  if fsItalic in Font.Style then
    WriteS(#9'font-style: italic;'+eolstr)
  else
    WriteS(#9'font-style: normal;'+eolstr);
  if fsBold in Font.Style then
    WriteS(#9'font-weight: 700;'+eolstr)
  else
    WriteS(#9'font-weight: 400;'+eolstr);
  if fsUnderline in Font.Style then
     WriteS(#9'text-decoration:underline;'+eolstr)
  else
     WriteS(#9'text-decoration:none;'+eolstr);
  WriteS(#9'color: ' +  ColorToHTMLColor(Font.Color) +';'+eolstr);
  WriteS(#9'background: ' +  ColorToHTMLColor(Color) +';'+eolstr);
  WriteS('}'+eolstr);
  // Write all styles
  for i := 0 to lang.Styles.Count - 1 do
    WriteStyle(lang.Styles.Items[i] as TSciStyle);

  WriteS('</style>'+eolstr);
  WriteS('</head>'+eolstr);
	if (Color <> clWhite) or (Color <> clWindow) then
		WriteS('<body bgcolor="' + ColorToHTMLColor(Color) + '">'+eolstr)
	else
		WriteS('<body>'+eolstr);

  StyleCurrent := GetStyleAt(0);
//  Line = Self.GetLine(0);
//	Level = (acc.LevelAt(line) & SC_FOLDLEVELNUMBERMASK) - SC_FOLDLEVELBASE;

  WriteS('<span>');
	WriteS('<span class="S'+ ToStr(styleCurrent) + '">');
	for i := 0 to lengthDoc -1 do begin
		ch := Char(GetCharAt(i) and $FF);
    Style := GetStyleAt(i);
  	if Style <> StyleCurrent then
    begin
  		WriteS('</span>');
			styleCurrent := style; //moved this before the <span writing.. Didn't write the correct style otherwise.
			WriteS('<span class="S'+ ToStr(styleCurrent) + '">');
	  end;
		Case Ord(ch) of
      Ord(' ') :
        begin
          if (Char(GetCharAt(i+1) and $FF) <> ' ') or (i+1 >= LengthDoc) then
            // Single space, kept as is
            WriteS(' ')
          else
            WriteS('&nbsp;');
        end;
      $9 :  //Tab
        for itab := 1 to TabSize do WriteS('&nbsp;');
      $D, $A :
        begin
  				if not ((ch = #13) and (GetCharAt(i+1) = $A)) then
          begin
	  				WriteS('<br/>');
            WriteS('</span>');
            StyleCurrent := GetStyleAt(i + 1);
            WriteS(eolstr);
            // we know it's the correct next style
            WriteS('<span class="S'+ IntToStr(styleCurrent) + '">');
          end;
			  end;
      Ord('<') : WriteS('&lt;');
      Ord('>') : WriteS('&gt;');
      Ord('&') : WriteS('&amp;');
      Ord('"') : WriteS('&quot;');
      Ord('æ') : WriteS('&aelig;');
      Ord('Æ') : WriteS('&AElig;');
      Ord('ø') : WriteS('&oslash;');
      Ord('Ø') : WriteS('&Oslash;');
      Ord('å') : WriteS('&aring;');
      Ord('Å') : WriteS('&Aring;');

    else
      WriteS(ch);
    end;
  end;
  WriteS('</span>');
  WriteS('</span>');
	WriteS(eolstr+'</body>'+eolstr+'</html>'+eolstr);
end;

//FlipVars may have a bug, but I've not been able to redo what caused it..
//To be checked out..
 { TODO 5 -ohdalis -cBug : Check out more throughly whether it's a bug in the FlipVars function. }
procedure TScintilla.FlipVars(const equalsign : String;spacebeforeandaftereq : Boolean;const endstatement : String);
  function SplitString(const src : String;var LeftSide,RightSide : String;const Separator,Eol : String;var notfound : Boolean) : String;
  var
    endstatementpos,eqpos,eollen,seplen : Integer;
  begin
    notfound:=False;
    eqpos:=Pos(Separator,src);
    seplen:=Length(Separator);
    if eqpos=0 then
    begin
      notfound:=True;
      Exit;
    end;
    if Eol='' then endstatementpos:=Length(src)
    else
      endstatementpos:=Pos(Eol,src);
    if endstatementpos=0 then
    begin
      endstatementpos:=Length(src);
      eollen:=0;
    end
    else eollen:=Length(Eol);
    leftside:=System.Copy(src,1,eqpos-1);
    rightside:=System.Copy(src,eqpos+seplen,endstatementpos-(eqpos+seplen));
    Result:=System.Copy(src,endstatementpos+eollen,Length(src));
  end;
var
  caretPosition,selectionStart,selectionEnd,selStartLine,selEndLine,numlines,lineStart,lineEnd,i,lineLength: Integer;
  move_caret,hasselection,notfound : Boolean;
  tmp,linebuf,whitespace,leftpart,rightpart: String;
begin
  SetLength(linebuf,1000);
  whitespace    :=' ';
  selectionStart:=GetSelectionStart;
  selectionEnd  :=GetSelectionEnd;
  caretPosition :=GetCurrentPos;
  hasselection  :=(selectionStart-selectionEnd)<>0;
  if (hasselection=False) then
  begin
    selStartLine:=LineFromPosition(caretPosition);
    selectionStart:=GetLineIndentPosition(selStartLine);
    selectionEnd:=GetLineEndPosition(LineFromPosition(caretPosition));
  end;
  move_caret    :=(caretPosition<selectionEnd);
  selStartLine  :=LineFromPosition(selectionStart);
  selEndLine    :=LineFromPosition(selectionEnd);
  numlines      :=selEndLine-selStartLine;
  if (numlines>1) and (selectionEnd=PositionFromLine(selEndLine)) then
  begin
    Dec(selEndLine);
    selectionEnd:=GetLineEndPosition(selEndLine);
  end;
  BeginUndoAction;
  for i:=selStartLine to selEndLine do
  begin
    lineStart:=GetLineIndentPosition(i);
    lineEnd:=GetLineEndPosition(i);
    if hasselection then
    begin
    if lineStart<selectionStart then lineStart:=selectionStart;
    if selectionEnd<lineEnd then lineEnd:=selectionEnd;
    end;
    lineBuf:=GetLineS(i);
    lineLength:=lineEnd-lineStart;
    if lineLength<=0 then Continue;
    SetLength(lineBuf,lineLength);
    GetRange(lineStart,lineEnd,PChar(lineBuf));

    if RangeIsAllWhiteSpace(lineStart,lineEnd)=False then
    begin
      tmp:='';
      repeat
        if notfound=False then
        begin
          if tmp<>'' then tmp:=tmp+' ';
          if spacebeforeandaftereq=True then
            tmp:=tmp+Trim(rightpart)+whitespace+equalsign+whitespace+Trim(leftpart)+endstatement
          else
            tmp:=tmp+Trim(rightpart)+equalsign+Trim(leftpart)+endstatement;
        end;
        lineBuf:=SplitString(lineBuf,leftpart,rightpart,equalsign,endstatement,notfound);
      until(notfound=True);
      if tmp<>'' then
      begin
        Inc(selectionEnd,Length(tmp)-lineLength);
        SetSel(lineStart,lineEnd);
        ReplaceSel(PChar(tmp));
      end;
    end;
  end;
  EndUndoAction;
  if move_caret=True then
  begin
    if hasselection then
    begin
      GotoPos(selectionEnd);
      SetCurrentPos(selectionStart);
    end else GotoPos(caretPosition);
  end else
    if hasselection then
    begin
      SetSel(selectionStart,selectionEnd);
    end else
      GotoPos(caretPosition);
end;

procedure TScintilla.SetCodeFoldingFlags(const Value : sciCodeFoldingFlags);
begin
  FCodeFoldingFlags:=Value;
	if foldFold in Value then
  begin
		Gutter2.Width := 14;
		// Respond to mouse click
		SetMarginSensitiveN(2, True);
		// Tell the lexer that we want folding info
    SetProperty('fold', '1');

    if foldCompact in FCodeFoldingFlags then
      SetProperty('fold.compact', '1')
    else
      SetProperty('fold.compact', '0');

    if foldComment in FCodeFoldingFlags then
      SetProperty('fold.comment', '1')  // Fold multiline comments
    else
      SetProperty('fold.comment', '0');
    if foldPreProcessor in FCodeFoldingFlags then
      SetProperty('fold.preprocessor', '1')
    else
     SetProperty('fold.preprocessor', '0');
    if foldAtElse in FCodeFoldingFlags then
      SetProperty('fold.at.else', '1')
    else
     SetProperty('fold.at.else', '0');
    if foldHTML in FCodeFoldingFlags then
      SetProperty('fold.html', '1')
    else
     SetProperty('fold.html', '0');
    if foldHTMLPreProcessor in FCodeFoldingFlags then
      SetProperty('fold.html.preprocessor', '1')
    else
     SetProperty('fold.html.preprocessor', '0');
    if foldCommentPython in FCodeFoldingFlags then
      SetProperty('fold.comment.python', '1')
    else
     SetProperty('fold.comment.python', '0');
    if foldQuotesPython in FCodeFoldingFlags then
      SetProperty('fold.quotes.python', '1')
    else
     SetProperty('fold.quotes.python', '0');

		//SetFoldFlags(16);
		// Set folding styles
		SetMarkers(FFoldMarkerType);
		SetMarginMaskN(2,Integer(SC_MASK_FOLDERS));
		//  The following is to handle SC_MOD_CHANGEFOLD in the Modified event handler
		SetModEventMask(GetModEventMask or SC_MOD_CHANGEFOLD);
	end else begin
		SetProperty('fold', '0');
    Gutter2.Width := 0;
  end;
end;

{$Ifdef USENEWLOADLEXER}
procedure TScintilla.LoadLexerLibrary(path : String);
var
  hinst : Cardinal;
  CountFn : GetLexerCountFn;
  NameFn : GetLexerNameFn;
  i,cnt : Integer;
  tmpnamebuf : String;
begin
  if assigned(FLibrariesLoaded) then
  begin
    if FLibrariesLoaded.IndexOf(SysUtils.LowerCase(path))<>-1 then
      Exit;
  end;
  if FileExists(path) then
  begin
    hinst:=LoadLibrary(PChar(path));
    if(hinst<>0) then
    begin
      CountFn:=GetProcAddress(hinst,'GetLexerCount');
      NameFn:=GetProcAddress(hinst,'GetLexerName');
      if assigned(CountFn) and assigned(NameFn) then
      begin
        cnt:=CountFn;
        for i:=0 to (cnt-1) do
        begin
          tmpnamebuf:='';
          SetLength(tmpnamebuf,200);
          NameFn(i,PChar(tmpnamebuf),200);
          if tmpnamebuf<>'' then
          begin
            if LanguageManager.HasLexer(tmpnamebuf)=false then
              LanguageManager.AddLexer(tmpnamebuf);
          end;
        end;
      end;
      inherited LoadLexerLibrary(PChar(path));
      if not assigned(FLibrariesLoaded) then
        FLibrariesLoaded:=TStringList.Create;
      FLibrariesLoaded.Add(SysUtils.LowerCase(path));
      FreeLibrary(hinst);
    end;
  end;
end;
{$Endif}

procedure TScintilla.SetFoldDrawFlags(const Value : sciFoldDrawFlags);
var
tmp : LongInt;
begin
tmp:=0;
  FFoldDrawFlags:=Value;
  if sciBoxIfExpanded in FFoldDrawFlags then
    tmp:=tmp+1;
  if sciAboveIfExpanded in FFoldDrawFlags then
    tmp:=tmp+2;
  if sciAboveIfNotExpanded in FFoldDrawFlags then
    tmp:=tmp+4;
  if sciBelowIfExpanded in FFoldDrawFlags then
    tmp:=tmp+8;
  if sciBelowIfNotExpanded in FFoldDrawFlags then
    tmp:=tmp+16;
  if sciHexLevels in FFoldDrawFlags then
    tmp:=tmp+64;
  SetFoldFlags(tmp);
end;

end.