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
{
	Author: hdalis (hdalis@users.sourceforge.net)
	Created: 05/06/2004
  $Id: ScintillaLanguageStyles.inc,v 1.5 2004/11/13 04:29:50 hdalis Exp $
	Purpose: Keywords and styles for a number of languages/lexers
     Note: The keywordlists are borrowed from the SciTE
           *.properties files where it was any, and later added new keywords
           if some was missing.
           Some of the styles are inherited from the DelphiWrapper v1.4
           by Simon Steele (ss@pnotepad.org) and (kvlahos@london.edu).

  History: 29/09/2004 Initial Release with Delphi Scintilla Interface Components
           30/10/2004 Added TSciLangSettings definitions for a number of languages.
           02/11/2004 Added the Asn1 and Vhdl lexer styles (Scintilla v1.62).
           25/01/2005 Fixed the styledefinition for XML. It seems it wasn't too fond
                      of no keywordlists at all.
					 29/01/2005 Added PHP as a language in it's own right, it is almost exactly the same as
											the HTML languagedefinition, but for some of the other components it's an
											advantage to have the PHP separated somewhat.

}

// Language settings, i.e assigned to the TSciLangItem items. See TSciLangSettings for what properties you can define.
const
  genericlanguagesettings : TSciLangSettings=(
    CommentBoxStart:'';CommentBoxMiddle:'';CommentBoxEnd:'';CommentBlock:'';CommentAtLineStart:True;
    CommentStreamStart:'';CommentStreamEnd:'';
    AssignmentOperator:'';EndOfStatementOperator:'';NumStyleBits:5;
  );
  fortranlanguagesettings : TSciLangSettings=(
    CommentBoxStart:'';CommentBoxMiddle:'';CommentBoxEnd:'';CommentBlock:'!~';CommentAtLineStart:True;
    CommentStreamStart:'';CommentStreamEnd:'';
    AssignmentOperator:'';EndOfStatementOperator:'';NumStyleBits:5;
  );

  cpplanguagesettings : TSciLangSettings=(
    CommentBoxStart:'/*';CommentBoxMiddle:'*';CommentBoxEnd:'*/';CommentBlock:'//';CommentAtLineStart:True;
    CommentStreamStart:'/*';CommentStreamEnd:'*/';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );

  sqllanguagesettings : TSciLangSettings=(
    CommentBoxStart:'/*';CommentBoxMiddle:'*';CommentBoxEnd:'*/';CommentBlock:'#';CommentAtLineStart:True;
    CommentStreamStart:'/*';CommentStreamEnd:'*/';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );

  vhdllanguagesettings : TSciLangSettings=(
    CommentBoxStart:'/*';CommentBoxMiddle:'*';CommentBoxEnd:'*/';CommentBlock:'--';CommentAtLineStart:True;
    CommentStreamStart:'/*';CommentStreamEnd:'*/';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );

  csslanguagesettings : TSciLangSettings=(
    CommentBoxStart:'/*';CommentBoxMiddle:'*';CommentBoxEnd:'*/';CommentBlock:'';CommentAtLineStart:True;
    CommentStreamStart:'/*';CommentStreamEnd:'*/';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  pascallanguagesettings : TSciLangSettings=(
    CommentBoxStart:'{';CommentBoxMiddle:'*';CommentBoxEnd:'}';CommentBlock:'//';CommentAtLineStart:True;
    CommentStreamStart:'{';CommentStreamEnd:'}';
    AssignmentOperator:':=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  vblanguagesettings : TSciLangSettings=(
    CommentBoxStart:'';CommentBoxMiddle:'';CommentBoxEnd:'';CommentBlock:'''';CommentAtLineStart:True;
    CommentStreamStart:'';CommentStreamEnd:'';
    AssignmentOperator:'=';EndOfStatementOperator:'';NumStyleBits:5;
  );
  avelanguagesettings : TSciLangSettings=(
    CommentBoxStart:'''';CommentBoxMiddle:'''';CommentBoxEnd:'''';CommentBlock:'''';CommentAtLineStart:True;
    CommentStreamStart:'''';CommentStreamEnd:'''';
    AssignmentOperator:'=';EndOfStatementOperator:'';NumStyleBits:5;
  );

  pythonlanguagesettings : TSciLangSettings=(
    CommentBoxStart:'#';CommentBoxMiddle:'#';CommentBoxEnd:'#';CommentBlock:'#';CommentAtLineStart:True;
    CommentStreamStart:'#';CommentStreamEnd:'#';
    AssignmentOperator:'=';EndOfStatementOperator:'';NumStyleBits:5;
  );
  perllanguagesettings : TSciLangSettings=(
    CommentBoxStart:'#';CommentBoxMiddle:'#';CommentBoxEnd:'#';CommentBlock:'#';CommentAtLineStart:True;
    CommentStreamStart:'#';CommentStreamEnd:'#';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  htmllanguagesettings: TSciLangSettings=(
    CommentBoxStart:'<!--';CommentBoxMiddle:' ';CommentBoxEnd:'-->';CommentBlock:'';CommentAtLineStart:True;
    CommentStreamStart:'<!--';CommentStreamEnd:'-->';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:7;
  );
  batlanguagesettings: TSciLangSettings=(
    CommentBoxStart:'rem';CommentBoxMiddle:'rem';CommentBoxEnd:'rem';CommentBlock:'rem';CommentAtLineStart:True;
    CommentStreamStart:'rem ';CommentStreamEnd:'rem ';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  lualanguagesettings: TSciLangSettings=(
    CommentBoxStart:'--[[';CommentBoxMiddle:' ';CommentBoxEnd:']]';CommentBlock:'--';CommentAtLineStart:True;
    CommentStreamStart:'--[[';CommentStreamEnd:']]';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  adalanguagesettings: TSciLangSettings=(
    CommentBoxStart:'--';CommentBoxMiddle:'--';CommentBoxEnd:'--';CommentBlock:'--';CommentAtLineStart:True;
    CommentStreamStart:'--';CommentStreamEnd:'--';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  nncrontablanguagesettings: TSciLangSettings=(
    CommentBoxStart:'\';CommentBoxMiddle:'\';CommentBoxEnd:'\';CommentBlock:'\';CommentAtLineStart:True;
    CommentStreamStart:'\';CommentStreamEnd:'\';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  mmixallanguagesettings:  TSciLangSettings=(
    CommentBoxStart:'%';CommentBoxMiddle:'%';CommentBoxEnd:'%';CommentBlock:'%';CommentAtLineStart:True;
    CommentStreamStart:'%';CommentStreamEnd:'%';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  tcllanguagesettings : TSciLangSettings=(
    CommentBoxStart:'##';CommentBoxMiddle:'#';CommentBoxEnd:'##';CommentBlock:'#';CommentAtLineStart:True;
    CommentStreamStart:'##';CommentStreamEnd:'##';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  lisplanguagesettings : TSciLangSettings=(
    CommentBoxStart:';;';CommentBoxMiddle:';;';CommentBoxEnd:';;';CommentBlock:';';CommentAtLineStart:True;
    CommentStreamStart:';;';CommentStreamEnd:';;';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  AutoItlanguagesettings : TSciLangSettings=(
    CommentBoxStart:';';CommentBoxMiddle:';';CommentBoxEnd:';';CommentBlock:';';CommentAtLineStart:True;
    CommentStreamStart:';';CommentStreamEnd:';';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );

  bullantlanguagesettings : TSciLangSettings=(
    CommentBoxStart:'@off';CommentBoxMiddle:'*';CommentBoxEnd:'@on';CommentBlock:';';CommentAtLineStart:True;
    CommentStreamStart:';';CommentStreamEnd:';';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );

  asmlanguagesettings : TSciLangSettings=(
    CommentBoxStart:';';CommentBoxMiddle:';';CommentBoxEnd:';';CommentBlock:';';CommentAtLineStart:True;
    CommentStreamStart:';';CommentStreamEnd:';';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );
  camllanguagesettings : TSciLangSettings=(
    CommentBoxStart:'(*';CommentBoxMiddle:'*';CommentBoxEnd:'*)';CommentBlock:'';CommentAtLineStart:True;
    CommentStreamStart:'(*';CommentStreamEnd:'*)';
    AssignmentOperator:'=';EndOfStatementOperator:';';NumStyleBits:5;
  );

const
	sci_KeyWordColor=TColor($00008080); 	// Color for Keywords
	sci_KeyWordColor2=TColor($00cccc00);	// Color for User Keywords etc
	sci_StringColor=TColor($0000ff00);		// Color for Strings,Chars
	sci_CommentColor=TColor($00909090);		// Color for Comments
  sci_CommentCmdColor=TColor($0000ff00);
  sci_CommentCmdErrorColor=TColor($000000ff);
	sci_NumberColor=TColor($000000e0);		// Color for Numbers
	sci_DefaultForeColor=TColor($00c0c0c0);	// Default foreground Color
	sci_DefaultBackColor=clDefault;
	sci_OperatorColor=TColor($00c0c0c0);		// Color for Operators
	sci_ErrorColor=TColor($000000ff);				// Color for Errors
	sci_PreprocColor=TColor($00ff00ff);			// Color for Preprocessors
	sci_IdentifierColor=TColor($00c0c0c0);	// Color for Identifiers
	sci_WhitespaceColor=TColor($00c0c0c0);	// Color for WhiteSpace
	sci_EOLNotClosedFore=TColor($00ffffff);	// Fore Color for unclosed strings
	sci_RegexForeColor=TColor($00ff32c0);		// Color for Regular Expressions

	sci_TagColor=TColor($00d0d000);         // Color for tags in HTML,XML,WML etc
	sci_AttributeColor=TColor($00C0A0A0);		// Color for attributes in HTML,XML,WML etc
	//sci_SymbolColor=TColor($804080);      // Color for symbols
	sci_SymbolColor=clDefault;      // Color for symbols
// BackColors
	sci_EOLNotClosedBack=TColor($00202020);	// Back Color for unclosed strings
	sci_RegexBackColor=clDefault;					// Back Color for Regular Expressions

Var
	DefaultStyles : array [0..4] of TSciLangStyle =
	(
(StyleNumber : 33; Name : 'LineNumbers'; FontName : 'Arial'; FontSize : 0; FontStyles : [];
			ForeColor : clSilver; BackColor : clBlack; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 34; Name : 'Ok Braces'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : clYellow; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 35; Name : 'Bad Braces'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : clRed; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 36; Name : 'Control Chars'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clBlack; BackColor : clSilver; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 37; Name : 'Indent Guide'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clGray; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
			);

{$Ifndef NO_SYN_PERLKEYWORDS}
PerlKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords: 'NULL __FILE__ __LINE__ __PACKAGE__ __DATA__ __END__ AUTOLOAD '+
'BEGIN CORE DESTROY END EQ GE GT INIT LE LT NE CHECK abs accept '+
'alarm and atan2 bind binmode bless caller chdir chmod chomp chop '+
'chown chr chroot close closedir cmp connect continue cos crypt '+
'dbmclose dbmopen defined delete die do dump each else elsif endgrent '+
'endhostent endnetent endprotoent endpwent endservent eof eq eval '+
'exec exists exit exp fcntl fileno flock for foreach fork format '+
'formline ge getc getgrent getgrgid getgrnam gethostbyaddr gethostbyname '+
'gethostent getlogin getnetbyaddr getnetbyname getnetent getpeername '+
'getpgrp getppid getpriority getprotobyname getprotobynumber getprotoent '+
'getpwent getpwnam getpwuid getservbyname getservbyport getservent '+
'getsockname getsockopt glob gmtime goto grep gt hex if index '+
'int ioctl join keys kill last lc lcfirst le length link listen '+
'local localtime lock log lstat lt m map mkdir msgctl msgget msgrcv '+
'msgsnd my ne next no not oct open opendir or ord our pack package '+
'pipe pop pos print printf prototype push q qq qr quotemeta qu '+
'qw qx rand read readdir readline readlink readpipe recv redo '+
'ref rename require reset return reverse rewinddir rindex rmdir '+
's scalar seek seekdir select semctl semget semop send setgrent '+
'sethostent setnetent setpgrp setpriority setprotoent setpwent '+
'setservent setsockopt shift shmctl shmget shmread shmwrite shutdown '+
'sin sleep socket socketpair sort splice split sprintf sqrt srand '+
'stat study sub substr symlink syscall sysopen sysread sysseek '+
'system syswrite tell telldir tie tied time times tr truncate '+
'uc ucfirst umask undef unless unlink unpack unshift untie until '+
'use utime values vec wait waitpid wantarray warn while write '+
'x xor y')
     );
{$Endif}
{$Ifndef NO_SYN_NNCRONTABSTYLES}
NncrontabStyles : array [0..8] of TSciLangStyle =
	(
(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Task start/end'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : sci_CommentColor; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'Section Keywords'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $00ffff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'Keywords'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Modificators'; FontName : ''; FontSize : 0; FontStyles : [fsItalic];
			ForeColor : $7f7f00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'Asterisk'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : clBlack; BackColor : $ffe0ff; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'Double Quoted String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 9; Name : 'Environment Variable'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}
{$Ifndef NO_SYN_NNCRONTABKEYWORDS}
  NncrontabKeywords : Array [0..2] of TSciLangKeywords =
     ((Name : 'Section keywords and Forth words';
       KeywordListNumber : 0;
       Keywords:  'AGAIN ALLOT AND Action BEGIN CASE COMPARE CONSTANT CREATE DO Days ELSE '+
									'ENDCASE ENDOF EVAL-SUBST EVALUATE FALSE Hours I IF LEAVE LOOP Minutes '+
									'Months NOT OF OFF ON OR PAD REPEAT Rule SET THEN TRUE Time UNTIL VALUE '+
									'VARIABLE WHILE WeekDays Years'
			 ),
			 (Name : 'nnCrontab keywords';
       KeywordListNumber : 1;
       Keywords:  '</JScript> </SCRIPT> </VBScript> <JScript> <VBScript> ACCESS-DATE BEEP '+
									'CALL_DIAL CALL_HANGUP CHAR CLIPBOARD CONSOLE CREATION-DATE CUR-DATE DATE- '+
									'DATE-INTERVAL DELAY DIAL DIR-CREATE DIR-DELETE DIR-EMPTY DIR-SIZE Day@ '+
									'Domain ERR-MSG EXIST FILE-ACCESS-DATE FILE-APPEND FILE-COPY FILE-CREATE '+
									'FILE-CREATION-DATE FILE-CROP FILE-DATE FILE-DELETE FILE-EMPTY FILE-EXIST '+
									'FILE-MOVE FILE-RENAME FILE-SIZE FILE-WRITE FILE-WRITE-DATE FOR-CHILD-WINDOWS '+
									'FOR-FILES FOR-WINDOWS FREE-SPACE GET-CUR-TIME GET-REG GET-VER GetTickCount '+
									'HANGUP HIDE-ICON HINT HINT-OFF HINT-POS HINT-SIZE HINTW HOST-EXIST Hour@ '+
									'IDLE INTERVAL IS-ARCHIVE IS-DIR IS-HIDDEN IS-READONLY IS-SYSTEM KILL LAUNCH '+
									'LOG LOGGEDON LOGOFF LogonBatch LogonInteractive LogonNetwork MOUSE-LBCLK '+
									'MOUSE-LBDCLK MOUSE-LBDN MOUSE-LBUP MOUSE-MOVE MOUSE-MOVER MOUSE-MOVEW '+
									'MOUSE-RBCLK MOUSE-RBDCLK MOUSE-RBDN MOUSE-RBUP MSG Min@ Mon@ '+
									'MonitorResponseTime NHOST-EXIST No ONLINE PAUSE PLAY-SOUND PLAY-SOUNDW '+
									'POP3-CHECK POWEROFF PROC-EXIST PROC-TIME PURGE-OLD PURGE-OLDA PURGE-OLDW '+
									'Password QSTART-APP QSTART-APPW QUERY QUIT RASDomain RASError RASPassword '+
									'RASPhone RASSecPassword RASUser RE-ALL RE-MATCH REBOOT REG-DELETE-KEY '+
									'REG-DELETE-VALUE REG-DWORD REG-SZ REMINDER RUN SEND-KEYS SEND-KEYS-DELAY '+
									'SHOW-ICON SHUTDOWN START-APP START-APPW START-QUIT START-TIME Sec@ '+
									'SecPassword THINT THINTW TimeSec@ TMSG TQUERY User WDay@ WIN-ACTIVATE '+
									'WIN-ACTIVE WIN-CLICK WIN-CLOSE WIN-EXIST WIN-HIDE WIN-HWND WIN-MAXIMIZE '+
									'WIN-MINIMIZE WIN-MOVE WIN-MOVER WIN-RESTORE WIN-SEND-KEYS WIN-SHOW '+
									'WIN-TERMINATE WIN-TOPMOST WIN-VER WIN-WAIT WINAPI WRITE-DATE WatchClipboard '+
									'WatchConnect WatchDir WatchDisconnect WatchDriveInsert WatchDriveRemove '+
									'WatchFile WatchProc WatchProcStop WatchWinActivate WatchWinCreate '+
									'WatchWinDestroy WatchWindow WinNT Year@ Yes'
			 ),
			 (Name : 'Modifiers';
       KeywordListNumber : 2;
       Keywords:  'ALL AboveNormalPriority AsService BelowNormalPriority FILESONLY '+
									'HighPriority IdlePriority LoadProfile NoActive NoDel NoLog NoRunAs '+
									'NormalPriority OnceADay OnceAHour OnceAMonth OnceAWeek RECURSIVE '+
									'RealtimePriority RunOnce SWHide ShowMaximized ShowMinimized ShowNoActivate '+
									'ShowNormal StartIn StartPos StartSize TODEPTH WATCH-CHANGE-ATTRIBUTES '+
									'WATCH-CHANGE-DIR-NAME WATCH-CHANGE-FILE-NAME WATCH-CHANGE-LAST-WRITE '+
									'WATCH-CHANGE-SECURITY WATCH-CHANGE-SIZE WaitFor WatchSubtree WithoutProfile'
			 )
			);
{$Endif}

{$Ifndef NO_SYN_CLWSTYLES} //CLARION
CLWStyles : array [0..14] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Label'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'User Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Integer Constant'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'Real Constant'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'Picture String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 9; Name : 'Compiler Directive'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 10; Name : 'Builtin Procedures/Function'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 11; Name : 'Structure/Data type'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 12; Name : 'Attribute'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 13; Name : 'Standard Equate'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 14; Name : 'Error'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}
{$Ifndef NO_SYN_CLWKEYWORDS}
  CLWKeywords : Array [0..6] of TSciLangKeywords =
     (
      (Name : 'Clarion Keywords';KeywordListNumber : 0;Keywords:  ''),
      (Name : 'Compiler Directives';KeywordListNumber : 1;Keywords:  ''),
      (Name : 'Built-in Procedures and Functions';KeywordListNumber : 2;Keywords:  ''),
      (Name : 'Structure and Data Types';KeywordListNumber : 3;Keywords:  ''),
      (Name : 'Attributes';KeywordListNumber : 4;Keywords:  ''),
      (Name : 'Standard Equates';KeywordListNumber : 5;Keywords:  ''),
      (Name : 'Reserved Words';KeywordListNumber : 6;Keywords:  '')
     );
{$Endif}


{$Ifndef NO_SYN_ADASTYLES}
AdaStyles : array [0..11] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'Operators (delimiters)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Character'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'End of line where character is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 7; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'End of line where string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 9; Name : 'Label'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $00007f; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 10; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 11; Name : 'Illegal token'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}

{$Ifndef NO_SYN_ADAKEYWORDS}
  AdaKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords:  'abort abstract accept access aliased all array at begin body '+
									'case constant declare delay delta digits do else elsif end entry exception exit for '+
									'function generic goto if in is limited loop new null of others out package pragma '+
									'private procedure protected raise range record renames requeue return reverse '+
									'select separate subtype tagged task terminate then type until use when while with '+
									{Operators}
									'abs and mod not or rem xor'
			)
			);

{$Endif}

{$Ifndef NO_SYN_PERLSTYLES}
PerlStyles : array [0..27] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'White Space'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Error'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clYellow; BackColor : clRed; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'POD: = at beginning of line'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $004000; BackColor : $E0FFE0; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 10; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 11; Name : 'Identifiers (functions, etc.)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 12; Name : 'Scalars: $var'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff32c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 13; Name : 'Array: @var'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff32c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 14; Name : 'Hash: %var'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff32c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 15; Name : 'Symbol table: *var'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff32c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 17; Name : 'Regex: /re/ or m{re}'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_RegexForeColor; BackColor : sci_RegexBackColor; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 18; Name : 'Substitution: s/re/ore/'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff32c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 19; Name : 'Long Quote (qq, qx, qr, qw)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $00ffff; BackColor : $a08080; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 20; Name : 'Back Ticks'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $00ffff; BackColor : $a08080; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 21; Name : 'Data Section: __DATA__ or __END__ at beginning of line'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $000060; BackColor : $d8f0ff; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 22; Name : 'Here-doc (delimiter)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff32c0; BackColor : $ddd0dd; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 23; Name : 'Here-doc (single quoted, q)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $7f007f; BackColor : $ddd0dd; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 24; Name : 'Here-doc (double quoted, qq)'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : $7f007f; BackColor : $ddd0dd; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 25; Name : 'Here-doc (back ticks, qx)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 26; Name : 'Single quoted string, generic'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 27; Name : 'qq = Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 28; Name : 'qx = Back ticks'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 29; Name : 'qr = Regex'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 30; Name : 'qw = Array'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)

);
{$Endif}

{$Ifndef NO_SYN_MMIXALSTYLES}
MMixalStyles : array [0..17] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'Divsion of leading whitespace in line'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Label'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $00007f; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 3; Name : 'Opcode (not validated)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 4; Name : 'Division between Label and Opcode'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 5; Name : 'Valid Opcode'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 6; Name : 'Unknown Opcode'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 7; Name : 'Division between Opcode and Operands'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 8; Name : 'Division of Operands'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 9; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 10; Name : 'Reference (to a Label)'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 11; Name : 'Char'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 12; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 13; Name : 'Register'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $7F007F; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 14; Name : 'Hexadecimal Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 15; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 16; Name : 'Symbol'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_SymbolColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
(StyleNumber : 17; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
);
{$Endif}

{$Ifndef NO_SYN_MMIXALKEYWORDS}

  MMixalKeywords : Array [0..2] of TSciLangKeywords =
     ((Name : 'Operation Codes';
       KeywordListNumber : 0;
       Keywords: '2ADDU 4ADDU 8ADDU 16ADDU ADD ADDU AND ANDNH ANDNL ANDNMH ANDNML '+
'BDIF BEV BN BNN BNP BNZ BOD BP BSPEC BYTE BZ CMP CMPU CSEV CSN CSNN CSNP CSNZ CSOD CSP CSWAP CSZ '+
'DIV DIVU ESPEC EXPR FADD FCMP FCMPE FDIV FEQL FEQLE FIX FIXU FLOT FLOTU FMUL FREM FSQRT FSUB FUN FUNE '+
'GET GETA GO GREG I_BIT INCH INCL INCMH INCML IS JMP LDA LDB LDBU LDHT LDO LDOU LDSF LDT LDTU LDUNC LDVTS LDW LDWU LOC LOCAL '+
'MOR MUL MULU MUX MXOR NAND NEG NEGU NNIX NOR NXOR O_BIT OCTA ODIF OR ORH ORL ORMH ORML ORN '+
'PBEV PBN PBNN PBNP PBNZ PBOD PBP PBZ POP PREFIX PREGO PRELD PREST PUSHGO PUSHJ PUT '+
'RESUME SAVE SET SETH SETL SETMH SETML SFLOT SFLOTU SL SLU SR SRU '+
'STB STBU STCO STHT STO STOU STSF STT STTU STUNC STW STWU SUB SUBU SWYM SYNC SYNCD TDIF TETRA TRAP TRIP UNSAVE '+
'WDIF WYDEXOR ZSEV ZSN ZSNN ZSNP ZSNZ ZSOD ZSP ZSZ'),
       (Name : 'Special Register';
        KeywordListNumber : 1;
        Keywords :  'rA rB rC rD rE rF rG rH rI rJ rK rL rM rN rO rP rQ rR rS rT rU rV rW rX rY rZ rBB rTT rWW rXX rYY rZZ'),
       (Name : 'Predefined Symbols';
        KeywordListNumber : 2;
        Keywords :  '@ Text_Segment Data_Segment Pool_Segment Stack_Segment '+
                    'StdErr StdIn StdOut '+
                    'Fopen Fclose Fread Fwrite Fgets Fputs Fgetws Fputws Ftell Fseek '+
                    'TextRead TextWrite BinaryRead BinaryWrite BinaryReadWrite')
     );
{$Endif}

{$Ifndef NO_SYN_LATEXSTYLES}
LatexStyles : array [0..4] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'WhiteSpace'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Command'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Tag'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'Math'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}


{$Ifndef NO_SYN_PASCALKEYWORDS}



  PascalKeywords : Array [0..1] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords: 'const type var begin end array set packed record string if then else ' +
       'while for to downto do with repeat until case of goto exit label procedure ' +
       'function nil file and or not xor div mod ' +
       'external asm inline object constructor destructor virtual far assembler near ' +
       'inherited stdcall cdecl export exports end. ' +
       'class ansistring raise try except on index name finally ' +
       'false true initialization finalization ' +
       'override overload at threadvar resourcestring as dispinterface in is out '+
       'program package library unit '+
       'interface uses implementation'),
       (Name : 'Class Keywords';
        KeywordListNumber : 1;
        Keywords :  'write read default public protected private property published stored automated')
     );
{$Endif}

{$Ifndef NO_SYN_PASCALSTYLES}
PascalStyles : Array [0..10] of TSciLangStyle =
		(
		(StyleNumber : 0; Name : 'White space'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		 (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Doc Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 16; Name : 'Class Keywords'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
     );
{$Endif}

{$Ifndef NO_SYN_CSSSTYLES}
	CSSStyles : Array[0..16] of TSciLangStyle=
	(
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'CSS1 Tag'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Class'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Pseudo Class'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Unknown Pseudo Class'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Unknown Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Value'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $00aa99; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'ID'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ff8080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Important'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ffffFF; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 12; Name : 'Directive'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $44aaff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 14; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 15; Name : 'CSS2 Tag'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $e0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 16; Name : 'Attribute'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)

	);
{$Endif}
{$Ifndef NO_SYN_CSSKEYWORDS}
	CSSKeywords : Array [0..2] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'color background-color background-image background-repeat background-attachment background-position background '+
'font-family font-style font-variant font-weight font-size font '+
'word-spacing letter-spacing text-decoration vertical-align text-transform text-align text-indent line-height '+
'margin-top margin-right margin-bottom margin-left margin '+
'padding-top padding-right padding-bottom padding-left padding '+
'border-top-width border-right-width border-bottom-width border-left-width border-width '+
'border-top border-right border-bottom border-left border '+
'border-color border-style width height float clear '+
'display white-space list-style-type list-style-image list-style-position list-style '+
'scrollbar-3dlight-color scrollbar-arrow-color scrollbar-base-color scrollbar-darkshadow-color '+
'scrollbar-face-color scrollbar-highlight-color scrollbar-shadow-color scrollbar-track-color'),
     (Name : 'Pseudo Classes';
       KeywordListNumber : 1;
       Keywords : 'first-letter first-line link active visited '+
'first-child focus hover lang before after left right first'),
     (Name : 'CSS2 Keywords';
       KeywordListNumber : 2;
       Keywords : 'border-top-color border-right-color border-bottom-color border-left-color border-color '+
'border-top-style border-right-style border-bottom-style border-left-style border-style '+
'top right bottom left position z-index direction unicode-bidi '+
'min-width max-width min-height max-height overflow clip visibility content quotes '+
'counter-reset counter-increment marker-offset '+
'size marks page-break-before page-break-after page-break-inside page orphans widows '+
'font-stretch font-size-adjust unicode-range units-per-em src '+
'panose-1 stemv stemh slope cap-height x-height ascent descent widths bbox definition-src '+
'baseline centerline mathline topline text-shadow '+
'caption-side table-layout border-collapse border-spacing empty-cells speak-header '+
'cursor outline outline-width outline-style outline-color '+
'volume speak pause-before pause-after pause cue-before cue-after cue '+
'play-during azimuth elevation speech-rate voice-family pitch pitch-range stress richness '+
'speak-punctuation speak-numeral')

		);
{$Endif}


{$Ifndef NO_SYN_VBKEYWORDS}

  VBKeywords : Array [0..3] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'and begin case call class continue do each else elseif end erase '+
       'error event exit false for function get gosub goto if implement in load loop lset '+
       'me mid new next not nothing on or property raiseevent rem resume return rset '+
       'select set stop sub then to true unload until wend while with withevents '+
       'attribute alias as boolean byref byte byval const compare currency date declare dim double '+
       'enum explicit friend global integer let lib long module object option optional '+
			 'preserve private public redim single static string type variant'),
       (Name : 'User 1';KeywordListNumber : 1;Keywords : ''),
       (Name : 'User 2';KeywordListNumber : 2;Keywords : ''),
       (Name : 'User 3';KeywordListNumber : 3;Keywords : '')

       );
{$Endif}

{$Ifndef NO_SYN_VBSTYLES}

  VBStyles : Array [0..9] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Whitespace'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Date'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $007F7F; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'String Not Closed (EOL)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $FFFFFF; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
     );
{$Endif}

{$Ifndef NO_SYN_POWERBASICSTYLES}
  PowerBasicStyles : Array [0..9] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Whitespace'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'String Not Closed (EOL)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $FFFFFF; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'Constant'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 14; Name : 'Asm'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
     );
{$Endif}

{$Ifndef NO_SYN_POWERBASICKEYWORDS}
  PowerBasicKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : '')
       );
{$Endif}


{$Ifndef NO_SYN_CPPKEYWORDS}
  CPPKeywords : Array [0..4] of TSciLangKeywords =
     ((Name : 'Primary keywords and identifiers';
			 KeywordListNumber : 0;
			 Keywords : '__asm _asm asm auto __automated bool break case catch __cdecl _cdecl cdecl char class '+
			 '__classid __closure const const_cast continue __declspec default delete __dispid do double dynamic_cast '+
			 ' else enum __except explicit __export export extern false __fastcall _fastcall __finally float for friend '+
			 ' goto if __import _import __inline inline int __int16 __int32 __int64 __int8 long __msfastcall __msreturn '+
			 ' mutable namespace new __pascal _pascal pascal private __property protected public __published register '+
			 ' reinterpret_cast return __rtti short signed sizeof static_cast static __stdcall _stdcall struct switch '+
			 ' template this __thread throw true __try try typedef typeid typename union unsigned using virtual void '+
			 'volatile wchar_t while dllexport dllimport naked noreturn nothrow novtable property selectany thread uuid '),
      (Name : 'Secondary keywords and identifiers';
       KeywordListNumber : 1;
			 Keywords : 'TStream TFileStream TMemoryStream TBlobStream TOleStream TStrings TStringList AnsiString String'+
			 ' WideString cout cin cerr endl fstream ostream istream wstring string deque list vector set multiset bitset'+
			 ' map multimap stack queue priority_queue'),
      (Name : 'Doc Comments';
			 KeywordListNumber : 2;
			 Keywords : 'a addindex addtogroup anchor arg attention '+
									'author b brief bug c class code date def defgroup deprecated dontinclude '+
									'e em endcode endhtmlonly endif endlatexonly endlink endverbatim enum example exception '+
									'f$ f[ f] file fn hideinitializer htmlinclude htmlonly '+
									'if image include ingroup internal invariant interface latexonly li line link '+
									'mainpage name namespace nosubgrouping note overload '+
									'p page par param post pre ref relates remarks return retval '+
									'sa section see showinitializer since skip skipline struct subsection '+
									'test throw todo typedef union until '+
									'var verbatim verbinclude version warning weakgroup $ @ < > \ & # { }'
			 ),
      (Name : 'Unused';KeywordListNumber : 3;Keywords : ''),
      (Name : 'Global classes and typedefs';
			 KeywordListNumber : 4;
			 Keywords : ''
			 )

      );
{$Endif}

{$Ifndef NO_SYN_CPPNOCASEKEYWORDS}
  CPPNoCaseKeywords : Array [0..4] of TSciLangKeywords =
     ((Name : 'Primary keywords and identifiers';KeywordListNumber : 0;Keywords : ''),
      (Name : 'Secondary keywords and identifiers';KeywordListNumber : 1;Keywords : ''),
      (Name : 'Doc Comments';KeywordListNumber : 2;Keywords : ''),
      (Name : 'Unused';KeywordListNumber : 3;Keywords : ''),
      (Name : 'Global classes and typedefs';KeywordListNumber : 4;Keywords : '')
      );
{$Endif}

{$Ifndef NO_SYN_TEXKEYWORDS}
  TEXKeywords : Array [0..6] of TSciLangKeywords =
     ((Name : 'TeX, eTeX, pdfTeX, Omega';KeywordListNumber : 0;Keywords : ''),
      (Name : 'ConTeXt Dutch';KeywordListNumber : 1;Keywords : ''),
      (Name : 'ConTeXt English';KeywordListNumber : 2;Keywords : ''),
      (Name : 'ConTeXt German';KeywordListNumber : 3;Keywords : ''),
      (Name : 'ConTeXt Czech';KeywordListNumber : 4;Keywords : ''),
      (Name : 'ConTeXt Italian';KeywordListNumber : 5;Keywords : ''),
      (Name : 'ConTeXt Romanian';KeywordListNumber : 6;Keywords : '')
      );
{$Endif}

{$Ifndef NO_SYN_TEXSTYLES}
  TEXStyles : Array [0..5] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Special'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Group'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Symbol'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Command'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Text'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);

{$Endif}

{$Ifndef NO_SYN_ERRORLISTSTYLES}
  ErrorListStyles : Array [0..20] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Python Error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'GCC Error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Microsoft Error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Command or return status'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Borland Error and Warnings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Perl Error and Warnings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : '.Net tracebacks'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Lua error and warning messages'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'CTags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Diff Changed !'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Diff Addition +'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 12; Name : 'Diff Deletion -'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 13; Name : 'Diff Message ---'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 14; Name : 'PHP Error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 15; Name : 'Essential Lahey Fortran 90 error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 16; Name : 'Intel Fortran Compiler error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 17; Name : 'Intel Fortran Compiler v8.0 error/warning'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 18; Name : 'Absoft Pro Fortran 90/95 v8.2 error or warning'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 19; Name : 'HTML Tidy'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 20; Name : 'Java runtime stack trace'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
  );
{$Endif}

{$Ifndef NO_SYN_CPPSTYLES}
  CPPStyles : Array [0..19] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'White space'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Doc Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Symbols/UUID'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_SymbolColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'EOL if string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 13; Name : 'Verbatim strings for C#'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 14; Name : 'Regular expressions'; FontName : 'Courier New'; FontSize : 0; FontStyles : [];
      ForeColor : sci_RegexForeColor; BackColor : sci_RegexBackColor; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 15; Name : 'Doc Comment Line'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 16; Name : 'User-defined keywords'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 17; Name : 'Comment keyword'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentCmdColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
      (StyleNumber : 18; Name : 'Comment keyword error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentCmdErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
      (StyleNumber : 19; Name : 'Global classes and typedefs'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)

     );

{$Endif}

{$Ifndef NO_SYN_CSKEYWORDS}
CSKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
			 KeywordListNumber : 0;
			 Keywords:  'abstract as base bool break byte case catch char checked class '+
									'const continue decimal default delegate do double else enum '+
									'event explicit extern false finally fixed float for foreach goto if '+
									'implicit in int interface internal is lock long namespace new null '+
									'object operator out override params private protected public '+
									'readonly ref return sbyte sealed short sizeof stackalloc static '+
									'string struct switch this throw true try typeof uint ulong '+
									'unchecked unsafe ushort using virtual void while'
			 )
			);
{$Endif}

{$Ifndef NO_SYN_JAVASCRIPTKEYWORDS}
  JavaScriptKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'abstract boolean break byte case catch char class '+
       'const continue debugger default delete do double else enum export extends '+
       'final finally float for function goto if implements import in instanceof '+
       'int interface long native new package private protected public '+
       'return short static super switch synchronized this throw throws '+
       'transient try typeof var void volatile while with true false null'));
{$Endif}

{$Ifndef NO_SYN_JAVAKEYWORDS}
  JavaKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'abstract boolean break byte case catch char class '+
       'const continue debugger default delete do double else enum export extends '+
       'final finally float for function goto if implements import in instanceof '+
       'int interface long native new package private protected public '+
       'return short static super switch synchronized this throw throws '+
       'transient try typeof var void volatile while with true false null'));
{$Endif}

{$Ifndef NO_SYN_RESOURCEKEYWORDS}
  ResourceKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Primary keywords and identifiers';
       KeywordListNumber : 0;
       Keywords : 'ACCELERATORS ALT AUTO3STATE AUTOCHECKBOX AUTORADIOBUTTON '+
									'BEGIN BITMAP BLOCK BUTTON CAPTION CHARACTERISTICS CHECKBOX CLASS '+
									'COMBOBOX CONTROL CTEXT CURSOR DEFPUSHBUTTON DIALOG DIALOGEX DISCARDABLE '+
									'EDITTEXT END EXSTYLE FONT GROUPBOX ICON LANGUAGE LISTBOX LTEXT '+
									'MENU MENUEX MENUITEM MESSAGETABLE POPUP '+
									'PUSHBUTTON RADIOBUTTON RCDATA RTEXT SCROLLBAR SEPARATOR SHIFT STATE3 '+
									'STRINGTABLE STYLE TEXTINCLUDE VALUE VERSION VERSIONINFO VIRTKEY'
));
{$Endif}

{$Ifndef NO_SYN_IDLKEYWORDS}
  IDLKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Primary keywords and identifiers';
       KeywordListNumber : 0;
       Keywords : 'aggregatable allocate appobject arrays async async_uuid '+
									'auto_handle '+
									'bindable boolean broadcast byte byte_count '+
									'call_as callback char coclass code comm_status '+
									'const context_handle context_handle_noserialize '+
									'context_handle_serialize control cpp_quote custom '+
									'decode default defaultbind defaultcollelem '+
									'defaultvalue defaultvtable dispinterface displaybind dllname '+
									'double dual '+
									'enable_allocate encode endpoint entry enum error_status_t '+
									'explicit_handle fault_status first_is float '+
									'handle_t heap helpcontext helpfile helpstring '+
									'helpstringcontext helpstringdll hidden hyper '+
									'id idempotent ignore iid_as iid_is immediatebind implicit_handle '+
									'import importlib in include in_line int __int64 __int3264 interface '+
									'last_is lcid length_is library licensed local long '+
									'max_is maybe message methods midl_pragma '+
									'midl_user_allocate midl_user_free min_is module ms_union '+
									'ncacn_at_dsp ncacn_dnet_nsp ncacn_http ncacn_ip_tcp '+
									'ncacn_nb_ipx ncacn_nb_nb ncacn_nb_tcp ncacn_np '+
									'ncacn_spx ncacn_vns_spp ncadg_ip_udp ncadg_ipx ncadg_mq '+
									'ncalrpc nocode nonbrowsable noncreatable nonextensible notify '+
									'object odl oleautomation optimize optional out out_of_line '+
									'pipe pointer_default pragma properties propget propput propputref '+
									'ptr public '+
									'range readonly ref represent_as requestedit restricted retval '+
									'shape short signed size_is small source strict_context_handle '+
									'string struct switch switch_is switch_type '+
									'transmit_as typedef '+
									'uidefault union unique unsigned user_marshal usesgetlasterror uuid '+
									'v1_enum vararg version void wchar_t wire_marshal attribute native noscript scriptable shared wstring inout'
		)
);
{$Endif}

{$Ifndef NO_SYN_HTMLKEYWORDS}
  HTMLKeywords : Array [0..5] of TSciLangKeywords =
     ((Name : 'HyperText';
       KeywordListNumber : 0;
       Keywords : 'a abbr acronym address applet area b base basefont '+
									'bdo big blockquote body br button caption center '+
									'cite code col colgroup dd del dfn dir div dl dt em '+
									'fieldset font form frame frameset h1 h2 h3 h4 h5 h6 '+
									'head hr html i iframe img input ins isindex kbd label '+
									'legend li link map menu meta noframes noscript '+
									'object ol optgroup option p param pre q s samp '+
									'script select small span strike strong style sub sup '+
									'table tbody td textarea tfoot th thead title tr tt u ul '+
									'var xml xmlns '+
									'abbr accept-charset accept accesskey action align alink '+
									'alt archive axis background bgcolor border '+
									'cellpadding cellspacing char charoff charset checked cite '+
									'class classid clear codebase codetype color cols colspan '+
									'compact content coords '+
									'data datafld dataformatas datapagesize datasrc datetime '+
									'declare defer dir disabled enctype event '+
									'face for frame frameborder '+
									'headers height href hreflang hspace http-equiv '+
									'id ismap label lang language leftmargin link longdesc '+
									'marginwidth marginheight maxlength media method multiple '+
									'name nohref noresize noshade nowrap '+
									'object onblur onchange onclick ondblclick onfocus '+
									'onkeydown onkeypress onkeyup onload onmousedown '+
									'onmousemove onmouseover onmouseout onmouseup '+
									'onreset onselect onsubmit onunload '+
									'profile prompt readonly rel rev rows rowspan rules '+
									'scheme scope selected shape size span src standby start style '+
									'summary tabindex target text title topmargin type usemap '+
									'valign value valuetype version vlink vspace width '+
									'text password checkbox radio submit reset '+
									'file hidden image framespacing scrolling allowtransparency bordercolor'
			),
      (Name : 'JavaScript';
       KeywordListNumber : 1;
       Keywords : 'abstract boolean break byte case catch char class '+
									'const continue debugger default delete do double else enum export extends '+
									'final finally float for function goto if implements import in instanceof '+
									'int interface long native new package private protected public '+
									'return short static super switch synchronized this throw throws '+
									'transient try typeof var void volatile while with'
			),
      (Name : 'VBScript';
       KeywordListNumber : 2;
       Keywords :  'and begin case call class continue do each else elseif end erase '+
									 'error event exit false for function get gosub goto if implement in load loop lset '+
									 'me mid new next not nothing on or property raiseevent rem resume return rset '+
									 'select set stop sub then to true unload until wend while with withevents '+
									 'attribute alias as boolean byref byte byval const compare currency date declare dim double '+
									 'enum explicit friend global integer let lib long module object option optional '+
									 'preserve private public redim single static string type variant'
			),
      (Name : 'Python';
       KeywordListNumber : 3;
       Keywords :  'and assert break class continue def del elif '+
									 'else except exec finally for from global if import in is lambda None '+
									 'not or pass print raise return try while yield'
			),
      (Name : 'PHP';
       KeywordListNumber : 4;
       Keywords :'and argv as argc break case cfunction class continue declare default do '+
								 'die echo else elseif empty enddeclare endfor endforeach endif endswitch '+
								 'endwhile e_all e_parse e_error e_warning eval exit extends false for '+
								 'foreach function global http_cookie_vars http_get_vars http_post_vars '+
								 'http_post_files http_env_vars http_server_vars if include include_once '+
								 'list new not null old_function or parent php_os php_self php_version '+
								 'print require require_once return static switch stdclass this true var '+
								 'xor virtual while __file__ __line__ __sleep __wakeup'
			),
      (Name : 'DTD Keywords';
       KeywordListNumber : 5;
       Keywords : 'ELEMENT DOCTYPE ATTLIST ENTITY NOTATION'
			)
	);
{$Endif}

{$Ifndef NO_SYN_ASN1STYLES}

  ASN1Styles : Array [0..10] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Numeric OID definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $7f7f00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Non OID numbers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Keywords'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Descriptors'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Types'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
    );
{$ENDIF}
{$Ifndef NO_SYN_ASN1KEYWORDS}
  ASN1Keywords : Array [0..3] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'ACCESS AGENT AUGMENTS BEGIN BITS CAPABILITIES CHOICE COMPLIANCE CONTACT CONVENTION '+
                  'DEFINITIONS DEFVAL DESCRIPTION DISPLAY END ENTERPRISE EXPORTS '+
                  'FALSE FROM GROUP GROUPS HINT IDENTITY IMPLIED IMPORTS INCLUDES INDEX INFO LAST MANDATORY MAX MIN MODULE '+
                  'NOTATION NOTIFICATION NULL OBJECTS OBJECT-TYPE OF ORGANIZATION PRODUCT RELEASE REFERENCE REQUIRES REVISION SEQUENCE SIZE STATUS SUPPORTS SYNTAX '+
                  'TEXTUAL TRAP TYPE TRAP-TYPE UPDATED VALUE VARIABLES VARIATION WRITE'),
      (Name : 'Attributes';
       KeywordListNumber : 1;
       Keywords : 'accessible create current deprecated for mandatory not notify not-accessible obsolete only optional read read-only read-write write'),
      (Name : 'Descriptors';
       KeywordListNumber : 2;
       Keywords : 'ABSENT ANY APPLICATION BIT BOOLEAN BY COMPONENT COMPONENTS DEFAULT DEFINED ENUMERATED EXPLICIT EXTERNAL '+
                  'IMPLICIT INIFINITY MAX MIN MINUS OPTIONAL PRESENT PRIVATE REAL SET TAGS TRUE'),
      (Name : 'Types';
       KeywordListNumber : 3;
       Keywords : 'Counter Counter32 Counter64 DisplayString Gauge Gauge32 IDENTIFIER INTEGER Integer32 IpAddress NetworkAddress NsapAddress '+
                  'OBJECT OCTET Opaque PhysAddress STRING TimeTicks UInteger32 UNITS Unsigned32')
);
{$Endif}

{$Ifndef NO_SYN_VHDLSTYLES}
  VHDLStyles : Array [0..14] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'WhiteSpace'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Bang Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Operators?'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'End of line where string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Std Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Attribute'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Std Function'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $208080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 12; Name : 'Std Package'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $208020; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 13; Name : 'Std Type'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $808020; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 14; Name : 'User defined identifiers and tasks'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $402080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
  );


{$Endif}
{$Ifndef NO_SYN_VHDLKEYWORDS}
  VHDLKeywords : Array [0..6] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'access after alias all architecture array assert attribute begin block '+
                  'body buffer bus case component configuration constant disconnect downto else elsif end entity exit file '+
                  'for function generate generic group guarded if impure in inertial inout is label library linkage literal '+
                  'loop map new next null of on open others out package port postponed procedure process pure range record '+
                  'register reject report return select severity shared signal subtype then to transport type unaffected '+
                  'units until use variable wait when while with'),
      (Name : 'Operators';
       KeywordListNumber : 1;
       Keywords : 'abs and mod nand nor not or rem rol ror sla sll sra srl xnor xor'),
      (Name : 'Attributes';
       KeywordListNumber : 2;
       Keywords : 'left right low high ascending image value pos val succ pred leftof rightof base range reverse_range '+
                  ' length delayed stable quiet transaction event active last_event last_active last_value driving '+
                  ' driving_value simple_name path_name instance_name'),
      (Name : 'Standard Functions';
       KeywordListNumber : 3;
       Keywords : 'now readline read writeline write endfile resolved to_bit to_bitvector to_stdulogic to_stdlogicvector '+
                  'to_stdulogicvector to_x01 to_x01z to_UX01 rising_edge falling_edge is_x shift_left shift_right rotate_left '+
                  'rotate_right resize to_integer to_unsigned to_signed std_match to_01'),
      (Name : 'Standard Packages';
       KeywordListNumber : 4;
       Keywords : 'std ieee work standard textio std_logic_1164 std_logic_arith std_logic_misc std_logic_signed '+
                  'std_logic_textio std_logic_unsigned numeric_bit numeric_std math_complex math_real vital_primitives '+
                  'vital_timing'),
      (Name : 'Standard Types';
       KeywordListNumber : 5;
       Keywords : 'boolean bit character severity_level integer real time delay_length natural positive string bit_vector '+
                  'file_open_kind file_open_status line text side width std_ulogic std_ulogic_vector std_logic '+
                  'std_logic_vector X01 X01Z UX01 UX01Z unsigned signed'),
      (Name : 'User Words';
       KeywordListNumber : 6;
       Keywords : '')

);
{$Endif}


{$Ifndef NO_SYN_HTMLSTYLES}

  HTMLStyles : Array [0..108] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Text'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Tags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Unknown Tags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Unknown Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Numbers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted strings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $0099aa; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Single quoted strings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Other inside tag'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Entities'; FontName : 'Times New Roman'; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $A0A0A0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'XML short tag end'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $C0C000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'XML identifier start'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $A000A0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'XML identifier end'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $A000A0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 14; Name : 'SCRIPT'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $0A0A00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 15; Name : 'ASP <% ... %>'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00ffff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 16; Name : 'ASP <% ... %>'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00ffff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 17; Name : 'CDATA'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00dfff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 18; Name : 'PHP'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00dddd; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 19; Name : 'Unquoted values'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 20; Name : 'XC Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 21; Name : 'SGML tags <! ... >'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 22; Name : 'SGML command'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 23; Name : 'SGML 1st param'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $f0ff0f; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 24; Name : 'SGML double string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 25; Name : 'SGML single string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 26; Name : 'SGML error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 27; Name : 'SGML special'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $FF6633; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 28; Name : 'SGML entity'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 29; Name : 'SGML comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 31; Name : 'SGML block'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $ff0000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
      //# Embedded Javascript
     (StyleNumber : 40; Name : 'JS Start'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $007F7F; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 41; Name : 'JS Default'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 42; Name : 'JS Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 43; Name : 'JS Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 44; Name : 'JS Doc Comment'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 45; Name : 'JS Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 46; Name : 'JS Word'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 47; Name : 'JS Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 48; Name : 'JS Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 49; Name : 'JS Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 50; Name : 'JS Symbols'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_SymbolColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 51; Name : 'JS EOL'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 52; Name : 'JS Regex'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_RegexForeColor; BackColor : sci_RegexBackColor; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
			//ASP JavaScript
     (StyleNumber : 55; Name : 'ASP JS Start'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $007F7F; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 56; Name : 'ASP JS Default'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 57; Name : 'ASP JS Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 58; Name : 'ASP JS Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 59; Name : 'ASP JS Doc Comment'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 60; Name : 'ASP JS Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 61; Name : 'ASP JS Word'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $e0e0e0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 62; Name : 'ASP JS Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 63; Name : 'ASP JS Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 64; Name : 'ASP JS Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 65; Name : 'ASP JS Symbols'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_SymbolColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 66; Name : 'ASP JS EOL'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 67; Name : 'ASP JS Regex'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_RegexForeColor; BackColor : sci_RegexBackColor; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),

      //# Embedded VBS
     (StyleNumber : 71; Name : 'VBS Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 72; Name : 'VBS Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 73; Name : 'VBS Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 74; Name : 'VBS KeyWord'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 75; Name : 'VBS String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 76; Name : 'VBS Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 77; Name : 'VBS Unterminated string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
      //# ASP VBS
     (StyleNumber : 81; Name : 'ASP Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 82; Name : 'ASP Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 83; Name : 'ASP Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 84; Name : 'ASP KeyWord'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 85; Name : 'ASP String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 86; Name : 'ASP Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 87; Name : 'ASP Unterminated string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
      //# Embedded Python
     (StyleNumber : 90; Name : 'Python Start'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $808080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 91; Name : 'Python Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $808080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 92; Name : 'Python Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 93; Name : 'Python Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 94; Name : 'Python String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 95; Name : 'Python Single quoted string'; FontName : 'Courier New'; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 96; Name : 'Python Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 97; Name : 'Python Triple quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00007F; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 98; Name : 'Python Triple double quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00007F; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 99; Name : 'Python Class name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $FF0000; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 100; Name : 'Python function or method name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $7F7F00; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 101; Name : 'Python function or method name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 102; Name : 'Python Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 104; Name : 'PHP Complex Variable'; FontName : ''; FontSize : 0; FontStyles : [fsItalic];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),     
      //# ASP Python
     (StyleNumber : 105; Name : 'ASP Python Start'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $808080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 106; Name : 'ASP Python Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $808080; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 107; Name : 'ASP Python Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 108; Name : 'ASP Python Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 109; Name : 'ASP Python String'; FontName : 'Courier New'; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 110; Name : 'ASP Python Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 111; Name : 'ASP Python Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 112; Name : 'ASP Python Triple quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00007F; BackColor : $CFEFCF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 113; Name : 'ASP Python Triple double quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00007F; BackColor : $CFEFCF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 114; Name : 'ASP Python Class name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $FF0000; BackColor : $CFEFCF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 115; Name : 'ASP Python function or method name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $7F7F00; BackColor : $EFFFEF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 116; Name : 'ASP Python function or method name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : $CFEFCF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 117; Name : 'ASP Python Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : $CFEFCF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
      //# PHP
     (StyleNumber : 118; Name : 'PHP Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 119; Name : 'PHP Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 120; Name : 'PHP Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 121; Name : 'PHP Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 122; Name : 'PHP Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 123; Name : 'PHP Variable'; FontName : ''; FontSize : 0; FontStyles : [fsItalic];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 104; Name : 'PHP Complex Variable'; FontName : ''; FontSize : 0; FontStyles : [fsItalic];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 124; Name : 'PHP Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 125; Name : 'PHP One line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 126; Name : 'PHP Variable in double quoted string'; FontName : ''; FontSize : 0; FontStyles : [fsItalic];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 127; Name : 'PHP operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)

    );
{$Endif}


{$Ifndef NO_SYN_XMLSTYLES}
  XMLStyles : Array [0..26] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Tags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Unknown Tags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Unknown Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Numbers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted strings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Single quoted strings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Other inside tag'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Entities'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'XML short tag end'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'XML identifier start'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'XML identifier end'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 17; Name : 'CDATA'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $000080; BackColor : $F0F0FF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 18; Name : 'XML Question'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $0000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 19; Name : 'Unquoted values'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 21; Name : 'SGML tags <! ... >'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 22; Name : 'SGML command'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 23; Name : 'SGML 1st param'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $f0ff0f; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 24; Name : 'SGML double string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 25; Name : 'SGML single string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 26; Name : 'SGML error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 27; Name : 'SGML special'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $FF6633; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 28; Name : 'SGML entity'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 29; Name : 'SGML comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 31; Name : 'SGML block'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $660000; BackColor : $E0CCCC; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
    );
{$Endif}

{$Ifndef NO_SYN_XMLKEYWORDS}
  XMLKeywords : Array [0..1] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords :  ''),
       (Name : 'SGML Keywords';
       KeywordListNumber : 5;
       Keywords :  'ELEMENT DOCTYPE ATTLIST ENTITY NOTATION')
       );
{$Endif}

{$Ifndef NO_SYN_PYTHONKEYWORDS}
  PythonKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords :  'and assert break class continue def del elif ' +
									 'else except exec finally for from global if import in is lambda None ' +
									 'not or pass print raise return try while yield'));
{$Endif}

{$Ifndef NO_SYN_PYTHONSTYLES}
  PythonStyles : Array [0..13] of TSciLangStyle =
    (
		 (StyleNumber : 0; Name : 'White space'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Triple quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Triple quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Class name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Function or method name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $7F7F00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'Comment blocks'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'End of line where string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
     );
{$Endif}


{$Ifndef NO_SYN_RUBYKEYWORDS}
  RubyKeywords : Array [0..0] of TSciLangKeywords =
    ( (Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : '__FILE__ and def end in or self unless __LINE__ begin '+
									'defined? ensure module redo super until BEGIN break do false next rescue '+
									'then when END case else for nil require retry true while alias class elsif if '+
									'not return undef yield'
			)
		);
{$Endif}


{$Ifndef NO_SYN_TCLSTYLES}
  TCLStyles : Array [0..13] of TSciLangStyle =
    (
		 (StyleNumber : 0; Name : 'Whitespace'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'String'; FontName : 'Courier New'; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Triple quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Triple double quotes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Class name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Function or method name definition'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $7F7F00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'Comment blocks'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'End of line where string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
     );
{$Endif}


{$Ifndef NO_SYN_TCLKEYWORDS}
  TCLKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords :
									{TCL}
									'after append array auto_execok auto_import auto_load '+
									'auto_load_index auto_qualify beep binary break case catch cd clock '+
									'close concat continue dde default echo else elseif encoding eof '+
									'error eval exec exit expr fblocked fconfigure fcopy file '+
									'fileevent flush for foreach format gets glob global history if '+
									'incr info interp join lappend lindex linsert list llength load '+
									'lrange lreplace lsearch lsort namespace open package pid '+
									'pkg_mkIndex proc puts pwd read regexp regsub rename resource '+
									'return scan seek set socket source split string subst switch '+
									'tclLog tclMacPkgSearch tclPkgSetup tclPkgUnknown tell time '+
									'trace unknown unset update uplevel upvar variable vwait while '+
									{TK}
									'bell bind bindtags button canvas checkbutton console '+
									'destroy entry event focus font frame grab grid image label listbox menu '+
									'menubutton message pack place radiobutton raise scale scrollbar '+
									'text tk tkwait toplevel winfo wm '+
									{ITCL}
									'@scope body class code common component configbody '+
									'constructor define destructor hull import inherit itcl itk itk_component '+
									'itk_initialize itk_interior itk_option iwidgets keep method '+
									'private protected public '+
									{TkCommands}
									'tkButtonDown tkButtonEnter tkButtonInvoke '+
									'tkButtonLeave tkButtonUp tkCancelRepeat tkCheckRadioInvoke tkDarken '+
									'tkEntryAutoScan 	tkEntryBackspace tkEntryButton1 tkEntryClosestGap '+
									'tkEntryInsert tkEntryKeySelect tkEntryMouseSelect tkEntryNextWord '+
									'tkEntryPaste tkEntryPreviousWord tkEntrySeeInsert tkEntrySetCursor '+
									'tkEntryTranspose tkEventMotifBindings tkFDGetFileTypes tkFirstMenu '+
									'tkFocusGroup_Destroy tkFocusGroup_In tkFocusGroup_Out tkFocusOK '+
									'tkListboxAutoScan tkListboxBeginExtend tkListboxBeginSelect '+
									'tkListboxBeginToggle tkListboxCancel tkListboxDataExtend '+
									'tkListboxExtendUpDown tkListboxMotion tkListboxSelectAll '+
									'tkListboxUpDown tkMbButtonUp tkMbEnter tkMbLeave tkMbMotion '+
									'tkMbPost tkMenuButtonDown tkMenuDownArrow tkMenuDup tkMenuEscape '+
									'tkMenuFind tkMenuFindName tkMenuFirstEntry tkMenuInvoke tkMenuLeave '+
									'tkMenuLeftArrow tkMenuMotion tkMenuNextEntry tkMenuNextMenu '+
									'tkMenuRightArrow tkMenuUnpost tkMenuUpArrow tkMessageBox '+
									'tkPostOverPoint tkRecolorTree tkRestoreOldGrab tkSaveGrabInfo '+
									'tkScaleActivate tkScaleButton2Down tkScaleButtonDown '+
									'tkScaleControlPress tkScaleDrag tkScaleEndDrag tkScaleIncrement '+
									'tkScreenChanged tkScrollButton2Down tkScrollButtonDown '+
									'tkScrollButtonUp tkScrollByPages tkScrollByUnits tkScrollDrag '+
									'tkScrollEndDrag tkScrollSelect tkScrollStartDrag tkScrollToPos  '+
									'tkScrollTopBottom tkTabToWindow tkTearOffMenu tkTextAutoScan '+
									'tkTextButton1 tkTextClosestGap tkTextInsert tkTextKeyExtend '+
									'tkTextKeySelect tkTextNextPara tkTextNextPos tkTextNextWord '+
									'tkTextPaste tkTextPrevPara tkTextPrevPos tkTextResetAnchor '+
									'tkTextScrollPages tkTextSelectTo tkTextSetCursor tkTextTranspose '+
									'tkTextUpDownLine tkTraverseToMenu tkTraverseWithinMenu tk_bisque '+
									'tk_chooseColor tk_dialog tk_focusFollowsMouse tk_focusNext '+
									'tk_focusPrev tk_getOpenFile tk_getSaveFile tk_messageBox '+
									'tk_optionMenu tk_popup tk_setPalette tk_textCopy tk_textCut '+
									'tk_textPaste'
));
{$Endif}

{$Ifndef NO_SYN_SQLKEYWORDS}
  SQLKeywords : Array [0..0] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'ACTION ADD AGGREGATE ALL '+
									'ALTER AFTER AND AS '+
									'ASC AVG AVG_ROW_LENGTH AUTO_INCREMENT '+
									'BETWEEN BIGINT BIT BINARY '+
									'BLOB BOOL BOTH BY '+
									'CASCADE CASE CHAR CHARACTER '+
									'CHANGE CHECK CHECKSUM COLUMN '+
									'COLUMNS COMMENT CONSTRAINT CREATE '+
									'CROSS CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP '+
									'DATA DATABASE DATABASES DATE '+
									'DATETIME DAY DAY_HOUR DAY_MINUTE '+
									'DAY_SECOND DAYOFMONTH DAYOFWEEK DAYOFYEAR '+
									'DEC DECIMAL DEFAULT DELAYED '+
									'DELAY_KEY_WRITE DELETE DESC DESCRIBE '+
									'DISTINCT DISTINCTROW DOUBLE DROP '+
									'END ELSE ESCAPE ESCAPED '+
									'ENCLOSED ENUM EXPLAIN EXISTS '+
									'FIELDS FILE FIRST FLOAT '+
									'FLOAT4 FLOAT8 FLUSH FOREIGN '+
									'FROM FOR FULL FUNCTION '+
									'GLOBAL GRANT GRANTS GROUP '+
									'HAVING HEAP HIGH_PRIORITY HOUR '+
									'HOUR_MINUTE HOUR_SECOND HOSTS IDENTIFIED '+
									'IGNORE IN INDEX INFILE '+
									'INNER INSERT INSERT_ID INT '+
									'INTEGER INTERVAL INT1 INT2 '+
									'INT3 INT4 INT8 INTO '+
									'IF IS ISAM JOIN '+
									'KEY KEYS KILL LAST_INSERT_ID '+
									'LEADING LEFT LENGTH LIKE '+
									'LINES LIMIT LOAD LOCAL '+
									'LOCK LOGS LONG LONGBLOB '+
									'LONGTEXT LOW_PRIORITY MAX MAX_ROWS '+
									'MATCH MEDIUMBLOB MEDIUMTEXT MEDIUMINT '+
									'MIDDLEINT MIN_ROWS MINUTE MINUTE_SECOND '+
									'MODIFY MONTH MONTHNAME MYISAM '+
									'NATURAL NUMERIC NO NOT '+
									'NULL ON OPTIMIZE OPTION '+
									'OPTIONALLY OR ORDER OUTER '+
									'OUTFILE PACK_KEYS PARTIAL PASSWORD '+
									'PRECISION PRIMARY PROCEDURE PROCESS '+
									'PROCESSLIST PRIVILEGES READ REAL '+
									'REFERENCES RELOAD REGEXP RENAME '+
									'REPLACE RESTRICT RETURNS REVOKE '+
									'RLIKE ROW ROWS SECOND '+
									'SELECT SET SHOW SHUTDOWN '+
									'SMALLINT SONAME SQL_BIG_TABLES SQL_BIG_SELECTS '+
									'SQL_LOW_PRIORITY_UPDATES SQL_LOG_OFF SQL_LOG_UPDATE SQL_SELECT_LIMIT '+
									'SQL_SMALL_RESULT SQL_BIG_RESULT SQL_WARNINGS STRAIGHT_JOIN '+
									'STARTING STATUS STRING TABLE '+
									'TABLES TEMPORARY TERMINATED TEXT '+
									'THEN TIME TIMESTAMP TINYBLOB '+
									'TINYTEXT TINYINT TRAILING TO '+
									'TYPE USE USING UNIQUE '+
									'UNLOCK UNSIGNED UPDATE USAGE '+
									'VALUES VARCHAR VARIABLES VARYING '+
									'VARBINARY WITH WRITE WHEN '+
									'WHERE YEAR YEAR_MONTH ZEROFILL '
			)
	);
{$Endif}

{$Ifndef NO_SYN_SQLSTYLES}
  SQLStyles : Array [0..12] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Whitespace'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Doc Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Single quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Symbols'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_SymbolColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'EOL if string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
     );
{$Endif}

{$Ifndef NO_SYN_PROPSSTYLES}
	PropsStyles : array [0..4] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Section'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Assignment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $99f3c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Default value (@)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
			);
{$Endif}

{$Ifndef NO_SYN_BATSTYLES}
	BatStyles : array [0..7] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Label'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00ffff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Hide command char (@)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'External Commands'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Variable'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $c000c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $99f3c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
			);
{$Endif}

{$Ifndef NO_SYN_BATKEYWORDS}
  BatKeywords : Array [0..0] of TSciLangKeywords =
    ( (Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'rem set if exist errorlevel for in do '+
									'break call chcp cd chdir choice cls country ctty date del erase dir echo '+
									'exit goto loadfix loadhigh mkdir md move path pause prompt rename ren '+
									'rmdir rd shift time type ver verify vol '+
									'com con lpt nul endlocal setlocal'
			)
		);
{$Endif}

{$Ifndef NO_SYN_MAKESTYLES}
	MakeStyles : array [0..6] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Variable'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $d0d000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $99f3c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Target'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $0000ee; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00ffff; BackColor : $00000ff; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
			);

{$Endif}

{$Ifndef NO_SYN_DIFFSTYLES}
	DiffStyles : array [0..6] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment (part before "diff ..." or "--- ..." and , Only in ..., Binary file...)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Command (diff ...)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00ffff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Source file (--- ...) and Destination file (+++ ...)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00009f; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Position setting (@@ ...)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $3f00ff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Line removal (-...)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $ffff00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Line addition (+...)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $ff22ff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
			);
{$Endif}

{$Ifndef NO_SYN_CONFSTYLES}

	ConfStyles : array [0..9] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'White Space'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Apache Runtime Directive'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'extensions (like .gz, .tgz, .html)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $00c0c0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'parameters for Apache Runtime directives'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'IP address'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $bfbf00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $c0c000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
			);

{$Endif}

{$Ifndef NO_SYN_CONFKEYWORDS}
	// Apache Configuration
  ConfKeywords : Array [0..1] of TSciLangKeywords =
     (
		 (Name : 'Directives';
       KeywordListNumber : 0;
       Keywords : 'accessconfig accessfilename action addalt '+
									'addaltbyencoding addaltbytype addcharset '+
									'adddefaultcharset adddescription '+
									'addencoding addhandler addicon addiconbyencoding '+
									'addiconbytype addlanguage addmodule addmoduleinfo '+
									'addtype agentlog alias aliasmatch '+
									'allow allowconnect allowoverride anonymous '+
									'anonymous_authoritative anonymous_logemail anonymous_mustgiveemail '+
									'anonymous_nouserid anonymous_verifyemail authauthoritative '+
									'authdbauthoritative authdbgroupfile authdbmauthoritative '+
									'authdbmgroupfile authdbmgroupfile authdbuserfile authdbmuserfile '+
									'authdigestfile authgroupfile authname authtype '+
									'authuserfile bindaddress browsermatch browsermatchnocase '+
									'bs2000account cachedefaultexpire cachedirlength cachedirlevels '+
									'cacheforcecompletion cachegcinterval cachelastmodifiedfactor cachemaxexpire '+
									'cachenegotiateddocs cacheroot cachesize checkspelling '+
									'clearmodulelist contentdigest cookieexpires cookielog '+
									'cookielog cookietracking coredumpdirectory customlog '+
									'defaulticon defaultlanguage defaulttype define '+
									'deny directory directorymatch directoryindex '+
									'documentroot errordocument errorlog example '+
									'expiresactive expiresbytype expiresdefault extendedstatus '+
									'fancyindexing files filesmatch forcetype '+
									'group header headername hostnamelookups '+
									'identitycheck ifdefine ifmodule imapbase '+
									'imapdefault imapmenu include indexignore '+
									'indexoptions keepalive keepalivetimeout languagepriority '+
									'limit limitexcept limitrequestbody limitrequestfields '+
									'limitrequestfieldsize limitrequestline listen listenbacklog '+
									'loadfile loadmodule location locationmatch '+
									'lockfile logformat loglevel maxclients '+
									'maxkeepaliverequests maxrequestsperchild maxspareservers metadir '+
									'metafiles metasuffix mimemagicfile minspareservers '+
									'mmapfile namevirtualhost nocache options order '+
									'passenv pidfile port proxyblock proxydomain '+
									'proxypass proxypassreverse proxyreceivebuffersize proxyremote '+
									'proxyrequests proxyvia qsc readmename '+
									'redirect redirectmatch redirectpermanent redirecttemp '+
									'refererignore refererlog removehandler require '+
									'resourceconfig rewritebase rewritecond rewriteengine '+
									'rewritelock rewritelog rewriteloglevel rewritemap '+
									'rewriteoptions rewriterule rlimitcpu rlimitmem '+
									'rlimitnproc satisfy scoreboardfile script '+
									'scriptalias scriptaliasmatch scriptinterpretersource scriptlog '+
									'scriptlogbuffer scriptloglength sendbuffersize '+
									'serveradmin serveralias servername serverpath '+
									'serverroot serversignature servertokens servertype '+
									'setenv setenvif setenvifnocase sethandler '+
									'singlelisten startservers threadsperchild timeout '+
									'transferlog typesconfig unsetenv usecanonicalname '+
									'user userdir virtualhost virtualdocumentroot '+
									'virtualdocumentrootip virtualscriptalias virtualscriptaliasip '+
									'xbithack from all'
		  ),
		 (Name : 'Parameters';
			 KeywordListNumber : 1;
			 Keywords : 'on off standalone inetd '+
									'force-response-1.0 downgrade-1.0 nokeepalive '+
									'indexes includes followsymlinks none '+
									'x-compress x-gzip'
			)
	);
{$Endif}

{$Ifndef NO_SYN_WMLSTYLES}
  WMLStyles : Array [0..26] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Tags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Unknown Tags'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_AttributeColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Unknown Attributes'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Numbers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted strings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Single quoted strings'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Other inside tag'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Entities'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'XML short tag end'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'XML identifier start'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'XML identifier end'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 17; Name : 'CDATA'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $000080; BackColor : $F0F0FF; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 18; Name : 'XML Question'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $0000a0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 19; Name : 'Unquoted values'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 21; Name : 'SGML tags <! ... >'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_TagColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 22; Name : 'SGML command'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : $a0a000; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 23; Name : 'SGML 1st param'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $f0ff0f; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 24; Name : 'SGML double string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 25; Name : 'SGML single string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 26; Name : 'SGML error'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_ErrorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 27; Name : 'SGML special'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $FF6633; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 28; Name : 'SGML entity'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 29; Name : 'SGML comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 31; Name : 'SGML block'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : $660000; BackColor : $E0CCCC; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
    );
{$Endif}

{$Ifndef NO_SYN_WMLKEYWORDS}
  WMLKeywords : Array [0..1] of TSciLangKeywords =
     (
		 (Name : 'Elements and Attributes';
       KeywordListNumber : 0;
       Keywords :
			 {Elements}
					'a access anchor big br em i p reset small strong card catch b do exit fieldset go head img input link meta noop onevent optgroup '+
					'option postfield prev receive refresh select send setvar spawn table td template throw timer tr u wml xml '+
				{Attributes}
					'href domain path title accesskey align id title newcontext ordered onenterforward onenterbackward ontimer name onthrow label optional type sendreferer '+
									'method accept-charset http-equiv content src localsrc height width vspace hspace '+
									'emptyok maxlength tabindex format size rel onpick value multiple iname ivalue onexit '+
									'columns xml:lang version encoding ';
			 ),
      (Name : 'DTD Keywords';
       KeywordListNumber : 5;
       Keywords : 'ELEMENT DOCTYPE ATTLIST ENTITY NOTATION';
			)
			);
{$Endif}

{$Ifndef NO_SYN_APDLKEYWORDS}
  APDLKeywords : Array [0..5] of TSciLangKeywords =
     (
		 (Name : 'Processors';KeywordListNumber : 0;Keywords :''),
		 (Name : 'Commands';KeywordListNumber : 1;Keywords :''),
     (Name : 'SlashCommands';KeywordListNumber : 2;Keywords :''),
     (Name : 'StarCommands';KeywordListNumber : 3;Keywords :''),
     (Name : 'Arguments';KeywordListNumber : 4;Keywords :''),
     (Name : 'Functions';KeywordListNumber : 5;Keywords :'')
			);
{$Endif}

{$Ifndef NO_SYN_APDLSTYLES}
	APDLStyles : array [0..12] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'CommentBlock'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Word'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Processor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Command'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'SlashCommand'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'StarCommand'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Argument'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 12; Name : 'Function'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
    );
{$Endif}

{$Ifndef NO_SYN_VXMLKEYWORDS}
  VXMLKeywords : Array [0..1] of TSciLangKeywords =
     (
		 (Name : 'Elements and Attributes';
       KeywordListNumber : 0;
       Keywords :
			 {Elements}
								'assign audio block break catch choice clear disconnect else elseif '+
								'emphasis enumerate error exit field filled form goto grammar help '+
								'if initial link log menu meta noinput nomatch object option p paragraph '+
								'param phoneme prompt property prosody record reprompt return s say-as '+
								'script sentence subdialog submit throw transfer value var voice vxml'+
				{Attributes}
								'accept age alphabet anchor application base beep bridge category charset '+
								'classid cond connecttimeout content contour count dest destexpr dtmf dtmfterm '+
								'duration enctype event eventexpr expr expritem fetchtimeout finalsilence '+
								'gender http-equiv id level maxage maxstale maxtime message messageexpr '+
								'method mime modal mode name namelist next nextitem ph pitch range rate '+
								'scope size sizeexpr skiplist slot src srcexpr sub time timeexpr timeout '+
								'transferaudio type value variant version volume xml:lang';
			 ),
      (Name : 'DTD Keywords';
       KeywordListNumber : 5;
       Keywords : 'public !doctype';
			)
			);
{$Endif}

{$Ifndef NO_SYN_LUASTYLES}
	LuaStyles : array [0..18] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'White Space'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Block Comment (5.0)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Char'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Literal String'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'Preprocessor (obsolete in Lua 4.0 and up)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'EOL when string not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 13; Name : 'Other Keywords 1 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 14; Name : 'Other Keywords 2 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 15; Name : 'Other Keywords 3 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 16; Name : 'Other Keywords 4 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 17; Name : 'Other Keywords 5 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 18; Name : 'Other Keywords 6 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 19; Name : 'Other Keywords 7 (testcolors)'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
			);
{$Endif}

{$Ifndef NO_SYN_LUAKEYWORDS}
  LuaKeywords : Array [0..5] of TSciLangKeywords =
     (
		 (Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords : 'and break do else elseif end false for function if '+
									'in local nil not or repeat return then true until while'
			 ),
		 (Name : 'Basic Functions';
       KeywordListNumber : 1;
       Keywords : '_VERSION assert collectgarbage dofile error gcinfo loadfile loadstring '+
									'print tonumber tostring type unpack '+
									{LUA4}
									'_ALERT _ERRORMESSAGE _INPUT _PROMPT _OUTPUT '+
									'_STDERR _STDIN _STDOUT call dostring foreach foreachi getn globals newtype '+
									'rawget rawset require sort tinsert tremove '+
									{LUA5}
									'_G getfenv getmetatable ipairs loadlib next pairs pcall '+
									'rawegal rawget rawset require setfenv setmetatable xpcall '+
									'string table math coroutine io os debug'
			 ),

		 (Name : 'String, (table) & math functions';
       KeywordListNumber : 2;
       Keywords :
			 {LUA4}
			  'abs acos asin atan atan2 ceil cos deg exp '+
				'floor format frexp gsub ldexp log log10 max min mod rad random randomseed '+
				'sin sqrt strbyte strchar strfind strlen strlower strrep strsub strupper tan '+
				{LUA5}
				'string.byte string.char string.dump string.find string.len '+
				'string.lower string.rep string.sub string.upper string.format string.gfind string.gsub '+
				'table.concat table.foreach table.foreachi table.getn table.sort table.insert table.remove table.setn '+
				'math.abs math.acos math.asin math.atan math.atan2 math.ceil math.cos math.deg math.exp '+
				'math.floor math.frexp math.ldexp math.log math.log10 math.max math.min math.mod '+
				'math.pi math.rad math.random math.randomseed math.sin math.sqrt math.tan'
			 ),

		 (Name : '(coroutines), I/O & system facilities';
       KeywordListNumber : 3;
       Keywords : 'openfile closefile readfrom writeto appendto '+
									'remove rename flush seek tmpfile tmpname read write '+
									'clock date difftime execute exit getenv setlocale time '+

									'coroutine.create coroutine.resume coroutine.status '+
									'coroutine.wrap coroutine.yield '+
									'io.close io.flush io.input io.lines io.open io.output io.read io.tmpfile io.type io.write '+
									'io.stdin io.stdout io.stderr '+
									'os.clock os.date os.difftime os.execute os.exit os.getenv os.remove os.rename '+
									'os.setlocale os.time os.tmpname'
			),
		 (Name : '???';
       KeywordListNumber : 4;
       Keywords : ''
			 ),
		 (Name : '??? 2';
       KeywordListNumber : 5;
       Keywords : ''
			 )

				);
{$Endif}

{$Ifndef NO_SYN_LISPKEYWORDS}
  LispKeywords : Array [0..0] of TSciLangKeywords =
     (
			 (Name : 'Keywords';
				 KeywordListNumber : 0;
				 Keywords : 'not defun + - * / = < > <= >= princ '+
										'eval apply funcall quote identity function complement backquote lambda set setq setf '+
										'defun defmacro gensym make symbol intern symbol name symbol value symbol plist get '+
										'getf putprop remprop hash make array aref car cdr caar cadr cdar cddr caaar caadr cadar '+
										'caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr '+
										'cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr cons list append reverse last nth '+
										'nthcdr member assoc subst sublis nsubst  nsublis remove length list length '+
										'mapc mapcar mapl maplist mapcan mapcon rplaca rplacd nconc delete atom symbolp numberp '+
										'boundp null listp consp minusp zerop plusp evenp oddp eq eql equal cond case and or let l if prog '+
										'prog1 prog2 progn go return do dolist dotimes catch throw error cerror break '+
										'continue errset baktrace evalhook truncate float rem min max abs sin cos tan expt exp sqrt '+
										'random logand logior logxor lognot bignums logeqv lognand lognor '+
										'logorc2 logtest logbitp logcount integer length nil'
			 )
		 );
{$Endif}

{$Ifndef NO_SYN_SCHEMEKEYWORDS}
  SchemeKeywords : Array [0..0] of TSciLangKeywords =
     (
			 (Name : 'Keywords';
				 KeywordListNumber : 0;
				 Keywords : '+ - * / = < > <= >= => '+
										'abs acos and angle append apply asin assoc assoc assq assv atan '+
										'begin boolean? '+
										'caar cadr call-with-current-continuation call/cc '+
										'call-with-input-file call-with-output-file call-with-values '+
										'car cdr '+
										'caar cadr cdar cddr '+
										'caaar caadr cadar caddr cdaar cdadr cddar cdddr '+
										'caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr '+
										'cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr '+
										'case ceiling char->integer '+
										'char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? '+
										'char-downcase char-lower-case? char-numeric? char-ready? '+
										'char-upcase char-upper-case? char-whitespace? '+
										'char<=? char<? char=? char>=? char>? char? '+
										'close-input-port close-output-port complex? cond cons cos '+
										'current-input-port current-output-port '+
										'define define-syntax delay denominator display do dynamic-wind '+
										'else eof-object? eq? equal? eqv? eval even? exact->inexact exact? '+
										'exp expt '+
										'floor for-each force '+
										'gcd '+
										'if imag-part inexact->exact inexact? input-port? integer->char integer? interaction-environment '+
										'lambda lcm length let let* let-syntax letrec letrec-syntax '+
										'list list->string list->vector list-ref list-tail list? load log '+
										'magnitude make-polar make-rectangular make-string make-vector '+
										'map max member memq memv min modulo '+
										'negative? newline not null-environment null? number->string number? numerator '+
										'odd? open-input-file open-output-file or output-port? '+
										'pair? peek-char input-port? output-port? positive? procedure? '+
										'quasiquote quote quotient '+
										'rational? rationalize read read-char real-part real? remainder reverse round '+
										'scheme-report-environment set! set-car! set-cdr! sin sqrt string '+
										'string->list string->number string->symbol string-append '+
										'string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? '+
										'string-copy string-fill! string-length string-ref string-set! '+
										'string<=? string<? string=? string>=? string>? string? '+
										'substring symbol->string symbol? syntax-rules '+
										'transcript-off transcript-on truncate '+
										'unquote unquote-splicing '+
										'values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? '+
										'with-input-from-file with-output-to-file write write-char '+
										'zero?'
			 )
		 );
{$Endif}
{$Ifndef NO_SYN_LISPSTYLES}
	LispStyles : array [0..7] of TSciLangStyle =
	(
		(StyleNumber : 0; Name : 'White Space'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'End of line where string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
		(StyleNumber : 9; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
			);
{$Endif}


{$Ifndef NO_SYN_ASMSTYLES}
//asm
ASMStyles : array [0..14] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'CPU Instruction'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'Math Instruction'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'Register'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 9; Name : 'Directive'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 10; Name : 'Directive Operand'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 11; Name : 'Comment Block'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 12; Name : 'Character'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 13; Name : 'String not terminated'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 14; Name : 'Extended Instruction'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}

{$Ifndef NO_SYN_ASMKEYWORDS}
  ASMKeywords : Array [0..5] of TSciLangKeywords =
     (
     (Name : 'CPU instructions';KeywordListNumber : 0;Keywords:  ''),
     (Name : 'FPU instructions';KeywordListNumber : 1;Keywords:  ''),
     (Name : 'Registers';KeywordListNumber : 2;Keywords:  ''),
     (Name : 'Directives';KeywordListNumber : 3;Keywords:  ''),
     (Name : 'Directive operands';KeywordListNumber : 4;Keywords:  ''),
     (Name : 'Extended instructions';KeywordListNumber : 5;Keywords:  '')
  );
{$Endif}

{$Ifndef NO_SYN_KIXKEYWORDS}
  KIXKeywords : Array [0..2] of TSciLangKeywords =
     (
     (Name : 'Commands';KeywordListNumber : 0;
     Keywords:'? and beep big break call cd cls color cookie1 copy '+
              'debug del dim display do until exit flushkb for each next function endfunction '+
              'get gets global go gosub goto if else endif md or password play quit '+
              'rd redim return run select case endselect set setl setm settime '+
              'shell sleep small use while loop'),
     (Name : 'Functions';KeywordListNumber : 1;
     Keywords:  'abs addkey addprinterconnection addprogramgroup '+
'addprogramitem asc ascan at backupeventlog box cdbl chr cint cleareventlog '+
'close comparefiletimes createobject cstr dectohex delkey delprinterconnection '+
'delprogramgroup delprogramitem deltree delvalue dir enumgroup enumipinfo enumkey '+
'enumlocalgroup enumvalue execute exist existkey expandenvironmentvars fix '+
'formatnumber freefilehandle getdiskspace getfileattr getfilesize getfiletime '+
'getfileversion getobject iif ingroup instr instrrev int isdeclared join '+
'kbhit keyexist lcase left len loadhive loadkey logevent logoff ltrim '+
'memorysize messagebox open readline readprofilestring readtype readvalue '+
'redirectoutput right rnd round rtrim savekey sendkeys sendmessage setascii '+
'setconsole setdefaultprinter setfileattr setfocus setoption setsystemstate '+
'settitle setwallpaper showprogramgroup shutdown sidtoname split srnd substr '+
'trim ubound ucase unloadhive val vartype vartypename writeline '+
'writeprofilestring writevalue'),
     (Name : 'Macro';KeywordListNumber : 2;
     Keywords:  'address build color comment cpu crlf csd curdir '+
'date day domain dos error fullname homedir homedrive homeshr hostname '+
'inwin ipaddress0 ipaddress1 ipaddress2 ipaddress3 kix lanroot ldomain '+
'ldrive lm logonmode longhomedir lserver maxpwage mdayno mhz monthno '+
'month msecs pid primarygroup priv productsuite producttype pwage ras '+
'result rserver scriptdir scriptexe scriptname serror sid site startdir '+
'syslang ticks time userid userlang wdayno wksta wuserid ydayno year')
  );
{$Endif}

{$Ifndef NO_SYN_KIXSTYLES}
//au3
KIXStyles : array [0..9] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'WhiteSpace'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'String 2'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Variables'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'Macro'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'Function'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 9; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);

{$Endif}

{$Ifndef NO_SYN_AUTOITSTYLES}
//au3
AutoItStyles : array [0..13] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'WhiteSpace'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Comment Block'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 4; Name : 'Function'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : $f000f0; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 5; Name : 'Keywords'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'Macro'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
			ForeColor : $ff33ff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 9; Name : 'Variable'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $ffff00; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 10; Name : 'Sent keys in string'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : $0088ff; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 11; Name : 'PreProcessor'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 12; Name : 'Special'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 13; Name : 'Expand'; FontName : ''; FontSize : 0; FontStyles : []; //1.63
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)

);
{$Endif}
//au3
{$Ifndef NO_SYN_AUTOITKEYWORDS}
  AutoItKeywords : Array [0..5] of TSciLangKeywords =
     ((Name : 'Keywords';
       KeywordListNumber : 0;
       Keywords:  'and byref case continueloop dim do else elseif endfunc endif endselect exit exitloop for func '+
                  'global if local next not or return select step then to until wend while exit'
			 ),
			 (Name : 'Functions';
       KeywordListNumber : 1;
       Keywords:  'abs acos adlibdisable adlibenable asc asin atan autoitsetoption autoitwingettitle autoitwinsettitle '+
                  'bitand bitnot bitor bitshift bitxor blockinput break call cdtray chr clipget clipput controlclick '+
                  'controlcommand controldisable controlenable controlfocus controlgetfocus controlgetpos controlgettext '+
                  'controlhide controlmove controlsend controlsettext controlshow cos dec dircopy dircreate dirmove '+
                  'dirremove drivegetdrive drivegetfilesystem drivegetlabel drivegetserial drivegettype drivesetlabel '+
                  'drivespacefree drivespacetotal drivestatus envget envset envupdate eval exp filechangedir fileclose '+
                  'filecopy filecreateshortcut filedelete fileexists filefindfirstfile filefindnextfile filegetattrib '+
                  'filegetlongname filegetshortname filegetsize filegettime filegetversion fileinstall filemove '+
                  'fileopen fileopendialog fileread filereadline filerecycle filerecycleempty filesavedialog fileselectfolder '+
                  'filesetattrib filesettime filewrite filewriteline guicreate guicreateex guidefaultfont guidelete '+
                  'guigetcontrolstate guihide guimsg guiread guirecvmsg guisendmsg guisetcontrol guisetcontroldata '+
                  'guisetcontrolex guisetcontrolfont guisetcontrolnotify guisetcoord guisetcursor guishow guiwaitclose '+
                  'guiwrite hex hotkeyset inidelete iniread iniwrite inputbox int isadmin isarray isdeclared isfloat '+
                  'isint isnumber isstring log memgetstats mod mouseclick mouseclickdrag mousedown mousegetcursor '+
                  'mousegetpos mousemove mouseup mousewheel msgbox number pixelchecksum pixelgetcolor pixelsearch '+
                  'processclose processexists processsetpriority processwait processwaitclose progressoff progresson '+
                  'progressset random regdelete regenumkey regenumval regread regwrite round run runasset runwait '+
                  'send seterror shutdown sin sleep soundplay soundsetwavevolume splashimageon splashoff splashtexton '+
                  'sqrt statusbargettext string stringaddcr stringformat stringinstr stringisalnum stringisalpha '+
                  'stringisascii stringisdigit stringisfloat stringisint stringislower stringisspace stringisupper '+
                  'stringisxdigit stringleft stringlen stringlower stringmid stringreplace stringright stringsplit '+
                  'stringstripcr stringstripws stringtrimleft stringtrimright stringupper tan timerstart timerstop '+
                  'tooltip traytip ubound urldownloadtofile winactivate winactive winclose winexists wingetcaretpos '+
                  'wingetclasslist wingetclientsize wingethandle wingetpos wingetstate wingettext wingettitle '+
                  'winkill winmenuselectitem winminimizeall winminimizeallundo winmove winsetontop winsetstate '+
                  'winsettitle winwait winwaitactive winwaitclose winwaitnotactive'
			 ),
			 (Name : 'Macros';
       KeywordListNumber : 2;
       Keywords:  '@appdatacommondir @appdatadir @autoitversion @commonfilesdir @compiled @computername @comspec '+
                  '@cr @crlf @desktopcommondir @desktopdir @desktopheight @desktopwidth @documentscommondir @error '+
                  '@favoritescommondir @favoritesdir @homedrive @homepath @homeshare @hour @ipaddress1 @ipaddress2 '+
                  '@ipaddress3 @ipaddress4 @lf @logondnsdomain @logondomain @logonserver @mday @min @mon @mydocumentsdir '+
                  '@osbuild @oslang @osservicepack @ostype @osversion @programfilesdir @programscommondir @programsdir '+
                  '@scriptdir @scriptfullpath @scriptname @sec @startmenucommondir @startmenudir @startupcommondir '+
                  '@startupdir @sw_hide @sw_maximize @sw_minimize @sw_restore @sw_show @systemdir @tab @tempdir '+
                  '@userprofiledir @username @wday @windowsdir @workingdir @yday @year'
			 ),
			 (Name : 'Sent Keys';
       KeywordListNumber : 3;
       Keywords:  '{!} {#} {^} {{} {}} {+} {alt} {altdown} {altup} {appskey} {asc} {backspace} {browser_back} '+
                  '{browser_favorites} {browser_forward} {browser_home} {browser_refresh} {browser_search} {browser_stop} {bs} {capslock} {ctrlbreak} '+
                  '{ctrldown} {ctrlup} {del} {delete} {down} {end} {enter} {esc} {escape} {f1} {f10} {f11} {f12} {f2} {f3} {f4} {f5} {f6} {f7} {f8} {f9} '+
                  '{home} {ins} {insert} {lalt} {launch_app1} {launch_app2} {launch_mail} {launch_media} {lctrl} {left} {lshift} {lwin} '+
                  '{lwindown} {media_next} {media_play_pause} {media_prev} {media_stop} {numlock} '+
                  '{numpad0} {numpad1} {numpad2} {numpad3} {numpad4} {numpad5} {numpad6} {numpad7} {numpad8} {numpad9} '+
                  '{numpadadd} {numpaddiv} {numpaddot} {numpadenter} {numpadmult} {numpadsub} {pause} {pgdn} {pgup} '+
                  '{printscreen} {ralt} {rctrl} {right} {rshift} {rwin} {rwindown} {scrolllock} {shiftdown} {shiftup} {sleep} {space} {tab} {up} '+
                  '{volume_down} {volume_mute} {volume_up}'
			 ),
 			 (Name : 'Preprocessor';
       KeywordListNumber : 4;
       Keywords:  '#include #include-once'
			 ),
 			 (Name : 'Special';
       KeywordListNumber : 5;
       Keywords:  '#region #endregion'
			 )


			);
{$Endif}

{$Ifndef NO_SYN_BULLANTKEYWORDS}
  BullantKeywords : Array [0..0] of TSciLangKeywords =
     (
     (Name : 'Keywords';KeywordListNumber : 0;Keywords:  '')
     );
{$Endif}

{$Ifndef NO_SYN_BULLANTSTYLES}
  BullantStyles : Array [0..9] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'White space'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Double quoted string'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Character'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 10; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 11; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 12; Name : 'EOL if string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
     );
{$Endif}

{$Ifndef NO_SYN_AVENUEKEYWORDS}
  AveKeywords : Array [0..5] of TSciLangKeywords =
     (
     (Name : 'Keywords 1';KeywordListNumber : 0;Keywords:  ''),
     (Name : 'Keywords 2';KeywordListNumber : 1;Keywords:  ''),
     (Name : 'Keywords 3';KeywordListNumber : 2;Keywords:  ''),
     (Name : 'Keywords 4';KeywordListNumber : 4;Keywords:  ''),
     (Name : 'Keywords 5';KeywordListNumber : 5;Keywords:  ''),
     (Name : 'Keywords 6';KeywordListNumber : 6;Keywords:  '')
     );
{$Endif}

{$Ifndef NO_SYN_AVESTYLES}
AveStyles : array [0..14] of TSciLangStyle =
	(
(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 3; Name : 'KeyWord'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 6; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 7; Name : 'Enum'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 8; Name : 'String not closed'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 9; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 10; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 11; Name : 'Keyword 1'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 12; Name : 'Keyword 2'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 13; Name : 'Keyword 3'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 14; Name : 'Keyword 4'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 15; Name : 'Keyword 5'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
(StyleNumber : 16; Name : 'Keyword 6'; FontName : ''; FontSize : 0; FontStyles : [];
			ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
			Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);


{$Endif}
{$Ifndef NO_SYN_BAANKEYWORDS}
  BaanKeywords : Array [0..1] of TSciLangKeywords =
     (
     (Name : 'Keywords 1';KeywordListNumber : 0;Keywords:  ''),
     (Name : 'Keywords 2';KeywordListNumber : 1;Keywords:  '')
     );
{$Endif}

{$Ifndef NO_SYN_BAANSTYLES}
  BaanStyles : Array [0..10] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 2; Name : 'DLL Usage'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 3; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 4; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 5; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 6; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 7; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 9; Name : 'EOL if string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1),
     (StyleNumber : 10; Name : 'Keyword 2'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeyWordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
     );
{$Endif}

{$Ifndef NO_SYN_EIFFELKEYWORDS}
  EiffelKeywords : Array [0..0] of TSciLangKeywords =
     (
     (Name : 'Keywords';KeywordListNumber : 0;
      Keywords: 'alias all and any as bit boolean '+
                'check class character clone create creation current '+
                'debug deferred div do double '+
                'else elseif end ensure equal expanded export external '+
                'false feature forget from frozen general '+
                'if implies indexing infix inherit inspect integer invariant is '+
                'language like local loop mod name nochange none not '+
                'obsolete old once or platform pointer prefix precursor '+
                'real redefine rename require rescue result retry '+
                'select separate string strip then true undefine unique until '+
                'variant void when xor')
     );
{$Endif}

{$Ifndef NO_SYN_EIFFELSTYLES}
  EiffelStyles : Array [0..8] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Keyword'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Character'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
     (StyleNumber : 8; Name : 'EOL if string is not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : True;CharSet: -1)
);
{$Endif}

{$Ifndef NO_SYN_ESCRIPTKEYWORDS}
  EScriptKeywords : Array [0..2] of TSciLangKeywords =
     (
     (Name : 'Primary keywords and identifiers';KeywordListNumber : 0;
      Keywords: 'basic basicio boats '+
'cfgfile file http npc os uo util accessible addmenuitem appendconfigfileelem '+
'applyconstraint applydamage applyrawdamage assignrecttoweatherregion '+
'append baseskilltorawskill boatfromitem broadcast ban cdbl cint cstr '+
'checklineofsight checklosat checkskill consumemana consumereagents consumesubstance '+
'createaccount createitematlocation createiteminbackpack createitemininventory createitemincontainer '+
'createmenu createmultiatlocation createnpcfromtemplate createrootiteminstoragearea createstoragearea '+
'clear_script_profile_counters close damage destroyitem destroymulti destroyrootiteminstoragearea '+
'detach disableevents disconnectclient distance disable enableevents enumerateitemsincontainer '+
'enumerateonlinecharacters equipfromtemplate equipitem eraseglobalproperty eraseobjproperty '+
'enable enabled erase events_waiting exists findconfigelem findobjtypeincontainer '+
'findrootiteminstoragearea findstoragearea fclose find fopen fread fseek ftell fwrite '+
'gamestat getamount getcommandhelp getconfigint getconfigintkeys getconfigmaxintkey getconfigreal '+
'getconfigstring '+
'getconfigstringkeys '+
'getconfigstringarray '+
'getelemproperty '+
'getequipmentbylayer '+
'getglobalproperty '+
'getharvestdifficulty '+
'getmapinfo '+
'getmenuobjtypes '+
'getobjproperty '+
'getobjtype '+
'getobjtypebyname '+
'getproperty '+
'getrawskill '+
'getregionstring '+
'getskill '+
'getspelldifficulty '+
'getstandingheight '+
'getworldheight '+
'grantprivilege '+
'harvestresource '+
'healdamage '+
'hex '+
'islegalmove '+
'insert '+
'keys '+
'listequippeditems '+
'listghostsnearlocation '+
'listhostiles '+
'listitemsatlocation '+
'listitemsnearlocation '+
'listitemsnearlocationoftype '+
'listmobilesinlineofsight '+
'listmobilesnearlocation '+
'listmobilesnearlocationex '+
'listobjectsinbox '+
'loadtusscpfile '+
'left '+
'len '+
'log_profile '+
'lower '+
'makeboundingbox '+
'move '+
'moveboat '+
'moveboatrelative '+
'movecharactertolocation '+
'moveitemtocontainer '+
'moveitemtolocation '+
'move_offline_mobiles '+
'openpaperdoll '+
'open '+
'pack '+
'performaction '+
'playlightningbolteffect '+
'playmovingeffect '+
'playmovingeffectxyz '+
'playobjectcenteredeffect '+
'playsoundeffect '+
'playsoundeffectprivate '+
'playstationaryeffect '+
'printtextabove '+
'printtextaboveprivate '+
'packages '+
'polcore '+
'position '+
'print '+
'queryparam '+
'randomdiceroll '+
'randomint '+
'rawskilltobaseskill '+
'readconfigfile '+
'readgameclock '+
'releaseitem '+
'registerforspeechevents '+
'registeritemwithboat '+
'requestinput '+
'reserveitem '+
'restartscript '+
'resurrect '+
'revokeprivilege '+
'runawayfrom '+
'runawayfromlocation '+
'runtoward '+
'runtowardlocation '+
'reverse '+
'run_script_to_completion '+
'saveworldstate '+
'selectmenuitem2 '+
'self '+
'sendbuywindow '+
'senddialoggump '+
'sendevent '+
'sendopenspecialcontainer '+
'sendpacket '+
'sendsellwindow '+
'sendskillwindow '+
'sendstringastipwindow '+
'sendsysmessage '+
'sendtextentrygump '+
'setanchor '+
'setglobalproperty '+
'setname '+
'setobjproperty '+
'setopponent '+
'setproperty '+
'setrawskill '+
'setregionlightlevel '+
'setregionweatherlevel '+
'setscriptcontroller '+
'setwarmode '+
'shutdown '+
'speakpowerwords '+
'splitwords '+
'startspelleffect '+
'subtractamount '+
'systemfindboatbyserial '+
'systemfindobjectbyserial '+
'say '+
'set_critical '+
'set_debug '+
'set_priority '+
'set_priority_divide '+
'set_script_option '+
'setcmdlevel '+
'setdex '+
'setint '+
'setlightlevel '+
'setmaster '+
'setname '+
'setpassword '+
'setstr '+
'shrink '+
'size '+
'sleep '+
'sleepms '+
'sort '+
'spendgold '+
'squelch '+
'start_script '+
'syslog '+
'system_rpm '+
'target '+
'targetcoordinates '+
'targetmultiplacement '+
'turnawayfrom '+
'turnawayfromlocation '+
'turnboat '+
'turntoward '+
'turntowardlocation '+
'toggle '+
'unloadconfigfile '+
'unpack '+
'unban '+
'unload_scripts '+
'upper '+
'walkawayfrom '+
'walkawayfromlocation '+
'walktoward '+
'walktowardlocation '+
'wander '+
'writehtml '+
'writehtmlraw '+
'wait_for_event '+
'movechar_forcelocation '+
'moveitem_forcelocation '+
'moveitem_normal '+
'scriptopt_debug '+
'scriptopt_no_interrupt '+
'scriptopt_no_runaway '+
'te_cancel_disable '+
'te_cancel_enable '+
'te_style_disable '+
'te_style_normal '+
'te_style_numerical '+
'tgtopt_check_los '+
'tgtopt_harmful '+
'tgtopt_helpful '+
'tgtopt_neutral '+
'tgtopt_nocheck_los '+
'setprop '+
'getprop'
      ),
     (Name : 'Intrinsic functions';KeywordListNumber : 1;
      Keywords: 'array const dictionary global local var and default in next not '+
                'or return to include use enum'),
     (Name : 'Extended and user defined functions';KeywordListNumber : 2;
      Keywords: 'while for endfor function program endprogram endfunction foreach case else elseif '+
                'if endcase endenum endforeach endif endwhile')
     );
{$Endif}
{$Ifndef NO_SYN_ESCRIPTSTYLES}
  EScriptStyles : Array [0..11] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Doc Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Primary keywords and identifiers'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Double quoted String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Operators'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Identifiers'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Braces'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Intrinsic functions'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Extended and user defined functions'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}


{$Ifndef NO_SYN_FORTHKEYWORDS}
  ForthKeywords : Array [0..5] of TSciLangKeywords =
     (
     (Name : 'Control keywords';KeywordListNumber : 0;Keywords: 'AGAIN BEGIN CASE DO ELSE ENDCASE ENDOF IF LOOP OF REPEAT THEN UNTIL  WHILE [IF] [ELSE] [THEN] ?DO'),
     (Name : 'Keywords';KeywordListNumber : 1;Keywords:
        'DUP DROP ROT SWAP OVER @ ! 2@ 2! 2DUP 2DROP 2SWAP 2OVER NIP R@ >R R> 2R@ 2>R 2R> '+
        '0= 0< SP@ SP! W@ W! C@ C! < > = <> 0<>'+
        'SPACE SPACES KEY? KEY THROW CATCH ABORT */ 2* /MOD CELL+ CELLS CHAR+ '+
        'CHARS MOVE ERASE DABS TITLE HEX DECIMAL HOLD <# # #S #> SIGN '+
        'D. . U. DUMP (.") >NUMBER '' IMMEDIATE EXIT RECURSE UNLOOP LEAVE HERE ALLOT , '+
        'C, W, COMPILE, BRANCH, RET, LIT, DLIT, ?BRANCH, ", >MARK >RESOLVE1 <MARK >RESOLVE '+
        'ALIGN ALIGNED USER-ALLOT USER-HERE HEADER DOES> SMUDGE HIDE :NONAME LAST-WORD '+
        '?ERROR ERROR2 FIND1 SFIND SET-CURRENT GET-CURRENT DEFINITIONS GET-ORDER FORTH '+
        'ONLY SET-ORDER ALSO PREVIOUS VOC-NAME. ORDER LATEST LITERAL 2LITERAL SLITERAL '+
        'CLITERAL ?LITERAL1 ?SLITERAL1 HEX-LITERAL HEX-SLITERAL ?LITERAL2 ?SLITERAL2 SOURCE '+
        'EndOfChunk CharAddr PeekChar IsDelimiter GetChar OnDelimiter SkipDelimiters OnNotDelimiter '+
        'SkipWord SkipUpTo ParseWord NextWord PARSE SKIP CONSOLE-HANDLES REFILL DEPTH ?STACK '+
        '?COMP WORD INTERPRET BYE QUIT MAIN1 EVALUATE INCLUDE-FILE INCLUDED >BODY +WORD '+
        'WORDLIST CLASS! CLASS@ PAR! PAR@ ID. ?IMMEDIATE ?VOC IMMEDIATE VOC WordByAddrWl '+
        'WordByAddr NLIST WORDS SAVE OPTIONS /notransl ANSI>OEM ACCEPT EMIT CR TYPE EKEY? '+
        'EKEY EKEY>CHAR EXTERNTASK ERASE-IMPORTS ModuleName ModuleDirName ENVIRONMENT? '+
        'DROP-EXC-HANDLER SET-EXC-HANDLER HALT ERR CLOSE-FILE CREATE-FILE CREATE-FILE-SHARED '+
        'OPEN-FILE-SHARED DELETE-FILE FILE-POSITION FILE-SIZE OPEN-FILE READ-FILE REPOSITION-FILE '+
        'DOS-LINES UNIX-LINES READ-LINE WRITE-FILE RESIZE-FILE WRITE-LINE ALLOCATE FREE RESIZE '+
        'START SUSPEND RESUME STOP PAUSE MIN MAX TRUE FALSE ASCIIZ> '+
        'R/O W/O ;CLASS ENDWITH OR AND /STRING SEARCH COMPARE EXPORT ;MODULE SPACE'
),
     (Name :  'Definition Words';KeywordListNumber : 2;Keywords: 'VARIABLE CREATE : VALUE CONSTANT VM: M: var dvar chars OBJ '+
              'CONSTR: DESTR: CLASS: OBJECT: POINTER '+
              'USER USER-CREATE USER-VALUE VECT '+
              'WNDPROC: VOCABULARY -- TASK: CEZ: MODULE:'),
     (Name : 'Prewords with one argument';KeywordListNumber : 3;Keywords: 'CHAR [CHAR] POSTPONE WITH [''] TO [COMPILE] CHAR ASCII \'''),
     (Name : 'Prewords with two arguments';KeywordListNumber : 4;Keywords: 'REQUIRE WINAPI:'),
     (Name : 'String definition keywords';KeywordListNumber : 5;Keywords: 'S" ABORT" Z" " ." C"')
     );
{$Endif}

{$Ifndef NO_SYN_FORTHSTYLES}
  ForthStyles : Array [0..11] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Comment ML'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'Control Keywords'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Keywords'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Definition Words'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Prewords with one argument'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Prewords with two arguments'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor2; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Locale'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}

{$Ifndef NO_SYN_FORTRANKEYWORDS}
  FortranKeywords : Array [0..2] of TSciLangKeywords =
     (
     (Name : 'Primary keywords and identifiers';KeywordListNumber : 0;
     Keywords: 'access action advance allocatable allocate '+
'apostrophe assign assignment associate asynchronous backspace '+
'bind blank blockdata call case character class close common '+
'complex contains continue cycle data deallocate decimal delim '+
'default dimension direct do dowhile double doubleprecision else '+
'elseif elsewhere encoding end endassociate endblockdata enddo '+
'endfile endforall endfunction endif endinterface endmodule endprogram '+
'endselect endsubroutine endtype endwhere entry eor equivalence '+
'err errmsg exist exit external file flush fmt forall form format '+
'formatted function go goto id if implicit in include inout '+
'integer inquire intent interface intrinsic iomsg iolength '+
'iostat kind len logical module name named namelist nextrec nml '+
'none nullify number only open opened operator optional out pad '+
'parameter pass pause pending pointer pos position precision '+
'print private program protected public quote read readwrite '+
'real rec recl recursive result return rewind save select '+
'selectcase selecttype sequential sign size stat status stop stream '+
'subroutine target then to type unformatted unit use value '+
'volatile wait where while write'),
     (Name : 'Intrinsic functions';KeywordListNumber : 1;
     Keywords: 'abs achar acos acosd adjustl adjustr '+
'aimag aimax0 aimin0 aint ajmax0 ajmin0 akmax0 akmin0 all allocated alog '+
'alog10 amax0 amax1 amin0 amin1 amod anint any asin asind associated '+
'atan atan2 atan2d atand bitest bitl bitlr bitrl bjtest bit_size bktest break '+
'btest cabs ccos cdabs cdcos cdexp cdlog cdsin cdsqrt ceiling cexp char '+
'clog cmplx conjg cos cosd cosh count cpu_time cshift csin csqrt dabs '+
'dacos dacosd dasin dasind datan datan2 datan2d datand date '+
'date_and_time dble dcmplx dconjg dcos dcosd dcosh dcotan ddim dexp '+
'dfloat dflotk dfloti dflotj digits dim dimag dint dlog dlog10 dmax1 dmin1 '+
'dmod dnint dot_product dprod dreal dsign dsin dsind dsinh dsqrt dtan dtand '+
'dtanh eoshift epsilon errsns exp exponent float floati floatj floatk floor fraction '+
'free huge iabs iachar iand ibclr ibits ibset ichar idate idim idint idnint ieor ifix '+
'iiabs iiand iibclr iibits iibset iidim iidint iidnnt iieor iifix iint iior iiqint iiqnnt iishft '+
'iishftc iisign ilen imax0 imax1 imin0 imin1 imod index inint inot int int1 int2 int4 '+
'int8 iqint iqnint ior ishft ishftc isign isnan izext jiand jibclr jibits jibset jidim jidint '+
'jidnnt jieor jifix jint jior jiqint jiqnnt jishft jishftc jisign jmax0 jmax1 jmin0 jmin1 '+
'jmod jnint jnot jzext kiabs kiand kibclr kibits kibset kidim kidint kidnnt kieor kifix '+
'kind kint kior kishft kishftc kisign kmax0 kmax1 kmin0 kmin1 kmod knint knot kzext '+
'lbound leadz len len_trim lenlge lge lgt lle llt log log10 logical lshift malloc matmul '+
'max max0 max1 maxexponent maxloc maxval merge min min0 min1 minexponent minloc '+
'minval mod modulo mvbits nearest nint not nworkers number_of_processors pack popcnt '+
'poppar precision present product radix random random_number random_seed range real '+
'repeat reshape rrspacing rshift scale scan secnds selected_int_kind '+
'selected_real_kind set_exponent shape sign sin sind sinh size sizeof sngl snglq spacing '+
'spread sqrt sum system_clock tan tand tanh tiny transfer transpose trim ubound unpack verify+'),
     (Name : 'Extended and user defined functions';KeywordListNumber : 2;
     Keywords: 'cdabs cdcos cdexp cdlog cdsin cdsqrt cotan cotand '+
'dcmplx dconjg dcotan dcotand decode dimag dll_export dll_import doublecomplex dreal '+
'dvchk encode find flen flush getarg getcharqq getcl getdat getenv gettim hfix ibchng '+
'identifier imag int1 int2 int4 intc intrup invalop iostat_msg isha ishc ishl jfix '+
'lacfar locking locnear map nargs nbreak ndperr ndpexc offset ovefl peekcharqq precfill '+
'prompt qabs qacos qacosd qasin qasind qatan qatand qatan2 qcmplx qconjg qcos qcosd '+
'qcosh qdim qexp qext qextd qfloat qimag qlog qlog10 qmax1 qmin1 qmod qreal qsign qsin '+
'qsind qsinh qsqrt qtan qtand qtanh ran rand randu rewrite segment setdat settim system '+
'timer undfl unlock union val virtual volatile zabs zcos zexp zlog zsin zsqrt')
     );
{$Endif}

{$Ifndef NO_SYN_FORTHSTYLES}
  FortranStyles : Array [0..14] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'String 1'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'String 2'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'String not closed'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_EOLNotClosedFore; BackColor : sci_EOLNotClosedBack; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Primary keywords and identifiers'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Intrinsic functions'; FontName : ''; FontSize : 0; FontStyles : [fsBold];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Extended and user defined functions'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_KeywordColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'Preprocessor'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_PreprocColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 12; Name : 'Operator 2'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 13; Name : 'Label'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 14; Name : 'Continuation'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);

{$Endif}

{$Ifndef NO_SYN_MSSQLKEYWORDS}
  MSSQLKeywords : Array [0..6] of TSciLangKeywords =
     (
     (Name : 'Statements';KeywordListNumber : 0;Keywords: ''),
     (Name : 'Data Types';KeywordListNumber : 1;Keywords: ''),
     (Name : 'System tables';KeywordListNumber : 2;Keywords: ''),
     (Name : 'Global variables';KeywordListNumber : 3;Keywords: ''),
     (Name : 'Functions';KeywordListNumber : 4;Keywords: ''),
     (Name : 'System Stored Procedures';KeywordListNumber : 5;Keywords: ''),
     (Name : 'Operators';KeywordListNumber :6;Keywords: '')
  );
{$Endif}
{$Ifndef NO_SYN_MSSQLSTYLES}
  MSSQLStyles : Array [0..16] of TSciLangStyle =
    (
		(StyleNumber : 0; Name : 'Default'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_WhitespaceColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 1; Name : 'Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 2; Name : 'Line Comment'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_CommentColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 3; Name : 'Number'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_NumberColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 4; Name : 'String'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_StringColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 5; Name : 'Operator'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_OperatorColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 6; Name : 'Identifier'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : sci_IdentifierColor; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 7; Name : 'Variable'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 8; Name : 'Column Name'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 9; Name : 'Statement'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 10; Name : 'Data Type'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 11; Name : 'System Table'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 12; Name : 'Global Variable'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 13; Name : 'Function'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 14; Name : 'Stored Procedure'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 15; Name : 'Default Pref Datatype'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1),
		(StyleNumber : 16; Name : 'Column Name 2'; FontName : ''; FontSize : 0; FontStyles : [];
      ForeColor : clDefault; BackColor : clDefault; CharCase : CASE_MIXED; Visible: True;
      Changeable : True; HotSpot : False;  EOLFilled : False;CharSet: -1)
);
{$Endif}

{$Ifndef NO_SYN_HASKELLSTYLES}
HaskellStyles : array[0..16] of TSciLangStyle=(
	(
		StyleNumber :0;Name: 'Default'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_WhitespaceColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :1;Name: 'Identifier'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_IdentifierColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :2;Name: 'Keyword'; FontName: ''; FontSize:15; FontStyles: [fsBold];
		ForeColor: sci_KeywordColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :3;Name: 'Number'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_NumberColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :4;Name: 'String'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_StringColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :5;Name: 'Character'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_StringColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :6;Name: 'Class'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :7;Name: 'Module'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :8;Name: 'Capital'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :9;Name: 'Data'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :10;Name: 'Import'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :11;Name: 'Operator'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_OperatorColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :12;Name: 'Instance'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :13;Name: 'Commentline'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_CommentColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :14;Name: 'Commentblock'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_CommentColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :15;Name: 'Commentblock 2'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_CommentColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :16;Name: 'Commentblock 3'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: sci_CommentColor; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	)
);

{$Endif}

{$Ifndef NO_SYN_CAMLSTYLES}
Objective_CamlStyles : array[0..13] of TSciLangStyle=(
	(
		StyleNumber :0;Name: 'White Space'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :1;Name: 'Identifiers'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :2;Name: 'Tag Names'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :3;Name: 'Keywords'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clOlive; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :4;Name: 'Keywords 2'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clOlive; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :5;Name: 'Linenum Directive'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :6;Name: 'Operators'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clDefault; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :7;Name: 'Numbers'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clRed; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :8;Name: 'Single Quoted String'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: $00FF00; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :9;Name: 'Double Quoted String'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: $00FF00; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :10;Name: 'Comment (Not Nested)'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clGray; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :11;Name: 'Comment (Singly Nested)'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clGray; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :12;Name: 'Comment (Doubly Nested)'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clGray; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	),
	(
		StyleNumber :13;Name: 'Comment (Triply Nested)'; FontName: ''; FontSize:15; FontStyles: [];
		ForeColor: clGray; BackColor: clDefault; CharCase: CASE_MIXED; Visible: True;
		Changeable:True; HotSpot:False; EOLFilled:False; CharSet:-1;
	)
);
{$Endif}
{$Ifndef NO_SYN_CAMLKEYWORDS}
Objective_CamlKeywords : array[0..1] of TSciLangKeywords=(
	(
		Name: 'Keywords';
		KeywordListNumber:0;
		Keywords: 'and as assert asr begin class constraint do '+
		'done downto else end exception external false for '+
		'fun function functor if in include inherit initializer '+
		'land lazy let lor lsl lsr lxor match '+
		'method mod module mutable new object of open '+
		'or private rec sig struct then to true '+
		'try type val virtual when while';
	),
	(
		Name: 'Optional Keywords';
		KeywordListNumber:1;
		Keywords: 'option Some None ignore';
	)
);
{$Endif}


