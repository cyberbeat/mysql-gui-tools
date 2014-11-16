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
 Author : hdalis (hdalis@users.sourceforge.net)
 Created: 30/09/2004, 04:24:38
     $Id: SciDocuments.pas,v 1.4 2004/11/13 04:29:50 hdalis Exp $
 Purpose: Tabbed scintilla component.
 Usage: Drop the TScintilla derived control and a TSciDocumentTabControl
        on a form, select the TScintillaBase derived control in the Editor property of TSciDocumentTabControl
        and viola.. Tabbed scintilla control.
     
 History 30/09/2004 Initial Release
         13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                    generate the help.
         11/11/2004 Added per tab hinting. Displays the full pathname of the file when ShowHint is True.
         14/11/2004 At last the TSciDoc saves foldingstates.
         26/11/2004 Added the property DefaultExt for setting the default file extension..
                    Added calls to the OnDetectHighlighter when we're attached to
                    a TScintilla component, and we're attaching ourself.
         05/01/2005 Changed some of the calling to FOnDetectHighlighter, tried to minimize number of calls.
                    Added the OnOpenedAndInited event, this is fired only when Open is called, and is called just
                    before the function exits when successful.. May be the ideal place to add updatingcode which
                    is dependent on the current highlighter, as it's not completely loaded until just at this time.
         24/01/2005 Fixed the Close function so that when the last document is closed, the OnChange is called correctly and,
                    in Remove the Modified is sat to False when we're closing the last document..
         26/01/2005 Found a somewhat serious bug. When the character '\' is the last in the file,
                    and there's no following line, then an exception was thrown 'cause a while statement
                    did the walkin one too many times. Fixed.
         01/02/2005 Somewhat fixed a little problem with the documents not storing properties/folding when changing
                    the page programmaticly via the Activate functions..
                    The TSciDoc class has been extended with a pair of new functions.
         05/02/2005 Quite messy code in this component really, but it does work however.. Some restructuring
                    sorely needed.. TODO: Restructure this component, and use TCollection,TCollectionItems
                    for storing the document settings..
         22/02/2005 A bug in TSciDoc.SetTabWidth caused the that the TabWidth wasn't set correctly.
                    It set the value stored in fIndentWidth instead.
         09/03/2005 Changed the way Remove removes the document. It seems more stable now..

}
{$Include SciCommonDef.Inc}
unit SciDocuments;
interface
Uses
	Windows, Classes, Controls,Contnrs,ComCtrls,Messages,SciLexer,SciLexerMemo;

type
	TSCEvent_onclosing = procedure(Sender : TObject; const TabIndex : Integer;var AllowClose : Boolean) of object;
  TSCEvent_detecthighlighter = procedure(Sender : TObject; const Extension : String;var Language : String) of object;
  TSCEvent_removeextradata = procedure(Sender : TObject) of object;

  TSciFoldStateRec=record
    Line : Integer;
    Expanded : Boolean;
  end;
  TSciDocumentTabControl=class;
	TSciDoc = class(TObject)
	private
		fEditor      : TScintillaBase;
    fTabCtrl     : TCustomTabControl;
		FTabName,
    fHighlighter,
    FFileName,
    FWordChars,
		FAutoStartChars,
		FAutoStopChars : string;
		documentid,
		FSelStart,
		FSelLength,
		FFirstLineInView,
    FCodePage  : LongInt;
		FIndex,FTabWidth,FIndentWidth : Integer;
		FUnicode,
		FReadOnly,
		FModified,
		FAutoIgnoreCase,
    FUseTabs  : Boolean;
    FIndentation : TIndentationOptions;
    FFoldStates : array of TSciFoldStateRec;
		FOnChanged   : TNotifyEvent;
    FExtraData : Pointer;
    FOnRemoveExtraData : TSCEvent_removeextradata;
		procedure   SetTabName(const Value : string);
		procedure   SetHighlighter(const Value : String);
		procedure   SetFileName(const Value : string);
		procedure   Changed;
		procedure   AddRef;
		procedure   Release;
		procedure   SetWordChars(const Value : String);
		procedure   AssignFromEditor;
		procedure   AssignToEditor;
    procedure   StoreFoldStates;
    procedure   RestoreFoldStates;
    procedure   SetIndentWidth(const Value : Integer);
    procedure   SetTabWidth(const Value : Integer);
    procedure   SetIndentation(const Value : TIndentationOptions);

		property    OnChanged : TNotifyEvent read FOnChanged write FOnChanged; // Used by TSciDocumentTabControl, internal
    procedure   SetModified(const Value : Boolean);
    procedure   SetUseTabs(const Value : Boolean);
    procedure   SetCodePage(const Value : LongInt);
	public
		constructor Create(pp : TScintillaBase;ttabctrl : TSciDocumentTabControl;const getcurrent : Boolean=false);
		destructor  Destroy;override;
		procedure   Activate;
    function    IsUntitled  : Boolean;
		function 	  IsActive    : Boolean;
    procedure   Close;
		property    TabName     : string read FTabName Write SetTabName;
    property    ExtraData   : Pointer read FExtraData write FExtraData;
    property    OnRemoveExtraData : TSCEvent_removeextradata read FOnRemoveExtraData write FOnRemoveExtraData;
  published
		//Get or set indentation options: - KeepIndent, - TabIndents, - BackSpaceUnIndents, - IndentationGuides
		property Indentation : TIndentationOptions read FIndentation write SetIndentation;
		//Number of characters for code (un)indending.	If set to zero then the TabWidth is used instead
		property IndentWidth : Integer read FIndentWidth write SetIndentWidth;
    property TabWidth : Integer read FTabWidth write SetTabWidth;
		function SPerform(Msg : LongInt; wParam : LongInt=0; lParam : LongInt=0) : LongInt;

		// If the current editor is a TScintilla derived class then this property are automatically saved and used
		property    Highlighter : String read fHighlighter Write SetHighlighter;
		property    FileName    : String read FFileName write SetFileName;
		property    Index       : Integer read FIndex write FIndex;
    property    Modified    : Boolean read FModified write SetModified;
		property    WordChars   : String read FWordChars write SetWordChars;
		property    AutoIgnoreCase : Boolean read FAutoIgnoreCase write FAutoIgnoreCase;
		property    AutoStartChars : String read FAutoStartChars write FAutoStartChars;
		property    AutoStopChars  : String read FAutoStopChars write FAutoStopChars;
    property    UseTabs : Boolean read FUseTabs write SetUseTabs;
    property    CodePage : LongInt read FCodePage write SetCodePage;
	end;

  TSCEvent_openedandinited = procedure(Sender : TObject; const TabIndex : Integer;doc : TSciDoc) of object;

	TSciDocumentTabControl = class(TCustomTabControl)
		private
			docs                 : TObjectList;
			fEditor              : TScintillaBase;
			FOnClosing           : TSCEvent_onclosing;
			FOnDetectHighlighter : TSCEvent_detecthighlighter;
      FOnOpenedAndInited   : TSCEvent_openedandinited;
      FDefaultExt          : String;
      inactivate           : Boolean;
      FDefaultTabName     : String;
      procedure   EvttabChanged(Sender : TObject);
			function    getCount : Integer;
			procedure   ChgTab(const newtab : Integer);
			function    GetDocItm(const Index : Integer) : TSciDoc;

			procedure   SetEditor(Value : TScintillaBase);
      function    GetActiveDocument : TSciDoc;
			procedure   Remove(const index : Integer);           // Remove the tab 'index'
      procedure   SetDefaultExt(const Value : String);
		protected
			function    CanChange : Boolean; override;
			procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure   Attach;                                  // Attaches the editor to this tabcontrol
			procedure   RefreshTabs;                             // Refreshes the tablist
      procedure   CMHintShow(var Message: TMessage);message CM_HINTSHOW;
			procedure   Change; override;

		public
			constructor Create(AOwner : TComponent);override;
			destructor  Destroy;override;
			function    Open(const filename : String) : Boolean; // Open a file
			procedure		Close(const index : Integer=-1;const askifneeded : Boolean=True);         // If 'index' is -1 then close the currently selected Tab, else close the selected index.
			function    NewDocument : Integer;                   // Init a new document, return tabindex
			procedure   Activate(const docno : LongInt);         // Activate the tab 'docno'
			function    Add(const tabname : String;const thefilename : String) : Integer;   // Add a tab, set the tabname to 'tabname'

			property    Count : Integer read getCount;           // Returns number of tabs/documents
			property    Document [const Index : Integer] : TSciDoc read GetDocItm; // Returns the document item for the specified tabindex
			property 	  DisplayRect;
			property    Tabs;
			property    TabIndex;  // must be after Tabs
      property    ActiveDocument : TSciDoc read GetActiveDocument;
		published
			// The current editor used.
			property    Editor : TScintillaBase read fEditor write SetEditor;
			// Fired when a Tab is about to be removed etc. Good place for confirmation dialogs etc.
			property    OnClosing : TSCEvent_onclosing read FOnClosing write FOnClosing;
			// This event is only active when a descentant of TScintilla is used
			property    OnDetectHighlighter : TSCEvent_detecthighlighter read FOnDetectHighlighter write FOnDetectHighlighter;
      property    OnOpenedAndInited : TSCEvent_openedandinited read FOnOpenedAndInited write FOnOpenedAndInited;
			property    Align;
			property    Anchors;
			property    BiDiMode;
			property    Constraints;
			property    DragCursor;
			property    DragMode;
			property    Enabled;
			property    Font;
			property    HotTrack default True;
			property    MultiLine;
			property    OwnerDraw;
			property    ParentBiDiMode;
			property    ParentShowHint default False;
			property    PopupMenu;
			property    RaggedRight;
			property    ScrollOpposite;
			property    ShowHint default True;
			property    Style;
			property    TabHeight;
			property    TabOrder;
			property    TabPosition;
			property    TabWidth;
			property    Visible;
			property    OnChange;
			property    OnChanging;
			property    OnContextPopup;
			property    OnDrawTab;
			property    OnEndDock;
			property    OnEndDrag;
			property    OnEnter;
			property    OnExit;
			property    OnMouseDown;
			property    OnMouseMove;
			property    OnMouseUp;
			property    OnResize;
			property    OnStartDock;
			property    OnUnDock;
      property    ParentFont default False;
      property    TabStop default False;
      property    DefaultExt : String read FDefaultExt write SetDefaultExt;
	end;

implementation
uses SysUtils,SciLexerMod,Graphics,Shellapi,Forms,sciSupport,SciResLang;//,RTLConsts;
const
cDefaultLexer='null';
var
  ci : CHARSETINFO  = ( ciCharset: 0; ciACP:0; fs : ( fsUsb:( 0, 0, 0, 0 ); fsCsb: ( 0, 0 ); ) );

function CodePageFromCharSet(CharSet : DWORD;documentCodePage : UINT) : LongWord;
var
  bci : Boolean;
  cp : UINT;
  cpi : CPINFO;
begin
  bci:=TranslateCharsetInfo(charSet,ci, TCI_SRCCHARSET);
  if bci then
    cp:=ci.ciACP
  else
    cp:=documentCodePage;
  if ((not IsValidCodePage(cp)) and (not GetCPInfo(cp,cpi))) then
    cp:=CP_ACP;
  Result:=cp;
end;

function GetFileExt(const fname : String) : String;
begin
  Result :=LowerCase(ExtractFileExt(fname));
  //if (Result='') or (Result='.') then
  //  Result:=LowerCase(ExtractFileName(fname));
end;


procedure   TSciDocumentTabControl.Attach;
var
	sci : TSciDoc;
  highl,theext : String;

begin
	if assigned(docs) then
	begin
    try
      Tabs.BeginUpdate;
      if (assigned(Tabs)) and (Tabs.Count>0) then
        Tabs.Clear;
      if (docs.Count>0) then docs.Clear;
      if assigned(fEditor) then
      begin
        sci:=TSciDoc.Create(fEditor,Self,true);
        sci.Index:=0;
        sci.TabName:=FDefaultTabName;
        sci.OnChanged:=EvttabChanged;
        if fEditor is TScintilla then
        begin
          highl:=cDefaultLexer;
          theext :=LowerCase(ExtractFileExt(FDefaultExt));
          if assigned(FOnDetectHighlighter) then
          begin
            FOnDetectHighlighter(self,theext,highl);
          end;
          sci.Highlighter:=highl;

        end;
        docs.Add(sci);
        Tabs.Add(sci.TabName);
        if assigned(FOnOpenedAndInited) then FOnOpenedAndInited(Self,sci.Index,sci);
      end;
    finally
      Tabs.EndUpdate;
    end;
	end;
end;

procedure   TSciDocumentTabControl.SetEditor(Value : TScintillaBase);
var
	tp: TPoint;
	r : TRect;
begin
	if (Value<>fEditor) then
	begin
		if assigned(Value) then
		begin
			fEditor:=Value;
			if (not (csWriting in ComponentState)) and (not(csDestroying in ComponentState)) then
			begin
				fEditor.Parent:=self;
				fEditor.Visible:=true;
				fEditor.Align:=alClient;
				docs:=TObjectList.Create(true);
				Attach;
				RefreshTabs;
			end;
		end else
		begin
			if assigned(docs) then
			begin
				FreeAndNil(docs);
			end;
			if assigned(Tabs) and (Tabs.Count>0) then
				Tabs.Clear;
			if fEditor.HandleAllocated then
			begin
				if assigned(Parent) then
				begin
					fEditor.Align:=alNone;
					r :=ClientRect;
					tp.x:=r.Left;
					tp.y:=r.Top;
					tp:=fEditor.ClientToParent(tp);
					fEditor.Parent:=Parent;
					fEditor.Left:=tp.x;
					fEditor.Top:=tp.y;
				end;
			end;
			fEditor:=nil;
		end;
	end;
end;

function TSciDocumentTabControl.getCount : Integer;
begin
	if assigned(docs) then
		result:=docs.Count
	else
		result:=0;
end;

procedure TSciDocumentTabControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited;
	if Operation=opRemove then
	begin
		if AComponent=fEditor then
		begin
			if assigned(docs) then FreeAndNil(docs);
			if assigned(Tabs) then Tabs.Clear;
			fEditor:=nil;
		end;
	end;
end;

procedure TSciDocumentTabControl.EvttabChanged(Sender : TObject);
var
	tmp : TSciDoc;
	ist : String;
begin
	tmp:=TSciDoc(Sender);
	if tmp.Index<Tabs.Count then
	begin
		Str(tmp.Index+1,ist);
    if (tmp.Index+1)<10 then
      Tabs.Strings[tmp.Index]:='&'+ist+' '+tmp.TabName
    else
	    Tabs.Strings[tmp.Index]:=ist+' '+tmp.TabName;
	end;
end;

constructor TSciDocumentTabControl.Create(AOwner : TComponent);
begin
	fEditor:=nil;
	docs:=nil;
  inactivate:=False;
	inherited Create(AOwner);
	DragKind:=dkDock;
	MultiSelect:=False;
  ParentFont:=False;
	TabStop:=False;
  ShowHint:=True;
  FDefaultExt:='.txt';
  FDefaultTabName:='<'+sUntitled+FDefaultExt+'>';
  ControlStyle:=ControlStyle-[csAcceptsControls];
end;


function TSciDocumentTabControl.GetDocItm(const Index : Integer) : TSciDoc;
begin
	if assigned(docs) then
		result :=TSciDoc(docs.Items[Index])
	else
		result:=nil;
end;

function TSciDocumentTabControl.NewDocument : Integer;
begin
	result:=Add(FDefaultTabName,sUntitled+FDefaultExt);
  if result<>-1 then
  TSciDoc(docs.Items[result]).Changed;
  if assigned(FOnOpenedAndInited) then FOnOpenedAndInited(Self,result,TSciDoc(docs.Items[result]));

end;

function    TSciDocumentTabControl.Open(const filename : String) : Boolean;
var
	itm : TSciDoc;
  highl,theext : String;
	doccnt : Integer;
  i : Integer;
  efname : String;
  fullfilename : String;
begin
  fullfilename:=filename;
	if FileExists(fullfilename) then
	begin
		doccnt:=docs.Count;
    for i:=0 to (doccnt-1) do
    begin
      if CompareText(TSciDoc(docs.Items[i]).FileName,fullfilename)=0 then
      begin
        ChgTab(i);
        Result:=True;
        Exit;
      end;
    end;
		if (doccnt>1) or (fEditor.Modified) or (StrLIComp(PChar(TSciDoc(docs.Items[0]).TabName),PChar(FDefaultTabName),Length(FDefaultTabName))<>0) then
		begin
      efname:=ExtractFileName(filename);
			itm:=TSciDoc(docs.Items[Add(efname,fullfilename)]);
			doccnt:=doccnt+1;
		end else
		begin
			itm:=TSciDoc(docs.Items[0]);
      itm.FileName:=fullfilename;
      if fEditor is TScintilla then
      begin
        highl:=cDefaultLexer;
        theext :=GetFileExt(filename);
        if assigned(FOnDetectHighlighter) then
        begin
          FOnDetectHighlighter(self,theext,highl);
        end;
        itm.Highlighter:=highl;
      end;
		end;
		fEditor.LoadFromFile(filename);
		if doccnt=1 then ChgTab(0);
    if assigned(FOnOpenedAndInited) then FOnOpenedAndInited(Self,itm.Index,itm);
		result:=true;
	end else
  begin
		doccnt:=docs.Count;
		if (doccnt>1) or (fEditor.Modified) or (StrLIComp(PChar(TSciDoc(docs.Items[0]).TabName),PChar(FDefaultTabName),Length(FDefaultTabName))<>0) then
		begin
      efname:=ExtractFileName(filename);
			itm:=TSciDoc(docs.Items[Add(efname,fullfilename)]);
			doccnt:=doccnt+1;
		end else
		begin
			itm:=TSciDoc(docs.Items[0]);
      itm.FileName:=fullfilename;
      if fEditor is TScintilla then
      begin
        highl:=cDefaultLexer;
        theext:=GetFileExt(filename);
        if assigned(FOnDetectHighlighter) then
        begin
          FOnDetectHighlighter(self,theext,highl);
        end;
        itm.Highlighter:=highl;
      end;
		end;
		if doccnt=1 then ChgTab(0);
    if assigned(FOnOpenedAndInited) then FOnOpenedAndInited(Self,itm.Index,itm);
		result:=true;
  end;
end;

procedure		TSciDocumentTabControl.Close(const index : Integer;const askifneeded : Boolean);
var
	allowclose : Boolean;
  idx : Integer;
begin
	allowclose:=true;
  if index=-1 then
    idx:=TabIndex
  else
    idx:=Index;

	if (askifneeded) and (assigned(FOnClosing)) then
	begin
    if TSciDoc(docs[idx]).IsActive<>True then
    begin
      Activate(idx);
    end;
	  FOnClosing(self,idx,allowclose)
  end;
	if allowclose then
    Remove(idx);
end;

function TSciDocumentTabControl.Add(const tabname : string;const thefilename : String) : Integer;
var
	sci,itm : TSciDoc;
	atidx : Integer;
  theext,highl : String;

begin
	if assigned(fEditor) and (assigned(docs)) then
	begin
    try
    Tabs.BeginUpdate;
		itm:=ActiveDocument;
		itm.AssignFromEditor;
		sci:=TSciDoc.Create(fEditor,Self,false);
		if tabname<>'' then
			sci.TabName:=tabname;
    if thefilename<>'' then
      sci.FileName:=thefilename;
		atidx:=docs.Add(sci);
		sci.Index:=atidx;
		Tabs.Insert(atidx,tabname);
		sci.OnChanged:=EvttabChanged;
    if thefilename<>'' then sci.Changed;
		if fEditor is TScintilla then
		begin
      highl:=cDefaultLexer;
      if thefilename<>'' then
        theext :=GetFileExt(thefilename);
			if assigned(FOnDetectHighlighter) then
			begin
				FOnDetectHighlighter(self,theext,highl);
			end;
		  sci.Highlighter:=highl;
		end;
		ChgTab(atidx);
		Result :=atidx;
    finally
      Tabs.EndUpdate;
    end;
	end else
		result:=-1;
end;

function    TSciDocumentTabControl.GetActiveDocument : TSciDoc;
begin
  Result :=TSciDoc(docs.Items[TabIndex]);
end;
procedure TSciDocumentTabControl.ChgTab(const newtab : Integer);
begin
	TabIndex:=newtab;
	Change;
end;

procedure   TSciDocumentTabControl.Remove(const index : Integer);
var
	sci : TSciDoc;
	id,cur,i : Integer;
  idxtoremove : Integer;
  highl,theext : String;
begin
	if (assigned(fEditor)) and (assigned(docs)) then
	begin
    try
      Tabs.BeginUpdate;
      sci:=TSciDoc(docs.Items[index]);
      idxtoremove:=sci.Index;
      if docs.Count=1 then
      begin
        fEditor.ClearAll;
      end else
      begin
        docs.Remove(sci);
        docs.Pack;
      end;

      if Tabs.Count>1 then
      begin
        cur:=idxtoremove;
        Tabs.Delete(idxtoremove);
        id:=Tabs.Count;
        if(id>cur+1) then
          Inc(cur)
        else if cur=0 then
        begin
        end else
          Dec(cur);
        for i:=idxtoremove to (docs.Count-1) do //this is moved here to adjust the indexes BEFORE changing tabs.. It got Activate confused it seemd.
        begin
          TSciDoc(docs.Items[i]).Index:=i;
        end;
        ChgTab(cur);
      end else
      begin
        TSciDoc(docs.Items[0]).FileName:=sUntitledFileName+FDefaultExt;
        fEditor.ClearDocument;
        TSciDoc(docs.Items[0]).Modified:=false;
        if fEditor is TScintilla then
        begin
          highl:=cDefaultLexer;
          theext :=FDefaultExt;
          if assigned(FOnDetectHighlighter) then
          begin
            FOnDetectHighlighter(self,theext,highl);
          end;
          sci.Highlighter:=highl;
        end;
        ChgTab(0);
      end;
    finally
      Tabs.EndUpdate;
    end;
	end;
end;

function TSciDocumentTabControl.CanChange : Boolean;
var
	itm : TSciDoc;
begin
	if (assigned(fEditor)) and (assigned(docs)) then
	begin
		itm :=ActiveDocument;
		if assigned(itm) then	itm.AssignFromEditor;
		result:=Inherited CanChange;
	end else
		result:=false;
end;

procedure TSciDocumentTabControl.Change;
begin
	if assigned(docs) then
	begin
		//Activate(TabIndex);
    TSciDoc(docs.Items[TabIndex]).Activate;
	end;
	inherited;
end;

procedure TSciDocumentTabControl.CMHintShow(var Message: TMessage);
var
Index: Integer;
begin
  with TCMHintShow(Message) do
  begin
    if not ShowHint then Message.Result := 1
    else
    begin
      with HintInfo^ do
      begin
        // show that we want a hint
        Result := 0;
        // predefined colors always get their names as tooltip
        index:=IndexOfTabAt(CursorPos.X,CursorPos.Y);
        if index<>-1 then
        begin
          HintStr:=TSciDoc(docs.Items[index]).FileName+#13+sLang+TSciDoc(docs.Items[index]).Highlighter;
          CursorRect:=TabRect(index);
        end
        else
          HintStr:=Hint;
      end;
    end;
  end;
end;

procedure TSciDocumentTabControl.Activate(const docno : LongInt);
begin
  if TabIndex<>docno then
  begin
  if (CanChange) then
  begin
    TabIndex:=docno;
	  if assigned(docs) then
    begin
      TSciDoc(docs.Items[docno]).Activate;
    end;
  end;
  end;
end;

destructor TSciDocumentTabControl.Destroy;
var
	i : Integer;
begin
	if (assigned(docs)) and (assigned(fEditor)) then
	begin
		for i:=0 to (docs.Count-1) do
		begin
			TSciDoc(docs.Items[i]).OnChanged:=nil;
      //Close(i);
		end;
		FreeAndNil(docs);
	end;
	inherited;
end;

procedure   TSciDocumentTabControl.SetDefaultExt(const Value : String);
var
oldtab : String;
i,cnt : Integer;
itm : TSciDoc;
theext,highl : String;

begin
  oldtab:=FDefaultTabName;
  FDefaultExt:=Value;
  if Pos('.',FDefaultExt)=0 then
    Insert('.',FDefaultExt,1);
  FDefaultTabName:='<'+sUntitled+FDefaultExt+'>';
  cnt:=Tabs.Count;
  for i:=0 to (cnt-1) do
  begin
    itm:=TSciDoc(docs.Items[i]);
    if StrLIComp(PChar(itm.TabName),PChar(oldtab),Length(oldtab))=0 then
    begin
      itm.TabName:=FDefaultTabName;
      itm.FileName:=sUntitledFileName+FDefaultExt;

      if fEditor is TScintilla then
      begin
        theext :=FDefaultExt;
        highl:=itm.Highlighter;
        if assigned(FOnDetectHighlighter) then
        begin
          FOnDetectHighlighter(self,theext,highl);
        end;
        if itm.Highlighter<>highl then
          itm.Highlighter:=highl;
      end;
    end;
  end;

end;

procedure   TSciDocumentTabControl.RefreshTabs;
var
	cnt : Integer;
	i : Integer;
	ist : String;
begin
  try
    Tabs.BeginUpdate;
    Tabs.Clear;
    if assigned(docs) then
    begin
      cnt:=docs.Count;
      for i:=0 to (cnt-1) do
      begin
        str(i+1,ist);
        if (i+1)<10 then
          Tabs.Add('&'+ist+' '+TSciDoc(docs.Items[i]).TabName)
        else
          Tabs.Add(ist+' '+TSciDoc(docs.Items[i]).TabName);
      end;
    end;
  finally
    Tabs.EndUpdate;
  end;
end;

procedure TSciDoc.SetFileName(const Value : string);
begin
	if Value<>FFileName then
	begin
		FFileName:=Value;
		if Value=(sUntitledFileName+TSciDocumentTabControl(fTabCtrl).FDefaultExt) then
			FTabName:='<'+sUntitled+TSciDocumentTabControl(fTabCtrl).FDefaultExt+'>'
		else
			FTabName:=ExtractFileName(Value);

		Changed;
	end;
end;

procedure   TSciDoc.SetWordChars(const Value : String);
begin
	if FWordChars<>Value then
	begin
		FWordChars:=Value;
		if IsActive then
			TScintillaBase(fEditor).WordChars:=FWordChars;
	end;
end;
procedure TSciDoc.Changed;
begin
	if assigned(FOnChanged) then FOnChanged(self);
end;


procedure TSciDoc.SetTabName(const Value : string);
begin
	if Value<>FTabName then
	begin
		FTabName:=Value;
		Changed;
	end;
end;

function TSciDoc.IsActive : Boolean;
var
	ptr : LongInt;
begin
	if fEditor=nil then
	begin
		Result:=false;
		Exit;
	end;
	ptr :=fEditor.GetDocPointer;
	if ptr=documentid then
		result :=true
	else
		result :=false;
end;

constructor TSciDoc.Create(pp : TScintillaBase;ttabctrl : TSciDocumentTabControl;const getcurrent : Boolean);
begin
	inherited Create;
  fTabCtrl:=ttabctrl;
	fEditor :=pp;
  FCodePage:=pp.GetCodePage;
  FUseTabs:=False;
	FTabName:='<'+sUntitled+TSciDocumentTabControl(fTabCtrl).FDefaultExt+'>';
	FModified:=false;
	FWordChars :='_'+sci_alphachars+sci_numericchars;
  FHighlighter:=cDefaultLexer;
  FAutoIgnoreCase:=False;
  FAutoStartChars:='';
  FAutoStopChars:='';
  FExtraData :=nil;
	FFileName :=sUntitledFileName+TSciDocumentTabControl(fTabCtrl).FDefaultExt;
	if getcurrent=false then
	begin
		documentid:=fEditor.CreateDocument;
		AddRef;
	end	else
	begin
		documentid:=fEditor.GetDocPointer;
		AddRef;
	end;
	AssignFromEditor;
end;

procedure   TSciDoc.StoreFoldStates;
var
maxLine : LongInt;
foldPoints : LongInt;
line : LongInt;
level : LongInt;
expanded : Boolean;
atpos : Integer;
begin
//FFoldStates : array of TSciFoldStateRec;
  FFoldStates:=nil;
  if Highlighter='null' then Exit;
  foldPoints:=0;
  maxline:=fEditor.GetLineCount;
  for line := 0 to (maxLine-1) do
  begin
    level := fEditor.GetFoldLevel(line);
    if ((level and SC_FOLDLEVELHEADERFLAG)<>0) then Inc(foldPoints);
  end;
  if foldPoints>0 then
  begin
    SetLength(FFoldStates,foldPoints);
    atpos:=0;
		for line := 0 to  (maxLine-1) do
    begin
      level := fEditor.GetFoldLevel(line);
      if ((level and SC_FOLDLEVELHEADERFLAG)<>0) then
      begin
        expanded:=fEditor.GetFoldExpanded(line);
        FFoldStates[atpos].Line:=line;
        FFoldStates[atpos].Expanded:=expanded;
        Inc(atpos);
      end;
	  end;
  end;
end;
procedure   TSciDoc.RestoreFoldStates;
var
i : LongInt;
expanded : Boolean;
begin
  if FFoldStates=nil then Exit;
  if Highlighter='null' then Exit;
  for i:=Low(FFoldStates) to High(FFoldStates) do
  begin
    expanded:=fEditor.GetFoldExpanded(FFoldStates[i].Line);
    if (FFoldStates[i].Expanded=False) and (expanded=True) then
    begin
      fEditor.ToggleFold(FFoldStates[i].Line);
    end;
    if (FFoldStates[i].Expanded=True) and (expanded=False) then
    begin
      fEditor.ToggleFold(FFoldStates[i].Line);
    end;
  end;
end;

procedure   TSciDoc.AssignFromEditor;
begin
	with fEditor as TScintillaMemo do
	begin
    StoreFoldStates;
		FFirstLineInView:=GetCurrentScrollPosition;
		FSelStart:=SelStart;
		FSelLength:=SelLength;
		FModified:=fEditor.Modified;
		FUnicode :=UseUnicode;
		FReadOnly :=ReadOnly;
		FWordChars :=WordChars;
    FIndentWidth:=IndentWidth;
    FTabWidth:=TabWidth;
    FIndentation:=Indentation;
    FUseTabs:=UseTabs;
    FCodePage:=GetCodePage;
	end;
	if fEditor is TScintilla then
  begin
    fHighlighter:=TScintilla(fEditor).LanguageManager.SelectedLanguage;
  end;

end;

procedure   TSciDoc.AssignToEditor;
var
	curtop : LongInt;
	xlinestart : LongInt;
	xlineend : LongInt;
	xlinetop : LongInt;
begin
	if fEditor is TScintilla then
  begin
    TScintilla(fEditor).LanguageManager.SelectedLanguage:=self.fHighlighter;
  end;
	with fEditor as TScintillaMemo do
	begin
    RestoreFoldStates;
		xlinestart:=LineFromPosition(FSelStart);
		xlineend:=LineFromPosition(FSelStart+FSelLength);
		EnsureVisibleEnforcePolicy(xlinestart);
		EnsureVisibleEnforcePolicy(xlineend);
		SelStart:=FSelStart;
		SelLength:=FSelLength;
		fEditor.Modified:=FModified;
		UseUnicode:=FUnicode;
		ReadOnly:=FReadOnly;
		WordChars :=FWordChars;
    Indentation:=FIndentation;
    TabWidth:=FTabWidth;
    IndentWidth:=FIndentWidth;
    UseTabs:=FUseTabs;
    SetCodePage(FCodePage);
    //Position the cursor back where it was
		curtop:=GetFirstVisibleLine;
		xlinetop:=VisibleFromDocLine(FFirstLineInView);
		LineScroll(0,xlinetop-curtop);
    if Visible and Showing then
		SetFocus;
	end;
end;

procedure TSciDoc.Activate;
begin
  if not IsActive then
  begin
    if (TSciDocumentTabControl(fTabCtrl).TabIndex<>Index) and (TSciDocumentTabControl(fTabCtrl).CanChange) then
    begin
      TSciDocumentTabControl(fTabCtrl).TabIndex:=Index;
    end;
    fEditor.SetDocPointer(documentid);
    AssignToEditor;
  end;
end;

destructor TSciDoc.Destroy;
begin
  FFoldStates:=nil;
  if assigned(FOnRemoveExtraData) then
  begin
    FOnRemoveExtraData(Self);
  end;
	Release;
	inherited;
end;
procedure TSciDoc.SetHighlighter(const Value : String);
begin
	fHighlighter:=Value;
	if IsActive then
	begin
		if fEditor is TScintilla then
		begin
      if Value<>TScintilla(fEditor).LanguageManager.SelectedLanguage then
			begin
        TScintilla(fEditor).LanguageManager.SelectedLanguage:=fHighlighter;
//        TScintilla(fEditor).ClearDocumentStyle;
			end;
		end;
	end;
end;

procedure   TSciDoc.SetIndentWidth(const Value : Integer);
begin
	fIndentWidth:=Value;
	if IsActive then
	begin
		if fEditor is TScintillaMemo then
		begin
      if Value<>TScintillaMemo(fEditor).IndentWidth then
			begin
        TScintillaMemo(fEditor).IndentWidth:=fIndentWidth;
			end;
		end;
	end;
end;

procedure   TSciDoc.SetTabWidth(const Value : Integer);
begin
	fTabWidth:=Value;
	if IsActive then
	begin
		if fEditor is TScintillaMemo then
		begin
      if Value<>TScintillaMemo(fEditor).TabWidth then
			begin
        TScintillaMemo(fEditor).TabWidth:=fTabWidth;
			end;
		end;
	end;

end;
procedure   TSciDoc.SetIndentation(const Value : TIndentationOptions);
begin
	fIndentation:=Value;
	if IsActive then
	begin
		if fEditor is TScintillaMemo then
		begin
      if Value<>TScintillaMemo(fEditor).Indentation then
			begin
        TScintillaMemo(fEditor).Indentation:=fIndentation;
			end;
		end;
	end;

end;

procedure TSciDoc.AddRef;
begin
	if (assigned(fEditor)) and (documentid<>0) then
	begin
		fEditor.AddRefDocument(documentid);
	end;
end;

procedure TSciDoc.Release;
begin
	if (assigned(fEditor)) and (documentid<>0) then
	begin
		if fEditor.HandleAllocated then
			fEditor.ReleaseDocument(documentid);
	end;
	documentid:=0;
end;

function    TSciDoc.IsUntitled  : Boolean;
begin
  if Pos('<',TabName)<>0 then
    Result:=True
  else
    Result:=False;
end;

procedure   TSciDoc.SetModified(const Value : Boolean);
begin
  FModified:=Value;
  if assigned(fEditor) and (IsActive=True) then
  begin
    fEditor.Modified:=Value;
  end;
end;
procedure   TSciDoc.Close;
begin
  if assigned(fTabCtrl) then
  TSciDocumentTabControl(fTabCtrl).Close(Index);
end;
function TSciDoc.SPerform(Msg : LongInt; wParam : LongInt=0; lParam : LongInt=0) : LongInt;
begin
  if not IsActive then Activate;
  Result:=fEditor.SPerform(Msg,wParam,lParam);
end;

procedure   TSciDoc.SetUseTabs(const Value : Boolean);
begin
  FUseTabs:=Value;
	if IsActive then
	begin
		if fEditor is TScintillaMemo then
		begin
      if Value<>TScintillaMemo(fEditor).UseTabs then
			begin
        TScintillaMemo(fEditor).UseTabs:=Value;
			end;
		end;
	end;
end;

procedure   TSciDoc.SetCodePage(const Value : LongInt);
begin
  FCodePage:=Value;
	if IsActive then
	begin
		if fEditor is TScintillaMemo then
		begin
      if Value<>TScintillaMemo(fEditor).GetCodePage then
			begin
        TScintillaMemo(fEditor).SetCodePage(Value);
			end;
		end;
	end;

end;

end.
