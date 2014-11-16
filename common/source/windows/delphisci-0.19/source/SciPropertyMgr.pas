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
	Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
	Author : hdalis (Jan Martin Pettersen)
	Created: 01/08/2004
			$Id: SciPropertyMgr.pas,v 1.5 2004/11/13 04:29:50 hdalis Exp $
	Purpose: Load/Save properties from a TScintilla/TScintillaAuto derived component

    Usage: Drop this component on the form, select the editor it should save and load properties
           from. Set the FileName property to the file it should read/write, and call Save or Load.

	History: 29/09/2004 First release
					 13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
											generate the help.
           26/10/2004 Moved the GetLexerMapperProps function to TSciLanguageManager.GetLexerProps (ScintillaSynLexer.Pas)
           14/11/2004 Changed the TSciPropertyLoader component to allow use with all classes descending from
                      TScintillaBase. A little 'feature' has also been fixed.. It searched for the '.' section.name divider
                      first, and if there wasn't a '.' in the propertyline before '=' then it would search the value for it,
                      and couldn't get a valid match for the line, and still contain the correct value.
                      Now it searches for the '=' first, and then matches properties within the left part,
                      and the right part of it is the value.
           02/12/2004 Moved all standalone functions to sciUtils.pas. They can, and probably will be used
                      by other units also.
                      Changed the format of the expected properties file.. The new one somewhat resembles
                      ini files, but with at least one distinction: You can continue the line with the '\' character,
                      as in C++/C..
           25/01/2005 Added the functions AddBool,AddInt,GetBool,GetInt.
                      Renamed the GetOtherSetting to GetOther, and the SetOtherSetting to SetOther.
           05/02/2005 Changed the TSCEvent_propsaveitem to TSCEvent_propsaveitems, and let the programmer
                      add all their settings in one go with OnSaveItems..
                      Some restructuring needed in this component.

           12/04/2005 Corrected the .dpk and .dpr to map to the Pascal language.
           18/04/2005 Changed the implementation of ExtensionManager, and moved it to SciFileExtensionsManager.pas,
                      and deprecated the old version which still is here when you uncomment
                      USENEWEXTENSIONMAPPER in SciCommonDef.Inc
           15/05/2005 Removed all references to ValueExtract, as it was unneccesary.
					 18/05/2005 The property StoreMinimal was added. When true, the propertyloaded
					            saves only the miniumum. i.e extensions,styles,keywords etc.
											but only the style property under the [default] tag.
											This is to ease the use if most of the config is saved
											to the registry.
}
{$Include SciCommonDef.Inc}
unit SciPropertyMgr;
interface
uses  Classes,SciLexer,SciLexerMod,Graphics,ScintillaLanguageManager;

Type
	TSCEvent_proploaditem = procedure(Sender : TObject; var CategoryName,PropName : String;var Num : Integer;var aValue : String;var Handled : Boolean) of object;
	TSCEvent_propsaveitems = procedure(Sender : TObject; settinglist : TStrings) of object;
{$Ifndef USENEWEXTENSIONMAPPER}
  TExtensionMapper=class(TObject)
    private
    extensions : TStrings;
    extsadded : Integer;
    function    GetProps : String; // Used by the propertymanager to output added extensions
    procedure   FillExtensionMap;
    procedure   SetExtensions(Value : TStrings);
    public
    constructor Create;
    destructor  Destroy;override;
    procedure   Add(const mapfromext : String;const maptolexer : String);
    function    DetectLexer(const extension : string;var TheWordChars : String) : String;
    property ExtensionsList : TStrings read extensions write SetExtensions;
  end;
{$Endif}

	{ Loads and saves properties for a TScintilladescending component.
		OnLoadItem and OnSaveItem may be used to save,parse additional properties
		if needed}
	TSciPropertyLoader=class(TComponent)
		private
			fFileName : String;
			fEditor         : TScintillaBase;
			fDefaultStyle   : TSciLangStyle;
      fLexerProperties     : TStrings;
			fOnPropLoadItem : TSCEvent_proploaditem;
			fOnPropSaveItems : TSCEvent_propsaveitems;
      fOnPropertiesLoaded : TNotifyEvent;
      fOtherSettings  : TStrings;
      FStoreMinimal : Boolean;
			procedure GenerateDefaultPropLine(var dest : String;component : TScintillaBase);
			procedure GenerateStylePropLine(var dest : String;const prefix : String;const styl : TSciStyle);
			procedure GenerateKwdPropLine(var dest : String;const prefix : String;const kwd : TSciKeywords);
			function  PropLineSplit(const s: String;var stylenum : Integer;var prop : String;var value : String) : Boolean;
	    function  ParseStyleDefinition(const definition : PChar;var sty : TSciLangStyle;defstyle : Boolean=False) : Boolean;
      procedure SetLexerProperties;
		protected
			function  ProcessDataStart(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
			function  ProcessDataEnd(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
			procedure Notification(AComponent: TComponent;Operation: TOperation);  override;
      procedure DoPropSave(lst : TStrings);
		public
      procedure   FillWithDefaultStyle(var sty : TSciLangStyle);
			constructor Create(AOwner : TComponent);override;
			destructor  Destroy;override;
			function    Load : Boolean; //Loads the propertyfile specified in 'FileName'
			function    Save : Boolean; //Saves to the propertyfile specified in 'FileName'
			function    LoadFromStream(fs : TStream) : Boolean;
			function    SaveToStream(fs : TStream) : Boolean;

      function    GetOther(const SettingName : String;const DefaultValue : String='';const AddIfNonExistent : Boolean=False) : String;
      procedure   AddOther(const SettingName : String;const value : String);
      function    GetInt(const SettingName : String;const DefaultValue : Integer=0;const AddIfNonExistent : Boolean=False) : Integer;
      function    GetBool(const SettingName : String;const DefaultValue : Boolean=False;const AddIfNonExistent : Boolean=False) : Boolean;
      procedure   AddInt(const SettingName : String;const value : Integer);
      procedure   AddBool(const SettingName : String;const value : Boolean);

		published
			property 		FileName : String read fFileName write fFileName;
			property 		Editor : TScintillaBase read FEditor write fEditor;
			property 		OnLoadItem : TSCEvent_proploaditem read fOnPropLoadItem write fOnPropLoadItem;
			property 		OnSaveItems : TSCEvent_propsaveitems read fOnPropSaveItems write fOnPropSaveItems;
      property    OnPropertiesLoaded : TNotifyEvent read fOnPropertiesLoaded write fOnPropertiesLoaded;
      property    StoreMinimal : Boolean read FStoreMinimal write FStoreMinimal default False;
	end;

{$Ifndef USENEWEXTENSIONMAPPER}
  function  ExtMapper : TExtensionMapper;
{$Endif}

implementation
uses
{$Ifdef USENEWEXTENSIONMAPPER}
SciFileExtensionsManager,
{$Endif}
Windows,SysUtils,SciLexerMemo,SciResLang,sciUtils;
{$Ifndef USENEWEXTENSIONMAPPER}
var
  FExtMap : TExtensionMapper;
{$Endif}

procedure TSciPropertyLoader.SetLexerProperties;
var
  i,cnt : Integer;
  v,n : String;
begin
  if not assigned(FEditor) then Exit;
  cnt:=fLexerProperties.Count;
  if cnt=0 then Exit;
  for i:=0 to (cnt-1) do
  begin
    n:=fLexerProperties.Names[i];
    if n<>'' then
    begin
      v:=fLexerProperties.Values[n];
      if(v<>'') then
      begin
        fEditor.SetProperty(PChar(n),PChar(v));
      end;
    end;
  end;
end;

// Quite a bit of multiple instances of the same code maybe, but temporary
// Generates a string with the default color and font settings.
// Does NOT support the EOLFilled etc attributes.
procedure TSciPropertyLoader.GenerateDefaultPropLine(var dest : String;component : TScintillaBase);
var
	tmpstr : String;

begin
	tmpstr:=spropStyle+'=';
	tmpstr:=tmpstr+spropFore+':'+ColourToString(component.Font.Color)+',';//fore
	tmpstr:=tmpstr+spropBack+':'+ColourToString(component.Color)+',';//back
  tmpstr:=tmpstr+Format('%s:%d',[spropsize,component.Font.Size])+',';//size
  tmpstr:=tmpstr+spropFont+':'+component.Font.Name+',';//font
  if fsBold in component.Font.Style then
    tmpstr:=tmpstr+spropBold+','//bold
  else
    tmpstr:=tmpstr+spropNot+spropBold+',';//notbold
  if fsItalic in component.Font.Style then
    tmpstr:=tmpstr+spropItalics+',' //italics
  else
    tmpstr:=tmpstr+spropNot+spropItalics+',';//notitalics

  if fsUnderline in component.Font.Style then
    tmpstr:=tmpstr+spropUnderlined+','//underlined
  else
    tmpstr:=tmpstr+spropNot+spropUnderlined+',';//notunderlined
  if fDefaultStyle.Visible=false  then
    tmpstr:=tmpstr+spropNot+spropVisible+',' //notvisible
  else
    tmpstr:=tmpstr+spropVisible+',';//visible
  if fDefaultStyle.EOLFilled  then
    tmpstr:=tmpstr+spropEolFilled+','//eolfilled
  else
    tmpstr:=tmpstr+spropNot+spropEolFilled+',';//noteolfilled
  if fDefaultStyle.Changeable  then
    tmpstr:=tmpstr+spropChangeable+','//changeable
  else
    tmpstr:=tmpstr+spropNot+spropChangeable+',';//notchangeable
  if fDefaultStyle.Hotspot  then
    tmpstr:=tmpstr+spropHotspot+','//hotspot
  else
    tmpstr:=tmpstr+spropNot+spropHotspot+',';//nothotspot
  case fDefaultStyle.CharCase of
    CASE_UPPER: tmpstr:=tmpstr+spropCase+':u,';
    CASE_LOWER: tmpstr:=tmpstr+spropCase+':l,';
  end;

  dest :=tmpstr;
  if dest[Length(dest)]=',' then Delete(dest,Length(dest),1);
end;

// Generates a string with the properties of a single style.
procedure TSciPropertyLoader.GenerateStylePropLine(var dest : String;const prefix : String;const styl : TSciStyle);
var
	tmpstr : String;
	tmpdescr : String;
begin
	tmpstr:=Format('%s%d=',[prefix,styl.StyleNumber]);
	tmpdescr:=styl.Name;
	tmpdescr:=ReplaceAChar(tmpdescr,',','|');
	tmpstr:=tmpstr+spropName+':'+tmpdescr+','; //name
	if (styl.ForeColor<>clDefault) and (styl.ForeColor<>fDefaultStyle.ForeColor) then
		tmpstr:=tmpstr+spropFore+':'+ColourToString(styl.ForeColor)+',';//fore

	if (styl.BackColor<>clDefault) and (styl.BackColor<>fDefaultStyle.BackColor) then
		tmpstr:=tmpstr+spropBack+':'+ColourToString(styl.BackColor)+',';//back

	if (styl.FontSize<>0) and (styl.FontSize<>fDefaultStyle.FontSize) then
		tmpstr:=tmpstr+Format('%s:%d',[spropSize,styl.FontSize])+',';//size

  if (styl.FontName<>'') and (styl.FontName<>fDefaultStyle.FontName)  then
		tmpstr:=tmpstr+spropFont+':'+styl.FontName+',';//font

  if (styl.FontStyles<>fDefaultStyle.FontStyles) then
  begin
		if ((fsBold in styl.FontStyles) <> (fsBold in fDefaultStyle.FontStyles)) then
		begin
			if fsBold in styl.FontStyles then
				tmpstr:=tmpstr+spropBold+','//bold
			else
				tmpstr:=tmpstr+spropNot+spropBold+',';//notbold
    end;
		if ((fsItalic in styl.FontStyles) <> (fsItalic in fDefaultStyle.FontStyles)) then
		begin
			if fsItalic in styl.FontStyles then
				tmpstr:=tmpstr+spropItalics+',' //italics
			else
				tmpstr:=tmpstr+spropNot+spropItalics+',';//notitalics
		end;
		if ((fsUnderline in styl.FontStyles) <> (fsUnderline in fDefaultStyle.FontStyles)) then
		begin
			if fsUnderline in styl.FontStyles then
				tmpstr:=tmpstr+spropUnderlined+','//underlined
			else
				tmpstr:=tmpstr+spropNot+spropUnderlined+',';//notunderlined
		end;
	end;//<>fontstyles

  if styl.Visible<>fDefaultStyle.Visible then
  begin
		if styl.Visible=false  then
			tmpstr:=tmpstr+spropNot+spropVisible+',' //notvisible
		else
			tmpstr:=tmpstr+spropVisible+',';//visible
  end;

  if styl.EOLFilled<>fDefaultStyle.EOLFilled then
  begin
		if styl.EOLFilled  then
			tmpstr:=tmpstr+spropEolFilled+','//eolfilled
		else
			tmpstr:=tmpstr+spropNot+spropEolFilled+',';//noteolfilled
  end;
  if styl.Changeable<>fDefaultStyle.Changeable then
  begin
		if styl.Changeable  then
			tmpstr:=tmpstr+spropChangeable+','//changeable
		else
			tmpstr:=tmpstr+spropNot+spropChangeable+',';//notchangeable
  end;
  if styl.Hotspot<>fDefaultStyle.Hotspot then
  begin
		if styl.Hotspot  then
			tmpstr:=tmpstr+spropHotspot+','//hotspot
		else
			tmpstr:=tmpstr+spropNot+spropHotspot+',';//nothotspot
  end;
  if styl.CharCase<>fDefaultStyle.CharCase then
  begin
		case styl.CharCase of
			CASE_UPPER: tmpstr:=tmpstr+spropCase+':u,';
			CASE_LOWER: tmpstr:=tmpstr+spropCase+':l,';
		end;
  end;
  dest :=tmpstr;
  if dest[Length(dest)]=',' then Delete(dest,Length(dest),1);
end;

// Generates a propertyline with the specified keywords. May split in multiple lines if
// more than 8 in the list.
procedure TSciPropertyLoader.GenerateKwdPropLine(var dest : String;const prefix : String;const kwd : TSciKeywords);
var
	tmpstr,tmpdescr : String;
	i,cnt,kwdonline : Integer;
  isfirst : Boolean;
begin
  tmpstr:=Format('%s%d=',[prefix,kwd.KeywordListNumber]);
  tmpdescr:=kwd.Name;
  tmpdescr :=ReplaceAChar(tmpdescr,',','|');

  tmpstr:=tmpstr+spropName+':'+tmpdescr+'::';//name
	cnt:=kwd.Keywords.Count;
	if cnt>0 then
	begin
		kwdonline:=0;
    isfirst:=true;
		for i:=0 to (cnt-1) do
		begin
			if (kwdonline>8) then
			begin
				tmpstr:=tmpstr+'\'+CrLf;
				kwdonline:=0;
				tmpstr:=tmpstr+kwd.Keywords.Strings[i];
			end else
      begin
        if isfirst then
        begin
          tmpstr:=tmpstr+kwd.Keywords.Strings[i];
          isfirst:=false;
        end else
			    tmpstr:=tmpstr+' '+kwd.Keywords.Strings[i];
      end;
			Inc(kwdonline);
		end;
		if tmpstr[Length(tmpstr)]='\' then Delete(tmpstr,Length(tmpstr),1);
	end;
	dest :=tmpstr;
end;


//Splits a propertyline in 'Lang',stylenum and prop.
//Lines like
// prop.lang.stylenum=value or
// prop.lang=value
// are accepted
function TSciPropertyLoader.PropLineSplit(const s: String;var stylenum : Integer;var prop : String;var value : String) : Boolean;
var
	tmp : String;
  propnamepart : String;
	code : Integer;
  i,cnt,dotstart : Integer;
begin
  tmp:=s;
  propnamepart:=WordBefore(tmp,'=');
  value:=tmp;
  cnt:=Length(propnamepart);
  dotstart:=-1;
  for i:=cnt downto 0 do
  begin
    if i>0 then
    case propnamepart[i] of
    '0'..'9':;
    else
    begin
      if i<>cnt then dotstart:=i;
      break;
    end;
    end;
  end;
  if dotstart=-1 then
    Prop:=propnamepart
  else
    begin
      if (dotstart>0) and (propnamepart[dotstart]='.') then
      begin
        Prop:=Copy(propnamepart,1,dotstart-1);
      end else
        Prop:=propnamepart;
        //Copy(propnamepart,1,dotstart);
      Delete(propnamepart,1,dotstart);
    end;
  if propnamepart='' then
  begin
    Result:=False;
    stylenum:=0;
    Exit;
  end;
  if propnamepart<>'' then
    val(propnamepart,stylenum,code);
  Result:=true;
end;

// Parses a string with a styledefinition into a TSciLangStyle
function TSciPropertyLoader.ParseStyleDefinition(const definition : PChar;var sty : TSciLangStyle;defstyle : Boolean) : Boolean;
var
	vxal,opt,cpComma,colon : PChar;
	i : Integer;
  Changeable,EOLFilled,Hotspot : Boolean;
	wasset : array [0..6] of Boolean;
	ccase : TSciCase;
	vis : Boolean;
	name : String;
begin
	if (definition = nil) or (definition^ = #0) then
	begin
		Result:=false;
		Exit;
	end;
	GetMem(vxal,Length(definition)+1);
	StrCopy(vxal,definition);
	opt:=vxal;
	Changeable:=true;
	Hotspot:=false;
	EOLFilled:=false;
	vis:=True;
	ccase:=CASE_MIXED;
	for i:=Low(wasset) to High(wasset) do wasset[i]:=False;
	while (opt<>nil) do
	begin
		// Find attribute separator
		cpComma := AnsiStrScan(opt, ',');
		if assigned(cpComma) then	// If found, we terminate the current attribute (opt) string
			cpComma^ := #0;

		// Find attribute name/value separator
		colon := AnsiStrScan(opt, ':');
		if assigned(colon) then
		begin
			// If found, we terminate the current attribute name and point on the value
			colon^:=#0;
			Inc(colon);
		end;
		if (0 = CompStr(opt, spropItalics,True)) then //'italics'
		begin
			Include(sty.FontStyles,fsItalic);
		end else
		if (0 = CompStr(opt, spropNot+spropItalics)) then //notitalics
		begin
			Exclude(sty.FontStyles,fsItalic);
		end else
		if (0 = CompStr(opt, spropBold)) then //bold
		begin
			Include(sty.FontStyles,fsBold);
		end else
		if (0 = CompStr(opt, spropNot+spropBold)) then //notbold
		begin
			Exclude(sty.FontStyles,fsBold);
		end else
		if (0 = CompStr(opt, spropFont)) then //font
		begin
			sty.FontName:=colon;
		end else
		if (0 = CompStr(opt, spropName)) then
		begin
			name:=colon;
			name:=ReplaceAChar(name,'|',',');
      wasset[5]:=true;
		end else
		if (0 = CompStr(opt, spropFore)) then //'fore'
		begin
			sty.ForeColor:=ColourFromString(Trim(colon),clSilver);
		end else
		if (0 = CompStr(opt, spropBack)) then //back
		begin
			sty.BackColor:=ColourFromString(Trim(colon),clBlack);
		end else
		if (0 = CompStr(opt, spropSize)) then //size
		begin
			sty.FontSize:=ToInt(colon);
		end else
		if (0 = CompStr(opt, spropEolFilled)) then //eolfilled
		begin
			EOLFilled:=true;
			wasset[4]:=true;
		end else
		if (0 = CompStr(opt, spropNot+spropEolFilled)) then //noteolfilled
		begin
			EOLFilled:=false;
			wasset[4]:=true;
		end else
		if (0 = CompStr(opt, spropUnderlined)) then //underlined
		begin
			Include(sty.FontStyles,fsUnderline);
		end else
		if (0 = CompStr(opt, spropNot+spropUnderlined)) then //notunderlined
		begin
			Exclude(sty.FontStyles,fsUnderline);
		end else
		if (0 = CompStr(opt, spropCase)) then //case
		begin
			wasset[0]:=true;
			ccase:= CASE_MIXED;
			if assigned(colon) then
			begin
				if (colon^ = 'u') then
					ccase:= CASE_UPPER
				else if (colon^ = 'l') then
					ccase:= CASE_LOWER;
			end;
		end else
		if (0 = CompStr(opt, spropVisible)) then //visible
		begin
			wasset[1]:=true;
			Vis:=true;
		end else
		if (0 = CompStr(opt, spropNot+spropVisible)) then //notvisible
		begin
			wasset[1]:=true;
			Vis:=false;
		end else
		if (0 = CompStr(opt, spropChangeable)) then //changeable
		begin
			wasset[2]:=true;
			Changeable:=true;
		end else
		if (0 = CompStr(opt, spropNot+spropChangeable)) then //notchangeable
		begin
			wasset[2]:=true;
			Changeable:=false;
		end else
		if (0 = CompStr(opt, spropHotspot)) then //hotspot
		begin
			wasset[3]:=true;
			Hotspot:=true;
		end else
		if (0 = CompStr(opt, spropNot+spropHotspot)) then //nothotspot
		begin
			wasset[3]:=true;
			Hotspot:=false;
		end;

		if assigned(cpComma) then
			opt := cpComma + 1
		else
			opt := nil;
	end;
	if assigned(vxal) then
	begin
		FreeMem(vxal);
	end;
  if defstyle=False then
  begin
    if (FDefaultStyle.FontSize<>0) and (sty.FontSize=FDefaultStyle.FontSize) then
      sty.FontSize:=0;
    if (FDefaultStyle.FontName<>'') and (CompStr(FDefaultStyle.FontName,sty.FontName)=0) then
      sty.FontName:='';
  end;
	if wasset[0] then sty.CharCase:=ccase;
	if wasset[1] then sty.Visible:=vis;
	if wasset[2] then sty.Changeable:=Changeable;
	if wasset[3] then sty.Hotspot:=Hotspot;
	if wasset[4] then sty.EOLFilled:=EOLFilled;
	if wasset[5] then sty.Name:=name;

	Result:=true;
end;

procedure TSciPropertyLoader.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditor) and (Operation = opRemove) then FEditor := nil;
end;

constructor TSciPropertyLoader.Create(AOwner : TComponent);
begin
  inherited;
  fFileName:='';
  FStoreMinimal:=False;
	fDefaultStyle.EOLFilled:=False;
	fDefaultStyle.Visible:=True;
	fDefaultStyle.CharCase:=CASE_MIXED;
	fDefaultStyle.Changeable:=True;
	fDefaultStyle.Hotspot:=False;
	fDefaultStyle.FontStyles:=[];
	fDefaultStyle.FontName:='';
	fDefaultStyle.FontSize:=0;
	fDefaultStyle.ForeColor:=clDefault;
	fDefaultStyle.BackColor:=clDefault;
  {$Ifdef COMPILER6_UP}
    fOtherSettings:=TStringList.Create;
    TStringList(fOtherSettings).CaseSensitive:=False;
    TStringList(fOtherSettings).Duplicates:=dupIgnore;
    TStringList(fOtherSettings).Sorted:=True;
    fLexerProperties:=TStringList.Create;
    TStringList(fLexerProperties).CaseSensitive:=False;
    TStringList(fLexerProperties).Duplicates:=dupIgnore;
    TStringList(fLexerProperties).Sorted:=True;
  {$Else}
    fOtherSettings:=TMyStringList.Create;
    TMyStringList(fOtherSettings).CaseSensitive:=False;
    TMyStringList(fOtherSettings).Duplicates:=dupIgnore;
    TMyStringList(fOtherSettings).Sorted:=True;
    fLexerProperties:=TStringList.Create;
    TMyStringList(fLexerProperties).CaseSensitive:=False;
    TMyStringList(fLexerProperties).Duplicates:=dupIgnore;
    TMyStringList(fLexerProperties).Sorted:=True;
  {$Endif}

end;

destructor  TSciPropertyLoader.Destroy;
begin
  if assigned(fOtherSettings) then fOtherSettings.Free;
  if assigned(fLexerProperties) then fLexerProperties.Free;
  inherited;
end;

procedure TSciPropertyLoader.FillWithDefaultStyle(var sty : TSciLangStyle);
begin
	sty.EOLFilled :=fDefaultStyle.EOLFilled;
	sty.Visible   :=fDefaultStyle.Visible;
	sty.CharCase  :=fDefaultStyle.CharCase;
	sty.Changeable:=fDefaultStyle.Changeable;
	sty.Hotspot   :=fDefaultStyle.Hotspot;
	sty.FontStyles:=fDefaultStyle.FontStyles;
	sty.FontName  :='';
	sty.FontSize  :=fDefaultStyle.FontSize;
	sty.ForeColor :=clDefault;
	sty.BackColor :=clDefault;
end;

function    TSciPropertyLoader.LoadFromStream(fs : TStream) : Boolean;
var
	lst : TStringList;
	cnt,i,aStyle,speedcnt,ignorelines : Integer;
	aSection,aLanguage,aProp,aValue,tmpstr : String;
	Handled : Boolean;
  ps : Integer;

begin
	lst:=nil;
  if fEditor=nil then
  begin
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,['TSciPropertyLoader']);
    Result:=false;
    Exit;
  end;
	try
    fOtherSettings.Clear;
    fLexerProperties.Clear;
		lst:=TStringList.Create;
		lst.LoadFromStream(fs);
		cnt :=lst.Count;
		ignorelines:=0;
		for i:=0 to (cnt-1) do
		begin
			speedcnt:=0;
			if ignorelines>0 then
			begin
				Dec(ignorelines);
				Continue;
			end;
			tmpstr:=Trim(lst.Strings[i]);
			if tmpstr='' then Continue;

			if Pos('#',tmpstr)=1 then Continue;
      ps:=Pos('[',tmpstr);
      if ps=1 then
      begin
        if Pos(']',tmpstr)>0 then
          Delete(tmpstr,Pos(']',tmpstr),1);
          Delete(tmpstr,1,1);
          aSection:=tmpstr;
          continue;
      end;
			while (tmpstr[Length(tmpstr)]='\') and ((i+speedcnt+1)<=(cnt-1)) do
			begin
				Inc(speedcnt);
        Delete(tmpstr,Length(tmpstr),1);
				  tmpstr:=Trim(tmpstr)+' '+Trim(lst.Strings[i+speedcnt]);
			end;
			if speedcnt<>0 then ignorelines:=speedcnt;
			if PropLineSplit(tmpstr,aStyle,aProp,aValue) then
			begin
        aLanguage:=aSection;
        if ProcessDataStart(aLanguage,aProp,aStyle,aValue)=false then
        begin
	        Handled:=false;
	        if assigned(fOnPropLoadItem) then
            fOnPropLoadItem(self,aLanguage,aProp,aStyle,aValue,Handled);
          if Handled=False then
            if ProcessDataEnd(aLanguage,aProp,aStyle,aValue)=False then
            begin
              if tmpstr<>'' then
              begin
                fOtherSettings.Add(Trim(tmpstr));
              end;
            end;
        end;
			end;//proplinesplit
		end;//for
  Self.SetLexerProperties;
	finally
		if assigned(lst) then lst.Free;
	end;
  if fEditor is TScintilla then
    TScintilla(fEditor).LanguageManager.Update;
  if assigned(fOnPropertiesLoaded) then fOnPropertiesLoaded(Self);
	Result:=true;
end;


procedure TSciPropertyLoader.DoPropSave(lst : TStrings);
begin
  if assigned(fOnPropSaveItems) then
  begin
    fOnPropSaveItems(Self,lst);
  end;
end;

{$Ifdef USENEWEXTENSIONMAPPER}
function GetFileExtensionProps : String;
var
  tmp : String;
  i,cnt : Integer;
  itm : TExtensionItem;
  FExtensionItems : TExtensionsToLanguageList;
  exts : String;
begin
  FExtensionItems :=ExtMapper.ExtensionsList;
  tmp:='';
  if FExtensionItems=nil then
  begin
    Result:=tmp;
    Exit;
  end;
    tmp:=CrLf+'['+spropExtension+']'+CrLf;
    cnt:=FExtensionItems.Count;
    for i:=0 to (cnt-1) do
    begin
      itm:=FExtensionItems.Items[i];
      exts:=Trim(itm.Extensions);
      if exts<>'' then
			  tmp:=tmp+itm.Language+'='+exts+CrLf;
    end;
    tmp:=tmp+CrLf;
  Result:=tmp;
end;
{$Endif}

function    TSciPropertyLoader.SaveToStream(fs : TStream) : Boolean;
var
	lst : TStringList;
	langcnt,stylcnt,kwdcnt,i,j : Integer;
{$Ifdef USENEWLOADLEXER}
  cnt : Integer;
{$Endif}

	itm : TSciLangItem;
	tmpstr,prefix,langname : String;
	tmpclr : TColor;
  aSection : String;
begin
  lst:=nil;
  if fEditor=nil then
  begin
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,[TSciPropertyLoader]);
    Result:=false;
    Exit;
  end;
	try
    fDefaultStyle.ForeColor:=fEditor.Font.Color;
    fDefaultStyle.BackColor:=fEditor.Color;
    fDefaultStyle.FontName:=fEditor.Font.Name;
    fDefaultStyle.FontSize:=fEditor.Font.Size;
    fDefaultStyle.FontStyles:=fEditor.Font.Style;
		lst:=TStringList.Create;
    tmpstr:='';
    DoPropSave(lst);

    lst.Add('['+spropDefault+']');
    aSection:=spropDefault;
		GenerateDefaultPropLine(tmpstr,fEditor);
		lst.Add(tmpstr);
    if FStoreMinimal=False then
    begin
      lst.Add(spropWordWrap+'='+ToStr(Integer(fEditor.WordWrap)));
      lst.Add(spropUnicode+'='+BoolToString(fEditor.UseUnicode));
      lst.Add(spropWordChars+'='+FEditor.WordChars);
      lst.Add(spropClearUndoAfterSave+'='+BoolToString(fEditor.ClearUndoAfterSave));
      tmpclr:=fEditor.GetCaretFore;
      lst.Add(spropCaretFore+'='+ColourToString(tmpclr)); //caretfore
      tmpclr:=fEditor.GetCaretLineBack;
      lst.Add(spropCaretBack+'='+ColourToString(tmpclr)); //caretback
      lst.Add(spropCaretLineVisible+'='+BoolToString(fEditor.GetCaretLineVisible));
      lst.Add(Format('%s=%d',[spropCaretPeriod,fEditor.GetCaretPeriod]));
      lst.Add(Format('%s=%d',[spropCaretWidth,fEditor.GetCaretWidth]));
      lst.Add(Format('%s=%d',[spropEOLMode,fEditor.GetEOLMode]));
      if fEditor is TScintillaMemo then
      begin
        lst.Add(spropSelectForeColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.SelFore));
        lst.Add(spropSelectBackColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.SelBack));
        lst.Add(spropMarkerForeColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.MarkerFore));
        lst.Add(spropMarkerBackColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.MarkerBack));
        lst.Add(spropBookMarkForeColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.BookMarkFore));
        lst.Add(spropBookMarkBackColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.BookMarkBack));
        lst.Add(spropFoldMarginHighlightColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.FoldHi));
        lst.Add(spropFoldMarginColor+'='+ColourToString(TScintillaMemo(fEditor).Colors.FoldLo));
        lst.Add(spropActiveHotspotForeColor+'='+ColourToString(TScintillaMemo(fEditor).ActiveHotSpot.ForeColor));
        lst.Add(spropActiveHotspotBackColor+'='+ColourToString(TScintillaMemo(fEditor).ActiveHotSpot.BackColor));
        lst.Add(spropActiveHotspotUnderlined+'='+BoolToString(TScintillaMemo(fEditor).ActiveHotSpot.Underlined));
        lst.Add(spropActiveHotspotSingleLine+'='+BoolToString(TScintillaMemo(fEditor).ActiveHotSpot.SingleLine));
        lst.Add(spropGutter+'='+BoolToString(Boolean(TScintillaMemo(fEditor).Gutter1.Width<>0)));
        lst.Add(spropLineNumbers+'='+BoolToString(Boolean(TScintillaMemo(fEditor).Gutter0.Width<>0)));
      end;
      lst.Add(Format('%s=%d',[spropEdgeColumn,fEditor.GetEdgeColumn]));
      lst.Add(Format('%s=%d',[spropEdgeMode,fEditor.GetEdgeMode]));
      lst.Add(spropEdgeColor+'='+ColourToString(fEditor.GetEdgeColour));
    end;
    if fEditor is TScintilla then
    begin
      if FStoreMinimal=False then
      begin
        lst.Add(spropCodeFolding+'='+BoolToString(foldFold in TScintilla(fEditor).Folding));
        lst.Add(Format('%s=%d',[spropFoldMarkerType,Integer(TScintilla(fEditor).FoldMarkerType)]));
        lst.Add(spropBraceHighlight+'='+BoolToString(TScintilla(fEditor).BraceHilite));
      end;
      if (fLexerProperties.Count<>0) then
      begin
        lst.Add('['+spropLexerPropertiesCategory+']');
        lst.AddStrings(fLexerProperties);
      end;
{$Ifndef USENEWEXTENSIONMAPPER}
      lst.Add(Extmapper.GetProps);
{$Else}
      lst.Add(GetFileExtensionProps);
{$Endif}
{$Ifdef USENEWLOADLEXER}
      if assigned(TScintilla(fEditor).LibrariesLoaded) then
      begin
        lst.Add('['+spropLexerLibraryCategory+']');
        cnt:=TScintilla(fEditor).LibrariesLoaded.Count;
        for i:=0 to (cnt-1) do
          lst.Add(spropLexerLib+'.'+ToStr(i)+'='+TScintilla(fEditor).LibrariesLoaded.Strings[i]);
      end;
{$Endif}

      langcnt:=TScintilla(fEditor).LanguageManager.LanguageList.Count;
      fDefaultStyle.FontName:=TScintilla(fEditor).Font.Name;
      fDefaultStyle.FontSize:=TScintilla(fEditor).Font.Size;
      fDefaultStyle.FontStyles:=TScintilla(fEditor).Font.Style;
      fDefaultStyle.ForeColor:=TScintilla(fEditor).Font.Color;
      fDefaultStyle.BackColor:=TScintilla(fEditor).Color;
      for i:=0 to (langcnt-1) do
      begin
        itm:=TSciLangItem(TScintilla(fEditor).LanguageManager.LanguageList.Items[i]);
        langname :=itm.Name;
        if langname='' then langname:=IntToStr(i);
        prefix :=langname+'.';
        lst.Add('['+langname+']');
        with itm do
        begin
          lst.Add(spropLexer+'='+itm.Lexer);
          if itm.NumStyleBits<>5 then
          lst.Add(spropNumStyleBits+'='+ToStr(itm.NumStyleBits));
          kwdcnt:=itm.Keywords.Count;
          for j:=0 to (kwdcnt-1) do
          begin
            GenerateKwdPropLine(tmpstr,spropKeywords+'.',TSciKeywords(itm.Keywords.Items[j]));
            if tmpstr<>'' then
            begin
              lst.Add(tmpstr);
              if j<(kwdcnt-1) then
              lst.Add('');
            end;
          end;
          stylcnt:=itm.Styles.Count;
          if (stylcnt>0) then
          begin
            lst.Add('');
          end;
          for j:=0 to (stylcnt-1) do
          begin
            GenerateStylePropLine(tmpstr,spropStyle+'.',TSciStyle(itm.Styles.Items[j]));
            if tmpstr<>'' then lst.Add(tmpstr);
          end;
          lst.Add('');
        end;
        lst.Add(spropCommentBoxStart+'='+itm.CommentBoxStart);
        lst.Add(spropCommentBoxEnd+'='+itm.CommentBoxEnd);
        lst.Add(spropCommentBoxMiddle+'='+itm.CommentBoxMiddle);
        lst.Add(spropCommentBlock+'='+itm.CommentBlock);
        lst.Add(spropCommentStreamStart+'='+itm.CommentStreamStart);
        lst.Add(spropCommentStreamEnd+'='+itm.CommentStreamEnd);
        lst.Add(spropAssignmentOperator+'='+itm.AssignmentOperator);
        lst.Add(spropEndOfStatementOperator+'='+itm.EndOfStatementOperator);
      end;
    end;

    if assigned(fOtherSettings) and (fOtherSettings.Count>0) then
    begin
      lst.Add('['+spropOther+']');
      lst.AddStrings(fOtherSettings);
    end;
		lst.SaveToStream(fs);
		Result:=true;
		Exit;
	finally
		if assigned(lst) then lst.Free;
	end;
  Result:=false;

end;
function    TSciPropertyLoader.Load : Boolean;
var
	fs : TFileStream;
begin
	fs:=nil;
	try
		try
			fs:=TFileStream.Create(fFileName,fmOpenRead);
			LoadFromStream(fs);
			Result:=true;
		except
			on EStreamError do
			begin
				Result:=false;
        raise Exception.CreateResFmt(@sCouldntLoadFromFile,[fFileName]);
			end;
		end;
	finally
		if assigned(fs) then fs.Free;
	end;
end;

// Parses the Categorynames: lexer, extension,default
function TSciPropertyLoader.ProcessDataStart(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
var
	tmpsty : TSciLangStyle;
begin
  Result:=False;
	if (AnsiCompareText(PropName,spropLexer)=0) then  // If it is a language definition
	begin
    if Editor is TScintilla then
		  TScintilla(Editor).LanguageManager.AddLanguage(CategoryName,aValue);
    Result:=True;
	end else
	if AnsiCompareText(CategoryName,spropExtension)=0 then // If it is a extension definition
	begin
    {$Ifdef USENEWEXTENSIONMAPPER}
    if PropName[1]='.' then
    begin
      ExtMapper.Add(PropName,aValue);
    end else
		  ExtMapper.Add(aValue,PropName);
    {$Else}
      ExtMapper.Add(PropName,aValue);
    {$Endif}
		Result:=True;
	end else
	if AnsiCompareText(CategoryName,spropLexerPropertiesCategory)=0 then // If it is a extension definition
	begin
    fLexerProperties.Add(PropName+'='+aValue);
		Result:=True;
	end else
	if (AnsiCompareText(CategoryName,spropDefault)=0)  then //If it is the default style
	begin
    if AnsiCompareText(spropCaretFore,PropName)=0 then //caretfore
		begin
			fEditor.SetCaretFore(ColorToRGB(ColourFromString(aValue,clBlack)));
			Result:=True;
		end else if AnsiCompareText(spropCaretBack,PropName)=0 then //caretlineback
		begin
			fEditor.SetCaretLineBack(ColorToRGB(ColourFromString(aValue,clSilver)));
			Result:=True;
		end else if AnsiCompareText(spropCaretPeriod,PropName)=0 then
		begin
			fEditor.SetCaretPeriod(ToInt(aValue));
			Result:=True;
		end else if AnsiCompareText(spropCaretWidth,PropName)=0 then
		begin
			fEditor.SetCaretWidth(ToInt(aValue));
			Result:=True;
		end else if AnsiCompareText(spropCaretLineVisible,PropName)=0 then
		begin
				fEditor.SetCaretLineVisible(StringToBool(aValue));
			Result:=True;
		end	else if (AnsiCompareText(PropName,spropStyle)=0) then
		begin
			tmpsty.EOLFilled:=false;
			tmpsty.Visible:=True;
			tmpsty.CharCase:=CASE_MIXED;
			tmpsty.Changeable:=True;
			tmpsty.Hotspot:=False;
      tmpsty.FontStyles:=[];
      tmpsty.FontName:='';
      tmpsty.FontSize:=0;
      tmpsty.ForeColor:=clDefault;
      tmpsty.BackColor:=clDefault;
			ParseStyleDefinition(PChar(aValue),tmpsty,true);
      fDefaultStyle.FontName:=tmpsty.FontName;
      fDefaultStyle.FontSize:=tmpsty.FontSize;
      fDefaultStyle.FontStyles:=tmpsty.FontStyles;
      fDefaultStyle.ForeColor:=tmpsty.ForeColor;
      fDefaultStyle.BackColor:=tmpsty.BackColor;
      fDefaultStyle.CharCase:=tmpsty.CharCase;
      fDefaultStyle.Visible:=tmpsty.Visible;
      fDefaultStyle.Changeable:=tmpsty.Changeable;
      fDefaultStyle.Hotspot:=tmpsty.Hotspot;
      fDefaultStyle.EOLFilled:=tmpsty.EOLFilled;
			fEditor.Color:=tmpsty.BackColor;
			fEditor.Font.Color:=tmpsty.ForeColor;
			fEditor.Font.Name:=tmpsty.FontName;
			fEditor.Font.Size:=tmpsty.FontSize;
			fEditor.Font.Style:=tmpsty.FontStyles;
			Result:=True;
		end else if (AnsiCompareText(PropName,spropWordWrap)=0) then //WordWrap
		begin
			fEditor.WordWrap:=TWordWrapType(ToInt(aValue));
			Result:=True;
		end else if (AnsiCompareText(PropName,spropWordChars)=0) then //WordWrap
		begin
			fEditor.WordChars:=aValue;
			Result:=True;
		end	else if (AnsiCompareText(PropName,spropEOLMode)=0) then			//SelBack
    begin
      fEditor.SetEOLMode(ToInt(aValue));
      Result:=True;
    end else if (AnsiCompareText(PropName,spropUnicode)=0) then			//UseUnicode
		begin
			fEditor.UseUnicode:=StringToBool(aValue);
			Result:=True;
		end	else if (AnsiCompareText(PropName,spropClearUndoAfterSave)=0) then			//ClearUndoAfterSave
		begin
			fEditor.ClearUndoAfterSave:=StringToBool(aValue);
			Result:=True;
		end	else if (AnsiCompareText(PropName,spropEdgeColumn)=0) then			//ClearUndoAfterSave
		begin
			fEditor.SetEdgeColumn(ToInt(aValue));
			Result:=True;
		end else if (AnsiCompareText(PropName,spropEdgeColor)=0) then			//ClearUndoAfterSave
		begin
			fEditor.SetEdgeColour(ColourFromString(aValue,clDefault));
			Result:=True;
		end else if (AnsiCompareText(PropName,spropEdgeMode)=0) then			//ClearUndoAfterSave
		begin
			fEditor.SetEdgeMode(ToInt(aValue));
			Result:=True;
		end else if (fEditor is TScintillaMemo) then
    begin
      if (AnsiCompareText(PropName,spropLineNumbers)=0) then			//ClearUndoAfterSave
      begin
        if StringToBool(aValue)=True then
        begin
          TScintillaMemo(fEditor).Gutter0.Width:=32;
          TScintillaMemo(fEditor).Gutter0.MarginType:=gutLineNumber;
        end else
        begin
          TScintillaMemo(fEditor).Gutter0.Width:=0;
        end;
        Result:=True;
      end else if (AnsiCompareText(PropName,spropGutter)=0) then			//ClearUndoAfterSave
      begin
        if StringToBool(aValue)=True then
        begin
          TScintillaMemo(fEditor).Gutter1.Width:=16;
          TScintillaMemo(fEditor).Gutter1.MarginType:=gutSymbol;
        end else
        begin
          TScintillaMemo(fEditor).Gutter1.Width:=0;
        end;
        Result:=True;
      end else
      if (AnsiCompareText(PropName,spropSelectForeColor)=0) then			//SelFore
      begin
        TScintillaMemo(fEditor).Colors.SelFore:=ColourFromString(aValue,clYellow);
        Result:=True;
      end	else if (AnsiCompareText(PropName,spropMarkerForeColor)=0) then
      begin
        TScintillaMemo(fEditor).Colors.MarkerFore:=ColourFromString(aValue,clYellow);
        Result:=True;
      end	else if (AnsiCompareText(PropName,spropMarkerBackColor)=0) then
      begin
        TScintillaMemo(fEditor).Colors.MarkerBack:=ColourFromString(aValue,clBlue);
        Result:=True;
      end	else if (AnsiCompareText(PropName,spropBookMarkForeColor)=0) then
      begin
        TScintillaMemo(fEditor).Colors.BookMarkFore:=ColourFromString(aValue,clWhite);
        Result:=True;
      end	else if (AnsiCompareText(PropName,spropBookMarkBackColor)=0) then
      begin
        TScintillaMemo(fEditor).Colors.BookMarkBack:=ColourFromString(aValue,clGray);
        Result:=True;
      end	else if (AnsiCompareText(PropName,spropFoldMarginHighlightColor)=0) then
      begin
        TScintillaMemo(fEditor).Colors.FoldHi:=ColourFromString(aValue,clBlack);
        Result:=True;
      end	else if (AnsiCompareText(PropName,spropFoldMarginColor)=0) then
      begin
        TScintillaMemo(fEditor).Colors.FoldLo:=ColourFromString(aValue,clBlack);
        Result:=True;
      end	else
      if (AnsiCompareText(PropName,spropSelectBackColor)=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).Colors.SelBack:=ColourFromString(aValue,clNavy);
        Result:=True;
      end else if (AnsiCompareText(PropName,spropActiveHotspotForeColor)=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.ForeColor:=ColourFromString(aValue,clYellow);
        Result:=True;
      end else if (AnsiCompareText(PropName,spropActiveHotspotBackColor)=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.BackColor:=ColourFromString(aValue,clMaroon);
        Result:=True;
      end else if (AnsiCompareText(PropName,spropActiveHotspotUnderlined)=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.Underlined:=StringToBool(aValue);
        Result:=True;
      end else if (AnsiCompareText(PropName,spropActiveHotspotSingleLine)=0) then			//SelBack
      begin
        TScintillaMemo(fEditor).ActiveHotSpot.SingleLine:=StringToBool(aValue);
        Result:=True;
      end;

      if fEditor is TScintilla then
      begin
        if (AnsiCompareText(PropName,spropCodeFolding)=0) then			//CodeFolding
        begin
          if StringToBool(aValue)=True then
            TScintilla(fEditor).Folding:=TScintilla(fEditor).Folding+[foldFold]
          else
            TScintilla(fEditor).Folding:=TScintilla(fEditor).Folding-[foldFold];
          Result:=True;
        end else if (AnsiCompareText(PropName,spropBraceHighlight)=0) then			//SelBack
        begin
          TScintilla(fEditor).BraceHilite:=StringToBool(aValue);
          Result:=True;
        end else if (AnsiCompareText(PropName,spropFoldMarkerType)=0) then
        begin
          TScintilla(fEditor).FoldMarkerType:=sciMarkerType(ToInt(aValue));
          Result:=True;
        end
      end;
    end; //scintillamemo
	end
{$Ifdef USENEWLOADLEXER}
  else if (Editor is TScintilla) and (AnsiCompareText(CategoryName,spropLexerLibraryCategory)=0) then // If it is a library
	begin
    if AnsiCompareText(PropName,spropLexerLib)=0 then
      TScintilla(Editor).LoadLexerLibrary(aValue);
		Result:=True;
	end;
{$Else}
;
{$Endif}

end;

const
dataendset =['s','S','k','K','c','C','A','a','e','E','n','N'];

// Parses the Propnames: style,keywords
function TSciPropertyLoader.ProcessDataEnd(var CategoryName,PropName : String;var Num : Integer;var aValue : String) : Boolean;
var
	kwdcnt,stycnt,srchend,j : Integer;
	wasdone : Boolean;
	tmpsty  : TSciLangStyle;
	stytmp  : TSciStyle;
	itm     : TSciLangItem;
	tmpkwd : TSciLangKeywords;
	kwdtmp : TSciKeywords;
	kwdname : String;
  c:Char;
begin
  c:=PropName[1];
  if not (c in dataendset) then
  begin
    Result:=False;
    Exit;
  end;

  if (AnsiCompareText(PropName,spropStyle)=0) or
  (AnsiCompareText(PropName,spropKeywords)=0) or
  (AnsiCompareText(PropName,spropCommentBoxStart)=0) or
  (AnsiCompareText(PropName,spropCommentBoxEnd)=0) or
  (AnsiCompareText(PropName,spropCommentBoxMiddle)=0) or
  (AnsiCompareText(PropName,spropCommentBlock)=0) or
  (AnsiCompareText(PropName,spropCommentStreamStart)=0) or
  (AnsiCompareText(PropName,spropCommentStreamEnd)=0) or
  (AnsiCompareText(PropName,spropAssignmentOperator)=0) or
  (AnsiCompareText(PropName,spropEndOfStatementOperator)=0) or
  (AnsiCompareText(PropName,spropNumStyleBits)=0) then
  begin
    if fEditor is TScintilla then
    begin
      Result:=false;
      itm:=TScintilla(fEditor).LanguageManager.LanguageList.Find(CategoryName);
      if itm=nil then
      begin
        itm:=TSciLangItem(TScintilla(fEditor).LanguageManager.LanguageList.Add);
        itm.Name:=CategoryName;
        itm.Lexer:=CategoryName;
      end;

      if assigned(itm) then
      begin
        if (AnsiCompareText(PropName,spropStyle)=0) then // If it is a style definition
        begin
          stycnt:=itm.Styles.Count;
          wasdone:=false;
          for j:=0 to (stycnt-1) do //Check if the style is in the list, and if so, just modify it.
          begin
            if TSciStyle(itm.Styles.Items[j]).StyleNumber=Num then
            begin
              TSciStyle(itm.Styles.Items[j]).AssignToRec(tmpsty);
              ParseStyleDefinition(PChar(aValue),tmpsty);
              TSciStyle(itm.Styles.Items[j]).AssignRec(tmpsty);
              wasdone:=true;
              Break;
            end;
          end;
          if wasdone=false then // If the style wasn't found
          begin
            stytmp:=TSciStyle(itm.Styles.Add);
            stytmp.AssignToRec(tmpsty);
            FillWithDefaultStyle(tmpsty);
            tmpsty.StyleNumber:=Num;
            ParseStyleDefinition(PChar(aValue),tmpsty);
            stytmp.AssignRec(tmpsty);
          end;

            //If the fEditor.parameter isn't nil and we're modifying the
            //currently active language then update display.


          //if itm.LanguageStr=fEditor.Highlighter.LanguageStr then
          //fEditor.Highlighter.Update;
          Result:=True;
        end else
        if AnsiCompareText(PropName,spropKeywords)=0 then // If it is a keyword definition
        begin
          kwdcnt:=itm.Keywords.Count;
          wasdone:=false;
          if (AnsiStrLIComp(PChar(spropName+':'),PChar(aValue),5)=0) then // If it's a keywordlist description
          begin
            srchend:=Pos('::',AValue);
            if srchend<>0 then
            begin
              kwdname:=Copy(aValue,6,srchend-6);
              Delete(aValue,1,srchend+1);
              kwdname:=ReplaceAChar(kwdname,'|',',');
            end else kwdname:='';
          end else kwdname:=''; //has'nt a keywordlist description

          for j:=0 to (kwdcnt-1) do //Check if the style is in the list, and if so, just modify it.
          begin
            if TSciKeywords(itm.Keywords.Items[j]).KeywordListNumber=Num then
            begin
              TSciKeywords(itm.Keywords.Items[j]).AssignToRec(tmpkwd);
              tmpkwd.KeyWords:=aValue;
              if kwdname<>'' then tmpkwd.Name:=kwdname;
              TSciKeywords(itm.Keywords.Items[j]).AssignRec(tmpkwd);
              wasdone:=true;
              Break;
            end;
          end;
          if wasdone=false then // If the keywordlist wasn't found
          begin
            kwdtmp:=TSciKeywords(itm.Keywords.Add);
            kwdtmp.AssignToRec(tmpkwd);
            if kwdname<>'' then tmpkwd.Name:=kwdname;
            tmpkwd.KeywordListNumber:=Num;
            tmpkwd.KeyWords:=aValue;
            kwdtmp.AssignRec(tmpkwd);
          end;//wasdone
          Result:=True;
        end else
          if (AnsiCompareText(PropName,spropCommentBoxStart)=0) then
          begin
            itm.CommentBoxStart:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropCommentBoxEnd)=0) then
          begin
            itm.CommentBoxEnd:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropCommentBoxMiddle)=0) then
          begin
            itm.CommentBoxMiddle:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropCommentBlock)=0) then
          begin
            itm.CommentBlock:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropCommentStreamStart)=0) then
          begin
            itm.CommentStreamStart:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropCommentStreamEnd)=0) then
          begin
            itm.CommentStreamEnd:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropAssignmentOperator)=0) then
          begin
            itm.AssignmentOperator:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropEndOfStatementOperator)=0) then
          begin
            itm.EndOfStatementOperator:=UnSlash(aValue);
            Result:=True;
          end else if (AnsiCompareText(PropName,spropNumStyleBits)=0) then
          begin
            itm.NumStyleBits:=ToInt(aValue);
            Result:=True;
          end;
      end;//itm<>nil
    end else//is not TScintilla
    Result:=True;
  end else
    Result:=False;
end;

function    TSciPropertyLoader.GetInt(const SettingName : String;const DefaultValue : Integer=0;const AddIfNonExistent : Boolean=False) : Integer;
begin
  Result:=ToInt(GetOther(SettingName,ToStr(DefaultValue),AddIfNonExistent));
end;
function    TSciPropertyLoader.GetBool(const SettingName : String;const DefaultValue : Boolean=False;const AddIfNonExistent : Boolean=False) : Boolean;
begin
  Result :=StringToBool(GetOther(SettingName,BoolToString(DefaultValue),AddIfNonExistent));
end;

procedure   TSciPropertyLoader.AddInt(const SettingName : String;const value : Integer);
begin
  AddOther(SettingName,ToStr(value));
end;

procedure   TSciPropertyLoader.AddBool(const SettingName : String;const value : Boolean);
begin
  AddOther(SettingName,BoolToString(value));
end;

procedure TSciPropertyLoader.AddOther(const SettingName : String;const value : String);
var
idx : Integer;
begin
  idx:=fOtherSettings.IndexOfName(SettingName);
  if idx<>-1 then
  begin
    fOtherSettings.Delete(idx);
    fOtherSettings.Add(SettingName+'='+value);
  end
  else
    fOtherSettings.Add(SettingName+'='+value);
end;

function TSciPropertyLoader.GetOther(const SettingName : String;const DefaultValue : String;const AddIfNonExistent : Boolean) : String;
begin
  if fOtherSettings.IndexOfName(SettingName)<>-1 then
  begin
    Result:=fOtherSettings.Values[SettingName];
  end else
  begin
    if AddIfNonExistent=True then AddOther(SettingName,DefaultValue);
    Result:=DefaultValue;
  end;
end;

function    TSciPropertyLoader.Save : Boolean;
var
  tmpstrm : TMemoryStream;
	fs : TFileStream;
begin
	fs:=nil;
  tmpstrm:=nil;
	try
		try
      tmpstrm:=TMemoryStream.Create;
			SaveToStream(tmpstrm);
      if tmpstrm.Size>2 then
      begin
        fs:=TFileStream.Create(fFileName,fmCreate);
        fs.CopyFrom(tmpstrm,0);
      end;
			Result:=true;
		except
			on EStreamError do
			begin
				Result:=false;
        raise Exception.CreateResFmt(@sCouldntWriteToFile,[fFileName]);
			end;
		end;
	finally
		if assigned(fs) then fs.Free;
    if assigned(tmpstrm) then tmpstrm.Free;
	end;
end;

{$Ifndef USENEWEXTENSIONMAPPER}
function ExtMapper : TExtensionMapper;
begin
  if FExtMap=nil then
  FExtMap:=TExtensionMapper.Create;
  Result:=FExtMap;
end;

constructor TExtensionMapper.Create;
begin
  extensions:=nil;
  FillExtensionMap;
end;
destructor TExtensionMapper.Destroy;
begin
  if assigned(extensions) then extensions.Free;
  inherited;
end;

procedure TExtensionMapper.SetExtensions(Value : TStrings);
begin
	if assigned(Value) then
		extensions.Assign(Value)
	else
		extensions.Clear;
end;

{Fill the list with default extensionmappings.
}
procedure TExtensionMapper.FillExtensionMap;
begin
  if extensions=nil then extensions:=TStringList.Create;
  with extensions do
  begin
		Add('.c=C++/C');Add('.cc=C++/C');Add('.cpp=C++/C');Add('.cxx=C++/C');Add('.h=C++/C');Add('.hh=C++/C');Add('.hxx=C++/C');Add('.hpp=C++/C');
    Add('.cs=C#');
		Add('.rc=Resource');Add('.rc2=Resource');Add('.dlg=Resource');
		Add('.htm=HTML');Add('.html=HTML');Add('.asp=HTML');Add('.shtml=HTML');Add('.htd=HTML');Add('.htt=HTML');Add('.cfm=HTML');Add('.tpl=HTML');Add('.hta=HTML');
    Add('.php=PHP');Add('.php3=PHP');Add('.phtml=PHP');
		Add('.xml=XML');Add('.xsl=XML');Add('.svg=XML');Add('.xul=XML');Add('.xsd=XML');Add('.dtd=XML');Add('.xslt=XML');Add('.axl=XML');Add('.xrc=XML');Add('.bpk=XML');Add('.dpk=XML');Add('.dpr=XML');Add('.bpr=XML');Add('.ant=XML');
		Add('.wml=WML');
		Add('.pl=PERL');Add('.pm=PERL');Add('.cgi=PERL');Add('.pod=PERL');
		Add('.css=CSS');
		Add('.pas=Pascal');Add('.inc=Pascal');
		Add('.sql=SQL');
		Add('.vb=VB');Add('.bas=VB');Add('.frm=VB');Add('.cls=VB');Add('.ctl=VB');Add('.pag=VB');Add('.dsr=VB');Add('.dob=VB');
		Add('.vbs=VBScript');Add('.dsm=VBScript');
		Add('.java=Java');
		Add('.js=JavaScript');
		Add('.idl=IDL');Add('.odl=IDL');
		Add('.py=Python');Add('.pyw=Python');
		Add('.tcl=TCL/TK');
		Add('.lua=LUA');
    Add('.mib=asn1');
    Add('.vhdl=vhdl');
		Add('.diff=Diff');Add('.patch=Diff');
		Add('.mak=Makefile');Add('makefile=Makefile');Add('.make=Makefile');Add('.iface=Makefile');
		Add('.bat=Batch');Add('.cmd=Batch');Add('.nt=Batch');
		Add('.conf=Apache Config');
		Add('.properties=Properties');Add('.ini=Properties');Add('.inf=Properties');Add('.reg=Properties');Add('.url=Properties');Add('.cfg=Properties');Add('.cnf=Properties');
		Add('.ads=ADA');Add('.adb=ADA');
		Add('.spf=nnCronTab');Add('.tab=nnCronTab');
		Add('.lsp=Lisp');Add('.lisp=Lisp');
		Add('.scm=Scheme');Add('.smd=Scheme');Add('.ss=Scheme');
		Add('.au3=AutoIt 3');
    Add('.tex=tex');Add('.sty=tex');
    Add('.sh=bash');Add('.bsh=bash');
    Add('.f90=Fortran');Add('.f95=Fortran');Add('.f2k=Fortran');
    Add('.f=Fortran77');Add('.for=Fortran77');
    Add('.em=EScript');Add('.src=EScript');
    Add('.e=Eiffel');
    Add('.bc=Baan');Add('.cln=Baan');
    Add('.ave=Avenue');
    Add('.asm=ASM');Add('.kix=KIX');
    extsadded:=0;
  end;
end;

// Add a language mapping.. New mappings overrides previous
procedure TExtensionMapper.Add(const mapfromext : String;const maptolexer : String);
var
  tmpstr : String;
begin
  if extensions=nil then Exit;
  if (mapfromext<>'') and (maptolexer<>'') then
  begin
  if mapfromext[1]<>'.' then
    tmpstr:='.'+mapfromext
  else tmpstr:=mapfromext;
  tmpstr:=tmpstr+'='+maptolexer;
  if extensions.IndexOf(tmpstr)=-1 then
  extensions.Insert(0,tmpstr); //Insert at top to override previous assignment
  Inc(extsadded);
  end;
end;

// Returns any extra languages that has been added. Used by SciPropertyMgr
function TExtensionMapper.GetProps : String;
var
  tmp : String;
  i : Integer;
begin
  tmp:='';
  if extensions=nil then
  begin
    Result:=tmp;
    Exit;
  end;
  if extsadded>0 then
  begin
    tmp:=CrLf+'['+spropExtension+']'+CrLf;
    for i:=0 to (extsadded-1) do
    begin
      if extensions.Names[i][1]<>'.' then
      tmp:=tmp+'.'+extensions.Strings[i]+CrLf
			else tmp:=tmp+extensions.Names[i]+'='+ValueFromIndex(extensions,i)+CrLf;
    end;
    tmp:=tmp+CrLf;
  end;
  Result:=tmp;
end;

function TExtensionMapper.DetectLexer(const extension : string;var TheWordChars : String) : String;
var
	i : Integer;
begin
  if extensions=nil then
  begin
    Result:='null';
    Exit;
  end;
  i:=extensions.IndexOfName(extension);
  if(i<>-1) then
  begin
		Result:=ValueFromIndex(extensions,i);
	end else
  Result :='null';
end;
initialization

finalization
FreeAndNil(FExtMap); //added to avoid a memoryleak after a tip by mak
{$Endif}

end.
