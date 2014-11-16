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
	Author : hdalis (Jan Martin Pettersen)
	Created: 18/04/2005
			$Id: SciFileExtensionsManager.pas,v 1.0 $
	Purpose: Manages file extensions

    Usage: Mostly internally used. Access the class via the ExtMapper function

	History: 18/04/2005 First release
											Moved the ExtMapper from SciPropertyMgr to SciFileExtensionsManager.Pas, and
											changed the way the extensions are stored.. Now it's a collection of items,
											with two properties. Language, and Extensions. Use ExtMapper.Add to add new extensions.

  Differences from the original ExtensionMapper:
    - Renamed the DetectLexer function to the more logical DetectLanguage
    - Added the possibility to remove a extension/language from the extensionmapping.
    - Added direct access to the extensions via the property ExtensionsList.
    - FillExtensionsMap is not called automatically because if the language isn't
    in the languagemanager, then it's possible it is created on the fly (with the NULL
    settings), and that may not be what you wished for.
    - FillExtensionsMap can now verify that a language exists before adding the extensions
    pertaining to that language.


}
{$Include SciCommonDef.Inc}
unit SciFileExtensionsManager;
interface
{$Ifdef USENEWEXTENSIONMAPPER}
uses Classes,ScintillaLanguageManager;
type
  TExtensionItem=class(TCollectionItem)
  private
    FLanguage : String;
    FExtensions : String;
    procedure SetLanguage(const Value : String);
    procedure SetExtensions(const Value : String);
  public
    procedure Assign(Source: TPersistent);override;
  published
    property Language : String read FLanguage write SetLanguage;
    property Extensions : String read FExtensions write SetExtensions;
  end;
  TExtensionsToLanguageList=class(TCollection)
  private
    FExtensionSeparator : AnsiChar;
    lastAccessed : TExtensionItem;
    procedure SetItem(Index:Integer; Value: TExtensionItem);
    function GetItem(Index:Integer): TExtensionItem;
    function LanguageFromExt(const ExtensionToFind : String;const Separator : AnsiChar) : String;
    function RemoveFromList(const ExtensionToFind : String;const Separator : AnsiChar) : Boolean;
    function AddIfNotSame(const MapFromWhatExtension : String;const ToWhatLanguage : String;const Separator : AnsiChar) : Boolean;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    procedure Assign(Source: TPersistent);override;
    property Items[Index : Integer] : TExtensionItem read GetItem write SetItem;
    function Add: TExtensionItem;
    procedure Delete(Index: Integer);
    procedure MapLanguageToExts(const WhatLanguage : String;const AllExtensions : String;CheckWith : TSciLanguageManager=nil);
    function GetLangItem(const WhatLanguage : String) : TExtensionItem;
  published
    property ExtensionSeparator : AnsiChar read FExtensionSeparator write FExtensionSeparator default ';';
  end;

  TExtensionMapper=class(TObject)
  private
    FExtensionItems : TExtensionsToLanguageList;
    procedure SetExtensionItems(Value : TExtensionsToLanguageList);
  public
    constructor Create;
    destructor  Destroy;override;
    procedure Add(const MapFromWhatExtension : String;const ToWhatLanguage : String);
    function DetectLanguage(const ExtensionToFind : string) : String;
    procedure Remove(const ExtensionToRemove : String);
    procedure RemoveAllWithLang(const MappedToWhatLang : String);
    procedure FillExtensionMap(LanguageManager : TSciLanguageManager=nil);
    property ExtensionsList : TExtensionsToLanguageList read FExtensionItems write SetExtensionItems;
  end;

function  ExtMapper : TExtensionMapper;
implementation
uses SciResLang,sciUtils,SysUtils;
var
  FExtMap : TExtensionMapper;

function ExtMapper : TExtensionMapper;
begin
  if FExtMap=nil then
  FExtMap:=TExtensionMapper.Create;
  Result:=FExtMap;
end;

constructor TExtensionMapper.Create;
begin
  FExtensionItems:=TExtensionsToLanguageList.Create(TExtensionItem);
end;
destructor TExtensionMapper.Destroy;
begin
  if assigned(FExtensionItems) then FExtensionItems.Free;
  inherited;
end;

{Fill the list with default extensionmappings.
}
procedure TExtensionMapper.FillExtensionMap(LanguageManager : TSciLanguageManager);
begin
  with FExtensionItems do
  begin
		MapLanguageToExts('C++/C','.c;.cc;.cpp;.cxx;.h;.hh;.hxx;.hpp',LanguageManager);
    MapLanguageToExts('C#','.cs',LanguageManager);
		MapLanguageToExts('Resource','.rc;.rc2;.dlg',LanguageManager);
		MapLanguageToExts('HTML','.htm;.html;.asp=;.shtml;.htd;.htt;.cfm;.tpl;.hta',LanguageManager);
    MapLanguageToExts('PHP','.php;.php3;.phtml',LanguageManager);
		MapLanguageToExts('XML','.xml;.xsl;.xslt;.svg;.xul;.xsd;.dtd;.axl;.xrc;.bpk;.bpr;.ant',LanguageManager);
		MapLanguageToExts('WML','.wml',LanguageManager);
		MapLanguageToExts('PERL','.pl;.pm;.cgi;.pod',LanguageManager);
		MapLanguageToExts('CSS','.css',LanguageManager);
		MapLanguageToExts('Pascal','.pas;.inc;.dpk;.dpr',LanguageManager);
		MapLanguageToExts('SQL','.sql',LanguageManager);
		MapLanguageToExts('VB','.vb;.bas;.frm;.cls;.ctl;.pag;.dsr;.dob',LanguageManager);
		MapLanguageToExts('VBScript','.vbs;.dsm',LanguageManager);
		MapLanguageToExts('Java','.java',LanguageManager);
		MapLanguageToExts('JavaScript','.js',LanguageManager);
		MapLanguageToExts('IDL','.idl;.odl',LanguageManager);
		MapLanguageToExts('Python','.py;.pyw',LanguageManager);
		MapLanguageToExts('TCL/TK','.tcl',LanguageManager);
		MapLanguageToExts('LUA','.lua',LanguageManager);
    MapLanguageToExts('asn1','.mib',LanguageManager);
    MapLanguageToExts('vhdl','.vhdl',LanguageManager);
		MapLanguageToExts('Diff','.diff;.patch',LanguageManager);
		MapLanguageToExts('Makefile','.mak;makefile;.make;.iface=Makefile',LanguageManager);
		MapLanguageToExts('Batch','.bat;.cmd;.nt',LanguageManager);
		MapLanguageToExts('Apache Config','.conf',LanguageManager);
		MapLanguageToExts('Properties','.properties;.ini;.inf;.reg;.url;.cfg;.cnf',LanguageManager);
		MapLanguageToExts('ADA','.ads;.adb',LanguageManager);
		MapLanguageToExts('nnCronTab','.spf;.tab',LanguageManager);
		MapLanguageToExts('Lisp','.lsp;.lisp',LanguageManager);
		MapLanguageToExts('Scheme','.scm;.smd;.ss',LanguageManager);
		MapLanguageToExts('AutoIt_3','.au3',LanguageManager);
    MapLanguageToExts('TEX','.tex;.sty',LanguageManager);
    MapLanguageToExts('BASH','.sh;.bsh',LanguageManager);
    MapLanguageToExts('Fortran','.f90;.f95;.f2k',LanguageManager);
    MapLanguageToExts('Fortran77','.f;.for',LanguageManager);
    MapLanguageToExts('EScript','.em;.src',LanguageManager);
    MapLanguageToExts('Eiffel','.e',LanguageManager);
    MapLanguageToExts('Baan','.bc;.cln',LanguageManager);
    MapLanguageToExts('Avenue','.ave',LanguageManager);
    MapLanguageToExts('ASM','.asm',LanguageManager);
    MapLanguageToExts('KIX','.kix',LanguageManager);
    MapLanguageToExts('Objective_Caml','.ml;.mli',LanguageManager);
    MapLanguageToExts('BlitzBasic','.bb',LanguageManager);
    MapLanguageToExts('PureBasic','.pb',LanguageManager);
  end;
end;

// Add a language mapping.. New mappings overrides previous
procedure TExtensionMapper.Add(const MapFromWhatExtension : String;const ToWhatLanguage : String);
var
  restOfString,CurrentWord : String;
begin
  if FExtensionItems=nil then Exit;
  restOfString:=MapFromWhatExtension;
  repeat
    CurrentWord:=WordBefore(restOfString,FExtensionItems.ExtensionSeparator);
    if CurrentWord<>'' then
      FExtensionItems.AddIfNotSame(CurrentWord,ToWhatLanguage,FExtensionItems.ExtensionSeparator);
  until restOfString='';
end;

function TExtensionsToLanguageList.RemoveFromList(const ExtensionToFind : String;const Separator : AnsiChar) : Boolean;
var
  itemCount,i : Integer;
  extensionItem : TExtensionItem;
begin
  Result:=False;
  itemCount:=Count;
  for i:=0 to (itemCount-1) do
  begin
    extensionItem:=Items[i];
    if IsWordInString(extensionItem.FExtensions,ExtensionToFind,True,separator) then
    begin
      extensionItem.Extensions:=BuildWordlistExcept(extensionItem.Extensions,ExtensionToFind,True,separator);
      lastAccessed:=extensionItem;
    end;
  end;
end;

procedure TExtensionMapper.RemoveAllWithLang(const MappedToWhatLang : String);
var
i,itemCount : Integer;
begin
  itemCount:=FExtensionItems.Count;
  for i:=0 to (itemCount-1) do
  begin
    if FExtensionItems.Items[i].FLanguage=MappedToWhatLang then
    begin
      FExtensionItems.Delete(i);
      Exit;
    end;
  end;
end;

function TExtensionsToLanguageList.AddIfNotSame(const MapFromWhatExtension : String;const ToWhatLanguage : String;const Separator : AnsiChar) : Boolean;
var
  itemCount,i : Integer;
  foundPos : Integer;
  extensionItem : TExtensionItem;
begin
  Result:=False;
  if (MapFromWhatExtension='') or (ToWhatLanguage='') then Exit;
  itemCount:=Count;
  if assigned(lastAccessed) and (lastAccessed.FLanguage=ToWhatLanguage) then
  begin
    if (IsWordInString(lastAccessed.FExtensions,MapFromWhatExtension,True,separator)) then
    begin
      Result:=True;
      Exit;
    end;
  end;

  for i:=0 to (itemCount-1) do
  begin
    extensionItem:=Items[i];
    if IsWordInString(extensionItem.FExtensions,MapFromWhatExtension,True,separator) then
    begin
      if extensionItem.FLanguage=ToWhatLanguage then
      begin
        lastAccessed:=extensionItem;
        Result:=False;
        Exit;
      end;
      foundPos:=Pos(MapFromWhatExtension,extensionItem.FExtensions);
      if(foundPos>0) then
      begin
        extensionItem.Extensions:=BuildWordlistExcept(extensionItem.Extensions,MapFromWhatExtension,True,separator);
      end;
    end;
    if extensionItem.FLanguage=ToWhatLanguage then
    begin
      if Trim(extensionItem.FExtensions)<>'' then
        extensionItem.Extensions:=extensionItem.FExtensions+Separator+MapFromWhatExtension
      else extensionItem.Extensions:=MapFromWhatExtension;
      Result:=True;
      lastAccessed:=extensionItem;
      Exit;
    end;
  end;
  extensionItem:=Add;
  extensionItem.Language:=ToWhatLanguage;
  extensionItem.Extensions:=MapFromWhatExtension;
  lastAccessed:=extensionItem;
  Result:=True;
end;

procedure   TExtensionMapper.Remove(const ExtensionToRemove : String);
begin
  FExtensionItems.RemoveFromList(ExtensionToRemove,FExtensionItems.ExtensionSeparator);
end;

function TExtensionsToLanguageList.LanguageFromExt(const ExtensionToFind : String;const Separator : AnsiChar) : String;
var
  itemCount,i : Integer;
  extensionItem : TExtensionItem;
begin
  Result:='null';
  if assigned(lastAccessed) then
  begin
    if IsWordInString(lastAccessed.FExtensions,ExtensionToFind,True,separator) then
    begin
      Result:=lastAccessed.FLanguage;
      Exit;
    end;
  end;
  itemCount:=Count;
  for i:=0 to (itemCount-1) do
  begin
    extensionItem:=Items[i];
    if IsWordInString(extensionItem.FExtensions,ExtensionToFind,True,separator) then
    begin
      Result:=extensionItem.FLanguage;
      lastAccessed:=extensionItem;
      Exit;
    end;
  end;
end;

function TExtensionsToLanguageList.GetLangItem(const WhatLanguage : String) : TExtensionItem;
var
  extensionItem : TExtensionItem;
  i,itemCount : Integer;
begin
  if assigned(lastAccessed) then
  begin
    if lastAccessed.FLanguage=WhatLanguage then
    begin
      Result:=lastAccessed;
      Exit;
    end;
  end;
  Result:=nil;
  itemCount:=Count;
  for i:=0 to (itemCount-1) do
  begin
    extensionItem:=Items[i];
    if extensionItem.FLanguage=WhatLanguage then
    begin
      Result:=extensionItem;
      lastAccessed:=extensionItem;
      Exit;
    end;
  end;
end;
procedure TExtensionsToLanguageList.MapLanguageToExts(const WhatLanguage : String;const AllExtensions : String;CheckWith : TSciLanguageManager);
var
  extensionItem : TExtensionItem;
begin
  if assigned(CheckWith) then
    if CheckWith.HasLanguage(WhatLanguage)=False then
      Exit;
  extensionItem:=GetLangItem(WhatLanguage);
  if not assigned(extensionItem) then
  begin
    extensionItem:=Add;
    extensionItem.Language:=WhatLanguage;
    lastAccessed:=extensionItem;
  end;
  extensionItem.Extensions:=AllExtensions;
end;

function TExtensionMapper.DetectLanguage(const ExtensionToFind : string) : String;
begin
  if FExtensionItems=nil then
  begin
    Result:='null';
    Exit;
  end;
  Result:=FExtensionItems.LanguageFromExt(ExtensionToFind,FExtensionItems.ExtensionSeparator);
end;

procedure TExtensionMapper.SetExtensionItems(Value : TExtensionsToLanguageList);
begin
  FExtensionItems.Assign(Value);
end;

procedure TExtensionItem.Assign(Source: TPersistent);
begin
  if Source is TExtensionItem then
  begin
    FLanguage:=TExtensionItem(Source).Language;
    FExtensions:=TExtensionItem(Source).Extensions;
  end else
  Inherited;
end;
procedure TExtensionItem.SetLanguage(const Value : String);
begin
  FLanguage:=Value;
  Changed(False);
end;

procedure TExtensionItem.SetExtensions(const Value : String);
begin
  FExtensions:=Value;
  Changed(False);
end;

procedure TExtensionsToLanguageList.SetItem(Index:Integer; Value: TExtensionItem);
begin
  inherited SetItem(Index,Value);
end;
function TExtensionsToLanguageList.GetItem(Index:Integer): TExtensionItem;
begin
  Result:=TExtensionItem(inherited GetItem(Index));
end;
function TExtensionsToLanguageList.Add: TExtensionItem;
begin
  Result:=TExtensionItem(inherited Add);
end;
constructor TExtensionsToLanguageList.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FExtensionSeparator:=';';
end;
procedure TExtensionsToLanguageList.Delete(Index: Integer);
begin
  lastAccessed:=nil;
  inherited Delete(Index);
end;

procedure TExtensionsToLanguageList.Assign(Source: TPersistent);
begin
  inherited;
  lastAccessed:=nil;
end;
initialization

finalization
FreeAndNil(FExtMap);//added to avoid a memoryleak after a tip by mak

{$Else}
  implementation
{$Endif}
end.
