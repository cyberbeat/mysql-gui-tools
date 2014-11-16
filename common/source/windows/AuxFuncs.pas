unit AuxFuncs;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  Classes, TntClasses, TntExtCtrls, Windows, Winsock, TntRegistry,
  TntComCtrls, ComCtrls, Forms, Messages,
  StdCtrls, Controls, Dialogs, ShlObj, Contnrs, Graphics,
  SysUtils, ShellAPI, WinSvc, ActiveX, ComObj,
  gnugettext, StrUtils, WinInet, TntForms, TntStdCtrls,
  Clipbrd, ExtCtrls, TntSysUtils, AxCtrls;

type
  MYX_MESSAGE_DLG_TYPE = (
    myx_mtError = 1,
    myx_mtInformation,
    myx_mtConfirmation,
    myx_mtEdit,
    myx_mtWarning
  );

  MYX_NODE_DATA_TYPE = (
    myx_ndt_pointer = 1,
    myx_ndt_tobject
  );

  PMYX_DRIVE_INFO = ^MYX_DRIVE_INFO;
  MYX_DRIVE_INFO = record
    VolumeLabel: WideString;
    VolumeNumber: WideString;
    FileSystem: WideString;
    DriveType: WideString;
    TotalSize: int64;
    FreeSpace: int64;
  end;

  TMyxModalDialog = class(TTntForm)
    MessageLbl: TTntLabel;
    ValueMemo: TTntMemo;
    DlgBtnList: TList;

    constructor Create(Titel: WideString; Messagetext: WideString;
      DlgType: MYX_MESSAGE_DLG_TYPE;
      buttons: WideString;
      doEdit: Boolean; editCaption: WideString; var editValue: WideString;
      editLinesNum: Integer = 1; editReadOnly: Boolean = False;
      ShowDisableMessageCheckbox: Boolean = False;
      PNGImgResName: WideString = ''); reintroduce;
    destructor Destroy; override;

    procedure doButtonClick(Sender: TObject);
    procedure HideEdit;
    procedure ShowEdit;
    procedure DoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    EditLbl: TTntLabel;
    TypeImage, InfoImage: TTntImage;
  published
    DisableMessageCheckbox: TTntCheckBox;
  private
    doEdit: Boolean;
  end;

  IShellLinkWFix = interface(IUnknown) { sl }
    [SID_IShellLinkW]
    function GetPath(pszFile: PWideChar; cchMaxPath: Integer;
      var pfd: TWin32FindDataW; fFlags: DWORD): HResult; stdcall;
    function GetIDList(var ppidl: PItemIDList): HResult; stdcall;
    function SetIDList(pidl: PItemIDList): HResult; stdcall;
    function GetDescription(pszName: PWideChar; cchMaxName: Integer): HResult; stdcall;
    function SetDescription(pszName: PWideChar): HResult; stdcall;
    function GetWorkingDirectory(pszDir: PWideChar; cchMaxPath: Integer): HResult; stdcall;
    function SetWorkingDirectory(pszDir: PWideChar): HResult; stdcall;
    function GetArguments(pszArgs: PWideChar; cchMaxPath: Integer): HResult; stdcall;
    function SetArguments(pszArgs: PWideChar): HResult; stdcall;
    function GetHotkey(var pwHotkey: Word): HResult; stdcall;
    function SetHotkey(wHotkey: Word): HResult; stdcall;
    function GetShowCmd(out piShowCmd: Integer): HResult; stdcall;
    function SetShowCmd(iShowCmd: Integer): HResult; stdcall;
    function GetIconLocation(pszIconPath: PWideChar; cchIconPath: Integer;
      out piIcon: Integer): HResult; stdcall;
    function SetIconLocation(pszIconPath: PWideChar; iIcon: Integer): HResult; stdcall;
    function SetRelativePath(pszPathRel: PWideChar; dwReserved: DWORD): HResult; stdcall;
    function Resolve(Wnd: HWND; fFlags: DWORD): HResult; stdcall;
    function SetPath(pszFile: PWideChar): HResult; stdcall;
  end;

procedure InitGlobalVars;
procedure InitForm(Form: TTntForm);
procedure SetDataFont(Controls: array of TControl);
procedure InitFrame(Frame: TTntFrame);
function RescaleFontSize(FontSize: Integer): Integer;

function GetXPStyleEnabled: Boolean;

function AddTreeViewChildNode(theTreeView: TTntTreeView;
  ParentNode: TTntTreeNode; Caption: WideString; Icon: Integer;
  Data: Pointer = nil): TTntTreeNode;
function AddListViewItem(theListView: TTntListView;
  ParentItem: TTntListItem; Caption: WideString; Icon: Integer;
  Data: Pointer = nil): TTntListItem;

procedure ClearTreeView(theTreeView: TTntTreeView;
  NodeDataType: MYX_NODE_DATA_TYPE);
procedure ClearListView(theListView: TTntListView;
  NodeDataType: MYX_NODE_DATA_TYPE);

function ShowModalDialog(Titel: WideString; Messagetext: WideString;
  DlgType: MYX_MESSAGE_DLG_TYPE = myx_mtInformation;
  buttons: WideString = 'OK'): Integer;
function ShowModalEditDialog(Titel: WideString; Messagetext: WideString;
  DlgType: MYX_MESSAGE_DLG_TYPE;
  buttons: WideString;
  doEdit: Boolean; editCaption: WideString; var editValue: WideString;
  editLinesNum: Integer = 1; editReadOnly: Boolean = False;
  ShowDisableMessageCheckbox: Boolean = False;
  PNGImgResName: WideString = ''): Integer;

function GetLocalHostName: WideString;
function GetFileSize(const FileName: WideString): Int64;
function GetFileDate(const FileName: WideString): TDateTime;
function GetFileVersion(fname: WideString): WideString;
function GetDrives: WideString;
function GetDriveInfo(drive: WideString): PMYX_DRIVE_INFO;
function FormatFileSize(size: double): WideString;

procedure CreateSubProcess(command: WideString; workingdir: WideString = ''; show: Boolean = True;
  wait4proz: Boolean = False);
function ExecuteShellAppl(CommandLine, InputFile, OutputFile: string): Boolean;

procedure MessageToAllForms(Msg: UINT; wParam: WPARAM; lParam: LPARAM);

function ServiceStop(sMachine, sService: WideString): boolean;
function ServiceStart(sMachine, sService: WideString): boolean;
function ServiceStatus(sMachine, sService: WideString): Integer;
function RemoveService(sMachine, sService: WideString): Boolean;

procedure DisableEnableControls(WinControl: TWinControl; Enable: Boolean);
procedure DisableEnablePages(PageControl: TTntPageControl; Enable: Boolean);
procedure InitHandles(WinControl: TWinControl);

function LoadTextFromFile(filename: WideString): WideString;
function LoadAnsiTextFromFile(filename: WideString): AnsiString;
procedure SaveAnsiTextToFile(filename: WideString; Text: AnsiString);

function ExtractNumber(s: WideString): Integer;
function ExtractString(s: WideString): WideString;
function ExtractText(s: WideString): WideString;

procedure WriteToReg(theRoot: HKEY; Key, Name: WideString; Typ: Integer; Val: WideString);
function ReadFromReg(theRoot: HKEY; Key, Name: WideString; Typ: Integer;
  DefaultVal: WideString; CanCreate: Boolean = True): WideString;

const
  CSIDL_PROGRAM_FILES = $0026;
  CSIDL_PROGRAM_FILES_COMMON = $002B;
  CSIDL_COMMON_APPDATA = $0023;
  CSIDL_PERSONAL = $0005;
  CSIDL_STARTUP = $0007;

function GetSystemDir: WideString;
function GetWindowsDir: WideString;
function GetApplDir: WideString;
function GetSpecialFolder(Folder: Integer): WideString;
function GetProgramFilesDir: WideString;
function GetCommonProgFilesDir: WideString;
function GetApplicationDataDir: WideString;
function GetHomeDir: WideString;
function GetTempDir: WideString;

function GetDefaultBrowser: WideString;
procedure BrowseWebPage(URL: WideString);
function GetExeByExtension(FileExtension: WideString): WideString;

function StringAlignLeft(s: WideString; len: Integer): WideString;
function StringAlignRight(s: WideString; len: Integer): WideString;

procedure CopyDiskFile(sourcefile, destinationfile: WideString; PromtBeforeOverwrite: Boolean);

procedure ShowHelp;

procedure DeleteComponentWithChildren(Component: TComponent; RemoveFromList: TList = nil);

function GetComputerNetName: WideString;

function GetWideStringTextWidth(DC: THandle; Text: WideString): Integer; overload;
function GetWideStringTextWidth(Canvas: TCanvas; Text: WideString): Integer; overload;

procedure DrawWideStringText(DC: HDC; lpString: PWideChar; nCount: Integer;
  lpRect: TRect; uFormat: Cardinal = DT_LEFT; AdjustRight: Boolean = False);

function IsWindowsVista: Boolean;
function IsWinXP: Boolean;
function IsWin2k: Boolean;
function IsWinNT: Boolean;
function IsWinNTPlatform: Boolean;

procedure ParseCommandLine(Line: WideString; Tokens: TTntStringList);
function ShortPathToLongPath(const Path: WideString): WideString;

procedure SendToEventLog(Msg: WideString; MessageType: Integer);

function IsConnectedToInternet: Boolean;

function FormatLibraryVersion(LibVersion: Integer): WideString;

function GetCharFromVKey(vkey: Word): WideString;

function GetTransparentImgList(size: Integer): TImageList;

function ReplaceTags(s: WideString; tags: WideString): WideString;

function BreakLine(Canvas: TCanvas; var S: WideString; Width: Integer): WideString;

procedure CreateShortcut(ShortcutName: string;
  ShortcutDirectory: string; DestinationFile: string;
  WorkingDir: string; arguments: string);

procedure RegisterFileType(ExtName: WideString; AppName: WideString;
  IconNr: Integer; Params: WideString = '');

function IsAcrobatInstalled: Boolean; //Adobe Acrobat Reader
procedure ShowPDF(Filename: WideString);
procedure PrintPDF(Filename: WideString);

function IsKeyDown(Key: Word): Boolean;

function CreateVaList(const Args: array of const): PChar;

procedure AddToFile(Filename: WideString; S: WideString);

function GetHTMLReplaceStringList: TStringList;
function HTMLDecode(Source: string; NamedChars: TStringList = nil): string;
function HTMLEncode(Source: string; NamedChars: TStringList = nil;
  EncodeSpaces: Boolean = False): string;

function IsNumeric(S: WideString): Boolean;

function SetGlobalEnvironment(const Name, Value: WideString;
  const User: Boolean = True): Boolean;
function GetGlobalEnvironment(const Name: WideString;
  const User: Boolean = True): WideString;

function GetSubstringCount(Substring: WideString; Text: WideString): Integer;

function GetShellLinkFileName(const LinkFile: WideString): WideString;

function FetchHttpRequest(Url: WideString;
  Username: WideString; Password: WideString;
  ProxyServer: WideString; ProxyBypass: WideString;
  ProxyUsername: WideString; ProxyPassword: WideString;
  var Error: Integer): string;

function ColorToHtmlColor(Color: TColor): String;
function HtmlColorToColor(Color: String): TColor;

procedure LoadRtfTextFromResource(RichEdit: TTntRichEdit; Resourcename: WideString);

function GetNewGUID: string;

function UTCtoLocalTime (UTCTime: TDateTime) : TDateTime;
function LocalTimeToUTC (LocalTime: TDateTime) : TDateTime;

procedure SetDefaultFonts(const AFont: TFont);
procedure SetDesktopIconFonts(const AFont: TFont);

function TaskDialog(const AHandle: THandle; const ATitle, ADescription, AContent: WideString; const Icon: Integer;
  const Buttons: TMsgDlgButtons): Integer;
procedure TaskMessage(const AHandle: THandle; AMessage: string);
function OpenSaveFileDialog(Parent: TWinControl; const DefExt, Filter, InitialDir, Title: string; var FileName: string;
  MustExist, OverwritePrompt, NoChangeDir, DoOpen: Boolean): Boolean;

// Vista support
procedure SetExplorerTheme(const AControl: TWinControl);
procedure SetVistaFonts(const AForm: TCustomForm);
procedure SetVistaContentFonts(const AFont: TFont);
function CompositingEnabled: Boolean;

// Global Variables
var
  ClearTypeOn: Boolean;
  TahomaFontAvailable: Boolean;
  DefaultFontName: string;
  DataFontName: string;
  DefaultFontHeight: Integer;
  DataFontHeight: Integer;

var
  CheckOSVerForFonts: Boolean = True;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  TntWindows, Types, PNGTools, Themes, UxTheme, CommDlg;

const
  VistaFont = 'Segoe UI';
  VistaContentFont = 'Calibri';
  XPContentFont = 'Verdana';
  XPFont = 'Tahoma';

  TD_ICON_BLANK = 0;
  TD_ICON_WARNING = 84;
  TD_ICON_QUESTION = 99;
  TD_ICON_ERROR = 98;
  TD_ICON_INFORMATION = 81;
  TD_ICON_SHIELD_QUESTION = 104;
  TD_ICON_SHIELD_ERROR = 105;
  TD_ICON_SHIELD_OK = 106;
  TD_ICON_SHIELD_WARNING = 107;

  TD_BUTTON_OK = 1;
  TD_BUTTON_YES = 2;
  TD_BUTTON_NO = 4;
  TD_BUTTON_CANCEL = 8;
  TD_BUTTON_RETRY = 16;
  TD_BUTTON_CLOSE = 32;

  TD_RESULT_OK = 1;
  TD_RESULT_CANCEL = 2;
  TD_RESULT_RETRY = 4;
  TD_RESULT_YES = 6;
  TD_RESULT_NO = 7;
  TD_RESULT_CLOSE = 8;

  TD_IDS_WINDOWTITLE       = 10;
  TD_IDS_CONTENT           = 11;
  TD_IDS_MAININSTRUCTION   = 12;
  TD_IDS_VERIFICATIONTEXT  = 13;
  TD_IDS_FOOTER            = 15;
  TD_IDS_RB_GOOD           = 16;
  TD_IDS_RB_OK             = 17;
  TD_IDS_RB_BAD            = 18;
  TD_IDS_CB_SAVE           = 19;

//----------------------------------------------------------------------------------------------------------------------

procedure InitGlobalVars;

var
  Parameter: UINT;

begin
  // Check if the current user has turned ClearType on.
  if IsWinXP then
  begin
    SystemParametersInfo(SPI_GETFONTSMOOTHINGTYPE, 0, @Parameter, 0);
    ClearTypeOn := (Parameter and FE_FONTSMOOTHINGCLEARTYPE) <> 0;
  end
  else
    ClearTypeOn := False;

  // Check if Tahoma is available.
  TahomaFontAvailable := Screen.Fonts.IndexOf('Tahoma') > -1;
  Screen.ResetFonts;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitForm(Form: TTntForm);

var
  i: Integer;
  OriginalFontSize: Integer;

begin
  TranslateComponent(Form);

  Form.AutoScroll := False;
  Form.Scaled := False;
  Form.PixelsPerInch := 96;

  OriginalFontSize := Form.Font.Size;

  if DefaultFontName <> '' then
  begin
    Form.Font.Name := DefaultFontName;
    Form.Font.Height := DefaultFontHeight;
  end;

  if (Screen.PixelsPerInch <> 96) or ClearTypeOn then
  begin
    // Change to Tahoma when MS Sans Serif is selected and
    // ClearTypeOn is activated.
    if ClearTypeOn and TahomaFontAvailable and
      ((DefaultFontName = 'MS Sans Serif') or (DefaultFontName = '')) then
      Form.Font.Name := 'Tahoma';

    if Screen.PixelsPerInch <> 96 then
    begin
      if IsWin2k and
        ((DefaultFontName = 'MS Sans Serif') or (DefaultFontName = '')) then
      begin
        Form.Font.Size := 7;
        Form.Font.Name := 'Tahoma';
      end
      else
      begin
        Form.Font.Height := -11;

        if (Form.Font.Name = 'MS Sans Serif') then
          Form.Font.Name:='Tahoma';
      end;
    end
    else
      Form.Font.Size := 8;

    Form.Font.Style := [];
  end;

  // Does nothing on older platforms.
  SetVistaFonts(Form);
  

  for i := 0 to Form.ComponentCount - 1 do
  begin
    if (Form.Components[i] is TTntLabel) then
      if (TLabel(Form.Components[i]).ParentFont = False) and
        (OriginalFontSize = TLabel(Form.Components[i]).Font.Size) then
      begin
        TLabel(Form.Components[i]).Font.Name := Form.Font.Name;
        TLabel(Form.Components[i]).Font.Size := Form.Font.Size;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetDataFont(Controls: array of TControl);

var
  i: Integer;

begin
  if (DataFontName = '') then
    Exit;

  for i := 0 to Length(Controls) - 1 do
  begin
    if (Controls[i].ClassNameIs('TTntEdit')) then
    begin
      TTntEdit(Controls[i]).Font.Name := DataFontName;
      TTntEdit(Controls[i]).Font.Height := DataFontHeight;
    end
    else
      if (Controls[i].ClassNameIs('TTntMemo')) then
      begin
        TTntMemo(Controls[i]).Font.Name := DataFontName;
        TTntMemo(Controls[i]).Font.Height := DataFontHeight;
      end
      else
        if (Controls[i].ClassNameIs('TTntLabel')) then
        begin
          TTntLabel(Controls[i]).Font.Name := DataFontName;
          TTntLabel(Controls[i]).Font.Height := DataFontHeight;
        end
        else
          if (Controls[i].ClassNameIs('TTntTreeView')) then
          begin
            TTntTreeView(Controls[i]).Font.Name := DataFontName;
            TTntTreeView(Controls[i]).Font.Height := DataFontHeight;
          end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function RescaleFontSize(FontSize: Integer): Integer;

begin
  Result := Round((FontSize * 96) / Screen.PixelsPerInch);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitFrame(Frame: TTntFrame);

begin
  if (Screen.PixelsPerInch <> 96) or (ClearTypeOn) then
  begin
    if (TahomaFontAvailable) then
      Frame.Font.Name := 'Tahoma';

    if (Screen.PixelsPerInch <> 96) then
      Frame.Font.Size := 6
    else
      Frame.Font.Size := 8;

    Frame.Font.Style := [];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetXPStyleEnabled: Boolean;

begin
  Result := ThemeServices.ThemesEnabled;
end;

//----------------------------------------------------------------------------------------------------------------------

function AddTreeViewChildNode(theTreeView: TTntTreeView; ParentNode: TTntTreeNode; Caption: WideString; Icon: Integer;
  Data: Pointer): TTntTreeNode;

var
  theTreeNode: TTntTreeNode;

begin
  if (ParentNode = nil) then
  begin
    theTreeNode := theTreeView.Items.Add(nil, Caption);
    theTreeNode.ImageIndex := Icon;
    theTreeNode.SelectedIndex := Icon;
  end
  else
  begin
    theTreeNode := theTreeView.Items.AddChild(ParentNode, Caption);
    theTreeNode.ImageIndex := Icon;
    theTreeNode.SelectedIndex := Icon;
  end;

  theTreeNode.Data := Data;

  AddTreeViewChildNode := theTreeNode;
end;

//----------------------------------------------------------------------------------------------------------------------

function AddListViewItem(theListView: TTntListView; ParentItem: TTntListItem; Caption: WideString; Icon: Integer;
  Data: Pointer = nil): TTntListItem;

var
  theListItem: TTntListItem;

begin
  if (ParentItem = nil) then
  begin
    theListItem := theListView.Items.Add;
    theListItem.Caption := Caption;
    theListItem.ImageIndex := Icon;
  end
  else
  begin
    theListItem := theListView.Items.Insert(theListView.Items.IndexOf(ParentItem));
    theListItem.Caption := Caption;
    theListItem.ImageIndex := Icon;
  end;

  theListItem.Data := Data;

  Result := theListItem;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ClearTreeView(theTreeView: TTntTreeView; NodeDataType: MYX_NODE_DATA_TYPE);

var
  i: Integer;

begin
  for i := 0 to theTreeView.Items.Count - 1 do
    if (theTreeView.Items[i].Data <> nil) then
    begin
      case NodeDataType of
        myx_ndt_pointer:
          FreeMem(theTreeView.Items[i].Data);
        myx_ndt_tobject:
          TObject(theTreeView.Items[i].Data).Free;
      end;
      theTreeView.Items[i].Data := nil;
    end;

  theTreeView.Items.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ClearListView(theListView: TTntListView; NodeDataType: MYX_NODE_DATA_TYPE);

var
  i: Integer;

begin
  if (theListView <> nil) then
  begin
    for i := 0 to theListView.Items.Count - 1 do
      if (theListView.Items[i].Data <> nil) then
      begin
        case NodeDataType of
          myx_ndt_pointer:
            FreeMem(theListView.Items[i].Data);
          myx_ndt_tobject:
            TObject(theListView.Items[i].Data).Free;
        end;
      end;

    theListView.Clear;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ShowModalDialog(Titel: WideString; Messagetext: WideString; DlgType: MYX_MESSAGE_DLG_TYPE;
  buttons: WideString): Integer;

var
  NotUsed: WideString;

begin
  Result := ShowModalEditDialog(Titel, Messagetext, DlgType, buttons,
    False, '', NotUsed);
end;

//----------------------------------------------------------------------------------------------------------------------

function ShowModalEditDialog(Titel: WideString; Messagetext: WideString; DlgType: MYX_MESSAGE_DLG_TYPE;
  buttons: WideString; doEdit: Boolean; editCaption: WideString; var editValue: WideString; editLinesNum: Integer = 1;
  editReadOnly: Boolean = False; ShowDisableMessageCheckbox: Boolean = False; PNGImgResName: WideString = ''): Integer;

var
  ModalDialog: TMyxModalDialog;

begin
  ModalDialog := TMyxModalDialog.Create(Titel, Messagetext, DlgType, buttons,
    doEdit, editCaption, editValue, editLinesNum, editReadOnly,
    ShowDisableMessageCheckbox, PNGImgResName);
  try
    Result := ModalDialog.ShowModal;

    if Result = mrCancel then  // VCL result
      Result := 0
    else
      if Result > 100 then
        Dec(Result, 100);      // Our result

    if (doEdit) and (ModalDialog.ValueMemo.Text <> '') then
      editValue := ModalDialog.ValueMemo.Text;
  finally
    ModalDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetLocalHostName: WideString;

var
  Buffer: array[0..MAX_PATH] of WideChar;
  Size: Cardinal;

begin
  Size := Length(Buffer);
  GetComputerNameW(Buffer, Size);
  SetString(Result, Buffer, Size);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetFileSize(const FileName: WideString): Int64;

var
  SizeLow, SizeHigh: DWord;
  FileHandle: Integer;

begin
  Result := 0;
  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  try
    if (FileHandle > 0) then
    begin
      SizeLow := Windows.GetFileSize(FileHandle, @SizeHigh);
      Result := (Int64(SizeHigh) shl 32) + SizeLow;
    end;
  finally
    FileClose(FileHandle);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetFileDate(const FileName: WideString): TDateTime;

var
  FileHandle: Integer;

begin
  Result := Now;

  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  try
    if (FileHandle > 0) then
      Result := FileDateToDateTime(FileGetDate(FileHandle));
  finally
    FileClose(FileHandle);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetFileVersion(fname: WideString): WideString;

var
  size: longint;
  vlen: DWord;
  FInfo: pointer;
  FLang: PInteger;
  vptr: pchar;

begin
  Result := '';

  size := GetFileVersionInfoSizeW(PWideChar(fname), vlen);
  if (size > 0) then
  begin
    GetMem(FInfo, size);
    if (GetFileVersionInfoW(PWideChar(fname), vlen, size, FInfo)) then
    begin
      // get languages
      VerQueryValue(FInfo, '\VarFileInfo\Translation', pointer(FLang), vlen);
      if VerQueryValue(FInfo, PChar(Format('\StringFileInfo\%4.4x%4.4x\FileVersion', [LoWord(FLang^),
        HiWord(FLang^)])), pointer(vptr), vlen) then
      begin
        Result := vptr;
      end;
    end;
    FreeMem(FInfo, size);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetDrives: WideString;

var
  r: LongWord;
  Drives: array[0..128] of char;
  pDrive: pchar;

begin
  r := GetLogicalDriveStrings(sizeof(Drives), Drives);
  if r = 0 then
    exit;
  if r > sizeof(Drives) then
    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));

  pDrive := Drives; // Point to the first drive
  while pDrive^ <> #0 do
  begin
    Result := Result + Copy(pDrive, 1, 2) + #13#10;

    inc(pDrive, 4); // Point to the next drive
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

function GetDriveInfo(drive: WideString): PMYX_DRIVE_INFO;

var
  DriveInfo: PMYX_DRIVE_INFO;
  x: Integer;
  VLabel, FSys: array[0..255] of WideChar;
  VLabelAnsi, FSysAnsi: array[0..255] of Char;
  SNr, FL, SysFlag: DWord;

begin
  new(DriveInfo);

  if (IsWinNTPlatform) then
  begin
    if (GetVolumeInformationW(PWideChar(Drive + '\'), VLabel, SizeOf(VLabel),
      @SNr, FL, SysFlag, FSys, SizeOf(FSys))) then
    begin
      DriveInfo.VolumeLabel := VLabel;
      DriveInfo.VolumeNumber := IntToStr(SNr);
      DriveInfo.FileSystem := WideString(FSys);

      x := GetDriveTypeW(PWideChar(Drive + '\'));
      case x of
        2:
          DriveInfo.DriveType := 'Floppy';
        3:
          DriveInfo.DriveType := 'Fixed';
        4:
          DriveInfo.DriveType := 'Network';
        5:
          DriveInfo.DriveType := 'CDRom';
        6:
          DriveInfo.DriveType := 'Ram';
      else
        DriveInfo.DriveType := '';
      end;

      //Using AnsiString should be safe since Drive will be A,B,..
      DriveInfo.TotalSize := DiskSize(Ord(UpCase(AnsiString(Drive)[1])) - 64);
      DriveInfo.FreeSpace := DiskFree(Ord(UpCase(AnsiString(Drive)[1])) - 64);
    end
    else
    begin
      DriveInfo.VolumeLabel := '';
      DriveInfo.VolumeNumber := '';
      DriveInfo.FileSystem := '';
      DriveInfo.TotalSize := 0;
      DriveInfo.FreeSpace := 0;
    end;
  end
  else
  begin
    if (GetVolumeInformation(PChar(string(Drive + '\')), VLabelAnsi, SizeOf(VLabelAnsi),
      @SNr, FL, SysFlag, FSysAnsi, SizeOf(FSysAnsi))) then
    begin
      DriveInfo.VolumeLabel := VLabelAnsi;
      DriveInfo.VolumeNumber := IntToStr(SNr);
      DriveInfo.FileSystem := string(FSysAnsi);

      x := GetDriveType(PChar(string(Drive + '\')));
      case x of
        2:
          DriveInfo.DriveType := 'Floppy';
        3:
          DriveInfo.DriveType := 'Fixed';
        4:
          DriveInfo.DriveType := 'Network';
        5:
          DriveInfo.DriveType := 'CDRom';
        6:
          DriveInfo.DriveType := 'Ram';
      else
        DriveInfo.DriveType := '';
      end;

      //Using AnsiString should be safe since Drive will be A,B,..
      DriveInfo.TotalSize := DiskSize(Ord(UpCase(AnsiString(Drive)[1])) - 64);
      DriveInfo.FreeSpace := DiskFree(Ord(UpCase(AnsiString(Drive)[1])) - 64);

    end
    else
    begin
      DriveInfo.VolumeLabel := '';
      DriveInfo.VolumeNumber := '';
      DriveInfo.FileSystem := '';
      DriveInfo.TotalSize := 0;
      DriveInfo.FreeSpace := 0;
    end;
  end;

  Result := DriveInfo;
end;

//----------------------------------------------------------------------------------------------------------------------

function FormatFileSize(size: double): WideString;

var
  s: WideString;

begin
  if (size < 1024 - 200) then
  begin
    size := size;
    s := 'B';
  end
  else
    if (size < 1024 * (1024 - 200)) then
    begin
      size := size / 1024;
      s := 'kB';
    end
    else
      if (size < 1024 * 1024 * (1024 - 200)) then
      begin
        size := size / (1024 * 1024);
        s := 'MB';
      end
      else
      begin
        size := size / (1024 * 1024 * 1024);
        s := 'GB';
      end;

  Result := FormatFloat('###,###,##0.#', size) + ' ' + s;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CreateSubProcess(command: WideString; workingdir: WideString = ''; show: Boolean = True; wait4proz: Boolean =
  False);

var
{$IFDEF MSWINDOWS}
  StartupInfo: TStartupInfoW;
  wdir: PWideChar;
  ProcessInfo: TProcessInformation;
{$ENDIF}

{$IFDEF LINUX}
  FCmdThread: TCmdExecThread;
{$ENDIF}

begin
{$IFDEF MSWINDOWS}
  if (workingdir <> '') then
    wdir := PWideChar(workingdir)
  else
    wdir := nil;

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := Sizeof(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  if (show) then
    StartupInfo.wShowWindow := SW_Show
  else
    StartupInfo.wShowWindow := SW_Hide;

  if (not (CreateProcessW(nil, PWideChar(command), nil, nil, False, Normal_PRIORITY_CLASS and CREATE_DEFAULT_ERROR_MODE,
    nil, wdir, StartupInfo, ProcessInfo))) then
    raise Exception.Create(command + ': '#13#10 + 'The Program could not be launched.'#13#10 +
      'Error Number ' + IntToStr(GetLastError) + #13#10 + SysErrorMessage(GetLastError));

  //If requested, wait for the programm to end
  if (wait4proz) then
  begin
    while (WaitForSingleObject(ProcessInfo.hProcess, 0) = WAIT_TIMEOUT) do
      Application.ProcessMessages;

    ProcessInfo.hProcess := 0;
  end;
{$ENDIF}

{$IFDEF LINUX}
  {if FCmdThread <> nil then
  begin
    if not FCmdThread.Done
      then raise Exception.Create('A command is already running')
    else FCmdThread.Free;
  end;}
  FCmdThread := TCmdExecThread.Create;
  //FCmdThread.OnComplete := InternalComplete;
  FCmdThread.Command := command;
  FCmdThread.Resume;

  //Wenn erwünscht, warten bis Programm beendet wird.
  if (wait4proz = 1) then
  begin
    while (not (FCmdThread.Done)) do
    begin
      Sleep(500);
      //Do drawing and stuff
      Application.ProcessMessages;
    end;
  end;
{$ENDIF}
end;

//----------------------------------------------------------------------------------------------------------------------

function ExecuteShellAppl(CommandLine, InputFile, OutputFile: string): Boolean;

var
  OldCursor: TCursor;
  //pCommandLine: array[0..MAX_PATH] of Char;
  //pInputFile, pOutPutFile: array[0..MAX_PATH] of Char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecAtrrs: TSecurityAttributes;
  hAppProcess, hAppThread, hInputFile, hOutputFile: THandle;

begin
  Result := False;

  if (InputFile <> '') then
    if not FileExists(InputFile) then
      raise Exception.CreateFmt('Input file %s ' +
        'does not exist.', [InputFile]);

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  hOutputFile := 0;
  hInputFile := 0;
  hAppThread := 0;
  hAppProcess := 0;

  try
    FillChar(SecAtrrs, SizeOf(SecAtrrs), #0);
    SecAtrrs.nLength := SizeOf(SecAtrrs);
    SecAtrrs.lpSecurityDescriptor := nil;
    SecAtrrs.bInheritHandle := True;

    if (InputFile <> '') then
    begin
      hInputFile := CreateFile(PChar(InputFile),
        GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE,
        @SecAtrrs, { pointer to security attributes }
        OPEN_ALWAYS, { how to create }
        FILE_ATTRIBUTE_TEMPORARY, { file attributes }
        0); { handle to file with attributes to copy }

      if hInputFile = INVALID_HANDLE_VALUE then
        raise Exception.CreateFmt(
          'WinApi function CreateFile returned an ' +
          'invalid handle value for the input file %s', [InputFile]);
    end;

    hOutputFile := CreateFile(PChar(OutPutFile),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      @SecAtrrs, { pointer to security attributes }
      CREATE_ALWAYS, { how to create }
      FILE_ATTRIBUTE_TEMPORARY, { file attributes }
      0); { handle to file with attributes to copy }

    if hOutputFile = INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt(
        'WinApi function CreateFile returned an ' +
        ' invalid handle value for the output file %s', [OutputFile]);

    FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdOutput := hOutputFile;
    StartupInfo.hStdError := hOutputFile;
    if (InputFile <> '') then
      StartupInfo.hStdInput := hInputFile;

    Result := CreateProcess(nil, { pointer to name of executable module }
      PChar(CommandLine),
      nil, { pointer to process security attributes }
      nil, { pointer to thread security attributes }
      True, { handle inheritance flag }
      CREATE_NEW_CONSOLE or
      REALTIME_PRIORITY_CLASS, { creation flags }
      nil, { pointer to new environment block }
      nil, { pointer to current directory name }
      StartupInfo, { pointer to STARTUPINFO }
      ProcessInfo); { pointer to PROCESS_INF }

    if Result then
    begin
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      hAppProcess := ProcessInfo.hProcess;
      hAppThread := ProcessInfo.hThread;
    end
    else
      raise Exception.Create('Function failure');

  finally
    if hOutputFile <> 0 then
      CloseHandle(hOutputFile);
    if hInputFile <> 0 then
      CloseHandle(hInputFile);
    if hAppThread <> 0 then
      CloseHandle(hAppThread);
    if hAppProcess <> 0 then
      CloseHandle(hAppProcess);

    Screen.Cursor := OldCursor;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MessageToAllForms(Msg: UINT; wParam: WPARAM; lParam: LPARAM);

var
  i: Integer;

begin
  for i := 0 to Screen.FormCount - 1 do
    Screen.Forms[i].Perform(Msg, wParam, lParam);
end;

//----------------------------------------------------------------------------------------------------------------------

// sMachine .. \\SERVER or empty for local machine
// sService ... service name, ie: mysqld
// return TRUE if successful

function ServiceStart(sMachine, sService: WideString): boolean;

var
  schm, // service control manager handle
    schs: SC_Handle; // service handle
  ss: TServiceStatus; // service status
  psTemp: PChar; // temp char pointer
  dwChkP: DWord; // check point

begin
  ss.dwCurrentState := 0;

  // connect to the service control manager
  schm := OpenSCManagerW(PWideChar(sMachine), nil, SC_MANAGER_CONNECT);

  // if successful...
  if (schm > 0) then
  begin
    // open a handle to the specified service
    schs := OpenServiceW(schm, PWideChar(sService), SERVICE_START or
      SERVICE_QUERY_STATUS);

    // if successful...
    if (schs > 0) then
    begin
      psTemp := nil;
      if (StartService(schs, 0, psTemp)) then
      begin
        Sleep(2000);

        // check status
        if (QueryServiceStatus(schs, ss)) then
        begin
          while (SERVICE_RUNNING <> ss.dwCurrentState) do
          begin
            // dwCheckPoint contains a
            // value that the service
            // increments periodically
            // to report its progress
            // during a lengthy
            // operation.
            //
            // save current value

            dwChkP := ss.dwCheckPoint;

            // wait a bit before
            // checking status again
            //
            // dwWaitHint is the
            // estimated amount of time
            // the calling program
            // should wait before calling
            // QueryServiceStatus() again
            //
            // idle events should be
            // handled here...
            //Sleep(ss.dwWaitHint);
            Sleep(2000);

            if (not QueryServiceStatus(schs, ss)) then
            begin
              // couldn't check status
              break;
            end;

            if (ss.dwCheckPoint <= dwChkP) then
            begin
              // QueryServiceStatus
              // didn't increment
              // dwCheckPoint as it
              // should have.
              // avoid an infinite
              // loop by breaking
              break;
            end;
          end;
        end;
      end;

      // close service handle
      CloseServiceHandle(schs);
    end;

    // close service control
    // manager handle
    CloseServiceHandle(schm);
  end;

  // return TRUE if
  // the service status is running
  Result := (SERVICE_RUNNING = ss.dwCurrentState);
end;

//----------------------------------------------------------------------------------------------------------------------

// sMachine .. \\SERVER or empty for local machine
// sService ... service name, ie: mysqld
// return TRUE if successful

function ServiceStop(sMachine, sService: WideString): boolean;

var
  schm, // service control manager handle
    schs: SC_Handle; // service handle
  ss: TServiceStatus; // service status
  dwChkP: DWord; // check point
  counter: Integer;

begin
  // connect to the service
  // control manager
  schm := OpenSCManagerW(PWideChar(sMachine), nil, SC_MANAGER_CONNECT);

  counter := 0;

  // if successful...
  if (schm > 0) then
  begin
    // open a handle to the specified service
    schs := OpenServiceW(schm, PWideChar(sService), SERVICE_STOP or
      SERVICE_QUERY_STATUS);

    // if successful...
    if (schs > 0) then
    begin
      if (ControlService(schs, SERVICE_CONTROL_STOP, ss)) then
      begin
        // check status
        if (QueryServiceStatus(
          schs,
          ss)) then
        begin
          while ((SERVICE_STOPPED
            <> ss.dwCurrentState) and (counter < 10)) do
          begin
            // dwCheckPoint contains a
            // value that the service
            // increments periodically
            // to report its progress
            // during a lengthy
            // operation.
            //
            // save current value
            dwChkP := ss.dwCheckPoint;

            // wait a bit before
            // checking status again
            //
            // dwWaitHint is the
            // estimated amount of time
            // the calling program
            // should wait before calling
            // QueryServiceStatus() again
            //
            // idle events should be
            // handled here...
            //Sleep(ss.dwWaitHint);
            Sleep(1000);

            inc(counter);

            if (not QueryServiceStatus(schs, ss)) then
            begin
              // couldn't check status
              break;
            end;

            if (ss.dwCheckPoint < dwChkP) then
            begin
              // QueryServiceStatus
              // didn't increment
              // dwCheckPoint as it
              // should have.
              // avoid an infinite
              // loop by breaking
              break;
            end;
          end;
        end;
      end;

      // close service handle
      CloseServiceHandle(schs);
    end;

    // close service control
    // manager handle
    CloseServiceHandle(schm);
  end;

  // return TRUE if
  // the service status is stopped
  Result := (SERVICE_STOPPED = ss.dwCurrentState);
end;

//----------------------------------------------------------------------------------------------------------------------

// sMachine .. \\SERVER or empty for local machine
// sService ... service name, ie: mysqld
// return codes:
//   -1 if not successful
//   SERVICE_STOPPED
//   SERVICE_RUNNING
//   SERVICE_PAUSED
//   SERVICE_START_PENDING
//   SERVICE_STOP_PENDING
//   SERVICE_CONTINUE_PENDING
//   SERVICE_PAUSE_PENDING

function ServiceStatus(sMachine, sService: WideString): Integer;

var
  SC: SC_Handle;
  SHwnd: SC_Handle;
  ST: TServiceStatus;

begin
  SC := OpenSCManager(nil, nil, GENERIC_READ);
  if (SC > 0) then
  begin
    SHwnd := OpenServiceW(SC, PWideChar(sService), SERVICE_QUERY_STATUS);
    if (SHwnd > 0) then
    begin
      if (not (QueryServiceStatus(SHwnd, ST))) then
        Result := -1
        //raise EMyxSystemError.Create(_('Cannot query service state'), GetLastError)
      else
        Result := ST.dwCurrentState;

      CloseServiceHandle(SHwnd);
    end
    else
      Result := -1;
      //raise EMyxSystemError.Create(_('Cannot query service state'), GetLastError);

    CloseServiceHandle(SC);
  end
  else
    Result := -1;
    //raise EMyxSystemError.Create(_('Cannot query service state'), GetLastError);
end;

//----------------------------------------------------------------------------------------------------------------------

//Returns 0 on success

function RemoveService(sMachine, sService: WideString): Boolean;

var
  SCMHandle: SC_Handle;
  ServiceHandle: SC_Handle;

begin
  SCMHandle := OpenSCManagerW(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if (SCMHandle > 0) then
  begin
    ServiceHandle := OpenServiceW(SCMHandle, PWideChar(sService), SERVICE_ALL_ACCESS);
    if (ServiceHandle > 0) then
    begin
      if (not (DeleteService(ServiceHandle))) then
        Result := False
        //raise EMyxSystemError.Create(_('Cannot delete service'), GetLastError)
      else
        Result := True;

      CloseServiceHandle(ServiceHandle);
    end
    else
      Result := False;
      //raise EMyxSystemError.Create(_('Cannot open service'), GetLastError);

    CloseServiceHandle(SCMHandle);
  end
  else
    Result := False;
    //raise EMyxSystemError.Create(_('Cannot connect to service manager'), GetLastError);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DisableEnableControls(WinControl: TWinControl; Enable: Boolean);

var
  i: Integer;

begin
  for i := 0 to WinControl.ControlCount - 1 do
  begin
    if WinControl.Controls[i].Tag <> -1 then
    begin
      WinControl.Controls[i].Enabled := Enable;

      if (WinControl.Controls[i].InheritsFrom(TWinControl)) then
        DisableEnableControls(TWinControl(WinControl.Controls[i]), Enable);
    end;
  end;

  WinControl.Enabled := Enable;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DisableEnablePages(PageControl: TTntPageControl; Enable: Boolean);

var
  i: Integer;

begin
  for i := 0 to PageControl.PageCount - 1 do
    DisableEnableControls(PageControl.Pages[i], Enable);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitHandles(WinControl: TWinControl);

var
  i: Integer;

begin
  WinControl.HandleNeeded;

  for i := 0 to WinControl.ControlCount - 1 do
  begin
    if (WinControl.Controls[i].InheritsFrom(TWinControl)) then
      InitHandles(TWinControl(WinControl.Controls[i]));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadTextFromFile(filename: WideString): WideString;

var
  Stream: TStream;
  Size: Integer;
  S: string;

begin
  if (FileExists(filename)) then
  begin
    Stream := TTntFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Size := Stream.Size - Stream.Position;
      SetString(S, nil, Size);
      Stream.Read(Pointer(S)^, Size);
    finally
      Stream.Free;
    end;

    Result := UTF8Decode(S);
  end
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadAnsiTextFromFile(filename: WideString): AnsiString;

var
  Stream: TStream;
  Size: Integer;
  S: string;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
  finally
    Stream.Free;
  end;

  Result := S;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SaveAnsiTextToFile(filename: WideString; Text: AnsiString);

var
  F: TextFile;

begin
  AssignFile(F, filename);
  Rewrite(F);
  try
    Write(F, Text);
  finally
    CloseFile(F);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ExtractNumber(s: WideString): Integer;

var
  i: Integer;
  num, fac: Integer;

begin
  num := 0;
  fac := 1;

  for i := Length(s) downto 1 do
    if (Ord(s[i]) >= Ord('0')) and (Ord(s[i]) <= Ord('9')) then
    begin
      num := num + (Ord(s[i]) - Ord('0')) * fac;
      fac := fac * 10;
    end;

  Result := num;
end;

//----------------------------------------------------------------------------------------------------------------------

function ExtractString(s: WideString): WideString;

var
  i: Integer;
  s1: WideString;

begin
  s1 := '';
  for i := 1 to Length(s) do
    if (Ord(s[i]) < Ord('0')) or (Ord(s[i]) > Ord('9')) then
      s1 := s1 + s[i];

  Result := s1;
end;

//----------------------------------------------------------------------------------------------------------------------

function ExtractText(s: WideString): WideString;

var
  i: Integer;
  s1: WideString;

begin
  s1 := '';

  for i := 1 to Length(s) do
    if ((Ord(s[i]) >= Ord('0')) and (Ord(s[i]) <= Ord('9'))) or //numbers
      ((Ord(s[i]) >= Ord('a')) and (Ord(s[i]) <= Ord('z'))) or //a-z
      ((Ord(s[i]) >= Ord('A')) and (Ord(s[i]) <= Ord('Z'))) or //A-Z
      (s[i] = ' ') or (s[i] = '_') or (s[i] = '.') then
      s1 := s1 + s[i];

  Result := s1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure WriteToReg(theRoot: HKEY; Key, Name: WideString; Typ: Integer; Val: WideString);

var
  MyReg: TTntRegistry;

begin
  try
    MyReg := TTntRegistry.Create(KEY_WRITE);
    MyReg.RootKey := theRoot;
    try
      MyReg.OpenKey(Key, True);
      case Typ of
        0: MyReg.WriteString(Name, Val);
        1: MyReg.WriteInteger(Name, StrToIntDef(Val, 0));
      end;
      MyReg.CloseKey;
    finally
      MyReg.Free;
    end;
  except
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ReadFromReg(theRoot: HKEY; Key, Name: WideString; Typ: Integer; DefaultVal: WideString;
  CanCreate: Boolean): WideString;

var
  MyReg: TTntRegistry;

begin
  try
    MyReg := TTntRegistry.Create(KEY_READ);
    MyReg.RootKey := theRoot;
    try
      MyReg.OpenKey(Key, CanCreate);
      case Typ of
        0: ReadFromReg := MyReg.ReadString(Name);
        1: ReadFromReg := IntToStr(MyReg.ReadInteger(Name));
      end;
      MyReg.CloseKey;
    finally
      MyReg.Free;
    end;
  except
    ReadFromReg := DefaultVal;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetSystemDir: WideString;

var
  SysDir: array[0..MAX_PATH] of Char;

begin
  GetSystemDirectory(SysDir, MAX_PATH);

  GetSystemDir := IncludeTrailingPathDelimiter(SysDir);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetWindowsDir: WideString;

var
  WinDir: array[0..MAX_PATH] of Char;

begin
  GetWindowsDirectory(WinDir, MAX_PATH);

  GetWindowsDir := IncludeTrailingPathDelimiter(WinDir);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetApplDir: WideString;

begin
  Result := WideIncludeTrailingBackslash(
    WideExtractFilePath(TntApplication.ExeName));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetSpecialFolder(Folder: Integer): WideString;

// Reads the file name of the special (system) folder given by the id in Folder.

var
  pMalloc: IMalloc;
  pidl: PItemIDList;
  Buffer: array[0..MAX_PATH] of WideChar;

begin
  // We need the IMalloc interface pointer to free memory allocated by the shell.
  if (SHGetMalloc(pMalloc) <> S_OK) then
    raise EInOutError.Create('Couldn''t get pointer to IMalloc interface.')
  else
  begin
    SHGetSpecialFolderLocation(0, Folder, pidl);
    SHGetPathFromIDListW(pidl, Buffer);
    Result := Buffer;

    // Free memory allocated by SHGetSpecialFolderLocation.
    pMalloc.Free(pidl);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetProgramFilesDir: WideString;

begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PROGRAM_FILES));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetCommonProgFilesDir: WideString;

begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMON));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetApplicationDataDir: WideString;

begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_APPDATA));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetHomeDir: WideString;

begin
  Result := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PERSONAL));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetTempDir: WideString;

var
  Buffer: array[0..Max_path] of char;

begin
  FillChar(Buffer, Max_Path + 1, 0);
  GetTempPath(Max_path, Buffer);
  Result := IncludeTrailingPathDelimiter(string(Buffer));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetDefaultBrowser: WideString;

var
  tmp: PChar;
  res: PChar;
  s: WideString;
  Reg: TTntRegistry;
  KeyName: WideString;

begin
  Reg := TTntRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    KeyName := 'htmlfile\shell\open\command';

    if (Reg.OpenKey(KeyName, False)) then
    begin
      s := Reg.ReadString('');
      Reg.CloseKey;
    end
    else
    begin
      tmp := StrAlloc(255);
      res := StrAlloc(255);

      try
        GetTempPath(255, tmp);
        FileCreate(tmp + 'htmpl.html');
        FindExecutable('htmpl.html', tmp, Res);
        s := ExtractFilePath(res) + ExtractFileName(res);
        SysUtils.DeleteFile(tmp + 'htmpl.html');
      finally
        StrDispose(tmp);
        StrDispose(res);
      end;
    end;
  finally
    Reg.Free;
  end;

  if (s = '') then
    s := 'explorer';

  Result := s;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetExeByExtension(FileExtension: WideString): WideString;

var
  ExtDesc: WideString;

begin
  with TTntRegistry.Create do
  begin
    try
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKeyReadOnly(FileExtension) then
      begin
        ExtDesc := ReadString('');
        CloseKey;

        if OpenKeyReadOnly(ExtDesc + '\Shell\Open\Command') then
        begin
          Result := ReadString('');
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;

  if (Result <> '') then
    if (Result[1] = '"') then
      Result := Copy(Result, 2, -1 + Pos('"', Copy(Result, 2, MaxINT)));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseCommandLine(Line: WideString; Tokens: TTntStringList);

// Splits the given command line into individual tokens. As usual, spaces separate entries but not when they are in quotes.

var
  S: WideString;
  Token: WideString;
  Head: PWideChar;
  Tail: PWideChar;
  QuoteChar: WideChar;

begin
  Head := PWideChar(Line);
  while Head^ <> #0 do
  begin
    // Skip leading white spaces.
    while Head^ in [WideChar(' '), WideChar(#8), WideChar(#13), WideChar(#10)] do
      Inc(Head);

    if Head^ <> #0 then
    begin
      // Now go for the quoted and unquoted parts. The can follow each other
      // in any combination. Keep scanning until we reach the end of the input or a white space
      // which is not in quotes.
      Token := '';
      repeat
        if Head^ in [WideChar(''''), WideChar('"')] then
        begin
          // Quoted part.
          QuoteChar := Head^;
          Inc(Head);

          if Head^ = #0 then
            Exit; // Asymmetric quote characters.

          Tail := Head;
          while not (Tail^ in [QuoteChar, WideChar(#0)]) do
            Inc(Tail);

          // Copy the string without quotes.
          SetString(S, Head, Tail - Head);

          if Tail^ <> #0 then
            Inc(Tail);
        end
        else
        begin
          // Unquoted part.
          Tail := Head + 1;
          while not (Tail^ in [WideChar(' '), WideChar(#8), WideChar(#13), WideChar(#10), WideChar(#0),
            WideChar(''''), WideChar('"')]) do
            Inc(Tail);
          SetString(S, Head, Tail - Head);
        end;

        Head := Tail;
        Token := Token + S;
      until Tail^ in [WideChar(' '), WideChar(#8), WideChar(#13), WideChar(#10), WideChar(#0)];

      Tokens.Add(Token);

      Head := Tail;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

// Not defined in Windows.pas
function GetLongPathNameW(lpszLongPath: PWideChar; lpszShortPath: PWideChar; cchBuffer: DWORD): DWORD; stdcall; external 'Kernel32.dll';

function ShortPathToLongPath(const Path: WideString): WideString;

// Converts the given path to its long form (if that exists). If Path is not an existing path or the long form
// does not exist for any reason the Path itself is returned.

var
  Buffer: array[0..MAX_PATH] of WideChar;
  Count: DWORD;

begin
  Count := GetLongPathNameW(PWideChar(Path), Buffer, SizeOf(Buffer));
  SetString(Result, Buffer, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

function StringAlignLeft(s: WideString; len: Integer): WideString;

begin
  if (Length(s) > len) then
    Result := Copy(s, 1, len)
  else
    Result := s + StringOfChar(' ', len - Length(s));
end;

//----------------------------------------------------------------------------------------------------------------------

function StringAlignRight(s: WideString; len: Integer): WideString;

begin
  if (Length(s) > len) then
    Result := Copy(s, 1, len)
  else
    Result := StringOfChar(' ', len - Length(s)) + s;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CopyDiskFile(sourcefile, destinationfile: WideString; PromtBeforeOverwrite: Boolean);

var
  NewFile: TTntFileStream;
  OldFile: TTntFileStream;

begin
  if (FileExists(sourcefile)) then
  begin
    if (FileExists(destinationfile)) and (PromtBeforeOverwrite) then
    begin
      if (MessageDlg(Format('The destination file %s does already exist. ' +
        'Do you want to overwrite this file?', [destinationfile]), mtCustom, [mbYes, mbNo], 0) = 3) then
        DeleteFile(destinationfile)
      else
        Exit;
    end;

    OldFile := TTntFileStream.Create(sourcefile, fmOpenRead or fmShareDenyWrite);
    try
      NewFile := TTntFileStream.Create(destinationfile, fmCreate {or fmShareDenyRead});

      try
        NewFile.CopyFrom(OldFile, OldFile.Size);
      finally
        FreeAndNil(NewFile);
      end;
    finally
      FreeAndNil(OldFile);
    end;
  end
  else
    MessageDlg(Format('The source file %s does not exist.', [sourcefile]), mtError, [mbOK], 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ShowHelp;

begin
  CreateSubProcess('hh ' + ChangeFileExt(Application.ExeName, '.chm'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DeleteComponentWithChildren(Component: TComponent; RemoveFromList: TList);

begin
  while (Component.ComponentCount > 0) do
  begin
    DeleteComponentWithChildren(Component.Components[0]);
  end;

  if (RemoveFromList <> nil) then
    if (RemoveFromList.IndexOf(Component) <> -1) then
      RemoveFromList.Delete(RemoveFromList.IndexOf(Component));

  Component.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetComputerNetName: WideString;

var
  buffer: array[0..255] of char;
  size: dword;

begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;

//----------------------------------------------------------------------------------------------------------------------

function GetWideStringTextWidth(Canvas: TCanvas; Text: WideString): Integer;

begin
  Result := GetWideStringTextWidth(Canvas.Handle, Text);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetWideStringTextWidth(DC: THandle; Text: WideString): Integer;

var
  Size: TSize;

begin
  GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Size);

  Result := Size.cx;
end;

//----------------------------------------------------------------------------------------------------------------------

// This function is taken from VirtualTrees, (C) Mike Lischke and was modified

procedure DrawWideStringText(DC: HDC; lpString: PWideChar; nCount: Integer; lpRect: TRect; uFormat: Cardinal;
  AdjustRight: Boolean);

// This procedure implements a subset of Window's DrawText API for Unicode which is not available for
// Windows 9x. For a description of the parameters see DrawText in the online help.
// Supported flags are currently:
//   - DT_LEFT
//   - DT_TOP
//   - DT_CALCRECT
//   - DT_NOCLIP
//   - DT_RTLREADING
//   - DT_SINGLELINE
//   - DT_VCENTER
// Differences to the DrawTextW Windows API:
//   - The additional parameter AdjustRight determines whether to adjust the right border of the given rectangle to
//     accomodate the largest line in the text. It has only a meaning if also DT_CALCRECT is specified.

var
  Head, Tail: PWideChar;
  Size: TSize;
  MaxWidth: Integer;
  TextOutFlags: Integer;
  TextAlign,
  OldTextAlign: Cardinal;
  TM: TTextMetric;
  TextHeight: Integer;
  LineRect: TRect;
  TextPosY,
  TextPosX: Integer;

  CalculateRect: Boolean;

begin
  // Prepare some work variables.
  MaxWidth := 0;
  Head := lpString;
  GetTextMetrics(DC, TM);
  TextHeight := TM.tmHeight;
  if uFormat and DT_SINGLELINE <> 0 then
    LineRect := lpRect
  else
    LineRect := Rect(lpRect.Left, lpRect.Top, lpRect.Right, lpRect.Top + TextHeight);

  CalculateRect := uFormat and DT_CALCRECT <> 0;

  // Prepare text output.
  TextOutFlags := 0;
  if uFormat and DT_NOCLIP = 0 then
    TextOutFlags := TextOutFlags or ETO_CLIPPED;
  if uFormat and DT_RTLREADING <> 0 then
    TextOutFlags := TextOutFlags or ETO_RTLREADING;

  // Determine horizontal and vertical text alignment.
  OldTextAlign := GetTextAlign(DC);
  TextAlign := TA_LEFT or TA_TOP;
  TextPosX := lpRect.Left;
  if uFormat and DT_RIGHT <> 0 then
  begin
    TextAlign := TextAlign or TA_RIGHT and not TA_LEFT;
    TextPosX := lpRect.Right;
  end
  else
    if uFormat and DT_CENTER <> 0 then
    begin
      TextAlign := TextAlign or TA_CENTER and not TA_LEFT;
      TextPosX := (lpRect.Left + lpRect.Right) div 2;
    end;

  TextPosY := lpRect.Top;
  if uFormat and DT_VCENTER <> 0 then
  begin
    // Note: vertical alignment does only work with single line text ouput!
    TextPosY := (lpRect.Top + lpRect.Bottom - TextHeight) div 2;
  end;
  SetTextAlign(DC, TextAlign);

  if uFormat and DT_SINGLELINE <> 0 then
  begin
    if CalculateRect then
    begin
      GetTextExtentPoint32W(DC, Head, nCount, Size);
      if Size.cx > MaxWidth then
        MaxWidth := Size.cx;
    end
    else
      ExtTextOutW(DC, TextPosX, TextPosY, TextOutFlags, @LineRect, Head, nCount, nil);
    OffsetRect(LineRect, 0, TextHeight);
  end
  else
  begin
    while (nCount > 0) and (Head^ <> WideChar(#0)) do
    begin
      Tail := Head;
      // Look for the end of the current line. A line is finished either by the WideString end or a line break.
      while (nCount > 0) and not (Tail^ in [WideChar(#0), WideChar(#13), WideChar(#10)]) and (Tail^ <> WideChar(#2028))
        do
      begin
        Inc(Tail);
        Dec(nCount);
      end;

      if CalculateRect then
      begin
        GetTextExtentPoint32W(DC, Head, Tail - Head, Size);
        if Size.cx > MaxWidth then
          MaxWidth := Size.cx;
      end
      else
        ExtTextOutW(DC, TextPosX, LineRect.Top, TextOutFlags, @LineRect, Head, Tail - Head, nil);
      OffsetRect(LineRect, 0, TextHeight);

      // Get out of the loop if the rectangle is filled up.
      if (nCount = 0) or (not CalculateRect and (LineRect.Top >= lpRect.Bottom)) then
        Break;

      if (nCount > 0) and (Tail^ = WideChar(#13)) or (Tail^ = WideChar(#2028)) then
      begin
        Inc(Tail);
        Dec(nCount);
      end;

      if (nCount > 0) and (Tail^ = WideChar(#10)) then
      begin
        Inc(Tail);
        Dec(nCount);
      end;
      Head := Tail;
    end;
  end;

  SetTextAlign(DC, OldTextAlign);
  if CalculateRect then
  begin
    if AdjustRight then
      lpRect.Right := lpRect.Left + MaxWidth;
    lpRect.Bottom := LineRect.Top;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function BreakLine(Canvas: TCanvas; var S: WideString; Width: Integer): WideString;

var
  i, j, w, l: Integer;

begin
  w := GetWideStringTextWidth(Canvas, S);

  if (w < Width) then
  begin
    Result := Trim(S);
    S := '';
  end
  else
  begin
    l := Length(S);
    i := Round(l / (w / Width));

    //Search for next space
    j := 0;
    while (Copy(S, i + j, 1) <> ' ') and (i + j < l) do
      inc(j);

    //If not found, or too long, search for prior space
    if (i + j >= l) or (GetWideStringTextWidth(Canvas, Copy(S, 1, i + j)) > Width) then
    begin
      j := 0;
      while (Copy(S, i + j, 1) <> ' ') and (i + j > 1) do
        dec(j);
    end;

    Result := Trim(Copy(S, 1, i + j));
    S := Copy(S, i + j, l);
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

function IsWindowsVista: Boolean;

begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsWinXP: Boolean;

begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsWin2k: Boolean;

begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) and (Win32MinorVersion = 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsWinNT: Boolean;

begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion = 4);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsWinNTPlatform: Boolean;

begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SendToEventLog(Msg: WideString; MessageType: Integer);

// Note: this procedure requires admin rights in the system if the event source has not been registered yet.

var
  hEventSvc: Cardinal;
  Registry: TTntRegistry;
  MyMsg: array[0..2] of PWideChar;

begin
  Registry := TTntRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if (not (Registry.OpenKey('System\CurrentControlSet\Services\EventLog\Application\MySQL Administrator', False)))
      then
    begin
      Registry.OpenKey('System\CurrentControlSet\Services\EventLog\Application\MySQL Administrator', True);
      Registry.WriteString('EventMessageFile', ExtractFilePath(Application.ExeName) + 'libmysqladminmsg.dll');
      Registry.WriteInteger('TypesSupported', 7);
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;

  hEventSvc := RegisterEventSource(nil, 'MySQL Administrator');
  if (hEventSvc <> 0) then
  begin
    try
      MyMsg[0] := PWideChar(Msg);
      MyMsg[1] := nil;

      if not (ReportEventW(hEventSvc, MessageType, 0, 1, nil, 1, 0, @MyMsg, nil)) then
      begin
        raise Exception.Create('Unable to write to the event log. (' + IntToStr(GetLastError) + ')' + #13#10 +
          SysErrorMessage(GetLastError));
      end;
    finally
      DeregisterEventSource(hEventSvc);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsConnectedToInternet: Boolean;

var
  dwConnectionTypes: DWORD;

begin
  dwConnectionTypes := INTERNET_CONNECTION_MODEM +
    INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;

  Result := InternetGetConnectedState(@dwConnectionTypes, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function FormatLibraryVersion(LibVersion: Integer): WideString;

var
  MajorVersion,
  MinorVersion,
  SubVersion: WideString;

begin
  MajorVersion := IntToStr(LibVersion div 10000);
  MinorVersion := IntToStr(LibVersion div 100 - (LibVersion div 10000) * 100);
  SubVersion := IntToStr(LibVersion - (LibVersion div 100) * 100);

  Result := MajorVersion + '.' + MinorVersion + '.' + SubVersion;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure BrowseWebPage(URL: WideString);

begin
  if (CompareText(Copy(URL, 1, 4), 'http') <> 0) then
    URL := URL + 'http:\\' + URL;

  ShellExecuteW(Application.Handle, 'open', PWideChar(URL),
    nil, nil, SW_NORMAL);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxModalDialog.Create(Titel: WideString; Messagetext: WideString;
  DlgType: MYX_MESSAGE_DLG_TYPE;
  buttons: WideString;
  doEdit: Boolean; editCaption: WideString; var editValue: WideString;
  editLinesNum: Integer = 1; editReadOnly: Boolean = False;
  ShowDisableMessageCheckbox: Boolean = False;
  PNGImgResName: WideString = '');

var
  DlgBtn: TTntButton;
  DlgButtonCaptions: TTntStringList;
  MessageLines: TTntStringList;
  I, W,
    PNGImgHeight: Integer;
  TotalBtnWidth,
    CurrentBtnWidth: Integer;
  Size: TSize;
  R: TRect;
  MaxMessageWidth: Integer;

begin
  CreateNew(nil);

  DisableMessageCheckbox := nil;

  InitForm(self);

  OnKeyDown := DoKeyDown;
  KeyPreview := True;

  self.doEdit := doEdit;

  DlgButtonCaptions := TTntStringList.Create;
  MessageLines := TTntStringList.Create;
  DlgBtnList := TList.Create;
  try
    MessageLines.Text := Messagetext;
    DlgButtonCaptions.Text := buttons;

    BorderStyle := bsDialog;
    Caption := Titel;
    Tag := 0;

    // Get number of text-Lines
    MaxMessageWidth := 0;
    for I := 0 to MessageLines.Count - 1 do
    begin
      GetTextExtentPoint32W(Canvas.Handle, PWideChar(MessageLines[I]), Length(MessageLines[I]), Size);
      if Size.cx > MaxMessageWidth then
        MaxMessageWidth := Size.cx;
    end;

    if MaxMessageWidth > 500 then
      MaxMessageWidth := 500;

    // Display message text.
    MessageLbl := TTntLabel.Create(self);
    MessageLbl.Parent := self;
    MessageLbl.Left := 80;
    MessageLbl.Top := 30;
    MessageLbl.Width := MaxMessageWidth;
    R := Rect(0, 0, MaxMessageWidth, 0);
    DrawTextW(Canvas.Handle, PWideChar(Messagetext), Length(Messagetext), R, DT_CALCRECT or DT_WORDBREAK);
    MessageLbl.Height := R.Bottom;
    MessageLbl.AutoSize := False;
    MessageLbl.WordWrap := True;
    MessageLbl.ShowHint := True;
    MessageLbl.Hint := MessageText;

    // Set message text after all settings are in place.
    MessageLbl.Caption := Messagetext;

    //Size the form according to the size of the label/buttons
    if (MessageLbl.Width + 80 + 32 > DlgButtonCaptions.Count * (80 + 10) + 42) then
      Width := MessageLbl.Width + 80 + 32
    else
      Width := DlgButtonCaptions.Count * (80 + 10) + 42;

    MessageLbl.Anchors := [akLeft, akTop, akRight];

    // Minimum height. Might be adjusted if editing is active.
    Height := 130 + MessageLbl.Height;

    //Add PNGImg
    if (PNGImgResName <> '') then
    begin
      InfoImage := TTntImage.Create(self);
      InfoImage.Parent := self;
      InfoImage.Top := MessageLbl.Top + MessageLbl.Height + 15;
      InfoImage.AutoSize := True;
      LoadPNGImageFromResource(PNGImgResName, InfoImage, True);

      InfoImage.Left := (Width - InfoImage.Width) div 2;

      PNGImgHeight := InfoImage.Height;

      Height := Height + PNGImgHeight + 20;
    end
    else
      PNGImgHeight := 0;

    //Create Edit
    if (doEdit) then
    begin
      if (editCaption <> '') then
      begin
        EditLbl := TTntLabel.Create(self);
        EditLbl.Parent := self;
        EditLbl.Left := 80;
        EditLbl.Top := MessageLbl.Top + MessageLbl.Height + 10 + PNGImgHeight + 10;
        GetTextExtentPoint32W(Canvas.Handle, PWideChar(editCaption), Length(editCaption), Size);
        EditLbl.Width := Size.cx;
        EditLbl.Caption := editCaption;
      end;

      ValueMemo := TTntMemo.Create(self);
      ValueMemo.Parent := self;
      ValueMemo.Font := Self.Font;
      if (editCaption <> '') then
        ValueMemo.Left := EditLbl.Left + EditLbl.Width + 14
      else
        ValueMemo.Left := 80;
      ValueMemo.Top := MessageLbl.Top + MessageLbl.Height + 10 - 4 + PNGImgHeight + 10;

      ValueMemo.Width := Width - ValueMemo.Left - 30;
      ValueMemo.Height := editLinesNum * Abs(ValueMemo.Font.Height) + 10;
      ValueMemo.Text := editValue;
      Self.Height := Self.Height + ValueMemo.Height + 10;

      ActiveControl := ValueMemo;

      // Disable multiline functionality if editLinesNum is 1.
      if editLinesNum = 1 then
      begin
        ValueMemo.WordWrap := False;
        ValueMemo.WantReturns := False;
      end;

      if (editReadOnly) then
      begin
        ValueMemo.ReadOnly := True;
        ValueMemo.Color := Color;
      end;

      if (ValueMemo.Text <> '') then
        ValueMemo.SelStart := Length(ValueMemo.Text);
    end
    else
      ValueMemo := nil;

    //Create DisableMessage Checkbox
    if (ShowDisableMessageCheckbox) then
    begin
      DisableMessageCheckbox := TTntCheckBox.Create(self);
      DisableMessageCheckbox.Parent := self;

      DisableMessageCheckbox.Left := 20;
      DisableMessageCheckbox.Top := Height - 70;
      DisableMessageCheckbox.Width := Width - 40;
      DisableMessageCheckbox.Caption := _('Do not show this message anymore.');

      Height := Height + 30;
    end;

    //Get total buttons width
    TotalBtnWidth := (DlgButtonCaptions.Count - 1) * 10;
    for I := 0 to DlgButtonCaptions.Count - 1 do
    begin
      W := Canvas.TextWidth(DlgButtonCaptions[I]) + 16;

      if W < 80 then
        W := 80;

      inc(TotalBtnWidth, W);
    end;

    //Create buttons
    CurrentBtnWidth := 0;
    for I := 0 to DlgButtonCaptions.Count - 1 do
    begin
      DlgBtn := TTntButton.Create(self);
      DlgBtn.Parent := self;
      DlgBtn.Caption := DlgButtonCaptions[I];

      W := Canvas.TextWidth(DlgButtonCaptions[I]) + 16;
      if W < 80 then
        W := 80;

      DlgBtn.Width := W;
      DlgBtn.Left := (Width - TotalBtnWidth) div 2 +
        CurrentBtnWidth;

      CurrentBtnWidth := CurrentBtnWidth + W + 10;

      {DlgBtn.Width:=80;
      DlgBtn.Left:=(Width-DlgButtonCaptions.Count*80+19) div 2+
        (80+10)*I;}

      DlgBtn.Top := Height - 70;
      DlgBtn.Height := 25;

      DlgBtn.ModalResult := I + 101;

      DlgBtn.Default := (I = 0);

      DlgBtnList.Add(DlgBtn);
    end;

    //Create image
    TypeImage := TTntImage.Create(self);
    TypeImage.Parent := self;
    TypeImage.Left := 20;
    TypeImage.Top := MessageLbl.Top - 5; // Just to have it nicely places move it up 5 pixels.
    case DlgType of
      myx_mtError:
        LoadPNGImageFromResource('messagedlg_error', TypeImage, True);
      myx_mtEdit:
        LoadPNGImageFromResource('messagedlg_edit', TypeImage, True);
      myx_mtConfirmation:
        LoadPNGImageFromResource('messagedlg_confirmation', TypeImage, True);
      myx_mtWarning:
        LoadPNGImageFromResource('messagedlg_warning', TypeImage, True);
    else
      // myx_mtInformation
      LoadPNGImageFromResource('messagedlg_information', TypeImage, True);
    end;

    if (DlgBtnList.Count > 0) and ((not (doEdit)) or (editReadOnly)) then
      ActiveControl := DlgBtnList[0];

    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    Position := poScreenCenter;
  finally
    MessageLines.Free;
    DlgButtonCaptions.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMyxModalDialog.Destroy;

begin
  DlgBtnList.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxModalDialog.doButtonClick(Sender: TObject);

begin
  TTntForm(Parent).Tag := ModalResult;
  TTntForm(Parent).Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxModalDialog.HideEdit;

var
  i: Integer;

begin
  ValueMemo.Visible := False;

  Height := MessageLbl.Top + MessageLbl.Height + 100;

  for i := 0 to DlgBtnList.Count - 1 do
    TTntButton(DlgBtnList[i]).Top := Height - 70;

  if (DlgBtnList.Count > 0) then
    ActiveControl := DlgBtnList[0];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxModalDialog.ShowEdit;

var
  i: Integer;

begin
  Height := ValueMemo.Top + ValueMemo.Height + 80;

  for i := 0 to DlgBtnList.Count - 1 do
    TTntButton(DlgBtnList[i]).Top := Height - 70;

  ValueMemo.Visible := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxModalDialog.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (Key = Ord('C')) and (ssCtrl in Shift) and not doEdit then
    Clipboard.AsText := MessageLbl.Caption
  else
    if Key = VK_ESCAPE then
      ModalResult := mrCancel;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetCharFromVKey(vkey: Word): WideString;

var
  keystate: TKeyboardState;
  retcode: Integer;

begin
  GetKeyboardState(keystate);
  SetLength(Result, 2);
  retcode := ToAscii(vkey, MapVirtualKey(vkey, 0), keystate, @Result[1], 0);
  case retcode of
    0: Result := ''; // no character
    1: SetLength(Result, 1);
    2: ;
  else
    Result := ''; // retcode < 0 indicates a dead key
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetTransparentImgList(size: Integer): TImageList;

var
  ImgList: TImageList;
  Bitmap: TBitmap;

begin
  ImgList := TImageList.Create(nil);
  ImgList.Width := size;
  ImgList.Height := size;
  Bitmap := TBitmap.Create;
  Bitmap.Width := size;
  Bitmap.Height := size;
  Bitmap.TransparentMode := tmFixed;
  Bitmap.TransparentColor := clBlack;
  ImgList.AddMasked(Bitmap, clBlack);
  Bitmap.Free;

  Result := ImgList;
end;

//----------------------------------------------------------------------------------------------------------------------

function ReplaceTags(s: WideString; tags: WideString): WideString;

var
  TagList: TTntStringList;
  i: Integer;

begin
  TagList := TTntStringList.Create;
  try
    TagList.Text := tags;
    for i := 0 to TagList.Count - 1 do
      s := AnsiReplaceText(s, TagList.Names[i], TagList.ValueFromIndex[i]);
  finally
    TagList.Free;
  end;

  Result := s;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure CreateShortcut(ShortcutName: string; ShortcutDirectory: string; DestinationFile: string; WorkingDir: string;
  Arguments: string);

var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  WFileName: WideString;
  
begin
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink := MyObject as IShellLink;
  MyPFile := MyObject as IPersistFile;

  with MySLink do
  begin
    SetArguments(PChar(Arguments));
    SetPath(PChar(DestinationFile));
    SetWorkingDirectory(PChar(ExtractFilePath(WorkingDir)));
  end;

  WFileName := IncludeTrailingPathDelimiter(ShortcutDirectory) +
    ShortcutName + '.lnk';
  MyPFile.Save(PWideChar(WFileName), False);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsAcrobatInstalled: Boolean; //Adobe Acrobat Reader

var
  reg: TTntRegistry;

begin
  Result := false;
  reg := TTntRegistry.Create(KEY_READ);
  with reg do
  begin
    try
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKey('CLSID\{CA8A9780-280D-11CF-A24D-444553540000}', False) then
        Result := true
    finally
      CloseKey;
      Free
    end
  end
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ShowPDF(Filename: WideString);

begin
  ShellExecuteW(Application.Handle, 'open', 'acrobat.exe', PWideChar(Filename), nil, SW_SHOWNORMAL);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure PrintPDF(Filename: WideString);

begin
  ShellExecuteW(0, 'open', 'acrord32', PWideChar('/p /h ' + Filename), nil, SW_HIDE);
end;

//----------------------------------------------------------------------------------------------------------------------

function IsKeyDown(Key: Word): Boolean;

const
  HighOrderBit = 15;

type
  BitSet = set of 0..15;
begin
  result := (HighOrderBit in BitSet(Word(GetKeyState(Key))));
end;

//----------------------------------------------------------------------------------------------------------------------

{function CreateVaList(const Args: array of const): Pointer;
var VaList: array of Pointer;
  i: Integer;
begin
  SetLength(VaList, Length(Args));

  for i:=0 to High(Args) do
    case Args[i].VType of
      vtInteger:
        VaList[i]:=PInteger(Args[i].VInteger);
      vtBoolean:
        VaList[i]:=PBoolean(Args[i].VBoolean);
      vtInt64:
        VaList[i]:=PInt64(Args[i].VInt64);
      vtPointer:
        VaList[i]:=Args[i].VPointer;
      vtAnsiString:
        VaList[i]:=PChar(Args[i].VAnsiString);
      vtExtended:
        VaList[i]:=PExtended(Args[i].VExtended);
      vtPChar:
        VaList[i]:=Args[i].VPChar;
    else
      raise EInOutError.Create('This variant argument is not supported.');
    end;

  Result:=VaList;
end;}

//----------------------------------------------------------------------------------------------------------------------

function CreateVaList(const Args: array of const): PChar;

var
  I: Integer;
  Pos: Integer;
  P: PChar;

  function IntSizeOf(N: Integer): Integer;

  begin
    Result := (N + SizeOf(Integer) - 1) and not (SizeOf(Integer) - 1);
  end;

begin
   // allocate more than enough
  I := SizeOf(Extended) * (Length(Args) + 1);
  GetMem(Result, I);
  FillChar(Result[0], I, #0);
  Pos := 0;
  for I := Low(Args) to High(Args) do
    case Args[I].VType of
      vtInteger:
        begin
          Move(Args[I].VInteger, Result[Pos], SizeOf(Integer));
          Inc(Pos, IntSizeOf(SizeOf(Integer)));
        end;
      vtChar:
        begin
          Move(Args[I].VChar, Result[Pos], SizeOf(Char));
          Inc(Pos, IntSizeOf(SizeOf(Char)));
        end;
      vtExtended:
        begin
          Move(Args[I].VExtended, Result[Pos], SizeOf(Extended));
          Inc(Pos, IntSizeOf(SizeOf(Extended)));
        end;
      vtPointer:
        begin
          Move(Args[I].VPointer, Result[Pos], SizeOf(Pointer));
          Inc(Pos, IntSizeOf(SizeOf(Pointer)));
        end;
      vtPChar:
        begin
          Move(Args[I].VPChar, Result[Pos], SizeOf(PChar));
          Inc(Pos, IntSizeOf(SizeOf(PChar)));
        end;
      vtAnsiString:
        begin
           // this one has to be checked
          Move(Args[I].VAnsiString, Result[Pos], SizeOf(Pointer));
          Inc(Pos, IntSizeOf(SizeOf(Pointer)));
        end;
      vtInt64:
        begin
           // this one is guessed
          Move(Args[I].VInt64^, Result[Pos], SizeOf(Int64));
          Inc(Pos, IntSizeOf(SizeOf(Int64)));
        end;
    else
      raise Exception.Create('Unsupported variant argument');
    end;

   // add an extra NULL pointer
  P := nil;
  Move(P, Result[Pos], SizeOf(Pointer));
  Inc(Pos, IntSizeOf(SizeOf(Pointer)));
  ReallocMem(Result, Pos);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RegisterFileType(ExtName: WideString; AppName: WideString; IconNr: Integer; Params: WideString);

// Registers the given application with a particular extension on a per-user-basis.
// No admin rights required to set the association.

var
  Reg: TTntRegistry;

begin
  Reg := TTntRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      OpenKey('\Software\Classes\.' + ExtName, True);
      WriteString('', ExtName + 'file');
      CloseKey;

      OpenKey('\Software\Classes\' + ExtName + 'file\DefaultIcon', True);
      WriteString('', AppName + ',' + IntToStr(IconNr));
      CloseKey;

      OpenKey('\Software\Classes\' + ExtName + 'file\shell\open\command', True);
      if (Params = '') then
        WriteString('', AppName + ' "%1"')
      else
        WriteString('', AppName + ' ' + Params);
      CloseKey;
    end;
  finally
    Reg.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AddToFile(Filename: WideString; S: WideString);

var
  F: TextFile;

begin
  AssignFile(F, Filename);

  if (FileExists(Filename)) then
    Append(F)
  else
    Rewrite(F);

  try
    Write(F, S);
  finally
    CloseFile(F);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetHTMLReplaceStringList: TStringList;

var
  NamedChars: TStringList;

begin
  NamedChars := TStringList.Create;
  NamedChars.Add('&amp;=&');
  NamedChars.Add('&quot;="');
  NamedChars.Add('&lt;=<');
  NamedChars.Add('&gt;=>');
  NamedChars.Add('&nbsp;= ');
  NamedChars.Add('&iexcl;=¡');
  NamedChars.Add('&cent;=¢');
  NamedChars.Add('&pound;=£');
  NamedChars.Add('&curren;=¤');
  NamedChars.Add('&yen;=¥');
  NamedChars.Add('&brvbar;=¦');
  NamedChars.Add('&sect;=§');
  NamedChars.Add('&uml;=¨');
  NamedChars.Add('&copy;=©');
  NamedChars.Add('&ordf;=ª');
  NamedChars.Add('&laquo;=«');
  NamedChars.Add('&not;=¬');
  NamedChars.Add('&shy;=-');
  NamedChars.Add('&reg;=®');
  NamedChars.Add('&macr;=¯');
  NamedChars.Add('&deg;=°');
  NamedChars.Add('&plusmn;=±');
  NamedChars.Add('&sup2;=²');
  NamedChars.Add('&sup3;=³');
  NamedChars.Add('&acute;=´');
  NamedChars.Add('&micro;=µ');
  NamedChars.Add('&para;=¶');
  NamedChars.Add('&middot;=·');
  NamedChars.Add('&cedil;=¸');
  NamedChars.Add('&sup1;=¹');
  NamedChars.Add('&ordm;=º');
  NamedChars.Add('&raquo;=»');
  NamedChars.Add('&frac14;=¼');
  NamedChars.Add('&frac12;=½');
  NamedChars.Add('&frac34;=¾');
  NamedChars.Add('&iquest;=¿');
  NamedChars.Add('&Agrave;=À');
  NamedChars.Add('&Aacute;=Á');
  NamedChars.Add('&Acirc;=Â');
  NamedChars.Add('&Atilde;=Ã');
  NamedChars.Add('&Auml;=Ä');
  NamedChars.Add('&Aring;=Å');
  NamedChars.Add('&AElig;=Æ');
  NamedChars.Add('&Ccedil;=Ç');
  NamedChars.Add('&Egrave;=È');
  NamedChars.Add('&Eacute;=É');
  NamedChars.Add('&Ecirc;=Ê');
  NamedChars.Add('&Euml;=Ë');
  NamedChars.Add('&Igrave;=Ì');
  NamedChars.Add('&Iacute;=Í');
  NamedChars.Add('&Icirc;=Î');
  NamedChars.Add('&Iuml;=Ï');
  NamedChars.Add('&ETH;=Ð');
  NamedChars.Add('&Ntilde;=Ñ');
  NamedChars.Add('&Ograve;=Ò');
  NamedChars.Add('&Oacute;=Ó');
  NamedChars.Add('&Ocirc;=Ô');
  NamedChars.Add('&Otilde;=Õ');
  NamedChars.Add('&Ouml;=Ö');
  NamedChars.Add('&times;=×');
  NamedChars.Add('&Oslash;=Ø');
  NamedChars.Add('&Ugrave;=Ù');
  NamedChars.Add('&Uacute;=Ú');
  NamedChars.Add('&Ucirc;=Û');
  NamedChars.Add('&Uuml;=Ü');
  NamedChars.Add('&Yacute;=Ý');
  NamedChars.Add('&THORN;=Þ');
  NamedChars.Add('&szlig;=ß');
  NamedChars.Add('&agrave;=à');
  NamedChars.Add('&aacute;=á');
  NamedChars.Add('&acirc;=â');
  NamedChars.Add('&atilde;=ã');
  NamedChars.Add('&auml;=ä');
  NamedChars.Add('&aring;=å');
  NamedChars.Add('&aelig;=æ');
  NamedChars.Add('&ccedil;=ç');
  NamedChars.Add('&egrave;=è');
  NamedChars.Add('&eacute;=é');
  NamedChars.Add('&ecirc;=ê');
  NamedChars.Add('&euml;=ë');
  NamedChars.Add('&igrave;=ì');
  NamedChars.Add('&iacute;=í');
  NamedChars.Add('&icirc;=î');
  NamedChars.Add('&iuml;=ï');
  NamedChars.Add('&eth;=ð');
  NamedChars.Add('&ntilde;=ñ');
  NamedChars.Add('&ograve;=ò');
  NamedChars.Add('&oacute;=ó');
  NamedChars.Add('&ocirc;=ô');
  NamedChars.Add('&otilde;=õ');
  NamedChars.Add('&ouml;=ö');
  NamedChars.Add('&divide;=÷');
  NamedChars.Add('&oslash;=ø');
  NamedChars.Add('&ugrave;=ù');
  NamedChars.Add('&uacute;=ú');
  NamedChars.Add('&ucirc;=û');
  NamedChars.Add('&uuml;=ü');
  NamedChars.Add('&yacute;=ý');
  NamedChars.Add('&thorn;=þ');
  NamedChars.Add('&yuml;=ÿ');

  Result := NamedChars;
end;

//----------------------------------------------------------------------------------------------------------------------

function HTMLDecode(Source: string; NamedChars: TStringList): string;

var
  i: Integer;

begin
  Result := Source;

  if (NamedChars = nil) then
  begin
    NamedChars := GetHTMLReplaceStringList;
    try
      for i := 0 to NamedChars.Count - 1 do
        Result := AnsiReplaceStr(Result,
          NamedChars.Names[i], NamedChars.ValueFromIndex[i]);
    finally
      NamedChars.Free;
    end;
  end
  else
    for i := 0 to NamedChars.Count - 1 do
      Result := AnsiReplaceStr(Result,
        NamedChars.Names[i], NamedChars.ValueFromIndex[i]);

end;

//----------------------------------------------------------------------------------------------------------------------

function HTMLEncode(Source: string; NamedChars: TStringList; EncodeSpaces: Boolean): string;

var
  i: Integer;
  
begin
  Result := Source;

  if (NamedChars = nil) then
  begin
    NamedChars := GetHTMLReplaceStringList;
    try
      for i := NamedChars.Count - 1 downto 0 do
      begin
        if (NamedChars.Names[i] = '&nbsp;') and (not (EncodeSpaces)) then
          Continue;

        Result := AnsiReplaceStr(Result,
          NamedChars.ValueFromIndex[i], NamedChars.Names[i]);
      end;
    finally
      NamedChars.Free;
    end;
  end
  else
    for i := NamedChars.Count - 1 downto 0 do
    begin
      if (NamedChars.Names[i] = '&nbsp;') and (not (EncodeSpaces)) then
        Continue;

      Result := AnsiReplaceStr(Result,
        NamedChars.ValueFromIndex[i], NamedChars.Names[i]);
    end;

end;

//----------------------------------------------------------------------------------------------------------------------

function IsNumeric(S: WideString): Boolean;

var
  I, L: Integer;

begin
  Result := True;

  L := Length(S);

  for I := 1 to L do
    if ((Ord(S[I]) < Ord('0')) or (Ord(S[I]) > Ord('9'))) and
      (S[I] <> '.') and (S[I] <> ',') then
    begin
      Result := False;
      break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SetGlobalEnvironment(const Name, Value: WideString; const User: Boolean = True): Boolean;

// Used by the IC project.
// Note: admin rights are necessary to use this function.

resourcestring
  REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';
  REG_USER_LOCATION = 'Environment';

begin
  with TTntRegistry.Create do
  try
    if User then { User Environment Variable }
      Result := OpenKey(REG_USER_LOCATION, True)
    else { System Environment Variable }
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      Result := OpenKey(REG_MACHINE_LOCATION, True);
    end;

    if Result then
    begin
      WriteExpandString(Name, Value); { Write Registry for Global Environment }
        { Update Current Process Environment Variable }
      SetEnvironmentVariableW(PWideChar(Name), PWideChar(Value));
        { Send Message To All Top Window for Refresh }
      SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
    end;
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetGlobalEnvironment(const Name: WideString; const User: Boolean = True): WideString;

// Used by the IC project.
// Note: admin rights are necessary to use this function.

var
  Buffer: array[0..32767] of WideChar;
  Res: Cardinal;

resourcestring
  REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';

begin
  if (User) then
  begin
    Res := GetEnvironmentVariableW(PWideChar(Name),
      Buffer, 32767);

    if (Res > 0) then
      Result := Buffer
    else
      Result := '';
  end
  else
  begin
    with TTntRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if (OpenKey(REG_MACHINE_LOCATION, True)) then
      begin
        Result := ReadString(Name);
      end;
    finally
      Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetSubstringCount(Substring: WideString; Text: WideString): Integer;

var
  I, P, L: Integer;
  S: WideString;

begin
  I := 0;
  S := Text;
  L := Length(Substring);

  P := Pos(Substring, S);
  while (P > 0) do
  begin
    Inc(I);

    S := Copy(S, P + L + 1, MaxInt);
    P := Pos(Substring, S);
  end;

  Result := I;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetShellLinkFileName(const LinkFile: WideString): WideString;

var
  SL: IShellLinkWFix;
  PF: IPersistFile;
  FindData: TWin32FindDataW;
  WStr: array[0..MAX_PATH] of WideChar;

begin
  OleCheck(CoCreateInstance(CLSID_ShellLink, nil,
    CLSCTX_INPROC_SERVER, IShellLinkW, SL));

  PF := SL as IPersistFile;

  OleCheck(PF.Load(PWideChar(LinkFile), STGM_READ));

  OleCheck(SL.Resolve(0, SLR_ANY_MATCH or SLR_NO_UI));

  with SL do
  begin
    OleCheck(SL.GetPath(WStr, MAX_PATH, FindData, 0)); //SLGP_SHORTPATH));
    Result := WStr;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function FetchHttpRequest(Url: WideString; Username: WideString; Password: WideString; ProxyServer: WideString;
  ProxyBypass: WideString; ProxyUsername: WideString; ProxyPassword: WideString; var Error: Integer): string;

const
  SXH_PROXY_SET_PROXY = 2;
  HTTPREQUEST_SETCREDENTIALS_FOR_SERVER = 0;
  HTTPREQUEST_SETCREDENTIALS_FOR_PROXY = 1;

var
  WinHttpRequest: OLEVariant;
  errorCode: Integer;

begin
  // Initially set the return value of the function to 0
  //Result := 0;

  // Create the WinHttpRequest COM object
  WinHttpRequest := CreateOLEObject('WinHttp.WinHttpRequest.5.1');

  if (ProxyServer <> '') then
  begin
    //Set proxy server and bypass list
    errorCode := WinHttpRequest.setProxy(SXH_PROXY_SET_PROXY,
      ProxyServer, ProxyBypass);
    if (errorCode <> S_OK) then
      raise EInOutError.Create('Could not set Proxy server.');
  end;

  errorCode := WinHttpRequest.setAutoLogonPolicy(0);
  if (errorCode <> S_OK) then
    raise EInOutError.Create('Could not call setAutoLogonPolicy.');

  {errorCode := WinHttpRequest.setTimeouts(20000, 20000, 30000, 30000);
  if (errorCode <> S_OK) then
    raise EInOutError.Create('Could not set timeouts.');}

  errorCode := WinHttpRequest.Open('GET', url, False);
  if (errorCode <> S_OK) then
    raise EInOutError.Create('Could not send GET request.');

  if (Username <> '') or (Password <> '') then
  begin
    errorCode := WinHttpRequest.SetCredentials(
      Username, Password,
      HTTPREQUEST_SETCREDENTIALS_FOR_SERVER);
    if (errorCode <> S_OK) then
      raise EInOutError.Create('Could not call SetCredentials().');
  end;

  if (ProxyUsername <> '') then
  begin
    errorCode := WinHttpRequest.SetCredentials(
      ProxyUsername, ProxyPassword,
      HTTPREQUEST_SETCREDENTIALS_FOR_PROXY);
    if (errorCode <> S_OK) then
      raise EInOutError.Create('Could not call SetCredentials().');
  end;

  errorCode := WinHttpRequest.Send();
  if (errorCode <> S_OK) then
    raise EInOutError.Create('Could not call Send().');

  Result := WinHttpRequest.ResponseText;

  Error := WinHttpRequest.Status;
end;

//----------------------------------------------------------------------------------------------------------------------

function ColorToHtmlColor(Color: TColor): String;

var
  ColVal: LongInt;

begin
  ColVal := ColorToRGB(Color);
  Result := '#' +
    IntToHex(ColVal and $FF, 2) +
    IntToHex(ColVal shr 8 and $FF, 2) +
    IntToHex(ColVal shr 16 and $FF, 2);
end;


//----------------------------------------------------------------------------------------------------------------------

function HtmlColorToColor(Color: String): TColor;

begin
  if (Color = '') then
    Result := clGray
  else
    Result := StringToColor('$' +
      Copy(Color, 6, 2) +
      Copy(Color, 4, 2) +
      Copy(Color, 2, 2));
end;

//----------------------------------------------------------------------------------------------------------------------

function GetNewGUID: string;

var
  Guid: TGUID;
  s: string;
begin
  CreateGUID(Guid);

  try
    s:=GUIDToString(Guid);
    Result:=Copy(s, 2, Length(s)-2);
  except
    WriteLn('Error: Cannot get a new GUID.');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure LoadRtfTextFromResource(RichEdit: TTntRichEdit; Resourcename: WideString);

var
  tmpStream: TTntResourceStream;
  HResInfo: THandle;

begin
  HResInfo := FindResourceW(HInstance, PWideChar(Resourcename), 'RTF');
  if (HResInfo <> 0) then
  begin
    tmpStream := TTntResourceStream.Create(HInstance, Resourcename, 'RTF');
    try
      RichEdit.PlainText := False;
      RichEdit.Lines.LoadFromStream(tmpStream);
    finally
      tmpStream.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetTimeBias: integer;

var
  TZInfo : TTimeZoneInformation;
  TZID   : LongInt;

begin
  TZID := GetTimeZoneInformation (TZInfo);
  Result := TZInfo.Bias;   // bias is in minutes
  if TZID = TIME_ZONE_ID_DAYLIGHT then
    Result := Result + TZInfo.DaylightBias;
end;

//----------------------------------------------------------------------------------------------------------------------

function UTCtoLocalTime (UTCTime: TDateTime) : TDateTime;

begin
  Result := UTCTime - GetTimeBias / (24 * 60);
end;

//----------------------------------------------------------------------------------------------------------------------

function LocalTimeToUTC (LocalTime: TDateTime) : TDateTime;

begin
  Result := LocalTime + GetTimeBias / (24 * 60);
end;

//----------------------------------------------------------------------------------------------------------------------

// Part of the code below is collected from public domain, e.g by Kevin Solway.

procedure SetExplorerTheme(const AControl: TWinControl);

// Sets the standard explorer theme for the given windows control on Windows XP or Vista.

begin
  if IsWinXP or IsWindowsVista then
    SetWindowTheme(AControl.Handle, 'explorer', nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetVistaFonts(const AForm: TCustomForm);

// Sets the form's font to the default Vista font.

begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AForm.Font.Name, VistaFont)
    and (Screen.Fonts.IndexOf(VistaFont) >= 0) then
  begin
    AForm.Font.Size := AForm.Font.Size;
    AForm.Font.Name := VistaFont;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetVistaContentFonts(const AFont: TFont);

// Applies the default font for text content under Vista. AFont should be the font property of a memo
// rich edit etc.

begin
  if (IsWindowsVista or not CheckOSVerForFonts)
    and not SameText(AFont.Name, VistaContentFont)
    and (Screen.Fonts.IndexOf(VistaContentFont) >= 0) then
  begin
    AFont.Size := AFont.Size + 2;
    AFont.Name := VistaContentFont;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetDefaultFonts(const AFont: TFont);

// Simply applies the default GUI font.

begin
  AFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetDesktopIconFonts(const AFont: TFont);

// Sets the given font to be the same as the desktop icons font or, in case there is no such font or
// querying it fails for other reasons, uses default windows font.

var
  LogFont: TLogFont;

begin
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
    AFont.Handle := CreateFontIndirect(LogFont)
  else
    SetDefaultFonts(AFont);
end;

//----------------------------------------------------------------------------------------------------------------------

// Access to the desktop window manager (DWM).

const
  dwmapi = 'dwmapi.dll';
  DwmIsCompositionEnabledSig = 'DwmIsCompositionEnabled';
  DwmExtendFrameIntoClientAreaSig = 'DwmExtendFrameIntoClientArea';
  TaskDialogSig = 'TaskDialog';

type
  TCompositionState = (
    csEnabled,
    csUndetermined,
    csDisabled
  );

var
  CompositionState: TCompositionState = csUndetermined;

function CompositingEnabled: Boolean;

var
  DLLHandle: THandle;
  DwmIsCompositionEnabledProc: function(pfEnabled: PBoolean): HRESULT; stdcall;
  Enabled: Boolean;

begin
  Result := False;
  if IsWindowsVista and (CompositionState = csUndetermined) then
  begin
    DLLHandle := LoadLibrary(dwmapi);

    if DLLHandle <> 0 then
    begin
      @DwmIsCompositionEnabledProc := GetProcAddress(DLLHandle,
        DwmIsCompositionEnabledSig);

      if @DwmIsCompositionEnabledProc <> nil then
      begin
        DwmIsCompositionEnabledProc(@Enabled);
        if Enabled then
          CompositionState := csEnabled
        else
          CompositionState := csDisabled;
      end
      else
        CompositionState := csDisabled;

      FreeLibrary(DLLHandle);
    end
    else
      CompositionState := csDisabled;

    Result := CompositionState = csEnabled;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ExtendGlass(const AHandle: THandle; const AMargins: TRect);

// See DwmExtendFrameIntoClientArea (MSDN) for a description.
 
type
  _MARGINS = packed record
    cxLeftWidth: Integer;
    cxRightWidth: Integer;
    cyTopHeight: Integer;
    cyBottomHeight: Integer;
  end;
  PMargins = ^_MARGINS;
  TMargins = _MARGINS;
  
var
  DLLHandle: THandle;
  DwmExtendFrameIntoClientAreaProc: function(destWnd: HWND; const pMarInset: PMargins): HRESULT; stdcall;
  Margins: TMargins;

begin
  if IsWindowsVista and CompositingEnabled then
  begin
    DLLHandle := LoadLibrary(dwmapi);

    if DLLHandle <> 0 then
    begin
      @DwmExtendFrameIntoClientAreaProc := GetProcAddress(DLLHandle,
        DwmExtendFrameIntoClientAreaSig);

      if (@DwmExtendFrameIntoClientAreaProc <> nil) then
      begin
        ZeroMemory(@Margins, SizeOf(Margins));
        Margins.cxLeftWidth := AMargins.Left;
        Margins.cxRightWidth := AMargins.Right;
        Margins.cyTopHeight := AMargins.Top;
        Margins.cyBottomHeight := AMargins.Bottom;

        DwmExtendFrameIntoClientAreaProc(AHandle, @Margins);
      end;

      FreeLibrary(DLLHandle);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  ButtonToNative: array[TMsgDlgBtn] of Integer = (
    TD_BUTTON_YES, // mbYes
    TD_BUTTON_NO,  // mbNo
    TD_BUTTON_OK,  // mbOK
    TD_BUTTON_CANCEL, // mbCancel
    TD_BUTTON_CANCEL, // mbAbort
    TD_BUTTON_RETRY,  // mbRetry
    0, // mbIgnore
    0, // mbAll
    0, // mbNoToAll
    0, // mbYesToAll
    0  // mbHelp
  );

function TaskDialog(const AHandle: THandle; const ATitle, ADescription, AContent: WideString; const Icon: Integer;
  const Buttons: TMsgDlgButtons): Integer;

var
  DLLHandle: THandle;
  res: integer;
  Handled: Boolean;
  Dmy: string;
  DlgType: TMsgDlgType;
  TaskDialogProc: function(HWND: THandle; hInstance: THandle; cTitle, cDescription, cContent: pwidechar; Buttons: Integer;
    Icon: integer; ResButton: pinteger): integer; cdecl stdcall;
  NativeButtons: Integer;
  Button: TMsgDlgBtn;

begin
  Result := 0;
  Handled := False;
  
  if IsWindowsVista then
  begin
    DLLHandle := LoadLibrary(comctl32);
    if DLLHandle >= 32 then
    begin
      @TaskDialogProc := GetProcAddress(DLLHandle, TaskDialogSig);

      if Assigned(TaskDialogProc) then
      begin
        NativeButtons := 0;
        for Button := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
          if Button in Buttons then
            NativeButtons := NativeButtons + ButtonToNative[Button];

        TaskDialogProc(AHandle, 0, PWideChar(ATitle), PWideChar(ADescription), PWideChar(AContent), NativeButtons,
          Icon, @res);

        Result := mrOK;

        case res of
          TD_RESULT_CANCEL:
            Result := mrCancel;
          TD_RESULT_RETRY:
            Result := mrRetry;
          TD_RESULT_YES:
            Result := mrYes;
          TD_RESULT_NO:
            Result := mrNo;
          TD_RESULT_CLOSE:
            Result := mrAbort;
        end;
        Handled := True;
      end;
     FreeLibrary(DLLHandle);
    end;
  end;

  if not Handled then
  begin
    DlgType := mtCustom;

    case Icon of
      TD_ICON_WARNING:
        DlgType := mtWarning;
      TD_ICON_QUESTION:
        DlgType := mtConfirmation;
      TD_ICON_ERROR:
        DlgType := mtError;
      TD_ICON_INFORMATION:
        DlgType := mtInformation;
    end;

    Dmy := ADescription;
    if AContent <> '' then
     begin
       if Dmy <> '' then
         Dmy := Dmy + #$D#$A + #$D#$A;
       Dmy := Dmy + AContent;
     end;
    Result := MessageDlg(Dmy, DlgType, Buttons, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TaskMessage(const AHandle: THandle; AMessage: string);

begin
  TaskDialog(AHandle, '', '', AMessage, 0, [mbOk]);
end;

//----------------------------------------------------------------------------------------------------------------------

function OpenSaveFileDialog(Parent: TWinControl;
                            const DefExt,
                            Filter,
                            InitialDir,
                            Title: string;
                            var FileName: string;
                            MustExist,
                            OverwritePrompt,
                            NoChangeDir,
                            DoOpen: Boolean): Boolean;

var
  ofn: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;
  
begin
  Result := False;
  FillChar(ofn, SizeOf(TOpenFileName), 0);
  with ofn do
  begin
    lStructSize := SizeOf(TOpenFileName);
    hwndOwner := Parent.Handle;
    lpstrFile := szFile;
    nMaxFile := SizeOf(szFile);
    if (Title <> '') then
      lpstrTitle := PChar(Title);
    if (InitialDir <> '') then
      lpstrInitialDir := PChar(InitialDir);
    StrPCopy(lpstrFile, FileName);
    lpstrFilter := PChar(ReplaceStr(Filter, '|', #0)+#0#0);
    if DefExt <> '' then
      lpstrDefExt := PChar(DefExt);
  end;

  if MustExist then
    ofn.Flags := ofn.Flags or OFN_FILEMUSTEXIST;
  if OverwritePrompt then
    ofn.Flags := ofn.Flags or OFN_OVERWRITEPROMPT;
  if NoChangeDir then
    ofn.Flags := ofn.Flags or OFN_NOCHANGEDIR;

  if DoOpen then
  begin
    if GetOpenFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
  else
  begin
    if GetSaveFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

end.

