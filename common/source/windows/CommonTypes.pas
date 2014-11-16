unit CommonTypes;

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
//
// This unit contains commonly used types, including definitions from the Windows SDK that are not yet available in Delphi.

interface

uses
  Windows, Messages,
  Unicode;
  
type
  // Tray notification definitions
  _NOTIFYICONDATAAIE5 = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of Char;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of Char;
    TimeoutOrVersion: UINT;
    szInfoTitle: array[0..63] of Char;
    dwInfoFlags: DWORD;
  end;

  _NOTIFYICONDATAAIE6 = record
    cbSize: Cardinal;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of Char;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of Char;
    TimeoutOrVersion: UINT;
    szInfoTitle: array[0..63] of Char;
    dwInfoFlags: DWORD;
    guidItem: TGUID;
  end;


  _NOTIFYICONDATAWIE5 = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of WideChar;
    TimeoutOrVersion: UINT;
    szInfoTitle: array[0..63] of WideChar;
    dwInfoFlags: DWORD;
  end;

  _NOTIFYICONDATAWIE6 = record
    cbSize: Cardinal;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of WideChar;
    TimeoutOrVersion: UINT;
    szInfoTitle: array[0..63] of WideChar;
    dwInfoFlags: DWORD;
    guidItem: TGUID;
  end;

  TNotifyIconDataAIE5 = _NOTIFYICONDATAAIE5;
  PNotifyIconDataAIE5 = ^TNotifyIconDataAIE5;
  TNotifyIconDataWIE5 = _NOTIFYICONDATAWIE5;
  PNotifyIconDataWIE5 = ^TNotifyIconDataWIE5;
  TNotifyIconDataAIE6 = _NOTIFYICONDATAAIE6;
  PNotifyIconDataAIE6 = ^TNotifyIconDataAIE6;
  TNotifyIconDataWIE6 = _NOTIFYICONDATAWIE6;
  PNotifyIconDataWIE6 = ^TNotifyIconDataWIE6;

const
  // >= IE 5.0
  NIN_SELECT    = WM_USER + 0;
  NINF_KEY      = $1;
  NIN_KEYSELECT = NIN_SELECT or NINF_KEY;

  // >= IE 5.01
  NIN_BALLOONSHOW      = WM_USER + 2;
  NIN_BALLOONHIDE      = WM_USER + 3;
  NIN_BALLOONTIMEOUT   = WM_USER + 4;
  NIN_BALLOONUSERCLICK = WM_USER + 5;

  // >= IE 5.0
  NIM_SETFOCUS       = $00000003;
  NIM_SETVERSION     = $00000004;
  NOTIFYICON_VERSION = 3;

  // >= IE 5.0
  NIF_STATE = $00000008;
  NIF_INFO  = $00000010;

  // >= IE 6.0
  NIF_GUID = $00000020;

  // >= IE 5.0
  NIS_HIDDEN     = $00000001;
  NIS_SHAREDICON = $00000002; // says this is the source of a shared icon

  // Notify Icon Infotip flags
  NIIF_NONE      = $00000000;
  // icon flags are mutually exclusive
  // and take only the lowest 2 bits
  NIIF_INFO      = $00000001;
  NIIF_WARNING   = $00000002;
  NIIF_ERROR     = $00000003;
  NIIF_ICON_MASK = $0000000F;
  // >= IE 5.01
  NIIF_NOSOUND   = $00000010;

type
  // IAdaptable is implemented by all classes that can provide special interfaces for customization or access.
  IAdaptable = interface
    ['{09A7F392-3416-4CB7-AE83-7EADB95CC1E2}']
    function GetAdapter(IID: TGUID): IInterface;
  end;

  IProgressMonitor = interface
    ['{E0CDA68D-29AA-4E9E-B15C-A4B26311F180}']
    procedure ProgressInit(const Max: Integer);
    procedure ProgressPosition(const Position: Integer);
    procedure ProgressStep(const Amount: Integer = 1);
    procedure ProgressFinish;
  end;

  // Helper interface to pass certain info about a schema between various classes that is to
  // avoid hard dependencies between them.
  ISchemaInfo = interface
    ['{22A442BE-B2D8-4885-A957-F79B42FF08C4}']
    // Returns a list of selected tables in the schema (used for GUI tasks).
    procedure GetSelectedTables(List: TWideStringList);
  end;
  
//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
 
