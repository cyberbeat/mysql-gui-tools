unit Instance;

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
// This unit contains code to prevent an application from starting more than once.
//
// The initial author is Mike Lischke.

interface

uses
  Windows, SysUtils, Classes;

const
  OtherInstanceMsgName = 'MySQLGeneralCommunicationMessage'; // Do not localize.
  OtherInstanceMutexName = 'MySQLMutexOtherInstance'; // Do not localize.

var
  // This message is posted to the (hidden) application window (and can be handled anywhere by hooking this window)
  // if another instance was started and forwarded execution to this instance.
  WM_OTHERINSTANCE: Cardinal;

function OtherInstanceIsRunning(const Name: WideString = ''): Boolean;
procedure GetInstanceParams(const List: TStringList);

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Forms;

const
  ParamSize = 32768; // The maximum number of Unicode characters that can be transferred from one instance to another.

type
  // Standard record containing general info shared between all instances.
  PInstanceInfo = ^TInstInfo;
  TInstInfo = record
    FirstInstance: THandle;             // Application handle of the first instance that created this record.
    ForeignExe: Boolean;                // Shows if this info was created for an application with different executable name
                                        // (True) or the same name as this application (False).
    Mutex: THandle;                     // Used for access synchronization between processes.
    Params: array[0..ParamSize - 1] of WideChar;
  end;

  // Short instance record used between different applications. Only a subset of the general case can be transferred.
  PShortInstInfo = ^TShortInstInfo;
  TShortInstInfo = record
    FirstInstance: THandle;
    ForeignExe: Boolean;
  end;

var
  MappingHandle: THandle;               // Non-shared file mapping handle used in this instance to access the shared data.

//----------------------------------------------------------------------------------------------------------------------

function OtherInstanceIsRunning(const Name: WideString = ''): Boolean;

// Checks whether another instance of a given application is running. It does this by asking for a file mapping object
// with either the given name or the name of this application (without path info). If this object exists then there is
// already an instance and True is returned. Otherwise the file mapping object is freshly created and this method returns false.

var
  Info: PInstanceInfo;
  CommandLine: PWideChar;
  InfoSize: Cardinal;
  MappingName: WideString;

begin
  // This test should not be necessary unless an application calls this method twice.
  Result := MappingHandle <> 0;
  if not Result then
  begin
    // Determine name to be used for the file mapping object.
    if Name = '' then
    begin
      InfoSize := SizeOf(TInstInfo);
      MappingName := ExtractFileName(Application.ExeName);
    end
    else
    begin
      InfoSize := SizeOf(TShortInstInfo);
      MappingName := Name;
    end;

    // Keep in mind CreateFileMappingW is not supported on Win 9x/Me!
    MappingHandle := CreateFileMappingW(DWORD(-1), nil, PAGE_READWRITE, 0, InfoSize, PWideChar(MappingName));
    if MappingHandle <> 0 then
    begin
      // Get a pointer to the shared memory area.
      Info := MapViewOfFile(MappingHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, InfoSize);
      // Did the mapping already exist?
      if GetLastError = ERROR_ALREADY_EXISTS then
      begin
        // Yes, there was already an instance that registered the shared area already. Pass it our command line and
        // get out of here.
        Result := True;
        // Command line can only be passed if we are dealing with an instance of the same application (actually, the
        // same application name was used for the shared area).
        if Name = '' then
        begin
          Info.ForeignExe := False;
          CommandLine := GetCommandLineW;

          // Synchronize access to the shared area.
          WaitForSingleObject(Info.Mutex, INFINITE);
          Move(CommandLine^, Info.Params, 2 * (lstrlenW(CommandLine) + 1)); // Move one char more for #0.
          ReleaseMutex(Info.Mutex);
        end
        else
          Info.ForeignExe := True;

        // Restore application window if it is minimized. Bring it to foreground and tell it that we were here.
        if IsIconic(Info.FirstInstance) then
          ShowWindow(Info.FirstInstance, SW_RESTORE);
        SetForegroundWindow(Info.FirstInstance);
        PostMessage(Info.FirstInstance, WM_OTHERINSTANCE, 0, 0);
      end
      else
      begin
        // This is the first instance. Store our info in the shared area.
        FillChar(Info^, InfoSize, 0);

        // The handle is that of the main form so WM_OTHERINSTANCE must be handled there if needed.
        Info.FirstInstance := Application.Handle;

        // Mutex is only needed if there is a shared memory area. And a shared area is only used for instances
        // of the same application.
        if Name = '' then
        begin
          Info.Mutex := CreateMutexW(nil, False, PWideChar(OtherInstanceMutexName + '#' + MappingName));
          Info.ForeignExe := False;
        end
        else
          Info.ForeignExe := True;
      end;
      // Free mapping to memory area.
      UnmapViewOfFile(Info);
    end
    else
      raise Exception.Create('Internal error (Instance check): file mapping failed');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure GetInstanceParams(const List: TStringList);

// Reads the current content of the shared memory area. The info is filled into the given string list.
// Note. TStringList is not Unicode aware so all strings are converted to ANSI using the current user locale.

var
  Info: PInstanceInfo;

begin
  if Assigned(List) and (MappingHandle <> 0) then
  begin
    Info := MapViewOfFile(MappingHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, SizeOf(TInstInfo));
    try
      if Info.ForeignExe then
        raise Exception.Create('internal error (Instance parameters): foreign executable');

      // Synchronize access to the shared area.
      WaitForSingleObject(Info.Mutex, INFINITE);
      List.Clear;
      List.CommaText := Info.Params;
      ReleaseMutex(Info.Mutex);
    finally
      UnmapViewOfFile(Info);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeInstanceInfo;

// Do some house keeping.

var
  Info: PInstanceInfo;

begin
  // Anything to do?
  if MappingHandle <> 0 then
  begin
    // The instance that created the mutex must also release it.
    Info := MapViewOfFile(MappingHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, SizeOf(TInstInfo));
    if Info = nil then
      Info := MapViewOfFile(MappingHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, SizeOf(TShortInstInfo));
    if (Info.FirstInstance = Application.Handle) and not Info.ForeignExe then
    begin
      // Wait until the mutex is free and then close it.
      WaitForSingleObject(Info.Mutex, INFINITE);
      CloseHandle(Info.Mutex);
    end;
    // Close the view and the mapping too.
    UnmapViewOfFile(Info);
    CloseHandle(MappingHandle);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  WM_OTHERINSTANCE := RegisterWindowMessage(OtherInstanceMsgName);
finalization
  FreeInstanceInfo;
end.

